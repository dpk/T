(herald (back_end vaxrep)
  (env t (orbit_top defs)))

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer 
;;; Science Department.  Permission to copy this software, to redistribute it, 
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

(define (rep-analyze-top node)
  (rep-analyze ((call-arg 1) (lambda-body node)))
  (rep-analyze ((call-arg 1) (lambda-body node))))

(define (rep-analyze node)
  (cond ((lambda-node? node)
         (rep-analyze-call (lambda-body node))
         (select (lambda-strategy node)
           ((strategy/label strategy/open) 
            (walk (lambda (var)
                    (or (eq? (variable-type var) type/top)
                        (neq? (variable-rep var) 'rep/pointer)
                        (set (variable-rep var) (most-important-rep var))))
                  (if (continuation? node)
                      (lambda-variables node)
                      (cdr (lambda-variables node)))))))))


(define (rep-analyze-call node)
  (let ((proc (call-proc node)))
    (cond ((lambda-node? proc)
           (walk rep-analyze (call-args node))
           (rep-analyze-call (lambda-body proc)))
	  ((not (primop-node? proc))
           (walk rep-analyze (call-args node)))
          ((eq? (primop-value proc) primop/Y)
           (rep-analyze ((call-arg 1) node))
           (destructure (((body . procs) 
                          (call-args (lambda-body ((call-arg 2) node)))))
             (walk rep-analyze procs)
             (rep-analyze body)))
          (else
	   (walk rep-analyze (call-args node))
	   (cond ((and (eq? (primop-value proc) primop/contents-location)
		       (lambda-node? ((call-arg 1) node))
		       (eq? (variable-rep (lambda-cont-var ((call-arg 1) node)))
			    'rep/pointer))
		  (set (variable-rep (lambda-cont-var ((call-arg 1) node)))
		       (primop.rep-wants (leaf-value ((call-arg 2) node))))))))))



(define (most-important-rep var)
  (iterate loop ((refs (variable-refs var)) (reps '()))
    (cond ((null? refs) 
           (select-rep (reverse! reps) (variable-type var)))
          (else
           (let* ((parent (node-parent (car refs)))
                  (proc (call-proc parent))
                  (number (call-arg-number (node-role (car refs)))))
             (cond ((primop-node? proc)
                    (cond ((primop.rep-wants (primop-value proc))
                           => (lambda (creps)
				(let ((rep 
				       (nth creps (fx- (fx- number
                                                    (call-exits parent))
							     1))))
				  (if (neq? rep '*)
				      (loop (cdr refs) (cons rep reps))
				      (let ((cont ((call-arg 1) parent)))
					(loop (cdr refs)
					      (if (leaf-node? cont)
						  (cons 'rep/pointer reps)
						  (let ((rep (variable-rep
							 (lambda-cont-var cont))))
					    (cons (if (eq? (rep-size rep) 4)
						      rep
						      'rep/integer)
						  reps)))))))))
                          ((eq? (primop-value proc) primop/contents-location)
			   (loop (cdr refs)
				 (cons
				  (if (fx= number 4) 'rep/integer 'rep/pointer)
				  reps)))
                          ((eq? (primop-value proc) primop/set-location)
			   (loop (cdr refs)
				 (cons 
                           (cond ((fx= number 5)
                                  'rep/integer)
                                 ((fx= number 3)
                                  (primop.rep-wants 
                                      (leaf-value ((call-arg 2) parent))))
                                 (else 'rep/pointer))
			   reps)))
                          (else 
                           (loop (cdr refs) reps))))
                   ((variable-known (leaf-value proc)) 
                    => (lambda (label)
                         (cond ((lambda-rest-var label) 
                                (loop (cdr refs) reps))
                               (else
				(loop (cdr refs)
				      (cons (variable-rep (nth (lambda-variables label)
							       (fx- number 1)))
					    reps))))))
                   (else
                    (loop (cdr refs) (cons 'rep/pointer reps)))))))))

(define (select-rep reps type)
  (cond ((null? reps)
	 'rep/pointer)
	((eq? type type/char)
	 (car reps))
	(else
	 (let ((size (rep-size (car reps))))
	   (iterate loop ((r (cdr reps)))
	     (cond ((null? r) (car reps))
		   ((fx= (rep-size (car r)) size)
		    (loop (cdr r)))
		   (else
		    (car (sort-list! reps (lambda (x y)
					    (fx> (rep-size x) (rep-size y))))))))))))



(define (access-with-rep node value rep)
  (cond ((variable? value)
         (let ((acc (access-value node value)))
           (cond ((rep-converter (variable-rep value) rep)
                  => (lambda (converter)
                       (let* ((rep-type (if (eq? rep 'rep/pointer) 'pointer 'scratch))
                              (reg (if (and (register? acc) 
                                            (eq? (reg-type acc) rep-type)
                                            (dying? value node))
                                        acc
                                        (get-register rep-type node '*))))
                         (converter node acc reg)
                         reg)))
                 (else acc))))
        ((eq? rep 'rep/pointer)
         (access-value node value))
        (else
         (value-with-rep value rep))))



(lset *reps* '(rep/char
               rep/extend
               rep/double
               rep/integer
               rep/integer-8-s
               rep/integer-8-u
               rep/integer-16-s
               rep/integer-16-u
               rep/string
               rep/pointer))
                                      
(define-constant size/byte 1)
(define-constant size/word 2)
(define-constant size/long 4)
(define-constant size/double 8)


(lset *rep-converter-table* (make-table 'reps))
                   
(walk (lambda (rep)
        (set (table-entry *rep-converter-table* rep) 
             (make-table rep)))
      *reps*)

(define (rep-size rep)
  (xcase rep
    ((rep/char rep/integer-8-u rep/integer-8-s) size/byte)
    ((rep/integer-16-u rep/integer-16-s) size/word)
    ((rep/pointer rep/integer rep/extend rep/string) size/long)
    ((rep/double) size/double)))



(define-local-syntax (define-rep-converter from to proc)
  `(set (table-entry (table-entry *rep-converter-table* ',to) ',from)
        ,proc))


(define-rep-converter rep/pointer rep/extend
  (lambda (node from to)
    (emit vax/addl3 from (machine-num tag/extend) to)))
                   
(define-rep-converter rep/pointer rep/string
  (lambda (node from to)             
    (let ((reg (cond ((register? from) from)
                     (else
                      (generate-move from S0)
                      S0))))
      (emit vax/addl3 (reg-offset reg 2) (reg-offset reg 6) S0)
      (emit vax/addl3 S0 (machine-num tag/extend) to))))

(define-rep-converter rep/pointer rep/char
   (lambda (node from to)
     (cond ((register? to)
            (emit vax/ashl (machine-num -8) from to))
           (else
            (let ((reg (get-register 'scratch node '*)))
              (emit vax/ashl (machine-num -8) from reg)
              (emit vax/movb reg to))))))


(define-rep-converter rep/pointer rep/integer
  (lambda (node from to)
    (emit vax/ashl (machine-num -2) from to)))

(define (pointer->integer-16 node from to)
     (cond ((register? to)
            (emit vax/ashl (machine-num -2) from to))
           (else
            (let ((reg (get-register 'scratch node '*)))
              (emit vax/ashl (machine-num -2) from reg)
              (emit vax/movw reg to)))))


(define-rep-converter rep/pointer rep/integer-16-u
  pointer->integer-16)

(define-rep-converter rep/pointer rep/integer-16-s
  pointer->integer-16)

(define (pointer->integer-8 node from to)
     (cond ((register? to)
            (emit vax/ashl (machine-num -2) from to))
           (else
            (let ((reg (get-register 'scratch node '*)))
              (emit vax/ashl (machine-num -2) from reg)
              (emit vax/movb reg to)))))



(define-rep-converter rep/pointer rep/integer-8-u
  pointer->integer-8)

(define-rep-converter rep/pointer rep/integer-8-s
  pointer->integer-8)
                 

;----------------------------

(define-rep-converter rep/char rep/pointer
  (lambda (node from to)
    (let ((temp (if (and (register? to) (eq? (reg-type to) 'scratch))
                    to
                    (get-register 'scratch node '*))))
      (emit vax/movzbl from temp)
      (emit vax/ashl (machine-num 8) temp to)
      (emit vax/movb (machine-num header/char) to))))
                              

;-----------------------------
                                                   
(define-rep-converter rep/integer rep/pointer
  (lambda (node from to)
    (emit vax/ashl (machine-num 2) from to)))

;--------------------------------

(define-rep-converter rep/integer-16-s rep/pointer
  (lambda (node from to)
    (rep-convert-safely node from to vax/cvtwl)))

(define-rep-converter rep/integer-16-s rep/integer
  (lambda (node from to)
    (emit vax/cvtwl from to)))

;----------------------------------

(define-rep-converter rep/integer-16-u rep/pointer
  (lambda (node from to)
    (rep-convert-safely node from to vax/movzwl)))

(define-rep-converter rep/integer-16-u rep/integer
  (lambda (node from to)
    (emit vax/movzwl from to)))

;------------------------------------

(define-rep-converter rep/integer-8-s rep/pointer
  (lambda (node from to)
    (rep-convert-safely node from to vax/cvtbl)))

(define-rep-converter rep/integer-8-s rep/integer
  (lambda (node from to)
    (emit vax/cvtbl from to)))

(define-rep-converter rep/integer-8-s rep/integer-16-s
  (lambda (node from to)
    (emit vax/cvtbw from to)))

(define-rep-converter rep/integer-8-s rep/integer-16-u
  (lambda (node from to)
    (emit vax/cvtbw from to)))
                                     
;---------------------------------------


(define-rep-converter rep/integer-8-u rep/pointer
  (lambda (node from to)
    (rep-convert-safely node from to vax/movzbl)))

(define-rep-converter rep/integer-8-u rep/integer
  (lambda (node from to)
    (emit vax/movzbl from to)))

(define-rep-converter rep/integer-8-u rep/integer-16-s
  (lambda (node from to)
    (emit vax/movzbw from to)))

(define-rep-converter rep/integer-8-u rep/integer-16-u
  (lambda (node from to)
    (emit vax/movzbw from to)))
                  
;(define-rep-converter rep/double rep/pointer
;  (lambda (node from to)
;    (free-register node AN)
;    (generate-slink-jump slink/make-double-float nil)
;    (emit vax/movd from (d@r AN 2))
;    (generate-move AN to)))

;(define-rep-converter rep/pointer rep/double
;  (lambda (node from to)
;    (cond ((register? from)
;           (emit vax/movd (d@r from 2) to))
;          (else
;           (let ((reg (get-register 'pointer node '*)))
;             (emit vax/movl from reg)
;             (emit vax/movd (d@r reg 2) to))))))



;------------------------------------------

(define (rep-converter from-rep to-rep)
  (table-entry (table-entry *rep-converter-table* to-rep) from-rep))


(define (rep-convert-safely node from to inst)
  (let ((temp (if (eq? (reg-type to) 'scratch)
                  to
                  (get-register 'scratch node '*))))
    (emit inst from temp)
    (emit vax/ashl (machine-num 2) temp to)))

(define (really-rep-convert node from from-rep to to-rep)
  (cond ((rep-converter from-rep to-rep)
         => (lambda (converter) (converter node from to)))
        ((eq? to-rep 'rep/pointer)
         (generate-move from to))
        ((neq? from to)
         (emit (vax-2op 'mov (rep-size to-rep)) from to))))
      


(define (rep-push node value to-rep)
  (cond ((addressable? value)
         (emit vax/pushl (value-with-rep value to-rep)))
        (else
         (let ((access (access-value node value))
               (from-rep (variable-rep value)))
           (cond ((eq? from-rep to-rep)
                  (emit vax/pushl access))
                 ((neq? (rep-size to-rep) size/long)
                  (emit vax/pushl (machine-num 0))
                  (really-rep-convert node access from-rep (@r 14) to-rep))
                 (else
                  (really-rep-convert node access from-rep (@-r 14) to-rep))))))
  (increment-stack))

(define (value-with-rep value rep)
  (xcond ((char? value)
          (xcond ((eq? rep 'rep/char)
                  (machine-num (char->ascii value)))
                 ((eq? rep 'rep/pointer)
                  (machine-num (fixnum-logior (fixnum-ashl (char->ascii value) 8)
                                              header/char)))))
         ((fixnum? value)
          (cond ((eq? rep 'rep/pointer)
                 (lit value))
                (else
                 (machine-num value))))
         ((eq? value '#T)
          (machine-num header/true))
         ((or (eq? value '#F) (eq? value '()))
          nil-reg)))
                

