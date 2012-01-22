(herald (back_end generate_y)
  (env t (orbit_top defs) (back_end closure) (back_end bookkeep)))

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

;;; Copyright (c) 1985 David Kranz

;;; GENERATE-LET&LABELS Divide up the procedures depending on whether they
;;; need to be closed or can be jumped to.

(define (generate-labels node)        
  (destructure (((cont master) (call-args node)))
    (destructure (((body . procs) (call-args (lambda-body master))))
      (xselect (lambda-strategy master)
        ((strategy/heap)
         (set-return-size node master cont)
         (generate-heap-labels node body procs))                                 
        ((strategy/vframe strategy/ezclose)
         (set-stack-return-size node master cont)
         (generate-stack-labels node master))
        ((strategy/label)
         (set-return-size node master cont)))
      (allocate-call (lambda-body body)))))

                                              
(define (set-stack-return-size node master cont)
   (set (lambda-live master)                                          
        (if (leaf-node? cont)
            (cons (fx+ (fetch-continuation-from-stack node (leaf-value cont))
                       (closure-size (environment-closure (lambda-env master))))
                  (let ((node (variable-y-lambda (leaf-value cont))))
                    (if (and (node-parent node)
                             (primop-ref? (call-proc (node-parent node)) primop/y))
                        node
                        '())))
            (list (closure-size (environment-closure (lambda-env master)))))))
                                                                  
(define (set-return-size node master cont)
   (set (lambda-live master)                                          
        (if (leaf-node? cont)
            (cons (fetch-continuation-from-stack node (leaf-value cont))
                  (let ((node (variable-y-lambda (leaf-value cont))))
                    (if (and (node-parent node)
                             (primop-ref? (call-proc (node-parent node)) primop/y))
                        node
                        '())))
            '(0))))
                                                                  


(define (generate-heap-labels node body closures)
    (if closures
        (let ((closure (environment-closure (lambda-env (car closures)))))
          (make-heap-closure node closure)
          (lock AN)
          (walk (lambda (var)
                  (let ((reg (get-register 'pointer node '*))
                        (offset (cdr (assq var (closure-env closure)))))
                    (generate-move-address (reg-offset AN offset) reg)
                    (mark var reg)))
                (filter (lambda (closure)
                          (memq? closure (lambda-live body)))
                        (cdr (closure-members closure))))
          (unlock AN)
          (if (memq? (car (closure-members closure)) (lambda-live body))
              (mark (car (closure-members closure)) AN)))))
                                                         
                                                         
(define (generate-stack-labels node vframe)
  (cond ((eq? (lambda-strategy vframe) strategy/vframe)
         (make-vframe-closure node 
                              vframe 
                              (environment-closure (lambda-env vframe)))
         (free-register node P)
         (generate-move-address (reg-offset SP 2) P)
         (mark (lambda-self-var vframe) P))
        (else
         (make-vframe-closure node 
                              vframe 
                              (environment-closure (lambda-env vframe))))))


(define (get-or-set-join-state node lamb)
  (let ((join (lambda-env lamb)))
    (if (eq? (join-point-global-registers join) 'not-yet-determined)
        (set-join-state node join lamb))
    join))

;;; SET-JOIN-STATE The first jump (compile time) is about to be made to this
;;; point.  We must set up places for the free variables to go.  For now,
;;; put one in a register and the rest in temporaries. Move them there.


(define (set-join-state node join lamb)
  (let ((p-ok? (not (join-point-contour-needed? join))))
    (lambda-queue lamb)
    (compute-label-arg-specs node lamb p-ok?)
    (let* ((-args (map car (join-point-arg-specs join)))
	   (args (if p-ok? `(,an ,@-args) `(,an ,p ,@-args)))
           (global '()))
      (iterate loop ((vars (join-point-env join)) (left '()))
        (cond ((null? vars)
               (do ((vars left (cdr vars)))
                   ((null? vars))
                 (let ((reg (get-free-register (car vars) args p-ok?)))
                   (push args reg)
                   (push global (cons reg (car vars))))))
              (else
               (let ((w (or (register-loc (car vars))
                            (temp-loc (car vars))
                            (likely-next-reg (car vars) lamb))))
                 (cond ((and (fixnum? w) 
                             (not (memq? w args)))
                        (push args w)
                        (push global (cons w (car vars)))
                        (loop (cdr vars) left))
                       ((register-loc (car vars))
                        => (lambda (reg)
                             (cond ((not (memq? reg args))
                                    (push args reg)
                                    (push global (cons reg (car vars)))
                                    (loop (cdr vars) left))
                                   (else
                                    (loop (cdr vars) (cons (car vars) left))))))
                       (else
                        (loop (cdr vars) (cons (car vars) left))))))))
      (or p-ok? (push global (cons P (join-point-contour join))))
      (set (join-point-global-registers join) global))))


(define (compute-label-arg-specs node label p-ok?)
  (receive (formals actuals) (if (continuation? label)
                                 (return (lambda-variables label)
                                         (call-args node))
                                 (return (cdr (lambda-variables label))
                                         (cdr (call-args node))))
  (iterate loop ((formals formals) (actuals actuals)
		 (args '()) (regs (if p-ok? (list AN) (list P AN))))
    (cond ((null? formals)
           (set (join-point-arg-specs (lambda-env label)) (reverse! args)))
          (else
      (let* ((w (likely-next-reg (car formals) label))
             (reg (cond ((and (fixnum? w)
			      (var-reg-compatable? (car formals) w)
			      (not (memq? w regs)))
			 w)
                        ((let ((reg (and (reference-node? (car actuals))
                                         (register-loc (leaf-value (car actuals))))))
                           (if (and reg
                                    (var-reg-compatable? (car formals) reg)
                                    (not (memq? reg regs)))
                               reg 
                               nil)))
                        (else
                         (get-free-register (car formals) regs p-ok?)))))
        (loop (cdr formals)                                    
              (cdr actuals)
              (cons (cons reg (variable-rep (car formals))) args)
              (cons reg regs))))))))

(define (var-reg-compatable? var reg)   
  (and (fxn= reg AN)
      (case (variable-rep var)
        ((rep/pointer)
         (select (variable-type var)
           ((type/fixnum type/char) '#t)
           (else (eq? (reg-type reg) 'pointer))))
        (else
         (eq? (reg-type reg) 'scratch)))))
                            
(define (variable-register-type var)
  (case (variable-rep var)
    ((rep/pointer) 
     (select (variable-type var)
       ((type/fixnum type/char) '*)
       (else 'pointer)))
    (else 'scratch)))
                  
  
(define (get-free-register var used p-ok?)
  (really-get-free-register (variable-register-type var) used nil p-ok?))

(define (really-get-free-register type used force? p-ok?)
  (xcase type
    ((pointer)
     (iterate loop ((i (if p-ok? P A1)))
       (cond ((fx>= i AN)               
              (cond (force?
                     (do ((j *real-registers* (fx+ j 1)))
                         ((if (fx>= j (fx+ *real-registers* *pointer-temps*))
                              (bug "ran out of registers in GET-FREE-REGISTER")
                              (not (memq? j used)))
                          j)))
                     (else 
                      (really-get-free-register type used t p-ok?))))
             ((memq? i used) (loop (fx+ i 1)))
             ((or force? (not (reg-node i))) i)
             (else (loop (fx+ i 1))))))                        
    ((scratch)
     (iterate loop ((i 0))
       (cond ((fx= i *scratch-registers*)
              (cond (force?
                     (do ((j (fx+ *real-registers* *pointer-temps*) (fx+ j 1)))
                         ((if (fx>= j *no-of-registers*)
                              (bug "ran out of registers in GET-FREE-REGISTER")
                              (not (memq? j used)))
                          j)))
                    (else
                     (really-get-free-register type used t p-ok?))))
             ((memq? i used) (loop (fx+ i 1)))
             ((or force? (not (reg-node i))) i)
             (else (loop (fx+ i 1))))))
    ((*)
     (iterate loop ((i 0))
       (cond ((and (not p-ok?) (fx= i P)) (loop A1))
             ((fx>= i AN)
              (cond (force?
                     (do ((j *real-registers* (fx+ j 1)))
                         ((if (fx>= j *no-of-registers*)
                              (bug "ran out of registers in GET-FREE-REGISTER")
                              (not (memq? j used)))
                          j)))
                    (else
                     (really-get-free-register type used t p-ok?))))
             ((memq? i used) (loop (fx+ i 1)))
             ((or force? (not (reg-node i))) i)
             (else (loop (fx+ i 1))))))))



