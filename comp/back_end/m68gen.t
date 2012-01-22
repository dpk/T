(herald (back_end m68gen)
  (env t (orbit_top defs) (back_end bookkeep)))

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

(define (generate-nil-test arg)
  (emit m68/cmp .l arg nil-reg))
    
;;; Eq?
;;; ---------------------------------------------------------------------


(define (eq?-comparator node)
  (destructure (((then else () ref1 ref2) (call-args node)))
    (let ((val1 (leaf-value ref1))
          (val2 (leaf-value ref2)))
      (let ((access2 (access-with-rep node val2 'rep/pointer)))
        (protect-access access2)
        (let ((access1 (access-with-rep node val1 'rep/pointer)))
          (cond ((or (register? access1) (eq? access1 nil-reg))
                 (emit m68/cmp .l access2 access1))
                ((or (register? access2) (eq? access2 nil-reg))
                 (emit m68/cmp .l access1 access2))
                (else
                 (let ((reg (get-register 'pointer node '*)))
                   (emit m68/move .l access1 reg)
                   (emit m68/cmp .l access2 reg))))
          (emit-jump 'jneq else then)
          (release-access access2))))))


(define (one-arg-primitive node)
  (destructure (((cont arg) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((var (leaf-value arg))
             (dest (cond ((register? t-spec)
                          (cond ((or (not (reg-node t-spec))
                                     (dying? (reg-node t-spec) node))
                                  t-spec)
                                (else
                                 (get-register (reg-type t-spec) node '*))))
                         ((and (dying? var node) (register-loc var))
                          => (lambda (reg)
                               (if (and (register? reg) (eq? (reg-type reg) t-spec))
                                   reg
                                   (get-register t-spec node '*))))
                         (else
                          (get-register t-spec node '*)))))
        (lock dest)
        (let ((acc (access-value node var)))
          (unlock dest)
          (kill-if-dying var node)
          (return acc dest t-rep))))))


(define (generate-closure-enclosing-object node)
     (receive (source target rep) (one-arg-primitive node)
       (let ((creg (cond ((and (register? source) (neq? source target))
                          source)
                         (else
                          (lock target)
                          (block0 (get-register 'pointer node '*)
                                  (unlock target))))))
         (generate-move source creg)
         (emit m68/move .l (reg-offset creg -2) target)   ; get template
         (emit m68/move .l (machine-num 0) SCRATCH)
         (emit m68/move .w (reg-offset target -6) SCRATCH) ; offset field in bytes
         (generate-move creg target)
         (emit m68/sub .l SCRATCH target))    ; pointer and scratch adjoined
         (mark-continuation node target)))


(define (generate-make-vector-extend node)
  (destructure (((#f type length size) (call-args node)))
    (let ((acc (access-with-rep node (leaf-value length) 'rep/pointer)))
      (free-register node AN)
      (emit m68/move .l acc SCRATCH)
      (emit m68/asl .l (machine-num 6) SCRATCH)
      (emit m68/move .b (machine-num (leaf-value type)) SCRATCH)
      (emit m68/move .l SCRATCH AN)
      (lock AN))
    (let ((acc (access-with-rep node (leaf-value size) 'rep/pointer)))
      (free-register node S1)
      (generate-move acc S1))
    (free-register node S2)
    (generate-slink-jump slink/make-extend)
    (unlock AN)
    (mark-continuation node AN)))


(define (generate-make-extend node)
  (destructure (((#f template size) (call-args node)))
    (let ((acc (access-with-rep node (leaf-value template) 'rep/pointer)))
      (free-register node AN)
      (generate-move acc AN)
      (lock AN))
    (let ((acc (access-with-rep node (leaf-value size) 'rep/pointer)))
      (free-register node S1)
      (generate-move acc S1))
    (free-register node S2)
    (generate-slink-jump slink/make-extend )
    (unlock AN)
    (mark-continuation node AN)))
     
                                             
(define (generate-make-cell node)
  (let ((cont ((call-arg 1) node)))
    (cond ((and (lambda-node? cont)
                (eq? (variable-definition (car (lambda-variables cont))) 'one))
           (receive (t-spec t-rep) (continuation-wants cont)
             (mark-continuation node (get-target-register node t-spec))))
          (else
           (free-register node AN)
           (free-register node S1)
           (emit m68/move .l (lit 1) S1)               ; 1 slot
           (emit m68/move .l (machine-num header/cell) AN)
           (free-register node S2)
           (lock AN)
           (generate-slink-jump slink/make-extend )
           (unlock AN)
           (mark-continuation node AN)))))

(define (generate-make-pair node)
  (free-register node AN)
  (generate-slink-jump slink/make-pair)
  (mark-continuation node AN))           


(define (generate-slink-ref node)
  (generate-primitive-reg-ref node 'slink))

(define (generate-task-ref node)
  (generate-primitive-reg-ref node 'task))

(define (generate-set-slink-ref node)
  (generate-set-primitive-reg-ref node 'slink))

(define (generate-set-task-ref node)
  (generate-set-primitive-reg-ref node 'task))


(define (generate-primitive-reg-ref node reg)
  (destructure (((cont arg) (call-args node)))
   (if (fixnum? (leaf-value arg))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let ((dest (get-target-register node t-spec)))
        (xcase reg
	  ((slink) (really-rep-convert node (d@nil (leaf-value arg)) 'rep/pointer
				            dest t-rep))
	  ((task) (really-rep-convert node (reg-offset TASK (leaf-value arg))
				      'rep/pointer dest t-rep)))
        (mark-continuation node dest))))))
                                                                  

(define (generate-set-primitive-reg-ref node reg)
  (destructure (((#f arg val) (call-args node))) 
   (if (fixnum? (leaf-value arg))
       (let ((acc (access-with-rep node (leaf-value val) 'rep/pointer)))
         (xcase reg
	   ((slink) (emit m68/move .l acc (d@nil (leaf-value arg))))
	   ((task) (emit m68/move .l acc (reg-offset TASK (leaf-value arg)))))))))

                 

(define (generate-stack-pointer node)
  (receive (t-spec t-rep) (continuation-wants ((call-arg 1) node))
    (let ((dest (get-target-register node t-spec)))
      (free-register node dest)
      (emit m68/move .l SP dest)
      (mark-continuation node dest))))
  

(define (generate-current-continuation node)
  (receive (t-spec t-rep) (continuation-wants ((call-arg 1) node))
    (let ((dest (get-target-register node t-spec)))
      (free-register node dest)
      (emit m68/move .l SP dest)
      (emit m68/add .l (machine-num 2) dest)
      (mark-continuation node dest))))

           
(define (generate-nary-setup node required)
  (if (eq? (lambda-strategy node) strategy/stack)
      (emit m68/neg .l NARGS))                           ; !!!
  (do ((i (fx+ A1 required) (fx+ i 1)))
      ((fx>= i (fx- *real-registers* 1)))
    (generate-move i (fx+ *real-registers* (fx- i A1))))
  (generate-move (machine-num required) S0)
  (generate-slink-jump slink/nary-setup)
  (mark (lambda-rest-var node) AN))


;;; GENERATE-HANDLER The situation is that the object is in A1 and its template 
;;; is in TP.  The  operation is in P.  We must use only the register AN.                                 

(define (hacked-get-register type node where) 
  (ignore type node where)
  (cond ((reg-node an)
	 => (lambda (x)
	      (set (register-loc x) nil)
	      (set (reg-node an) nil))))
  AN)

(define (generate-handler node)                            
  (let ((leaves (call-args (lambda-body ((call-arg 3) (lambda-body node)))))
        (methods (cdddr (call-args (lambda-body node)))))
    (cond ((null? methods)
           (emit m68/move .l nil-reg AN)
           (emit m68/rts))
          (else
      (bind ((get-register hacked-get-register))
        (mark (lambda-self-var *lambda*) A1)
        (generate-jump (car leaves))
        (let ((last ((call-arg 3) (lambda-body node))))
          (do ((l leaves (cdr l))
               (methods methods (cdr methods)))
              ((null? l)
               (emit-tag last)
               (emit m68/move .l nil-reg AN)
               (emit m68/rts)
               (clear-slots))
            (generate-handler-test (car l) 
                                   (car methods) 
                                   (if (null? (cdr l)) last (cadr l))))))))))

(define (generate-handler-test leaf method next)
  (emit-tag leaf)
  (emit m68/cmp .l (access-value nil (leaf-value leaf)) P)
  (let ((el-hacko (cons nil nil)))
    (emit-jump 'jneq next el-hacko)
    (emit-tag el-hacko))
  (lambda-queue method)
  (generate-move-address (template method) AN)
  (emit m68/rts))
  

(define (generate-undefined-effect node)
  (generate-move (access-value node (leaf-value ((call-arg 1) node))) A1)
  (generate-jump-absolute (*d@nil slink/undefined-effect))
  (clear-slots))


(define (generate-vframe-test amount)
  (let ((hack1 (cons nil nil))
        (hack2 (cons nil nil)))
    (emit m68/cmp .b (machine-num header/vframe) (d@r 15 3))
    (emit-jump 'jneq hack2 hack1)
    (emit-tag hack1)                       
    (adjust-stack-pointer amount)
    (generate-jump hack2)
    (emit-tag hack2)))
        

(define (generate-set node location value)
  (let ((access (if (lambda-node? value)        
		    (cond ((access/make-closure node value))
			  (else AN))
		    (access-with-rep node (leaf-value value) 'rep/pointer))))
    (protect-access access)
    (let ((loc (lookup node (get-lvalue (leaf-value location)) nil))
	  (hack1 (cons nil nil))
	  (hack2 (cons nil nil)))
      (let ((reg (get-register 'pointer node '*)))
	(release-access access)
	(generate-move loc reg)
	(generate-move access (reg-offset reg 2))
	(emit m68/tst .b (reg-offset reg 0))
	(emit-jump 'jneq hack1 hack2)
	(emit-tag hack1)                       
	(emit m68/move .l reg (reg-offset TASK task/extra-pointer))
	(generate-slink-jump slink/set)
	(generate-jump hack2)
	(emit-tag hack2)))))

(define (generate-remove-state-object node)
  (let ((cont (car (call-args node))))
    (if (and (lambda-node? cont)
	     (not (lambda-rest-var cont))
	     (variable-refs (lambda-cont-var cont)))
	(receive (t-spec t-rep) (continuation-wants cont)
	  (let ((dest (get-target-register node t-spec)))
	    (emit m68/move .l (reg-offset sp 4) dest)
	    (mark-continuation node dest)))))
  (or (not (method-lambda (node-parent node)))
      (emit m68/add .w ($ 20) sp)))
