(herald (back_end generate)
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

(define (generate-let node)
  (destructure (((body . exprs) (call-proc+args node)))
    (iterate loop ((exprs exprs) (vars '()) (lambdas '()) (stack nil))
      (cond ((null? exprs)
             (really-generate-let node body vars lambdas stack))
            ((not (lambda-node? (car exprs)))
             (loop (cdr exprs) (cons (car exprs) vars) lambdas stack))
            ((eq? (lambda-strategy (car exprs)) strategy/label)
             (loop (cdr exprs) vars lambdas stack))
            ((eq? (lambda-strategy (car exprs)) strategy/stack)
             (loop (cdr exprs) vars lambdas (car exprs)))
            (else
             (loop (cdr exprs) vars (cons (car exprs) lambdas) stack))))
    (allocate-call (lambda-body body))))

                       
;; ************** the variables of leaves MUST BE FREE. They should have been
;; substituted.  Otherwise there would be aliasing.

(define (really-generate-let node body leaves closures stack)  
  (let ((bind-leaf 
          (lambda (leaf) 
            (if (variable-binder (leaf-value leaf))
                (bug "lexical variable being bound by LET ~s" (leaf-value leaf)))
            (let* ((var (nth (lambda-variables body)
                                 (fx- (call-arg-number (node-role leaf)) 1)))
                   (acc (access-value node (leaf-value leaf)))
                   (reg (get-register (if (eq? (variable-rep var) 'rep/pointer)
                                          'pointer 'scratch)
                                      node '*)))
              (really-rep-convert node acc 'rep/pointer reg (variable-rep var))
              (set (register-loc (leaf-value leaf)) nil)
              (mark var reg)))))                            
    (cond (stack
           (set (lambda-strategy body) strategy/stack)
           (set (lambda-env body) (lambda-env stack))))
    (cond ((not closures) 
           (walk bind-leaf leaves))
          (else
           (cond ((get-member closures)
                  => (lambda (member)
           (let ((closure (environment-closure (lambda-env member))))
             (make-heap-closure node closure)
             (lock AN)
             (walk (lambda (var)
                     (let ((reg (get-register 'pointer node '*))
                           (offset (cdr (assq var (closure-env closure)))))
                       (generate-move-address (reg-offset AN offset) reg)
                       (mark var reg)))
                   (cdr (closure-members closure)))
             (unlock AN)
             (mark (car (closure-members closure)) AN)))))
	   (walk bind-leaf leaves)
	   (walk (lambda (closure)
                   (lambda-queue closure)
                   (mark (lambda-self-var closure) 
                         (->register 'pointer node closure '*)))
                 (filter (lambda (l)
                           (eq? (environment-closure (lambda-env l)) 
                                *unit*))
                         closures))))))

(define (get-member closures)
  (iterate loop ((closures closures))
    (cond ((null? closures) nil)     
          ((neq? (environment-closure (lambda-env (car closures)))
                 *unit*)
           (car closures))
          (else (loop (cdr closures))))))


                                    


;;; RESULT-WANT-LOC Determines where to target the result of a primop.
;;; If the continuation is a variable, it is a return of one argument in
;;; A1.  Otherwise we look at the most important use (now done non-optimally)
;;; and see where is is needed.  If it is the argument to a primop we look
;;; at the arg-specs of that primop, otherwise it is in a position for the
;;; standard calling sequence.

(define-operation (foreign-name foreign) nil)

(define (continuation-wants cont)
  (cond ((lambda-node? cont)
         (cond ((n-ary? cont) (return '* 'rep/pointer))
               (else
                (let ((var (car (lambda-variables cont))))
                  (return (likely-next-reg var cont)
                          (variable-rep var))))))
        (else 
         (return A1 'rep/pointer))))

(define (likely-next-reg var cont)
  (let ((spec (really-likely-next-reg var cont)))
    (cond ((fixnum? spec)
           (if (eq? (reg-type spec) 'pointer)
               (if (eq? (variable-rep var) 'rep/pointer) spec 'scratch)
               (if (neq? (variable-rep var) 'rep/pointer) spec 'pointer)))
          (else spec))))


(define (really-likely-next-reg var cont)
  (let ((refs (mem (lambda (x ref) 
                     (fx= (lambda-trace (node-parent (node-parent ref))) x))
                   (lambda-trace cont)
                   (variable-refs var))))
    (iterate loop ((refs refs))
      (if (null? refs)
          (variable-register-type var)
          (let* ((parent (node-parent (car refs)))
                 (proc (call-proc parent))
                 (number (call-arg-number (node-role (car refs)))))
            (cond ((primop-node? proc)
                   (cond ((primop.arg-specs (primop-value proc))
                          => (lambda (specs)
			       (let ((spec (nth specs (fx- (fx- number
							      (call-exits parent))
							      1))))
				 (if (eq? spec '*)
				     (loop (cdr refs))
				     spec))))
                         (else
                          (loop (cdr refs)))))
                  ((variable-known (leaf-value proc))
                   => (lambda (label)
                        (cond ((neq? (lambda-strategy label) strategy/label)
                               (fx- (fx+ number *scratch-registers*)
                                    (call-exits parent)))
                              ((join-point-arg-specs (lambda-env label))
                               => (lambda (args)
                                    (car (nth args
                                      (fx- (fx- number (call-exits parent)) 1)))))
                              (else (loop '())))))
                  (else
                   (fx- (fx+ number *scratch-registers*)
                        (call-exits parent)))))))))




;;; locatives
                                

(define (generate-locative node)
  (receive (t-spec t-rep) (continuation-wants ((call-arg 1) node))
    (let* ((dest (get-target-register node t-spec))
           (acc (lookup node (get-lvalue (leaf-value ((call-arg 2) node))) nil)))
      (free-register node dest)
      (generate-move acc dest)
      (mark-continuation node dest))))


(define (get-lvalue var)
  (cond ((ass (lambda (x y)
                (and (loc-list? y)
                     (eq? x (loc-list-var y))))
              var
              (closure-env *unit*))
         => car)
        (else
         (bug "couldn't find lvalue ~s" var))))


(define (mark-continuation node reg)
  (let ((cont (car (call-args node))))
    (if (lambda-node? cont)
        (if (not (n-ary? cont))
            (mark (car (lambda-variables cont)) reg))
        (generate-move reg A1))))

;;; Data manipulation
;;; ---------------------------------------------------------------------


(define (generate-define-var node)
  (let* ((value ((call-arg 3) node)))
    (cond ((and (lambda-node? value) 
             (not (eq? (primop.definition-variant (leaf-value (call-proc node)))
                       'lset))
             (eq? (environment-closure (lambda-env value)) *unit*))
           (lambda-queue value))
          ((primop-node? value))
          (else
           (generate-set node ((call-arg 2) node) value)))))



    
                                                           



