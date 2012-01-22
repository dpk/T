(herald (front_end simplify_call)
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

;;;                 Simplifying Call-nodes
;;;===========================================================================
;;; Simplify the node in the car of NODE-PAIR.  Trys a series of simplification
;;; procedures, going back to the beginning whenever a change is made.  The 
;;; simplifiers are only allowed to change the node and its descendents.  No
;;; changes may be made to any other part of the tree.

(define (simplify-call lambda-node)
  (let ((node (lambda-body lambda-node)))
    (cond ((node-simplified? node)
           node)
          (else
           (iterate loop ((node node))
             (let ((proc (call-proc node)))
               (cond ((and (lambda-node? proc)
                           (simplify-let proc node))
                      (loop (lambda-body lambda-node)))
                     ((and (reference-node? proc)
                           (integrate-definition proc))
                      (loop (lambda-body lambda-node)))
                     ((simplify-call-ignoring-exits node proc)
                      (loop (lambda-body lambda-node)))
                     ((simplify-call-using-exits node)
                      (loop (lambda-body lambda-node)))
                     (else nil))))))))

;;; Simplify the non-exit arguments of NODE and NODE itself.  Returns T if any
;;; change is made.

(define (simplify-call-ignoring-exits node proc)
  (set (node-simplified? proc) t) ; Nothing to do here anyway
  (simplify-non-exit-args node)
  (set (node-simplified? node) t)
  (or (simplify-call-using-proc proc node)
      (not (node-simplified? node))))

;;; Simplify the exits of NODE.  Remove it if has no side effects and its value
;;; is not used.

(define (simplify-call-using-exits node)
  (simplify-exit-args node)
  (or (flush-unused-call node)
      (not (node-simplified? node))))

;;; Simplify the specified children.  These use the NODE-SIMPLIFIED? flag
;;; to determine if a change has been made.

(define (simplify-non-exit-args node)
  (walkcdr simplify (nthcdr (call-args node) (call-exits node))))

;;; Simplify the exits of call-node NODE.  If the node does a test, propogate
;;; appropriate results of the test down the two arms.  This is a small (but
;;; helpful) bit of type inferencing.

(define (simplify-exit-args node)
  (cond ((fx= 1 (call-exits node))
         (simplify (call-args node)))
        ((fx= 2 (call-exits node))
         (add-to-value-table node 'true)
         (simplify (call-args node))
         (add-to-value-table node 'false)
         (simplify (cdr (call-args node)))
         (add-to-value-table node nil))
        (else
         nil)))

;;; *VALUE-TABLE* is bound by MAKE-CODE-TREE+SHAPE

(lset *value-table* (make-table '*value-table*))

(define (add-to-value-table call value)
  (destructure (((#f #f test arg1 arg2) (call-args call)))
    (cond ((and (primop-ref? test primop/test)
                (primop-ref? arg1 primop/true?)
                (reference-node? arg2))
           (set (table-entry *value-table* (reference-variable arg2))
                value))
          (else
           nil))))

;;; Calls to literals are flushed.
;;; Primops are simplified using their own methods.
;;; Calls to objects are simplified (the handler is flushed).
;;; If the second argument is a reference to a known object operation dispatch
;;;   will be attempted.

(define (simplify-call-using-proc proc node)
  (cond ((object-node? proc)
         (replace proc (detach (object-proc proc)))
         t)
        ((or (not (leaf-node? proc))
             (literal-node? proc))
         nil)
        ((known-primop proc)
         => (lambda (primop)
              (primop.simplify primop node)))
;;      ((and (bound-to-operation? (call-proc node))
;;            (cdr (call-args node))
;;            (bound-to-object? ((call-arg 2) node)))
;;       (simplify-operation-dispatch node obj-exp))
        (else
         nil)))

;;; Remove a call that has no side effects and produces no useful result.

(define (flush-unused-call node)
  (cond ((and (not (side-effects? (call-proc node)))
              (unused-call? node))
         (replace node (detach (lambda-body ((call-arg 1) node))))
         t)
        (else
         nil)))

(define (unused-call? node)
  (and (fx= 1 (call-exits node))
       (leaf-node? (call-proc node))
       (lambda-node? ((call-arg 1) node))
       (every? (lambda (var)
                 (or (not var)
                     (null? (variable-refs var))))
               (lambda-rest+variables ((call-arg 1) node)))))

(define (side-effects? proc)
  (cond ((known-primop proc)
         => primop.side-effects?)
        (else
         t)))

;;; OBJ is an object-lambda.  The methods are searched to see if there is
;;; one corresponding to the procedure being called.  If so, the method is
;;; substituted in-line.

(define (simplify-operation-dispatch call obj def)
  (destructure (((#f op? proc ops methods) obj))
    (ignore op? proc)
    (let ((op-def (variable-definition (reference-variable (call-proc call))))
          (env (definition-env def)))
      (iterate loop ((ops ops) (methods methods))
        (cond ((null? ops)
               nil)
              ((let ((var (vector->variable (car ops) env)))
                 (and (variable? var)
                      (eq? op-def (variable-definition var))))
               (replace-operation-with-method call (car methods) def))
              (else
               (loop (cdr ops) (cdr methods))))))))

;;;  (<op> <cont> <object> . <args>)
;;;   => (<method> <cont> <object>  . <args>)
;;; where <method> is <object>'s method for <op>.

(define (replace-operation-with-method call method def)
  (let ((new (create-call-node (fx+ 1 (length (call-args call))) 1)))
    (mark-reference-used (call-proc call))
    (mark-reference-used ((call-arg 2) call))
    (relate call-proc new (vector->node method (definition-env def)))
    (relate-call-args new `(,(detach ((call-arg 1) call))
                            . ,(map detach (cdr (call-args call)))))
    (replace call new)
    t))


