(herald (front_end simplify)
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

;;;  Optimization of CPS code tree

;;; Post-CPS code has these properties:
;;;   For every LAMBDA node L:
;;;     - L's body is a call.
;;;     - L's parent is a call or an object, or else L is the top of the tree.
;;;   For every call node N:
;;;     - N's procedure and arguments are all non-calls.
;;;     - N's parent is a LAMBDA.

;;; (SIMPLIFY node-pair)
;;;============================================================================
;;;   Post-CPS optimizer.  All simplifications are done by changing the
;;; structure of the node tree.  NODE-PAIR is a pair whose CAR contains a
;;; leaf-node or a lambda-node.
;;;
;;; There are three requirements for the simplification procedures:
;;;    1) They must return T if the tree has been changed and NIL otherwise.
;;;    2) Only the node being simplified and its descendents may be changed.
;;;    3) If a node is changed the NODE-SIMPLIFIED? flag of that node and all
;;;       its ancestors must be set to false.

;;; Keep simplifying a node until it stops changing.

(define (simplify node-pair)
  (let ((node (car node-pair)))
    (cond ((not (node-simplified? node))
           (iterate loop ((node node))
             (if (cond ((lambda-node? node) (simplify-lambda node))
                       ((leaf-node?   node) (simplify-leaf   node))
                       ((object-node? node) (simplify-object node))
                       (else nil))
                 (loop (car node-pair))
                 (set (node-simplified? node) t)))))))


;;; (SIMPLIFY-LEAF node)
;;;==========================================================================
;;;   Leaf nodes are simplified only if they are variables with declaration
;;; information.

(define (simplify-leaf node)
  (cond ((and (reference-node? node)
              (not (nonvalue-reference? node)))
         (integrate-definition node))
        (else
         nil)))

;;; (INTEGRATE-DEFINITION node)
;;;============================================================================
;;;    NODE is a reference to a variable with declared information.  The value
;;; is substituted in-line if possible, otherwise nothing is done.
;;;  
;;; This needs to deal with call exit nodes....

(define (integrate-definition node)
  (let* ((var (reference-variable node))
         (def (get-variable-definition var)))
    (cond ((not (and def
                     (eq? 'constant (definition-variant def))
                     (definition-value def)))
           nil)
          ((not (node-type-check? node (get-definition-type def node)))
           (fix-early-bound-variable-error node (get-definition-type def node)))
          ((get-integrable-node node def)
           => (lambda (new)
                (replace node new)
                (if (and (primop-node? new)
                         (eq? (node-role new) call-proc))
                    (primop.presimplify (primop-value new) (node-parent new)))
                (mark-variable-used var)
                t))
          (else nil))))

(define (node-type-check? node type)
  (cond ((eq? (node-role node) call-proc)
         (and (callable-type? type)
              (arg-check-of-type type (node-parent node))))
        (else t)))

;;; Get the value of the variable referenced by NODE and check to see if
;;; it can be substituted in-line.  Primops are asked if they want to be
;;; integrated.  Objects are taken care of by their own procedure.  Literals
;;; are always integrated, callable values are integrated in call position
;;; only.

(define (get-integrable-node node def)
  (cond ((definition->primop def)
         => (lambda (primop)
              (if (primop.integrate? primop node)
                  (create-primop-node primop)
                  nil)))
        ((definition->object def)
         => (lambda (obj)
              (get-integrable-object obj node def)))
        ((neq? (node-role node) call-proc)
         (if (eq? (definition-type def) 'literal)
             (definition->node def)
             nil))
        (else
         (definition->node def))))

;;; The procedure of the object is integrated if it is referenced in call
;;; position.  Operation dispatch is not currently dealt with.

(define (get-integrable-object obj node def)
  (cond ((eq? (node-role node) call-proc)
         (if (pair? obj)
             (vector->node (caddr obj) (definition-env def))
             nil))
        (else
         nil)))  ;;; Eventually do operation dispatch here.

;;; (SIMPLIFY-LAMBDA node)
;;;============================================================================
;;;     Simplify a lambda node.
;;; (lambda () (x)) => x if the node is an exit or an argument to a lambda
;;; node.

(define (simplify-lambda node)
  (simplify-call node)
  (cond ((and (or (call-exit? node)
                  (let-node? (node-parent node)))
              (not (lambda-rest-var node))
              (null? (lambda-variables node))
              (null? (call-args (lambda-body node)))
              (reference-node? (call-proc (lambda-body node)))
              (variable-binder
                (reference-variable (call-proc (lambda-body node)))))
         (replace node (detach (call-proc (lambda-body node))))
         t)
        (else
         nil)))

(define (let-node? node)
  (and (call-node? node)
       (lambda-node? (call-proc node))))

;;; Simplifying objects
;;;   The child nodes are all simplified.  If the procedure is a literal-node
;;;;or a reference to a variable bound to a literal it is replace by a
;;; lambda-node that calls PRIMOP/UNDEFINED-EFFECT.
;;;
;;; If the node is in call position should it be replaced with its procedure?

(define (simplify-object node)
  (simplify (object-proc-pair node))
  (walkcdr simplify (object-operations node))
  (walkcdr simplify (object-methods node))
  (let ((proc (known-value (object-proc node))))
    (cond ((and proc 
                (or (and (node? proc) (literal-node? proc))
                    (and (pair? proc) (eq? (car proc) 'literal))))
           (replace (object-proc node)
                    (let-nodes ((l1 (() c)
                                    (($ primop/undefined-effect)
                                     1
                                     (* c)
                                     '"calling an object that has no procedure")))
                      l1))
           t)
          (else nil))))
  










