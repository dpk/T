(herald (front_end analyze)
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

;;; Checking for user errors and gathering information...

;;; Needs to deal with object nodes

(lset *definitions* '())
(lset *uses* '())

;;; Return lists of variable definitions and uses.
;;;   *DEFINITIONS* is a list of the variables defined in the given node tree.
;;;   *USES* is a list of uses in the following form:
;;;     (<variable> <location> <use>)
;;;   <location> is either 'TOP or the variable whose value contains the use.
;;;   <use> is one of 'CALL-ARG 'OPERATION, '(CALL . <number of arguments>),
;;;     or 'WEIRD.

(define (def-and-use-analyze node)
  (set *definitions* '())
  (set *uses* '())
  (check-call-node (lambda-body node))
  (return *definitions*
          (map! (lambda (u)
                  (cons *current-module-exp* u))
                *uses*)))

;;; Check a value node - look at the body of lambdas and the definitions of
;;; variables.     

(define (check-value-node node)
  (cond ((lambda-node? node)
         (check-call-node (lambda-body node))
         (if (fx> (length (lambda-variables node))
                  (fx+ *maximum-number-of-arguments* 1))
             (too-many-lambda-variables node)))
        ((object-node? node)
         (check-value-list (object-operations node))
         (check-value-list (object-methods node))
         (check-value-node (object-proc node)))
        ((and (reference-node? node)
              (variable-definition (reference-variable node)))
         => (lambda (def)
              (if (local-definition? def)
                  (push *uses*
                        `(,(reference-variable node) ,(node-use node))))))))

(define (check-value-list list)
  (walk (lambda (node)
          (if node
              (check-value-node node)))
        list))

(define (too-many-lambda-variables node)
  (let* ((var (create-variable 'x))
         (l-node (create-lambda-node 'l (list var))))
    (fix-user-error (lambda-body node)
           "maximum formal count exceeded: ~D formal parameters, maximum is ~D"
           (length (cdr (lambda-variables node)))
           *maximum-number-of-arguments*)
    (relate lambda-body l-node (detach (lambda-body node)))
    (replace node l-node)))

;;; Check calls.  Checks the procedure and then dispatches on the type of
;;; the call.

(define (check-call-node call)
  (cond ((fx> (length (call-args call)) (fx+ *maximum-number-of-arguments* 1))
         (fix-user-error call
             "maximum argument count exceeded: ~D arguments, maximum is ~D"
             (length (cdr (call-args call)))
             *maximum-number-of-arguments*))
        ((variable-definition? call)
         (check-value-node ((call-arg '1) call))
         (check-value-node ((call-arg '3) call)) 
         (add-definition-value call))
        ((primop-ref? (call-proc call) primop/y)
         (check-y ((call-arg '2) call))
         (check-value-node ((call-arg '1) call)))
        ((lambda-node? (call-proc call))
         (set-check-flags (lambda-variables (call-proc call)) (call-args call))
         (check-value-list (call-proc+args call))
         (reset-check-flags (lambda-variables (call-proc call)))
         (check-call-using-proc call))
        (else
         (check-value-list (call-proc+args call))
         (check-call-using-proc call))))

;;; Does CALL define a variable.

(define (variable-definition? call)
  (let ((primop (known-primop (call-proc call))))
    (and primop
         (primop.definition? primop)
         (neq? (primop.definition-variant primop) 'set)
         (fx= '3 (length (call-args call)))
         (reference-node? ((call-arg '2) call))
         (variable-definition (reference-variable ((call-arg '2) call))))))

;;; Set the definition value and type of a variable.

(define (add-definition-value call)
  (destructure (((proc cont ref val) (call-proc+args call)))
    (ignore proc)
    (let* ((var (reference-variable ref))
           (def (variable-definition var))
           (variant (definition-variant def)))
      (ignore cont)   
      (if (and (not (memq? var *definitions*))
               (neq? variant 'set))
          (push *definitions* var))
      (real-add-definition-value var val))))

(define (real-add-definition-value var val)
  (let ((def (variable-definition var)))
    (cond ((and (or (eq? (definition-variant def) 'define)
                    (eq? (definition-variant def) 'constant))
                (not (definition->primop def)))
           (if (eq? (definition-variant def) 'constant)
               (set (definition-value def) (node->vector val)))
           (set (definition-type def)
                (get-node-definition-type val))
           t)
          (else nil))))

;;; Type check of a call using the type of the procedure.

(define (check-call-using-proc node)
  (let ((proc (call-proc node)))
    (cond ((literal-node? proc)
           (fix-call-to-literal node (literal-value proc)))
          ((reference-node? proc)
           (check-call-to-var node (reference-variable proc)))
          ((and (lambda-node? proc)      
                (not (arg-check-of-lambda proc node)))
           (fix-call-to-lambda node proc)))))

;;; Special procedure for checking calls to Y.

(define (check-y l-node)
  (let ((vals (cdr (call-args (lambda-body l-node)))))
    (set-check-flags (cdr (lambda-variables l-node))
                     (map thunk-value vals))
    (check-value-node ((call-arg '1) (lambda-body l-node)))
    (check-value-list vals)
    (reset-check-flags (lambda-variables l-node))))

;;; Variables that have known values keep those values in the VARIABLE-FLAG
;;; field for the purposes of type checking.

(define (clear-check-flags node)
  (if (lambda-node? (call-proc node))
      (reset-check-flags (lambda-variables (call-proc node)))))

(define (set-check-flags vars args)
  (walk (lambda (var val)
          (if (and var val (lambda-node? val))
              (set (variable-flag var) val)))
        vars
        args))

(define (reset-check-flags vars)
  (walk (lambda (var)
          (if var (set (variable-flag var) nil)))
        vars))

;;; Checking a call to a known variables

(define (check-call-to-var call var)
  (cond ((variable-binder var)
         (check-call-to-lexical-var call var))
        ((get-variable-definition var)
         => (lambda (def)
              (if (not (local-definition? def))
                  (check-call-to-bound-var call def))))))

(define (check-call-to-lexical-var call var)
  (let ((type (variable-flag var)))
    (cond ((and (node? type)
                (lambda-node? type)
                (not (arg-check-of-lambda type call)))
           (fix-call-to-bound-lambda call var type)))))

(define (check-call-to-bound-var call def)
  (let ((type (definition-type def)))
    (cond ((eq? type 'literal)
           (fix-call-to-early-bound-literal (call-proc call))
           (replace-with-free-variable (call-proc call)))
          ((and (pair? type)
                (eq? (car type) 'proc)
                (not (arg-check-of-type type call)))
           (fix-call-to-early-bound-proc (call-proc call))))))

(define (arg-check-of-lambda proc node)
  (let ((left-over (fx- (length (call-args node))
                        (length (lambda-variables proc)))))
    (or (fx= left-over '0)
        (and (fx> left-over '0)
             (lambda-rest-var proc)))))
    
(define (arg-check-of-type type node)
  (cond ((eq? type 'object)
         t)
        (else
         (let ((left-over (fx- (length (call-args node))
                                       (caddr type))))
           (or (fx= left-over '0)
               (and (fx> left-over '0)
                    (cadr type)))))))

;;; Is TYPE okay if we ignore the continuation

(define (arg-check-of-return-type type node)
  (cond ((eq? type 'object)
         t)
        (else
         (let ((left-over (fx- (fx- (length (call-args node)) 1)
                               (caddr type))))
           (or (fx= left-over '0)
               (and (fx> left-over '0)
                    (cadr type)))))))

;;; The way in which a node is used.  Returns one of (CALL . <# of arguments>),
;;; OPERATION, CALL-ARG, or WEIRD.

(define (node-use node)
  (let ((role (node-role node)))
    (cond ((eq? role call-proc)
           `(call . ,(length (call-args (node-parent node)))))
          ((object-op? role) 'operation)
          ((call-arg? node) 'call-arg)
          (else 'weird))))

(define (use-type use)
  (definition-type (variable-definition (cadr use))))

(define (check-uses new-uses old-uses)
  (iterate loop ((uses (append new-uses old-uses))
                 (left '()))
    (cond ((null? uses)
           left)
          ((use-type (car uses))
           => (lambda (type)
                (check-variable-use (car uses) type)
                (loop (cdr uses) left)))
          (else
           (loop (cdr uses) (cons (car uses) left))))))

(define (check-variable-use use var-type)
  (destructure (((loc var use-type) use))
    (cond ((or (not var-type)
               (eq? use-type 'call-arg)
               (eq? use-type 'weird))
           t)
          ((eq? use-type 'operation)
           t)  ; Operations are not annotated yet
          ((or (not (pair? use-type))
               (neq? 'call (car use-type)))
           (bug '"unknown use-type ~S in CHECK-VARIABLE-USE" use-type))
          ((eq? var-type 'literal)
           (user-message-with-location 'warning
                                       loc
                                       '"call to ~S which is bound to a literal" 
                                       nil
                                       (variable-name var)))
          ((and (pair? var-type)
                (eq? (car var-type) 'proc))
           (if (not (arg-check-of-use var-type use-type))
               (user-message-with-location
                'warning
                loc
                '"wrong number of arguments in a call to ~A"
                nil
                (variable-name var)))))))

(define (arg-check-of-use var-type use-type)
  (let ((left-over (fx- (cdr use-type)
                        (caddr var-type))))
    (or (fx= left-over '0)
        (and (fx> left-over '0)
             (cadr var-type)))))

;;; Quick version of the above.  Just finds defs and uses.  This is used on
;;; integrable definitions before they are simplified.

(define (quick-def-and-use-analyze node)
  (let ((uses '()) (defs '()))
    (iterate tree-walk ((node node))
      (cond ((lambda-node? node)
             (let ((call (lambda-body node)))
               (if (variable-definition? call)
                   (let ((var (reference-variable ((call-arg '2) call))))
                     (push defs var)))
               (walk tree-walk (call-proc+args call))))
            ((object-node? node)
             (tree-walk (object-proc node))
             (walk tree-walk (object-methods node)))
            ((and (reference-node? node)
                  (variable-definition (reference-variable node)))
             => (lambda (def)
                  (let ((var (reference-variable node)))
                    (if (and (local-definition? def)
                             (not (memq? var uses)))
                        (push uses var)))))))
    (return defs uses)))






