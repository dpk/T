(herald (front_end declare)
  (env t (orbit_top defs)))

;;; Declarations

;;; (ANNOTATE <annotations> . <forms>)
;;; (DECLARE <key> . <data>)
;;; (DECLARE <key> . <data>) => (ANNOTATE ((<key> . <data)) (VALUES))

;;; (declare ignore x y z)
;;; (declare ignorable x y z)
;;; (declare constant x)
;;; (declare integrate x)
;;; (declare local x)
;;; (declare simplifier x (lambda (node) ...))
;;; (declare type-safe-closed-form x y z)
;;;
;;; (annotate (integrate-here) x)
;;;

(define declaration-handler-table
  (make-table 'declaration-handler-table))

;;; Call the handler for a declaration.

(define (process-global-declaration form shape)
  (destructure (((#f key . stuff) form))
    (cond ((table-entry declaration-handler-table key)
           => (lambda (handler)
                (handler stuff shape)))
          (else
           (orbit-warning '"ignoring unknown declaration type ~S~%"
                          `(declare ,key . ,stuff))))))
    
;;; Issue various warnings about lexical variable use and nonuse.

(define (process-lexical-declarations var)
  (let ((flags (variable-flags var)))
    (let ((used? (not (null? (variable-refs var))))
          (ignore? (memq? 'ignore flags))
          (ignorable? (memq? 'ignorable flags)))
      (cond ((memq? 'duplicate flags)
             (variable-message 'error
                               var
                               '"duplicate identifier ~S"
                               '"all but one use will be ignored"))
            ((not (or used? ignore? ignorable?))
             (variable-message 'warning
                               var
                               '"unreferenced variable ~S"
                               nil))
            ((and used? ignore?)
             (variable-message 'error
                               var
                               '"ignored variable ~S is referenced"
                               '"the declaration will be ignored"))))
    (cond ((memq? 'local flags)
           (variable-message 'warning
                             var
                            '"lexical variable ~S is declared to be LOCAL"
                            '"the declaration will be ignored")))
    (if flags
        (modify (variable-flags var)
                (lambda (l)
                  (filter! (lambda (f)
                             (not (memq? f '(ignore ignorable duplicate local))))
                           l))))))

(define (variable-message type var message action)
  (user-message type
                message
                action
                (variable-name var)))

;;; A warning message for missing variables.

(define (missing-declaration-variable-warning name key)
  (user-message-without-location
   'warning
   '"~S is in a ~S declaration and it has no top-level definition"
   '"the declaration will be ignored"
   name 
   key))


;;;                   the actual declarations
;;;=============================================================================

;;; (DEFINE-DECLARATION PATTERN SHAPE-VAR . BODY)
;;;
;;; Pattern is a lambda list, the car of which is the declaration keyword being 
;;; defined.  SHAPE-VAR will be bound to the shape.  BODY is the code that will
;;; be executed for each instance of the declaration.

(define-local-syntax (define-declaration pattern var . body)
  (let ((name (car pattern))
        (exp (generate-symbol 'exp)))
    `(set (table-entry declaration-handler-table ',name)
          (lambda (,exp . ,var)
            (ignorable . ,var)
            (destructure ((,(cdr pattern) ,exp))
              ,@body)))))

;;; (IGNORE . names)
;;; (IGNORABLE . names)
;;; (LOCAL . names)
;;; (CONSTANT . names)
;;; (TYPE-SAFE-CLOSED-FORM . names)
;;;
;;;  These just check that the names have the correct type of binding and mark
;;; the bound variable with the flag corresponding to the declaration.

(define-declaration (ignore . names) (shape)
  (walk (lambda (name)
          (let ((var (obtain-variable shape name)))
            (if (variable? var)
                (push (variable-flags var) 'ignore))))
        names))

(define-declaration (ignorable . names) (shape)
  (walk (lambda (name)
          (let ((var (obtain-variable shape name)))
            (if (variable? var)
                (push (variable-flags var) 'ignorable))))
        names))

(define-declaration (local . names) (shape)
  (walk (lambda (name)
          (cond ((new-env-definition shape name)
                 => (lambda (def)
                      (push (definition-data def) 'local)))
                (else
                 (missing-declaration-variable-warning name 'local))))
        names))

(define-declaration (constant . names) (shape)
  (walk (lambda (name)
          (cond ((new-env-definition shape name)
                 => (lambda (def)
                      (case (definition-variant def)
                        ((define) (set (definition-variant def) 'constant))
                        ((lset set)
                  (orbit-warning '"~S variable ~S is declared to be constant"
                                        (definition-variant def)
                                        name))
                        ((multiple)
           (orbit-warning '"The CONSTANT declaration for ~S will be ignored"
                          name))     
                        (else
                         (bug '"funny variant ~S" (definition-variant def))))))
                (else
                 (missing-declaration-variable-warning name 'constant))))
        names))

(define-declaration (type-safe-closed-form . names) (shape)
  (walk (lambda (name)
          (cond ((new-env-definition shape name)
                 => (lambda (def)
                      (push (definition-data def) 'type-safe-closed-form)))
                (else
                 (missing-declaration-variable-warning
                   name 'type-safe-closed-form))))
        names))

;;; (SIMPLIFIER name exp)
;;; EXP is evaluated in ORBIT-ENV and should be a procedure of one argument.
;;; It will be called on any call to the variable NAME.

(define-declaration (simplifier name exp) (shape)
  (cond ((new-env-definition shape name)
         => (lambda (def)
              (let* ((clauses `(((primop.simplify self node) (,exp node))
                                ((primop.integrate? self node) nil)))
                     (primop (eval (primop-code name '() clauses) orbit-env)))
                (set (primop.source primop) clauses)
                (add-new-primop shape primop)
                (set (definition-value def)
                     (node->vector (create-primop-node primop))))))
        (else
         (missing-declaration-variable-warning name 'simplifier))))


