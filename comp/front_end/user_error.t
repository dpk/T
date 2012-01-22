(herald (front_end user_error)
  (env t (orbit_top defs)))

;;; Error and warning messages for the lusers.

;;;                    USER-MESSAGE and friends
;;;============================================================================

;;; Various aliases for PRINT-USER-MESSAGE.

(define (user-message type string action . args)
  (real-user-message type string action args))

(define (real-user-message type string action args)
  (print-user-message type string action *current-module-exp* args))

(define (user-message-with-location type loc string action . args)
  (print-user-message type string action loc args))

(define (user-message-without-location type string action . args)
  (print-user-message type string action nil args))

;;; TYPE is either ERROR or WARNING, STRING is a format string, ACTION is
;;; either #F or a format string describing any action taken, LOC is #F or a
;;; module-exp structure.  ARGS is a list of arguments for the format strings.
;;;
;;; This is an internal call, use one of the above instead.

(define (print-user-message type string action loc args)
  (apply format
         *noise+error*
         `("~%;** ~A: " ,string 
           . ,(if action `("~%;   Action: " ,action) nil))
         (case type
           ((warning) "Warning")
           ((error) "Error")
           (else (bug "unknown type of user message ~S" type)))
         args)
  (if loc (print-location *noise+error* loc)))

;;; Print out the something to indicate the source code corresponding to
;;; MODULE-EXP.

(define (print-location stream module-exp)
  (format stream "~&;   Location: in ")
  (let ((def (module-exp-def module-exp)))
    (cond ((not def)
           (print-one-line (module-exp-source module-exp) stream)
           (newline stream))
          ((variable? def) 
           (format stream "definition of ~S~%" (variable-name def)))
          (else
           (format stream "definition of ~S~%" def)))))

;;; Printing out a minimally reconstructed version of a lambda's source code.

(define (show-lambda node)
  (iterate loop ((vars (reverse! (map variable-name
                                 (cdr (lambda-variables node)))))
                 (res (if (lambda-rest-var node)
                          (variable-name (lambda-rest-var node))
                          '())))
    (cond ((null? vars)
           (format nil "(LAMBDA ~S ...)" res))
          (else
           (loop (cdr vars) (cons (car vars) res))))))


;;;                6000000 possible user errors   
;;;=============================================================================
;;; I do not like this interface.

(define (fix-user-error call string . args)
  (let ((message (apply format nil string args)))
    (real-user-message 'error
                       message
                       "replacing with undefined effect"
                       '())
    (replace-call-with-undefined-effect call message)))

(define (fix-early-binding-error ref string . args)
  (let ((message (apply format nil string args)))
    (real-user-message 'warning
                       message
                       nil
                       '())
    (replace-with-free-variable ref)))

(define (fix-call-to-lambda call proc)
  (fix-user-error call
                  "wrong number of arguments in a call to ~A"
                  (show-lambda proc)))

(define (fix-call-to-literal call value)
  (fix-user-error call "call to literal ~A" value))

(define (fix-call-to-bound-lambda call var val)
  (fix-user-error call
                  "wrong number of arguments in a call to ~S bound to ~A"
                  (variable-name var)
                  (show-lambda val)))

(define (fix-call-to-early-bound-literal ref)
  (fix-early-binding-error ref
                           "call to ~S which is bound to a literal" 
                           (variable-name (reference-variable ref))))

(define (fix-call-to-early-bound-proc ref)
  (fix-early-binding-error ref
                           "wrong number of arguments in a call to ~S"
                            (variable-name (reference-variable ref))))

(define (fix-early-bound-type-error ref)
  (fix-early-binding-error ref
                           "~S is of the wrong type"
                            (variable-name (reference-variable ref))))

(define (fix-call-to-values call type)
  (let ((count (fx+ -1 (length (call-args call)))))
    (fix-user-error call
                    '"returning ~A value~P when ~A expected"
                    (if (fx= '0 count) '"no" count)
                    count
                    (values-error-string type))))

(define (fix-call-to-receive-values call type)
  (let ((count (length (call-args call))))
    (fix-user-error call
                    '"returning ~A value~P when ~A expected"
                    (if (fx= '0 count) '"no" count)
                    count
                    (values-error-string type))))

(define (values-error-string type)
  (let ((n-ary? (cadr type))
        (count (caddr type)))
    (cond ((and (not n-ary?) (fx= count '0))
           '"none are")
          ((and (not n-ary?) (fx= count '1))
           '"one is")
          ((not n-ary?) 
           (format nil '"~D are" count))
          ((fx= count '1)
           '"at least one is")
          (else
           (format nil '"at least ~D are" count)))))

(define (fix-early-bound-variable-error ref type)
  (cond ((not (eq? (node-role ref) call-proc))
         (fix-early-bound-type-error ref))
        ((eq? type 'literal)
         (fix-call-to-early-bound-literal ref))
        (else
         (fix-call-to-early-bound-proc ref))))


;;;                   removing errors from the tree
;;;============================================================================

;;; Replace CALL with an undefined effect that will print out MESSAGE when
;;; encountered at run time.

(define (replace-call-with-undefined-effect call message)                
  (let ((new-call (create-call-node 3 1)))
    (relate call-proc new-call (create-primop-node primop/undefined-effect))
    (relate (call-arg 1)
            new-call
            (if (fx= 0 (call-exits call))
                (detach (call-proc call))
                (detach ((call-arg 1) call))))
    (relate (call-arg 2) new-call (create-literal-node message))
    (replace call new-call)))

(define (containing-definition-name node)
  (let ((def (module-exp-def *current-module-exp*)))
    (cond ((not def)       "at top level")
          ((variable? def) (variable-name def))
          (else            def))))
           
;;; Replace REF with a reference to a free variable of the same name.  This 
;;; is used to correct early binding errors.

(define (replace-with-free-variable ref)
  (replace ref
           (create-reference-node
            (get-free-alias-variable (reference-variable ref)))))

(define (get-free-alias-variable var)
  (cond ((get-child-variable var 'free-alias)
         => identity)
        (else
         (let ((new-var (create-variable (variable-name var))))
           (add-child-variable var new-var 'free-alias)
           new-var))))


;;;                     CONTAINING-DEFINITION
;;;============================================================================
;;; This is debugging procedure for printing out the location of a node.  If
;;; the front end is running the form can be gotten directly from
;;; *CURRENT-MODULE-EXP*.  If not, it tries to find a definition whose value
;;; contains NODE by following the continuations.
;;;
;;; This should be in some other file.  It is no longer used in this one.

(define (containing-definition node)
  (cond (*current-module-exp*
         => (lambda (exp)
              (fresh-line (standard-output))
              (print-one-line (standard-output) module-exp-form)
              (new-line (standard-output))))
        ((and (call-node? node)
              (known-variable-definition? node))
         (format t "~&At the definition of ~S~%"
                     (variable-name
                      (reference-variable ((call-arg 2) node)))))
        (else
         (real-containing-definition node)))
  *repl-wont-print*)

(define (real-containing-definition node)
  (iterate loop ((node node))
    (let ((parent (node-parent node)))
      (cond ((not (node? parent))
             (format t "~&at top level~%"))
            ((not (and (call-node? parent)
                       (known-variable-definition? parent)))
             (loop parent))
            ((eq? node ((call-arg 3) parent))
             (format t "~&in the definition of ~S~%"
                     (variable-name
                      (reference-variable ((call-arg 2) parent)))))
            ((eq? node ((call-arg 1) parent))
             (format t "~&at top level after the definition of ~S~%"
                     (variable-name
                      (reference-variable ((call-arg 2) parent)))))
            (else
             (format t "~&at top level~%"))))))

(define (known-variable-definition? call)
  (and (variable-definition? call)
       (neq? 'set
             (primop.definition-variant (known-primop (call-proc call))))))

;;;; Useful debugging procedure
;(define (show-defs node)
;  (iterate loop ((node node))
;    (cond ((lambda-node? node)
;           (let ((def (containing-definition node)))
;             (format t "~&~D ~S~%" 
;                       (object-hash node)
;                       (if def (variable-name def) 'top-level))
;             (walk loop (call-proc+args (lambda-body node)))))
;          ((call-node? node)
;           (walk loop (call-proc+args node))))))
