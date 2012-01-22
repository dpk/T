(herald special_form
  (env tsys))

;;; Needs PATTERN

(define (make-syntax-descriptor id predicate)
  (object nil
    ((syntax-descriptor? self) t)
    ((identification self) id)
    ((print self stream)
     (print-syntax-descriptor self stream))
    ((syntax-check-predicate self) predicate)))

(define (print-syntax-descriptor self port)
  (let ((id (identification self)))
    (cond ((eq? self (syntax-table-entry standard-syntax-table id))
           (format port "#[Syntax~_~S]" id))
          ;; Temporary kludge - no JOIN.
          ((or (eq? self 'quasiquote)
               (eq? self 'unquote)
               (eq? self 'unquote-splicing))
           (format port "#[Internal-syntax~_~S]" id))
          (else
;++ fix this
           (format port "#{Syntax~_~S~_~S}"
                   (object-hash self) id)))))

;;; Used only by EVAL, this procedure in the process of being phased out.

(define (obtain-syntax-table-entry table symbol spect)
  (ignore spect)
  (cond ((syntax-table-entry table symbol)
         => identity)
        (else
         (error "unknown special form ~S" symbol))))

;;; All the special forms

;;; OBJECT is still a macro for the interpreter and thus it is commented out
;;; below.

(define-local-syntax (define-special-form form)
  (let ((pattern (if (null? (cdr form)) 'null? (cdr form))))
    `(*define-syntax t-implementation-env
                     ',(car form)
                     (make-syntax-descriptor ',(car form)
                                             (pattern-predicate ,pattern)))))

;;; (QUOTE THING)
;;; (LAMBDA (VARS) . FORMS)
;;; (CALL PROC . ARGS)
;;; (BLOCK . FORMS)
;;; (SET-VARIABLE-VALUE ID VAL)
;;; (DEFINE-VARIABLE-VALUE ID VAL)
;;; (LSET-VARIABLE-VALUE ID VAL)
;;; (VAR-LOCATIVE ID)
;;; (DECLARE KEY . STUFF)
;;; (PRIMOP ID () . METHODS) or (PRIMOP ID (FORMALS) (METHODS) . METHODS) 
;;; (IF TEST CONSEQUENT . MAYBE-ALTERNATE)
;;; (LABELS (SPECS) . FORMS)
;;; (DEFINE-LOCAL-SYNTAX ID VALUE)
;;; (DEFINE-LOCAL-SYNTAX (ID . VARS) . FORMS)
;;; (LET-SYNTAX (SPECS) . FORMS)
;;; (OBJECT PROC . METHODS)
;;; (THE-ENVIRONMENT)
;;; (LOCALE ID . FORMS)

(define-special-form (quote #f))
(define-special-form (lambda formals-list? . (+ #f)))
(define-special-form (call . (+ #f)))
(define-special-form (block . (+ #f)))
(define-special-form (set-variable-value symbol? #f))
(define-special-form (define-variable-value symbol? #f))
(define-special-form (lset-variable-value symbol? #f))
(define-special-form (var-locative symbol?))
(define-special-form (declare symbol? . (* #f)))
(define-special-form (primop symbol? . (| (null? . (* valid-method-form?))
                                          ((+ symbol?)
                                           (* valid-method-form?)
                                           . (* valid-method-form?)))))
(define-special-form (if #f #f . (| null? (#f))))
(define-special-form (labels (* valid-binding-spec?) . (+ #f)))
(define-special-form (define-local-syntax . valid-binding-spec?))
(define-special-form (let-syntax (* valid-binding-spec?) . (+ #f)))
(define-special-form (object #f . (* valid-method-form?)))
(define-special-form (the-environment))
(define-special-form (locale identifier? . (* #f)))

(define-special-form
 (define-foreign symbol?
   (symbol? . (* (foreign-parameter-spec?
                  foreign-representation-spec?
                  . (| null? symbol?))))
   symbol?))

;;; Obsolete or questionable

(define-special-form (bound? symbol?))
(define-special-form (variable-value symbol?))
(define-special-form (named-lambda symbol? formals-list? . (+ #f)))

;;; Useful predicates - where to put them?

(define (identifier? x)
  (or (not x)
      (symbol? x)))

(define (formals-list? l)
  (iterate loop ((l l))
    (cond ((null? l) t)
          ((atom? l)
           (identifier? l))
          ((identifier? (car l))
           (loop (cdr l)))
          (else nil))))

;;; BINDING-SPEC == (ID VALUE) or ((ID . VARS) . FORMS)

(define valid-binding-spec?
  (pattern-predicate (| (symbol? #f)
                        ((symbol? . formals-list?)
                         . (+ #f)))))

;;; METHOD == ((OP SELF-ID . IDS) . FORMS)
;;; where SELF-ID == ID or (SELF OBJ)

(define valid-method-form?
  (let ((self-var? (pattern-predicate
                    (| identifier?
                       (identifier? identifier?)))))
    (pattern-predicate ((#f self-var? . formals-list?) . (+ #f)))))


;;; Predicates for DEFINE-FOREIGN

(define (foreign-parameter-spec? obj)
  (memq? obj '(IN OUT IN/OUT VAR IGNORE)))

(define (foreign-representation-spec? obj)
  (memq? obj '(rep/integer
               rep/integer-8-s
               rep/integer-8-u
               rep/integer-16-s
               rep/integer-16-u
               rep/char
               rep/value
               rep/extend
               rep/extend-pointer
               rep/string
               rep/string-pointer
               rep/pointer
               rep/double)))

(define (foreign-return-spec? obj)
  (or (foreign-representation-spec? obj)
      (eq? obj 'rep/address)
      (eq? obj 'ignore)))
