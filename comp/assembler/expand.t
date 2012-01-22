(herald (hax expand t 10))

;;; (EXPAND form syntax-table)  ->  form
;;;     Recursively macro-expands the form.

(define (tas-expand exp syntax)
  (cond ((not (pair? exp)) exp)
        (else
         (let ((head (car exp)))
           (cond ((symbol? head)
                  (cond ((syntax-table-entry syntax head)
                         => (lambda (descr)
                              (tas-expand-special-form descr exp syntax)))
                        (else
                         (tas-expand-call exp syntax))))
                 ((syntax-descriptor? head)
                  (tas-expand-special-form head exp syntax))
                 (else
                  (tas-expand-call exp syntax)))))))

(define (tas-expand-macros exp syntax) (tas-expand exp syntax))

(define *expander-table* (make-table '*expander-table*))

(define-local-syntax (define-expander pat vars . body)
  (let ((exp-var (generate-symbol 'exp)))
    `(set (table-entry *expander-table*
                       (syntax-table-entry standard-syntax-table
                                           ',(car pat)))
          (lambda ,vars
            (ignorable ,@vars)
            (destructure ((,(cdr pat) (cdr ,(car vars))))
              ,@body)))))

(define (tas-expand-special-form descr exp syntax)
  (cond ((table-entry *expander-table* descr)
         => (lambda (proc) (proc exp syntax)))
        (else
         (cond ((macro-expander? descr)
                (tas-expand (expand-macro-form descr exp syntax) syntax))
               (else
                (error "unknown special form~%  ~S"
                       `(tas-expand ,exp ,syntax)))))))

(define-expander (quote obj) (exp syntax)
  (ignore obj)
  exp)

(define-expander (variable-value name) (exp syntax)
  `(,(car exp) ,name))

(define-expander (call . forms) (exp syntax)
  (tas-expand-call forms syntax))

(define-expander (lambda vars . body) (exp syntax)
  `(,(car exp) ,vars ,@(tas-expand-body body syntax)))

(define-expander (named-lambda name vars . body) (exp syntax)
  `(,(car exp) ,name ,vars  ,@(tas-expand-body body syntax)))

(define-expander (if . rest) (exp syntax)
  `(,(car exp) ,@(tas-expand-body rest syntax)))

(define-expander (labels specs . body) (exp syntax)
  `(,(car exp) ,(map (lambda (spec)
                       `(,(car spec) 
                         ,@(tas-expand-body (cdr spec) syntax)))
                     specs)
               ,@(tas-expand-body body syntax)))

(define-expander (locale var . body) (exp syntax)
  `(,(car exp) ,var ,@(tas-expand-body body syntax)))

(define-expander (block . body) (exp syntax)
  `(,(car exp) ,@(tas-expand-body body syntax)))

(define-expander (define-variable-value var val) (exp syntax)
  `(,(car exp) ,var ,(tas-expand val syntax)))

(define-expander (lset var val) (exp syntax)
  `(,(car exp) ,var ,(tas-expand val syntax)))

(define-expander (set-variable-value var val) (exp syntax)
  `(,(car exp) ,var ,(tas-expand val syntax)))

(define-expander (define-local-syntax . spec) (exp syntax)
  (tas-set-local-syntax syntax spec)
  `(define-local-syntax ,@spec))

(define-expander (let-syntax specs . body) (exp syntax)
  (let ((syntax (make-syntax-table syntax nil)))
    (walk (lambda (spec) (tas-set-local-syntax syntax spec))
          specs)
    `(let-syntax ,specs . ,(tas-expand-body body syntax))))

(define (tas-set-local-syntax syntax spec)
  (let ((pat (car spec))
        (body (cdr spec))
        (foo (lambda (sym exp)
               (set (syntax-table-entry syntax sym)
                    (eval exp ((*value t-implementation-env
                                       'env-for-syntax-definition)
                               syntax)))
               sym)))
    (cond ((pair? pat)
           (foo (car pat) `(macro-expander ,pat ,@body)))
          (else
           (foo pat (car body))))))

(define (tas-expand-call exp table)
  (map (lambda (arg) (tas-expand arg table)) exp))

(define (tas-expand-body exp table)
  (map (lambda (form) (tas-expand form table)) exp))
