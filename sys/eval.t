(herald (tsys eval)    ;** dont change this herald
        (env tsys (osys kernel)))

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

;;;; The Evaluator

;;; COMPILE is an S-expression preprocessor.  It takes source code,
;;; represented as S-expression, and makes a code tree.  Its main
;;; purposes are (a) expanding macros and (b) dead-reckoning local
;;; variable references.  This preprocessing makes code run faster
;;; than it would if a straightforward S-expression interpreter was
;;; used.

;;; For the purposes of this module, the terms "static" and "global"
;;; both mean "free with respect to the expression being compiled".
;;; "Local" or "lambda-bound" mean "bound by some lambda-expression
;;; within the expression being compiled".

;;; Code ("S-code") trees are represented as closures.  To interpret
;;; a code tree it is only necessary to call it.

;;; A SHAPE is a compile-time structure which describes the
;;; representation that the local variable environment will have
;;; at runtime.

;;; EVAL: copied from the T manual.

(define (eval exp env)
  (run-compiled-code (standard-compiler exp (env-syntax-table env))
                     env))

(define (standard-compiler exp syntax)
  (compile-top exp syntax nil))

(lset *current-module* nil)

;;; Like STANDARD-COMPILER, but gets its input from a port.  Sets
;;; up a compiled expression which is a loaded-file; i.e. handles
;;; LOADED-FILE-HERALD and LOADED-FILE-SOURCE appropriately.

(define-constant initial-exp-values (list (undefined-value "empty file")))


(define (standard-compile-port port syntax herald)
  (let ((source (port-name port)))
    (bind ((*current-module* source))
      (object nil
              ((run-compiled-code self env)
               (iterate loop ((vals initial-exp-values))
                 (let ((exp (read port)))
                   (cond ((eof? exp)
                          (set port nil)            ;drop pointer
                          (apply return vals))
                         (else
                          (receive vals
                                   (run-compiled-code
                                    (compile-top exp syntax self)
                                    env)
                            (load-print vals)
                            (loop vals)))))))
              ((compiled-code? self) '#t)
              ((get-loaded-file self) self)
              ((loaded-file-herald self) herald)
              ((loaded-file-source self) source)
              ((identification self)        ; For BACKTRACE
               (filename-name (herald-filename herald)))
              ((print-info self)
               (filename-name (herald-filename herald)))
              ((print-type-string self) "Loaded-file")))))

;;; COMPILE-TOP - this is the top-level entry into the compiler.
;;; Keeps track of all variables free in the expression; when the
;;; code is actually run, it creates a vector where locatives to
;;; the variables can be stored.

(lset *syntax-table*   nil)
(lset *free-vars*      nil)
(lset *free-var-count* 0)

(define (compile-top exp syntax loaded-file)
  (bind ((*free-vars* (make-table '*free-vars*))
         (*free-var-count* 1)
         (*syntax-table* syntax))
    (let* ((code (compile exp loaded-file nil))
           (free-var-count *free-var-count*))
      (object nil
              ((run-compiled-code self env)
               (let ((env (enforce environment? env))
                     (genv (vector-fill (make-vector free-var-count) nil)))
                 (set (vref genv 0) env)
                 ;? (add-active-segment env genv)
                 (run code env genv)))
              ((print-type-string self) "Compiled-code")))))

(define-integrable (genv-env genv) (vref genv 0))

;;; ---- Local variable spec stuff.

;;; Local variable specifiers (LVSPEC's) are implemented as fixnums
;;; divided into two bit fields, BACK and OVER.

(define-integrable lvspec? fixnum?)

(define-integrable (add-contour args lenv) (cons lenv args))

(define-integrable (lvspec back over)
  (fixnum-logior back (fixnum-ashl over 13)))

(define-integrable (lvspec-back spec)
  (fixnum-logand spec 8191))

(define-integrable (lvspec-over spec)
  (fixnum-ashr spec 13))               ; signed

(define-integrable cenv-first cdr)
(define-integrable cenv-rest  car)
(define-integrable cenv-end?  atom?)

(define-integrable lenv-first cdr)
(define-integrable lenv-rest  car)
(define-integrable lenv-end?  atom?)

;;; ---- Utilities for the stack debugger.

(define (interpreter-frame? frame)
  ;; Incredible kludge.
  (let ((z (get-loaded-file frame)))
    (and z
         (let ((fn (herald-filename (loaded-file-herald z))))
           (and (eq? (filename-name fn) 'eval)
                (eq? (filename-dir fn) 'tsys)
                (frame-any scode? frame))))))

(define (interpreter-frame-code frame)
  (frame-any (lambda (obj) (if (scode? obj) obj nil))
             frame))

;;; Called from GET-ENVIRONMENT.

(define (interpreter-frame-env frame)
  (let ((code (interpreter-frame-code frame)))
    (and code
         (let ((shape (get-shape code)))
           (and shape
                (let ((lenv (frame-any heuristically-lenv? frame)))
                  (cond (lenv (make-local-env shape lenv))
                        (else (get-environment shape)))))))))


;;; (RUN code lenv genv) -> value
;;;     is the way to run a piece of intermediate code.

(define-integrable (run code lenv genv)
  (*run* lenv genv code))      ; make TN's pack better?? ;++ flush

(define (standard-run lenv genv code)
  (cond ((lvspec? code)                 ; Local variable
         (fetch-from-lenv code lenv))
        ((extend? code)                 ; General expression
         (code lenv genv))
        (else                           ; Literal
         code)))

(lset *run* standard-run)

;;; (SCODE (LAMBDA (lenv genv) . body) . methods) is the standard
;;; way to create code tree nodes.  S-code is the term from MIT
;;; Scheme for intermediate code.

(lset *scode-count* 0)

;++ changed for T3 objects

(define-local-syntax (scode . rest)
  `(block (set *scode-count* (fx+ *scode-count* 1))
          (object ,@rest 
            ((scode? self) t)
            ((disclose self)
             (cond ((get-shape self)
                    => (lambda (shape) (disclose-scode self shape)))
                   (else nil)))
            ((get-proc-name self)
             (cond ((get-shape self)
                    => get-proc-name)
                   (else nil)))            ; ???
            ((get-loaded-file self)
             (cond ((get-shape self)
                    => get-loaded-file)
                   (else nil)))
            ((print-type-string self) "Compiled-expression"))))

(define-predicate scode?)

(define-operation (get-shape code) nil)

(define (empty-shape) nil)

(define-operation (disclose-scode code shape)
  (cond ((lvspec? code)
         (invert-lvspec shape code))
        ((extend? code)
         '<expression>)
        (else
         `',code)))

(define (disclose-scode-list shape . stuff)
  (map (lambda (code) (disclose-scode code shape)) stuff))

;;; -------------------- Main dispatch.

(define (compile exp shape fn?)
  (cond ((atom? exp)
         (compile ((atom-expander *syntax-table*) exp) shape fn?))
        ((not (proper-list? exp))
         (compile-error shape "expression is an improper list~%  ~S" exp))
        (else
         (let ((head (car exp)))
           (cond ((symbol? head)
                  (cond ((syntax-table-entry *syntax-table* head)
                         => (lambda (descr)
                              (cond ((lambda-bound? shape head)
                                     (warning
 '("form beginning with symbol ~S is being interpreted as a~%"
   "**~13tspecial form and not as a call~%"     ;Weird indentation^2
   "**~13t~S~%")
                                             head
                                             exp)))
                              (compile-special-form descr exp shape fn?)))
                        (else
                         (compile-call exp shape))))
                 ((syntax-descriptor? head)
                  (compile-special-form head exp shape fn?))
                 (else
                  (compile-call exp shape)))))))

(define (compile-special-form descr exp shape fn?)
  (let ((new-exp (check-special-form-syntax descr exp)))
    (cond ((neq? exp new-exp)
           ;; An error was reported, and luser gave us a new form.
           (compile new-exp shape fn?))
          ((table-entry compilator-table descr)
           ;; Syntax primitively understood by this evaluator.
           => (lambda (proc) (proc descr exp shape fn?)))
          (else
           ;; Non-primitive syntax; assume it's a macro.
           (compile (expand-macro-form descr exp *syntax-table*)
                    shape
                    fn?)))))

(define (compile-call exp shape)
  (let ((proc (compile (car exp) shape t))
        (args (map (lambda (arg) (compile arg shape nil))
                   (cdr exp))))
    (case (length (cdr exp))
      ((0) (scode (lambda (lenv genv) ((run proc lenv genv)))
                  ((get-shape self) shape)
                  ((disclose-scode self shape)
                   (disclose-scode-list shape proc))))
      ((1) (let ((arg0 (car args)))
             (scode (lambda (lenv genv)
                      ((run proc lenv genv) (run arg0 lenv genv)))
                    ((get-shape self) shape)
                    ((disclose-scode self shape)
                     (disclose-scode-list shape proc arg0)))))
      ((2) (let ((arg0 (car args))
                 (arg1 (cadr args)))
             (scode (lambda (lenv genv)
                      ((run proc lenv genv) (run arg0 lenv genv)
                                            (run arg1 lenv genv)))
                    ((get-shape self) shape)
                    ((disclose-scode self shape)
                     (disclose-scode-list shape proc arg0 arg1)))))
      ((3) (let ((arg0 (car args))
                 (arg1 (cadr args))
                 (arg2 (caddr args)))
             (scode (lambda (lenv genv)
                      ((run proc lenv genv) (run arg0 lenv genv)
                                            (run arg1 lenv genv)
                                            (run arg2 lenv genv)))
                    ((get-shape self) shape)
                    ((disclose-scode self shape)
                     (disclose-scode-list shape proc
                                          arg0 arg1 arg2)))))
      ((4) (let ((arg0 (car args))
                 (arg1 (cadr args))
                 (arg2 (caddr args))
                 (arg3 (car (cdddr args))))
             (scode (lambda (lenv genv)
                      ((run proc lenv genv) (run arg0 lenv genv)
                                            (run arg1 lenv genv)
                                            (run arg2 lenv genv)
                                            (run arg3 lenv genv)))
                    ((get-shape self) shape)
                    ((disclose-scode self shape)
                     (disclose-scode-list shape proc
                                          arg0 arg1 arg2 arg3)))))
      (else (scode (lambda (lenv genv)
                     (apply (run proc lenv genv)
                            (map (lambda (arg) (run arg lenv genv))
                                 args)))
                   ((get-shape self) shape)
                   ((disclose-scode self shape)
                    (apply disclose-scode-list shape proc args)))))))

;;; The special forms.

(define-local-syntax (define-compilator pat args . body)
  (destructure (((name . foo) pat))
    (let ((spect ((*value t-implementation-env 'arglist->argspectrum)
                  foo)))
      `(set (table-entry compilator-table
                         (obtain-syntax-table-entry 
                               (env-syntax-table (the-environment))
                               ',name
                               ',spect))
            (lambda (#f %%exp%% . ,args)
              (destructure ((,foo (cdr %%exp%%)))
                . ,body))))))

(define compilator-table (make-table 'compilator-table))

(define-compilator (quote thing) (shape fn?)
  (ignore shape fn?)
  (compile-literal thing))

(define (compile-literal obj)
  (cond ((or (fixnum? obj)
             (extend? obj))
         (scode (lambda (lenv genv) (ignore lenv genv) obj)
                ((disclose-scode self shape)
                 (ignore shape)
                 `',obj)))
        (else                              ; Hack - see RUN
         obj)))

(define-compilator (call proc . rest) (shape fn?)
  (compile-call (cons proc rest) fn?))
  
(define compiled-undefined-if-value
  (compile-literal undefined-if-value))

(define-compilator (if test con . alts) (shape fn?)
  (let ((test (compile test shape nil))
        (con  (compile con shape fn?))  ; ??
        (alt  (cond ((null? alts) compiled-undefined-if-value)
                    ((null? (cdr alts))
                     (compile (car alts) shape fn?))    ; ??
                    (else
                     (compile-error shape
                                    "illegal IF syntax~%  ~S"
                                    `(if ,test ,con ,@alts))))))
    (scode (lambda (lenv genv)
             (if (run test lenv genv) (run con lenv genv) (run alt lenv genv)))
           ((disclose-scode self shape)
            (cond ((eq? alt compiled-undefined-if-value)
                   `(if ,@(disclose-scode-list shape test con)))
                  (else
                   `(if ,@(disclose-scode-list shape test con alt)))))
           ((get-shape self) shape))))

(define-compilator (block . body) (shape fn?)
  (compile-block body shape fn?))

(define (compile-block exp-list shape fn?)
  (cond ((null-list? exp-list) (compile-literal nil))
        ((null-list? (cdr exp-list)) (compile (car exp-list) shape fn?))
        (else
         (let ((code (map (lambda (exp) (compile exp shape nil))
                          exp-list)))
           (scode (lambda (lenv genv)
                    (do ((c code (cdr c)))
                        ((null? (cdr c)) (run (car c) lenv genv))
                      (run (car c) lenv genv)))
                  ((get-shape self) shape)
                  ((disclose-scode self shape)
                   `(block ,@(map (lambda (c)
                                    (disclose-scode c shape))
                                  code))))))))

;;; LAMBDA.

(define-compilator (lambda vars . body) (shape fn?)
  (compile-lambda nil vars body shape fn?))

(define-compilator (named-lambda name vars . body) (shape fn?)
  (compile-lambda name vars body shape fn?))
                          
;++ (define (duplicate-identifiers? arg-list)
;++   (iterate loop ((l arg-list))
;++     (cond ((memq? (car l) (cdr l))
;++            (error "LAMBDA with duplicate identifier in argument list - ~s~%"
;++                   (cons name arg-list)))
;++           (else (loop (cdr l))))))

;++ changed for T3 objects and no more TC bug; removed statistics
;++ test for duplicate identifiers - see above

(define (compile-lambda name vars body-exps outer-shape fn?)
  (let ((cenv (let ((others (shape-cenv outer-shape)))
                (cond ((null? vars) others) ; ****
                      (else (add-contour vars others)))))
        (spect (arglist->argspectrum vars)) ; unnecessary cons
        (body nil))
    (labels ((shape (object (lambda (lenv genv)
               (object (lambda args           ; return a lexical closure
                   (let ((nargs (compatible-with-argspectrum? args spect)))
                     (cond ((not nargs)
                            (handle-wrong-number-args (or name (disclose shape))
                                                      spect
                                                      args))
                           ((and (fx= (car spect) 0) (not (cdr spect)))
                            (run body lenv genv))
                           (else
                            (run body (add-contour args lenv) genv)))))
                 ((get-environment self) (make-local-env outer-shape lenv))
                 ((get-loaded-file self) (get-loaded-file outer-shape))
                 ((identification self) name)
                 ((argspectrum self) spect)
                 ((disclose self) (disclose shape))))
           ((scode? self) t)
           ((shape-cenv self) cenv)
           ((get-shape self) outer-shape)       ; ???!?
           ((identification self) name)
           ((disclose self)
            `(lambda ,(cond ((and (fx= (car spect) 0) (not (cdr spect)))   '())
                            (else (cenv-first cenv)))
               . ,body-exps))
           ((get-proc-name self)     ;For backtrace!
            (or name (get-proc-name outer-shape)))
           ((get-loaded-file self) (get-loaded-file outer-shape))
           ((disclose-scode self shape) (ignore shape) (disclose self))
           ((print-type-string self) "Open-procedure"))))
    (set *scode-count* (fx+ *scode-count* 1))
    (set body (compile-block body-exps shape nil))
    shape)))

(define-operation (shape-cenv shape)
  shape)

(define (handle-wrong-number-args name spectrum args)
  (let ((n     (car spectrum))
        (nary? (cdr spectrum)))
    (error (list "wrong number of arguments to procedure -~%"
                 "**~10t~s~%**~10t~s takes~a ~a argument~p.~%")
           (cons name args)
           name
           (if nary? " at least" "")
           n
           n)))

(define-compilator (object . stuff) (shape fn?)
  (compile (expand-object-form stuff) shape fn?))

;;; -------------------- Other randomness.

(define-compilator (the-environment) (shape fn?)
  (ignore fn?)
  (scode (lambda (lenv genv)
           (ignore genv)
           (make-local-env shape lenv))
         ((disclose-scode self shape)
          (ignore shape)
          '(the-environment))))

(define-compilator (bound? var) (shape fn?)
  (ignore fn?)
  (cond ((lambda-bound? shape var)
         (compile-literal t))
        (else
         (scode (lambda (lenv genv)
                  (ignore lenv)
                  (*bound? (genv-env genv) var))))))

(define-compilator (lset-variable-value var val) (shape fn?)
  (ignore fn?)
  (compile-lbind var val shape nil))

(lset *current-definition* nil)

(define (current-definition) *current-definition*)

(define-compilator (define-variable-value var val) (shape fn?)
  (ignore fn?)
  (bind ((*current-definition* var))
    (compile-lbind var val shape t)))

(define (compile-lbind var val shape define?)
  (cond ((lambda-bound? shape var)
         (warning "~S or ~S on a ~S-bound variable~%  ~G~%"
                  'define 'lset 'lambda
                  `(,(if define? 'define 'lset) ,var ,val))
         (compile `(,(t-syntax 'set-variable-value) ,var ,val)
                  shape nil))
        (else
         (let ((valx (compile val shape nil)))
           (scode (lambda (lenv genv)
                    ((if define? *define *lset)
                     (genv-env genv) var (run valx lenv genv)))
                  ((get-shape self) shape)
                  ((disclose-scode self shape)
                   `(,(if define? 'define 'lset)
                     ,var ,(disclose-scode valx shape))))))))

;;; Local syntax: DEFINE-LOCAL-SYNTAX, LET-SYNTAX

(define-compilator (define-local-syntax . spec) (shape fn?)
  (ignore fn?)
  (compile-literal (set-local-syntax *syntax-table* spec)))

(define-compilator (let-syntax specs . body) (shape fn?)
  (let ((syntax (make-syntax-table *syntax-table* nil)))
    (walk (lambda (spec)
            (set-local-syntax syntax spec))
          specs)
    (bind ((*syntax-table* syntax))
      (compile-block body shape fn?))))

(define (set-local-syntax syntax spec)        ;auxiliary for above
  (let ((pat (car spec))
        (body (cdr spec)))
    (receive (sym exp)
             (cond ((pair? pat)
                    (return (car pat)
                            `(,(t-syntax 'macro-expander) ,pat . ,body)))
                   (else
                    (return pat (car body))))
      (set (syntax-table-entry syntax sym)
           (eval exp (env-for-syntax-definition syntax)))
      sym)))

;++ flush (define-compilator (locale var . body) (shape fn?)
;  (ignore fn?)
;  (let ((code (compile-top (blockify body)
;                           *syntax-table*
;                           (get-loaded-file shape))))
;    (scode (lambda (lenv genv)
;             (let ((new-env (make-locale (make-local-env shape lenv)
;                                         var)))
;               (if var
;                   (bind (((print-env-warnings?) nil))
;                     (*define new-env var new-env)))
;               (run-compiled-code code new-env)))
;           ((disclose-scode self shape)
;            `(locale ,var ,@body)))))

;;; Implement LABELS as a source rewrite.

(define-compilator (labels specs . body) (shape fn?)
  (compile
   (iterate loop ((s specs)
                  (vars '())
                  (inits '()))
    (cond ((null-list? s)
           `((,(t-syntax 'lambda) ,vars
               ,@(reverse! inits)
               . ,body)
             ,@(map (lambda (var) (ignore var) 'unbound-label)
                    vars)))
          (else
           (let ((spec (car s)))
             (cond ((atom? spec)
                    (syntax-error "bad ~S spec~%  (~S (... ~S ...) ...)"
                                  'labels 'labels spec))
                   ((atom? (car spec))
                    (loop (cdr s)
                          (cons (car spec) vars)
                          (cons `(,(t-syntax 'set-variable-value) ,@spec)
                                inits)))
                   (else
                    (loop (cdr s)
                          (cons (caar spec) vars)
                          (cons `(,(t-syntax 'set-variable-value)
                                  ,(caar spec)
                                  (,(t-syntax 'lambda) ,(cdar spec)
                                                       ,@(cdr spec)))
                                inits))))))))
  shape fn?))

(define-compilator (declare . stuff) (shape fn?)
  (ignore fn?)
  (compile-literal 'declare))

(define-compilator (primop . stuff) (shape fn?)
  (ignore fn?)
  (error "primops don't interpret yet -~%~10t~s" '(primop . ,stuff)))

(define-compilator (define-foreign . stuff) (shape fn?)
  (ignore fn?)
  (error "foreign definitions don't interpret yet -~%~10t~S" 
         '(define-foreign . ,stuff)))

;;; Generally useful utility:

(define (compile-error shape . rest)
  (compile (apply syntax-error rest) shape nil))

;;; -------------------- Variable and environment stuff.

;;; Three primitive operations on variable bindings: fetch, store, locative.

(define-compilator (variable-value var) (shape fn?)
  (compile-var var shape fn?))

(define (compile-var var shape fn?)
  (and fn?
       (syntax-table-entry *syntax-table* var)
       (warning "call to variable ~S is not being treated as a special form~%"
                var))
  (cond ((shape-lookup shape var)
         => (lambda (spec) (compile-lexvar spec)))
        (else
         (compile-static shape var))))

(define-compilator (set-variable-value var val) (shape fn?)
  (ignore fn?)
  (let ((valx (compile val shape nil)))
    (cond ((shape-lookup shape var)
           => (lambda (spec) (compile-set-lexvar spec valx)))
          (else
           (compile-assign-static valx shape var)))))

(define-compilator (var-locative var) (shape fn?)
  (ignore fn?)
  (cond ((shape-lookup shape var)
         => (lambda (spec)
              (scode (lambda (lenv genv)
                       (ignore genv)
                       (lexvar-locative var spec lenv))
                     ((get-shape self) shape))))
        (else
         (compile-static-locative var shape))))

;;; Fetch, store and locative operations for static variables:

;;; Returns a ZZ pair (var . index)

(define (get-static-zz var)
  (or (table-entry *free-vars* var)
      (set (table-entry *free-vars* var)
           (cons var (swap *free-var-count* (fx+ *free-var-count* 1))))))

(define-integrable (get-locative genv zz)
  (or (vref genv (cdr zz))
      (really-get-locative genv zz)))

(define (really-get-locative genv zz)
  (cond ((env-lookup (genv-env genv) (car zz) nil nil)
         => (lambda (loc)
              (vset genv (cdr zz) loc)
              loc))
        (else
         (object nil
           ((contents self)
            (cond ((vref genv (cdr zz)) => contents)
                  (else
                   (error "variable ~S is unbound" (car zz)))))
           ((set-contents self val)
            (cond ((vref genv (cdr zz))
                   => (lambda (loc) (set-contents loc val)))
                  (else
                   (vset genv
                         (cdr zz)
                         (reluctantly-bind (genv-env genv) (car zz)))
                   (set-contents self val))))
           ((locative? self) t)
           ((print-type-string self) "Locative")))))

(define (compile-static shape var)
  (let ((zz (get-static-zz var)))
    (scode (lambda (lenv genv)
             (ignore lenv)
             (let ((loc (get-locative genv zz)))
               (cond ((vcell? loc)
                      (let ((z (vcell-contents loc)))
                        (cond ((nonvalue? z)
                               (no-op (error "bound variable ~S has no value"
                                             (car zz))))
                              (else z))))
                     (else
                      (contents loc)))))
           ((get-shape self) shape)
           ((disclose-scode self shape)
            (ignore shape)
            (let ((var (car zz)))
              (cond ((and (symbol? var)
                          (not (syntax-table-entry standard-syntax-table
                                                   var)))
                     var)
                    (else `(variable-value ,var))))))))

(define (compile-assign-static valx shape var)
  (let ((zz (get-static-zz var)))
    (scode (lambda (lenv genv)
             (let ((val (run valx lenv genv))
                   (loc (get-locative genv zz)))
               (set-contents loc val)))                  
           ((get-shape self) shape)
           ((disclose-scode self shape)
            `(set ,(car zz) ,(disclose-scode valx shape))))))

(define (compile-static-locative var shape)
  (let ((zz (get-static-zz var)))
    (scode (lambda (lenv genv)
             (ignore lenv)
             (get-locative genv zz))
           ((get-shape self) shape))))

;;; Local environment stuff

(define (heuristically-lenv? obj)
  (iterate loop ((l obj) (i 0))
    (cond ((lenv-end? l)
           (if (environment? l) obj nil))
          ((not (proper-list? (lenv-first l))) nil)
          ((fx> i 1000) nil)    ; Circularity hack
          (else
           (loop (lenv-rest l) (fx+ i 1))))))

(define (lenv-end lenv)
  (cond ((atom? lenv) lenv)
        (else (lenv-end (lenv-rest lenv)))))

;;; Shape lookup stuff.

;;; (SHAPE-LOOKUP shape exp) -> lvspec or false
;;;     Returns either a local variable spec (lvspec), which is actually
;;;     a fixnum in two bit fields, or false if the variable isn't
;;;     locally bound.

(define (lambda-bound? shape var)
  (shape-lookup shape var))

;;; Look for local variable in SHAPE; return an LVSPEC if there is
;;; one.  Returns false if no local variable exists.

(define (shape-lookup shape var)
  (iterate loop1 ((v (shape-cenv shape))
                  (m 0))
    (cond ((cenv-end? v) nil)
          (else
           (iterate loop2 ((w (cenv-first v))
                           (n 1))               ; ?
             (cond ((atom? w)
                    (cond ((eq? var w) (lvspec m (fx- 0 n)))
                          (else (loop1 (cenv-rest v) (fx+ m 1)))))
                   ((eq? var (car w))
                    (lvspec m n))       ; success
                   (else
                    (loop2 (cdr w) (fx+ n 1)))))))))

(define (make-local-env shape lenv)          
  (cond ((lenv-end? lenv) lenv)
        (else
         (object (lambda (var local? create?)
                   (cond ((or local? create?)
                          (error '("illegal to create new bindings"
                                   " in this environment~%  ~S")
                                 `(env-lookup ... ,var ,local? ,create?)))
                         ((shape-lookup shape var)
                          => (lambda (spec)
                               (lexvar-locative var spec lenv)))
                         (else
                          (env-lookup (lenv-end lenv) var local? create?))))
           ((env-superior self) (lenv-end lenv))
           ((walk-local-env self proc)
            (really-walk-local-env (shape-cenv shape) lenv proc))
           ((crawl-exhibit-env self)
            (format (terminal-output) "Local variable environment:~%")
            (walk-local-env self
                     (lambda (var val)
                       (let ((to (terminal-output)))
                         (format to "  ~8S = " var)
                         (print-one-line val to)
                         (fresh-line to))))
            (format (terminal-output) "Outer environment: ~S~%"
                    (lenv-end lenv)))
           ((get-environment self) self)
           ((get-loaded-file self)
            (get-loaded-file (env-superior self)))
           ((environment? self) t)
           ((print-type-string self) "Environment")))))
 

(define-operation (walk-local-env env proc))

(define (really-walk-local-env cenv lenv proc)
  (iterate loop1 ((v cenv)
                  (e lenv))
    (cond ((cenv-end? v) nil)
          (else
           (iterate loop2 ((w (cenv-first v))
                           (f (lenv-first e)))
             (cond ((atom? w)
                    (cond ((not (null? w))
                           (proc w f)))
                    (loop1 (cenv-rest v) (lenv-rest e)))
                   (else
                    (proc (car w) (car f))
                    (loop2 (cdr w) (cdr f)))))))))

;;; Given a local variable spec, and the shape to which it's relative,
;;; return the name of the variable.  This depends on the fact that
;;; shapes and local environments have the same representation!

(define (invert-lvspec shape lvspec)
  (run (compile-lexvar lvspec) (shape-cenv shape) 'lose))

;;; Fetch, store, locative for local variable.

(lset *scode-lexvar-count* 0)

(define (compile-lexvar spec)
  (set *scode-lexvar-count* (fx+ *scode-lexvar-count* 1))
  spec)

(define (compile-set-lexvar spec valx)
  (scode (lambda (lenv genv)
           (store-into-lenv spec lenv (run valx lenv genv)))
         ((disclose-scode self shape)
          `(set ,(invert-lvspec shape spec)
                ,(disclose-scode valx shape)))))

(define (lexvar-locative var spec lenv)
  (object nil
          ((contents self)
           (fetch-from-lenv spec lenv))
          ((set-contents self value)
           (store-into-lenv spec lenv value))
          ((locative? self) t)
          ((identification self) var)
          ((print-type-string self) "Locative")))

;;; Get the value of a local variable.

(define (fetch-from-lenv spec lenv)
  (cond ((fx> (lvspec-over spec) 0)     ; conditional moved out of middle
         (let ((back (lvspec-back spec))
               (over (lvspec-over spec)))
            (do ((e1 lenv (lenv-rest e1))
                 (i1 0 (fx+ i1 1)))
                ((fx= i1 back)
                 (do ((e2 (lenv-first e1) (cdr e2))
                      (i2 1 (fx+ i2 1)))
                     ((fx= i2 over) (car e2)))))))
        (else
         (let ((back (lvspec-back spec))
               (over (lvspec-over spec)))
           (do ((e1 lenv (lenv-rest e1))
                (i1 0 (fx+ i1 1)))
               ((fx= i1 back)
                (do ((e2 (lenv-first e1) (cdr e2))
                     (i2 -1 (fx- i2 1)))
                    ((fx= i2 over) e2))))))))

;;; Set the value of a local variable.

(define (store-into-lenv spec lenv val)
  (let ((back (lvspec-back spec))
        (over (lvspec-over spec)))
    (do ((e1 lenv (lenv-rest e1))
         (i1 0 (fx+ i1 1)))
        ((fx= i1 back)
         (cond ((fx> over 0)
                (do ((e2 (lenv-first e1) (cdr e2))
                     (i2 1 (fx+ i2 1)))
                    ((fx= i2 over) (set (car e2) val))))
               (else
                ;; LENV-FIRST here assumed to be same as CDR!
                (do ((e2 e1 (cdr e2))
                     (i2 -1 (fx- i2 1)))
                    ((fx= i2 over) (set (cdr e2) val)))))))))
