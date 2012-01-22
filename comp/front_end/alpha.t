(herald (front_end alpha)
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

;;; Macro expansion, alphatization, and creating the node tree.
;;;============================================================================

;;; The syntax table containing all special forms of interest to the
;;; compiler.

(define primitive-syntax-table
  (make-syntax-table false 'primitive-syntax-table))

;;; A table of compilator procedures to handle instances of special forms the
;;; compiler knows about.

(define primitive-handler-table
  (make-table 'primitive-handler-table))

;;;    This translates expressions into node trees.  Expressions include
;;; symbols, pairs, variable records, primops, nodes, and self-evaluating
;;; things.  All ->NODE actually does is to dispatch to other procedures.
;;;
;;; SYNTAX is the syntax table to be used in compiling the expression.
;;; SHAPE is an object that keeps track of the environment.
;;;
;;; Three values are returned: the top of the node tree created and the 
;;; node tree's continuation's parent and role.  The continuation itself is
;;; EMPTY.  If the top of the tree is not a call node the contination's parent
;;; and role are undefined.
;;;
;;; Thus if node C1 is to be the continuation of the expression (+ A B) :
;;;      (RECEIVE (NODE C-PARENT C-ROLE)
;;;               (->NODE '(+ A B) <syntax> <shape>)
;;;        (RELATE C-ROLE C-PARENT C1))

(define (->node exp syntax shape)
  (cond ((variable? exp)
         (return (create-reference-node exp) nil nil))
        ((primop? exp)
         (return (create-primop-node exp) nil nil))
        ((node? exp)
         (return exp nil nil))
        ((pair? exp)
         (pair->node exp syntax shape))
        (else
         (->node ((atom-expander syntax) exp) syntax shape))))

;;; More dispatching

(define (pair->node exp syntax shape)
  (let ((head (car exp)))
    (cond ((not (proper-list? exp)) 
           (->node (syntax-error '"expression is an improper list~%  ~S" exp)
                   syntax shape))
          ((syntax-descriptor? head)
           (special-form->node head exp syntax shape))
          ((syntax-table-entry syntax head)
           => (lambda (desc)
                (special-form->node desc exp syntax shape)))
          (else
           (make-call exp syntax shape)))))

;;; Does syntax error checking, primitive syntax dispatch, and macro 
;;; expansion.  The error checking is done by CHECK-SPECIAL-FORM, a T system
;;; procedure.

(define (special-form->node descr exp syntax shape)
  (let ((proc (table-entry primitive-handler-table descr))
        (new-exp (check-special-form-syntax descr exp)))
    (cond ((neq? exp new-exp)
           ;; An error was reported, and luser gave us a new form.
           (->node new-exp syntax shape))
          (proc
           (proc exp syntax shape))
          ((macro-expander? descr)
           (->node (expand-macro-form descr exp syntax) syntax shape))
          (else
           (syntax-error "special form unknown to this compiler~%  ~S" exp)))))

;;;===========================================================================
;;;                    SPECIAL FORMS
;;;===========================================================================

;;; Syntax for defining compilators for the special forms that the compiler
;;; recognizes.  The syntax is:
;;;
;;; (DEFINE-COMPILER-SYNTAX (<syntax-name> . <argument form>)
;;;                         (<syntax table variable> <shape variable>)
;;;   . <expression->node translation code>)
;;;
;;; This puts a syntax descriptor into PRIMITIVE-SYNTAX-TABLE and a handler in
;;; PRIMITIVE-HANDLER-TABLE using the syntax descriptor as a key.  

(define-local-syntax (define-compiler-syntax pattern vars . body)
  (let* ((name (car pattern))
         (sym (concatenate-symbol 'syntax/ name))
         (exp (generate-symbol 'exp)))
    `(let ((descr (syntax-table-entry (env-syntax-table t-implementation-env)
                                      ',name)))
       (set (syntax-table-entry primitive-syntax-table ',name) descr)
       (set (table-entry primitive-handler-table descr)
            (lambda (,exp . ,vars)
              (ignorable . ,vars)
              (destructure ((,(cdr pattern) (cdr ,exp)))
                ,@body)))
       (define ,sym descr))))

;;; (QUOTE <blah>)
;;; => a literal node containing <blah>.

(define-compiler-syntax (quote value) (syntax shape)
  (return (create-literal-node (free-copy-tree value)) nil nil))

;;; (LAMBDA vars . body)
;;; => a lambda node, courtesy of ALPHA-LAMBDA

(define-compiler-syntax (lambda vars . body) (syntax shape)
  (alpha-lambda 'p vars body syntax shape))

;;; Aaaarrggghh!!!!

(define-compiler-syntax (named-lambda name vars . body) (syntax shape)
  (alpha-lambda name vars body syntax shape))

;;; Variable structures are created for the variables (including the
;;; continuation variable) and added to the shape (not including the
;;; continuation variable).  The body is converted into a call node with
;;; its continuation being the continuation variable.  The variables
;;; are then removed from the shape and have any declarations processed
;;; (this includes adding cells for variables that are set).

(define (alpha-lambda name var-names body syntax shape)
  (let* ((vars (map! (lambda (name)
                       (if (null? name) nil (create-variable name)))
                     (fix-vars (cons-from-freelist 'k var-names))))
         (real-vars (cons-from-freelist (car vars) (cddr vars))))
    (bind-variables shape real-vars)
    (let ((node (create-lambda-node name vars)))
      (receive (value-node c-parent c-role)
               (make-block body syntax shape)
        (relate lambda-body node value-node)
        (relate c-role c-parent (create-reference-node (cadr vars)))
        (unbind-variables shape real-vars)
        (walk (lambda (var)
                (cond (var
                       (process-lexical-declarations var)
                       (cond ((memq? 'lexical (variable-flags var))
                              (introduce-cell var)
                              (modify (variable-flags var)
                                      (lambda (l) (delq! 'lexical l))))))))
              vars)
        (return-to-freelist real-vars)
        (return node nil nil)))))

;;; Makes a proper list out of VARS by putting the last CDR onto the front.
;;; (v1 v2 ... vN . X) => (X v1 v2 ... vN)

(define (fix-vars vars)
  (do ((vars vars (cdr vars))
       (res '() (cons-from-freelist (car vars) res)))
      ((atom? vars)
       (cons vars (reverse! res)))))

;;; VARIABLE-VALUE

(define-compiler-syntax (variable-value name) (syntax shape)
  (return (create-reference-node (obtain-variable shape name)) nil nil))

;;; SET-VARIABLE-VALUE  VAR-LOCATIVE  LSET  DEFINE-VARIABLE-VALUE
;;; Binding information is added to the shape and a call to the appropriate
;;; primop is returned.  In the case of LSET and DEFINE-VARIABLE-VALUE a
;;; warning is issued if the name being defined is already lexically bound.

(define-compiler-syntax (set-variable-value name value) (syntax shape)
  (let ((var (add-definition shape name 'set)))
    (make-call `(,primop/*set-var ,var ,value) syntax shape)))

(define-compiler-syntax (var-locative name) (syntax shape)
  (let ((var (add-definition shape name 'set)))
    (make-call `(,primop/*locative ,var) syntax shape)))

(define-compiler-syntax (lset-variable-value name value) (syntax shape)
  (global-variable-set 'lset primop/*lset name value syntax shape))

(define-compiler-syntax (define-variable-value name value) (syntax shape)
  (global-variable-set 'define primop/*define name value syntax shape))

(define (global-variable-set variant primop name value syntax shape)
  (let ((var (add-definition shape name variant)))
    (cond ((and (variable? var)
                (variable-binder var))
           (user-message 'warning
                         '"lexically bound variable ~S is being ~A"
                         '"the variable will be set instead"
                         (variable-name var)
                         (if (eq? 'lset (primop.definition-variant primop))
                             '"lset"
                             '"defined"))
           (make-call `(,primop/*set-var ,var ,value) syntax shape))
          (else
           (make-call `(,primop ,var ,value) syntax shape)))))

;;; (DECLARE key . stuff)
;;; Uses DECLARATION-HANDLER-TABLE to deal with declarations.  A literal node
;;; 'DECLARE is returned.

(define-compiler-syntax (declare key . stuff) (syntax shape)
  (cond ((table-entry declaration-handler-table key)
         => (lambda (handler)
              (handler stuff shape)))
        (else
         (orbit-warning "ignoring unknown declaration type ~S"
                        `(declare ,key . ,stuff))))
  (return (create-literal-node 'declare) nil nil))

;;; (PRIMOP id formals . clauses)
;;;   The special form for introducing primitive operations.  The primop is
;;; constructed, compiled, installed and added to the expression.
;;;   The name should be changed to stop all the warning messages for PRIMOP
;;; variables.

(define-compiler-syntax (primop id formals . clauses) (syntax shape)
  (let ((primop (eval (primop-code id formals clauses) orbit-env)))
    (set (primop.source primop) clauses)
    (add-new-primop shape primop)
    (make-call `(,primop/*primop ,primop) syntax shape)))

;;; (IF p c a)
;;; => ((LAMBDA (J)
;;;       (PRIMOP/CONDITIONAL (LAMBDA () (J C))
;;;                           (LAMBDA () (J A))
;;;                           PRIMOP/TEST
;;;                           PRIMOP/TRUE?
;;;                           P))
;;;     <cont>)

(define-compiler-syntax (if tested con . maybe-alt) (syntax shape)
  (let ((alt (if (null? maybe-alt) primop/undefined (car maybe-alt))))
    (let* ((j-var (create-variable 'j))
           (j-lambda (create-lambda-node 'p (flist2 nil j-var '())))
           (j-call (create-call-node 2 1))
           (call (list primop/conditional
                       (thunkify con j-var syntax shape)
                       (thunkify alt j-var syntax shape)
                       primop/test
                       primop/true?
                       tested))
           (c-call (make-call-with-exits 2 call syntax shape)))
      (relate call-proc j-call j-lambda)
      (relate lambda-body j-lambda c-call)
      (return j-call j-call (call-arg 1)))))

;;; Turn EXP into a thunk that calls CONT-VAR when it returns.

(define (thunkify exp cont-var syntax shape)
  (let ((l-node (create-lambda-node 'c (flist1 nil '()))))
    (receive (call c-parent c-role)
             (make-block (list exp) syntax shape)
      (relate lambda-body l-node call)
      (relate c-role c-parent (create-reference-node cont-var))
      l-node)))

;;; (LABELS ((v1 e1) (v2 e2) ... (vn en)) . body)
;;; => (PRIMOP/Y (LAMBDA (K v1 v2 ... vn)
;;;                (K (LAMBDA () . body)
;;;                   (LAMBDA (C1) (C1 e1))
;;;                   (LAMBDA (C2) (C2 e2))
;;;                   ...
;;;                   (LAMBDA (Cn) (Cn en)))))
;;; If the specs are empty this is just BLOCK

(define-compiler-syntax (labels specs . body) (syntax shape)
  (cond ((null? specs)
         (make-block body syntax shape))
        (else
         (receive (vars vals)
                  (parse-labels-specs specs)
           (make-y-call vars vals body syntax shape)))))

;;; This binds the variables, creates the PRIMOP/Y call, and then unbinds
;;; the variables.

(define (make-y-call vars vals body syntax shape)
  (bind-variables shape vars)
  (receive (b-call c-parent c-role)
           (make-block body syntax shape)
    (receive (args b-call)
             (make-y-args vars vals b-call syntax shape)
      (unbind-variables shape vars)
      (let-nodes ((b-lambda (#f c1) b-call)
                  (y-lambda (#f c2 . vars) ((* c2) 0 (^ b-lambda) . args)))
        (let ((call (create-call-node 3 1)))
          (relate call-proc call (create-primop-node primop/y))
          (relate (call-arg 2) call y-lambda)
          (relate c-role c-parent (create-reference-node c1))
          (walk (lambda (var)
                  (if var (process-lexical-declarations var)))
                vars)
          (return call call (call-arg 1)))))))

;;; Transform the VALS into thunks.  If a variable is set a cell introduced to
;;; hold the value and a call is added to the body to set the intitial value.

(define (make-y-args vars vals b-call syntax shape)
  (let ((thunks (map (lambda (val)
                       (->value-node `(,syntax/lambda () ,val) syntax shape))
                     vals)))
    (iterate loop ((vars vars) (thunks thunks)
                   (args '()) (b-call b-call))
      (cond ((null? vars)
             (return (reverse! args) b-call))
            ((or (not (car vars))
                 (not (memq? 'lexical (variable-flags (car vars)))))
             (loop (cdr vars) (cdr thunks)
                   (cons (car thunks) args) b-call))
            (else
             (let ((var (car vars)))
               (hack-references var var)
               (set (variable-flags var)
                    (delq! 'lexical (variable-flags var)))
               (loop (cdr vars) (cdr thunks)
                     (cons (labels-make-cell-thunk) args)
                     (add-set-contents b-call var (car thunks)))))))))

(define (add-set-contents call var thunk)
  (let-nodes ((c1 ((! thunk) 0 (^ l1)))
               (l1 (#f v) (($ primop/set-location)
                           1
                           (^ l2) ($ primop/cell-value) (* v) (* var)))
                (l2 (#f) call))
    c1))

;;; Parse SPECS into a list of variables and a list of value expressions.
;;; The SPECS may have implicit lambdas that need to be made explicit.

(define (parse-labels-specs specs)
  (return (free-map (lambda (spec)
                      (let ((pat (car spec)))
                        (create-variable (if (atom? pat) pat (car pat)))))
                     specs)
          (map (lambda (spec)
                 (let ((pat (car spec)))
                   (cond ((atom? pat) (cadr spec))
                         (else `(,(t-syntax 'named-lambda) ,(car pat) ,(cdr pat)
                                   . ,(cdr spec))))))
               specs)))

;;; (BLOCK . expressions)
;;; Handled by MAKE-BLOCK

(define-compiler-syntax (block . exp-list) (syntax shape)
  (make-block exp-list syntax shape))

;;; Local syntax
;;; Warn the luser and do what you can.

(define-compiler-syntax (define-local-syntax . spec) (syntax shape)
  (set-local-syntax syntax spec)
  (orbit-warning
    '"DEFINE-LOCAL-SYNTAX not at top level. It will not have proper scope.")
  (return (create-primop-node primop/undefined) nil nil))

;;; Let syntax

(define-compiler-syntax (let-syntax specs . body) (syntax shape)
  (let ((new-syntax (make-syntax-table syntax nil)))
    (walk (lambda (spec) (set-local-syntax new-syntax spec))
          specs)
    (make-block body new-syntax shape)))

;;; (OBJECT <proc>
;;;   ((<op1> . <args1>) . <method1>)
;;;      ...
;;;   ((<opN> . <argsN>) . <methodN>))
;;; =>
;;; (OBJECT <proc>
;;;         (<op1> ... <opN>)
;;;         ((LAMBDA (() () () . <args1>) . <method1>)
;;;             ...
;;;          (LAMBDA (() () () . <argsN>) . <methodN>)))
;;;
;;;   <op> may be a call in which case we eventually lose.
;;;
;;; (OBJECT (LAMBDA ...)
;;;   ((IDENTIFICATION SELF) '<name>)) compiles as LAMBDA with name <name>
;;;

(define-compiler-syntax (object proc . specs) (syntax shape)
  (cond ;((and (pair? proc)
        ;      (syntax-reference? (car proc) syntax/lambda syntax)
        ;      (fx= 1 (length specs))
        ;      (identification-method (car specs) syntax shape))
        ; => (lambda (name)
        ;      (destructure (((#f vars . body) proc))
        ;        (alpha-lambda name vars body syntax shape))))
        ((not (every? (lambda (x)
                        (simple-object-method? x shape))
                      specs))
         (make-object-using-macro proc specs syntax shape))
        (else                           
         (alpha-object proc specs syntax shape))))

(define (alpha-object proc specs syntax shape)
  (receive (proc-node c-parent c-role)
           (->node proc syntax shape)
    (cond ((call-node? proc-node)
           (alpha-object-bail-out proc-node c-parent c-role specs
                                  syntax shape))
          ((not (or (lambda-node? proc-node)
                    (known-literal? proc-node)))
           (make-object-using-macro proc-node specs syntax shape))
          (else
           (receive (ops methods)
                    (parse-object-specs specs syntax shape)
             (let ((node (create-object-node nil (length ops))))
               (relate-object-ops node ops)
               (relate-object-methods node methods)
               (relate object-proc node proc-node)
               (return node nil nil)))))))

(define (known-literal? node)
  (or (literal-node? node)
      (and (reference-node? node)
           (let ((var (reference-variable node)))
             (and (variable-definition var)
                  (eq? (get-definition-type (variable-definition var) node)
                       'literal))))))

(define (make-object-using-macro proc specs syntax shape)
  (->node (expand-object-form (cons proc specs))
          syntax shape))

(define (alpha-object-bail-out proc-node pc-parent pc-role specs syntax shape)
  (let ((p-val (create-variable 'v)))
    (receive (node c-parent c-role)
             (make-object-using-macro p-val specs syntax shape)
      (let-nodes ((l1 (#f (p-val p-val)) node))
        (relate pc-role pc-parent l1)
        (return proc-node c-parent c-role)))))

;;; Is SPEC of the form ((IDENTIFICATION SELF) '<symbol>) ?

;(define (identification-method spec syntax shape)
;  (destructure ((((op #f . rest) form . r-forms) spec))
;    (if (and (eq? op 'identification)
;             (null? rest)
;             (null? r-forms)
;             (pair? form)
;             (syntax-reference? (car form) syntax/quote syntax)
;             (symbol? (cadr form)))
;        (cadr form)
;        nil)))

;(define (syntax-reference? form desc syntax)
;  (or (eq? desc form)
;      (and (symbol? form)
;           (eq? desc (syntax-table-entry syntax form)))))

;;; ((OP . ARGS) . BODY)

(define (simple-object-method? exp shape)
  (and (pair? exp)
       (pair? (car exp))
       (symbol? (caar exp))
       (not (variable-binder (obtain-variable shape (caar exp))))))

;;; Parse SPECS into a list of operation nodes and method nodes.

(define (parse-object-specs specs syntax shape)
  (iterate loop ((specs specs) (ops '()) (methods '()))
    (cond ((null? specs)
           (return (reverse! ops) (reverse! methods)))
          (else
           (destructure ((((op state . vars) . body) (car specs)))
             (let ((op (->value-node op syntax shape))
                   (method (->value-node (make-method state vars body)
                                         syntax shape)))
               (loop (cdr specs)
                     (cons-from-freelist op ops)
                     (cons-from-freelist method methods))))))))

;;; Parse a method clause.  There are two forms of these.

(define (make-method state vars body)
  (cond ((atom? state)        ; old form
         `(,syntax/lambda (,state . ,vars)   
               (,syntax/declare ignorable ,state)
	       (,primop/remove-state-object)
               . ,body))
        ((fxn= 2 (length state))
         (error "bad syntax in state section of method clause ~S" state))
        (else
         (destructure (((self obj) state))
          `(,syntax/lambda (,self . ,vars)  
             ((,syntax/lambda (,obj)  . ,body)
	      (,primop/remove-state-object)))))))

;;; (THE-ENVIRONMENT)
;;; This becomes a reference to the special variable *THE-ENVIRONMENT* that
;;; the compiler knows about.

(define-compiler-syntax (the-environment) (syntax shape)
  (return (create-reference-node *the-environment*) nil nil))

;;; The following are not yet (and may never be) implemented:

;;; (let-reference ((a x) (b y)) ...)
;;;   ==>  (*let-reference (lambda (a b) ...)
;;;                        (locative x)
;;;                        (locative y))

;;; (locale () ... (define a x) ... (define b y) ...)
;;;   ==>  (labels ((a (block ... x))
;;;                 (b (block ... y)))
;;;          ...)

;;; (locale var . body)
;;;   ==>  (let ((env (make-locale (environment))))
;;;          (((expression (lambda (var) . body)) env) env)

;;; (expression E) ==> (*expression (lambda (env ... free vars ...) E))
;;; (environment)  ==> (*environment outer-env '(name1 ...) var1 ...)
;;; (define-unless var pred thunk) ==> (*define-unless env 'var pred thunk)


