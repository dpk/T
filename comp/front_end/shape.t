(herald shape)
                            
;;;============================================================================
;;;                             SHAPES
;;;============================================================================

;;;   A shape contains all the variables lexically bound at a particular
;;; moment.  Variables are added to a shape using the procedure BIND-VARIABLES
;;; and removed using UNBIND-VARIABLES.
;;;   The shape also keeps track of the locale variable usage.

(define-structure-type shape
  table        ; table of stacks of lexically bound variables, indexed by name
  free         ; table of free variables
  env          ; the superior definition-env to the module being compiled
  new-env      ; a table of names => variables
  borrowed     ; variables copied from ENV
  system       ; variables copied from the system
  introduced   ; variables referenced by integrated procedures
  primops      ; primops defined in this module
  )

(define (create-shape env)
  (let ((new (make-shape)))
    (set (shape-table        new) (make-table 'shape-table))
    (set (shape-free         new) (make-table 'shape-free))
    (set (shape-borrowed     new) (make-table 'shape-borrowed))
    (set (shape-system       new) (make-table 'shape-system))
    (set (shape-introduced   new) (make-pair-table 'shape-introduced))
    (set (shape-env          new) env)
    (set (shape-new-env      new) (make-table 'shape-new-env))
    (set (shape-primops      new) '())
    new))

;;; Hash tables indexed by pairs.

(define (make-pair-table id)
  (create-%table id 0 t pair?
                 (lambda (l)
                   (fixnum-abs (fx+ (descriptor-hash (car l))
                                    (descriptor-hash (cdr l)))))
                 (lambda (x y)
                   (and (eq? (car x) (car y))
                        (eq? (cdr x) (cdr y))))))

;;; Get the appropriate variable.
;;; This depends on (IF '() '#t '#f) => #f

(define (obtain-variable shape name)
  (cond ((table-entry (shape-table shape) name)    ; lexically bound
         => car)
        (else
         (obtain-locale-variable shape name))))

(define (obtain-locale-variable shape name)
  (cond ((table-entry (shape-new-env shape) name)  ; defined in this locale
         => identity)
        ((table-entry (shape-borrowed shape) name) ; already gotten from env
         => identity)
        (((shape-env shape) name)                  ; defined in env
         => (lambda (def)
              (let ((var (create-variable name)))
                (set (variable-definition var) def)
                (set (variable-refs var) '())
                (set (table-entry (shape-borrowed shape) name) var)
                var)))
        ((table-entry (shape-free shape) name)     ; already found free
         => identity)
        (else                                      ; never seen before
         (let ((var (create-variable name)))
           (set (table-entry (shape-free shape) name) var)
           var))))

;;; Binding and unbinding lexical variables.

(define (bind-variables shape vars)
  (let ((table (shape-table shape)))
    (walk (lambda (var)
            (if var
                (free-table-push table (variable-name var) var)))
          vars)))

;;; Remove the variables from the table checking for duplicate names in
;;; VARS.

(define (unbind-variables shape vars)
  (let ((table (shape-table shape)))
    (walk (lambda (var)
            (if var
                (let* ((name (variable-name var))
                       (entry (table-entry table name)))
                  (cond ((and entry (eq? var (car entry)))
                         (free-table-pop table name))
                        ((any? (lambda (v)
                                 (and (variable? v)
                                      (eq? name (variable-name v))
                                      (neq? v var)))
                               vars)
                         (modify (table-entry table name)
                                 (lambda (l)
                                   (delq! var l)))
                         (push (variable-flags var) 'duplicate))
                        (else
                         (bug "variable ~S not in shape ~S" var shape))))))
          vars)))

;;; Adding locale variables to the shape.  What happens depends on whether or
;;; not a variable of the same name is already in the shape.
;;; If a variable is found in:
;;;   TABLE        A bug if VARIANT isn't SET.
;;;                Add the variable to LEXICAL.
;;;   FREE         Move the variable to NEW-ENV.
;;;   BORROWED     Shadowing message if the variants are not compatible.
;;;                Warning if the borrowed variable's value has been integrated.
;;;                Move the variable to NEW-ENV.
;;;   ENV          Shadowing message if the variants are not compatible.
;;;                Add a new variable to NEW-ENV.
;;;   NEW-ENV      Warning message if the variants are not compatible.
;;;                Additional warning if the earlier variable's value has been
;;;                integrated somewhere.
;;;   not found    Add a new variable to NEW-ENV

(define (add-definition shape name variant)
  (cond ((table-entry (shape-table shape) name)
         => (lambda (pair)
              (let ((var (car pair)))
                (if (neq? variant 'set)
                    (orbit-warning '"global definition on lexical variable ~S" var))
                (if (not (memq? 'lexical (variable-flags var)))
                    (push (variable-flags var) 'lexical))
                var)))
        (else
         (add-global-definition shape name variant))))

(define (add-global-definition shape name variant)
  (cond ((table-entry (shape-new-env shape) name)     ; defined in this locale
         => (lambda (var)
              (let ((def (variable-definition var)))
                (cond ((not (definition-variant def))
                       (set (definition-variant def) variant))
                      (else
                       (check-multiple-defs name def variant)))
                var)))
        ((table-entry (shape-free shape) name)        ; already found free
         => (lambda (var)
              (set (table-entry (shape-free shape) name) nil)
              (add-to-new-env var variant shape)
              var))
        ((table-entry (shape-borrowed shape) name)    ; already gotten from env
         => (lambda (var)
              (let ((def (variable-definition var)))
                (cond ((not (compatible-variants variant
                                                 (definition-variant def)))
                       (orbit-warning '"shadowing ~S~%" name)
                       (if (memq? 'integrated (variable-flags var))
                           (orbit-warning
                       '"shadowed definition of ~S has already been integrated"
                                          name))))
                (set (table-entry (shape-borrowed shape) name) nil)
                (add-to-new-env var variant shape)
                var)))
        (((shape-env shape) name)                     ; defined in env
         => (lambda (def)
              (if (not (compatible-variants variant (definition-variant def)))
                  (orbit-warning '"shadowing ~S~%" name))
              (let ((var (create-variable name)))
                (add-to-new-env var variant shape)
                var)))
        (else                                         ; never seen before
         (let ((var (create-variable name)))
           (add-to-new-env var variant shape)
           var))))

(define (add-to-new-env var variant shape)
  (let ((new (make-definition-entry var (shape-env shape) '() variant nil nil)))
    (set (definition-env new) (shape-env shape))
    (set (table-entry (shape-new-env shape) (variable-name var)) var)))

;;; Check that two definitions are compatible, issuing a warning if they aren't.
;;; The only compatible variant pairs are (SET, LSET) and (SET, SET).
;;; Most of this is just to get a coherent error message.

(define (check-multiple-defs name def variant)
  (let ((variant1 (definition-variant def)))
    (cond ((compatible-variants variant variant1)
           => (lambda (variant)
                (if (not (memq? 'set (definition-data def)))
                    (push (definition-data def) 'set))
                (set (definition-variant def) variant)))
          ((eq? 'multiple variant1)
           (orbit-warning '"~S is also ~A~%" name (variant-string variant)))
          ((eq? variant variant1)
           (set (definition-variant def) 'multiple)
           (orbit-warning '"~S is ~A twice~%" name (variant-string variant)))
          (else
           (set (definition-variant def) 'multiple)
           (orbit-warning '"~S is both ~A and ~A~%"
                          name
                          (variant-string variant)
                          (variant-string variant1))))))

(define (compatible-variants v1 v2)
  (cond ((and (eq? v1 'set)
              (eq? v2 'set))
         'set)
        ((or (and (eq? v1 'lset)
                  (eq? v2 'set))
             (and (eq? v2 'lset)
                  (eq? v1 'set)))
         'lset)
        (else nil)))

(define (variant-string variant)
  (case variant
    ((set)      '"set")
    ((lset)     '"lset")
    ((define)   '"defined")
    (else (bug '"VARIANT-STRING got a funny variant ~S" variant))))

;;;               interface routines
;;;===========================================================================

;;; Add a primop to the list.

(define (add-new-primop shape primop)
  (push (shape-primops shape) primop))

;;; Get the variable NAME in the new environment if it is there.

(define (new-env-definition shape name)
  (let ((var (table-entry (shape-new-env shape) name)))
    (cond ((and var (variable-definition var))
           => identity)
          (else nil))))

;;; A hack used when a reference to a system variable is needed.

(define (get-system-variable name)
  (cond ((table-entry (shape-system *shape*) name)
         => identity)
        ((base-early-binding-env name)
         => (lambda (def)
              (let ((var (create-variable name)))
                (set (variable-definition var) def)
                (set (variable-refs var) '())
                (set (table-entry (shape-system *shape*) name) var)
                var)))
        ((obtain-locale-variable *shape* name)
         => (lambda (var)
;              (orbit-warning
;                     "variable ~S not in system, it will be treated as free~&"
;                     name)
              (set (table-entry (shape-system *shape*) name) var)
              var))
        (else
         (bug '"missing system variable ~S" name))))

;;; This should not be used.

(define (get-free-variable name)
  (obtain-locale-variable *shape* name))

;;; This is called when a procedure being integrated contains free references.

(define (obtain-locale-bound-variable name env)
  (cond ((eq? env *new-env*)
         (obtain-locale-variable *shape* name))
        (else
         (let ((key (cons name env)))
           (cond ((table-entry (shape-introduced *shape*) key)
                  => identity)
                 (else 
                  (let ((def (env name))
                        (var (create-variable name)))
                    (set (variable-definition var) def)
                    (set (variable-refs var) '())
                    (set (table-entry (shape-introduced *shape*) key) var)
                    var)))))))







