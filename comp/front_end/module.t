(herald (front_end module)
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

;;;                    PROCESSING MODULES
;;;==========================================================================

;;; Temporary debugging flag
(lset *front-debug* nil)

;;; The expression being compiled
(lset *current-module-exp* nil)

;;; The top level procedure.  EXPS is a list of expressions to be compiled
;;; using SYNTAX and EARLY-BINDING-ENV.  SHAPEs keep track of variable scoping.
;;; USES hold information about forward references to global variables so that
;;; they can checked against the definitions when the definitions are
;;; processed. 

(define (file-exps->nodes+shape exps syntax)
  (let ((shape (create-shape *new-env*)))
    (bind ((*variable-id* '0)
           (*shape* shape)
           (*syntax* (make-syntax-table syntax '*syntax*))
           (*current-module-exp* nil)
           (*value-table* (make-table '*value-table*))  ; simplify-call
           (*definitions* nil)                          ; analyze
           (*uses*        nil)                          ; analyze
           (*child-vars* '())                           ; nodestuff
           (*vector->lexical-var*                       ; nodestuff
            (make-infinite-vector 0 false '*vector->lexical-var*)))
      (if *cross-compiling?*
          (install-system-constants *running-system-constants*))
      (let ((exps (make-module-exps (expand-forms exps syntax) shape)))
        (receive (uses plain integrated)
                 (do-integrable-exps exps shape)
          (exps->nodes plain uses shape)
          (let ((nodes (finish-exps (append integrated plain))))
            (remove-child-vars)
            (new-print-variable-info (shape-new-env shape)
                                     (shape-free shape)
                                     (shape-borrowed shape)
                                     *early-binding-env*)
            (if *cross-compiling?*
                (install-system-constants *target-system-constants*))
            (return nodes shape)))))))

;;; A structure to hold all of the information about an expression.

(define-structure-type module-exp
  source       ; source expression
  form         ; expression
  node         ; node
  syntax       ; associated syntax table
  index        ; index of this expression in the module
  def          ; name or variable defined at top level
  variant      ; type of definition
  uses         ; global variables used
  )

(define (create-module-exp form syntax index source)
  (let ((new (make-module-exp)))
    (receive (def variant)
             (form-definition form)
      (set (module-exp-source  new) source)
      (set (module-exp-form    new) form)
      (set (module-exp-node    new) nil)
      (set (module-exp-syntax  new) syntax)
      (set (module-exp-def     new) def)
      (set (module-exp-variant new) variant)
      (set (module-exp-uses    new) nil)
      (set (module-exp-index   new) index)
      new)))
             
;;; If a form is a simple definition, return the variable being defined and
;;; the type of definition.  This also replaces the syntax with the 
;;; corresponding primop.  This prevents the alphatizer from seeing the
;;; definition.

(define (form-definition form)
  (cond ((not (and (pair? form)
                   (proper-list? form)
                   (fx= 3 (length form))
                   (symbol? (cadr form))))
         (return nil nil))
        ((eq? (car form) syntax/define-variable-value)
         (set (car form) primop/*define)
         (return (cadr form) 'define))
        ((eq? (car form) syntax/lset-variable-value)
         (set (car form) primop/*lset)
         (return (cadr form) 'lset))
        (else
         (return nil nil))))

;;; Turn a list of forms into a list of MODULE-EXP records.

(define (make-module-exps forms shape)
  (iterate loop ((forms forms) (i 0) (exps '()))
    (cond ((null? forms)
           (process-global-declarations (reverse! exps) shape))
          (else
           (destructure ((((form syntax source) . forms) forms))
             (let ((exp (create-module-exp form syntax i source)))
               (add-def-to-shape exp shape)
               (loop forms (fx+ i 1) (cons exp exps))))))))
                     
(define (process-global-declarations exps shape)
  (filter! (lambda (exp)
             (cond ((and (pair? (module-exp-form exp))
                         (eq? syntax/declare (car (module-exp-form exp))))
                    (process-global-declaration (module-exp-form exp) shape)
                    nil)
                   (else t)))
           exps))

;;;             MAKING THE NEW DEFINITION ENVIRONMENT
;;;===========================================================================

;;; Put the definition from EXP into the new definition environment of SHAPE.

(define (add-def-to-shape exp shape)
  (cond  ((module-exp-def exp)
          (add-global-definition shape
                                 (module-exp-def exp)
                                 (module-exp-variant exp))
          (let ((var (table-entry (shape-new-env shape)
                                  (module-exp-def exp))))
            (if var (set (module-exp-def exp) var))))))
  

;;;                    EXPRESSIONS->NODES
;;;===========================================================================

;;; Transform EXPS into transmogrified nodes.

(define (exps->nodes exps uses shape)
  (iterate loop ((exps exps) (uses uses))
   (cond ((null? exps)
          (return))
         ((module-exp-node (car exps))
          (loop (cdr exps) uses))
         (else
          (set *current-module-exp* (car exps))
          (let* ((node (exp->node (car exps) shape))
                 (uses (transmogrify-node node uses)))
            (loop (cdr exps) uses))))))

(define (exp->node exp shape)
  (if *front-debug*
      (pretty-print exp (terminal-output)))
  (let ((node (->value-node `(,syntax/lambda () ,(module-exp-form exp))
                            (module-exp-syntax exp)
                            shape)))
    (set (module-exp-node exp) node)
    (if *front-debug*
        (pp-cps node (terminal-output)))
    node))

;;; Simplify NODE and check the types of its references to global variables.

(define (transmogrify-node node uses)
  (simplify-call node)
  (orbit-debug '"~&Simplified tree: ~%")
  (if *debug-flag*
      (pp-cps node (terminal-output)))
  (receive (defs new-uses)
           (def-and-use-analyze node)
    (let ((forward-uses (check-uses new-uses uses)))
      (walk (lambda (var)
              (format *noise+terminal* '"~&~S~%" (variable-name var)))
            defs)
      forward-uses)))

;;; The final pass over the expressions before they are turned over to the
;;; code generator.  This puts the expressions back into their original order.

(define (finish-exps exps)
  (map! (lambda (exp)
          (set *current-module-exp* exp)
          (if (and (variable? (module-exp-def exp))
                   (memq? 'type-safe-closed-form 
                          (definition-data
                           (variable-definition (module-exp-def exp)))))
              (make-type-safe (module-exp-node exp)))
          (fixup-node-tree (module-exp-node exp)))
        (sort-list exps
                   (lambda (x y)
                     (fx< (module-exp-index x)
                          (module-exp-index y))))))

;;; This is used for converting subexpressions, usually the closed-compiled
;;; versions of primops, into nodes.

(define (subexpression->code-tree exp)
  (if *front-debug*
      (pretty-print exp (terminal-output)))
  (let* ((node (->value-node `(,syntax/lambda () ,exp)
                             *syntax*
                             *shape*)))
      (if *front-debug*
          (pp-cps node (terminal-output)))
      node))


;;;               PROCESSING INTEGRABLE DEFINITIONS
;;;===========================================================================

;;; Finds the expressions that contain integrable definitions and 
;;; transmogrifies them into node trees.  The nodes are sorted so that each is
;;; simplified before its definition is needed in simplifying any of the
;;; others.  ODD-INTS are expressions that contain integrable definitions in
;;; such a way that the compiler cannot determine what value the defined
;;; variable has.
;;; Afterwards the expressions are put back into their original order.

(define (do-integrable-exps exps shape)
  (iterate loop ((exps exps) (ints '()) (plain '()))
    (cond ((null? exps)
           (return (transmogrify-integrable-nodes ints)
                   (sort-list plain
                              (lambda (x y)
                                (fx< (module-exp-index x)
                                     (module-exp-index y))))
                   ints))
          ((not (need-value? (module-exp-def (car exps)) shape))
           (loop (cdr exps) ints (cons (car exps) plain)))
          (else
           (process-integrable-exp (car exps) shape)
           (loop (cdr exps) (cons (car exps) ints) plain)))))

(define (need-value? name shape)
  (and name
       (let ((def (variable-definition
                   (if (variable? name)
                       name
                       (table-entry (shape-new-env shape) name)))))
         (and def (eq? 'constant (definition-variant def))))))

(define (transmogrify-integrable-nodes ints)
  (iterate loop ((ints (int-defs->sorted-nodes ints)) (uses '()))
    (cond ((null? ints)
           uses)
          (else
           (set *current-module-exp* (car ints))
           (loop (cdr ints)
                 (transmogrify-node (module-exp-node (car ints)) uses))))))

;;; Convert an expression containing an integrable definition into a node tree.
;;; The node-tree is analyzed to get the variable that is defined and any
;;; integrable variables that are used in the definition.  Nothing is done if
;;; it is not clear what value the node-tree defines.

(define (process-integrable-exp exp shape)
  (set *current-module-exp* exp)
  (let ((node (exp->node exp shape)))
    (receive (defs uses)
             (quick-def-and-use-analyze node)
      (cond ((and (fx= 1 (length defs))
                  (eq? (module-exp-def exp)
                       (car defs)))
             (set (module-exp-uses exp) (delq! (car defs) uses)))
            (else
             (bug '"expression ~S contains several defs" exp)))
      (return))))

;;; This procedure sorts INTS so that no integrable variable is used before it
;;; is defined.  (TABLE-ENTRY INT-TABLE <def>) is T if the definition has
;;; already been scheduled and NIL if it hasn't.

(define (int-defs->sorted-nodes ints)
  (let ((int-table (make-table 'int-table)))
    (walk (lambda (int)
            (set (table-entry int-table (module-exp-def int)) t))
          ints)
    (iterate loop ((ints ints) (res '()))
      (cond ((null? ints)
             (reverse! res))
            (else 
             (receive (ready unready)
                      (get-ready-int-defs ints int-table)
               (walk (lambda (int)
                       (set (table-entry int-table (module-exp-def int))
                            nil))
                     ready)
               (loop unready (append! ready res))))))))

;;; GET-READY-INT-DEFS finds the INT-DEFs that have no unscheduled
;;; predecessors.  If there aren't any then there is a recursive loop of
;;; integrable definitions and something should be done.

(define (get-ready-int-defs ints table)
  (iterate loop ((ints ints) (unready '()) (ready '()))
    (cond ((and (null? ints) ready)
           (return (sort-list ready
                              (lambda (x y)
                                (fx> (module-exp-index x)
                                     (module-exp-index y))))
                   unready))
          ((null? ints)
           (error '"integration loop ~S"
                  (map (lambda (int) (variable-name (module-exp-def int)))
                       unready)))
          ((ready? (car ints) table)
           (loop (cdr ints) unready (cons (car ints) ready)))
          (else
           (loop (cdr ints) (cons (car ints) unready) ready)))))

(define (ready? int table)
  (let ((uses (filter! (lambda (use)
                         (table-entry table use))
                       (module-exp-uses int))))
    (set (module-exp-uses int) uses)
    (null? uses)))

;;;              BUILDING EXPRESSION NODES INTO A CODE TREE
;;;============================================================================

;;; Build nodes into a tree.  An extra thunk is wrapped around the entire
;;; tree because the rest of the compiler expects it to be there.

(define (rebuild nodes)
  (map (lambda (n) (set (node-parent n) empty))
       nodes)
  (do ((first (car nodes) (attach first (car nodes)))
       (nodes (cdr nodes) (cdr nodes)))
      ((null? nodes)
       (let ((top (value-node->thunk first)))
         (set (node-parent top) nil)
         top))))

;;; NODE -> (LAMBDA () V) { = (LAMBDA (C) (C V)) in CPS}

(define (value-node->thunk node)
  (let* ((c-var (create-variable 'k))
         (new-l (create-lambda-node 'b (flist2 nil c-var '())))
         (call (create-call-node 2 0)))
    (relate call-proc call (create-reference-node c-var))
    (relate (call-arg 1) call node)
    (relate lambda-body new-l call)
    new-l))

;;; Make SECOND the continuation of FIRST.

(define (attach first second)
  (let ((new-l (create-lambda-node 'b (flist1 (create-variable 'ignore) '())))
        (c-var (car (lambda-variables first))))
    (relate lambda-body new-l (detach (lambda-body second)))
    (case (length (variable-refs c-var))
      ((0)
       (bug '"top level lambda ~S doesn't use its continuation" first))
      ((1)
       (let ((ref (car (variable-refs c-var))))
         (cond ((eq? (node-role ref) call-proc)
                (replace (node-parent ref) (detach (lambda-body new-l)))
                (erase-all new-l))
               (else
                (replace ref new-l)))
         (relate lambda-body second (detach (lambda-body first)))
         (erase first)))
      (else
       (let ((call (create-call-node 2 1)))
         (relate call-proc call first)    
         (relate (call-arg 1) call new-l)
         (relate lambda-body second call))))
    second))

;;;                            DEBUGGING
;;;============================================================================

(define (free-refs name var)
  (format t '"~&~S: ~S~%"
          name
          (let ((l
                 (map (lambda (ref)
                        (let ((loc (containing-definition (node-parent ref))))
                          (if loc (variable-name loc) 'top-level)))
                      (variable-refs var))))
            (if (memq? 'top-level l)
                (cons 'top-level (delq! 'top-level l))
                l))))

(define (make-trees name erase?)
  (receive (exp early-binding-env syntax #f)
           (read-file (->filename name))
    (front-init early-binding-env
                (lambda ()
                  (receive (nodes env)
                           (file-exps->nodes+shape (cddr exp) syntax)
                    (cond (erase?
                           (walk erase-all nodes)
                           (return '() env))
                          (else
                           (return nodes env))))))))



