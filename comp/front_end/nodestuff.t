(herald (front_end nodestuff)
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

;;;============================================================================
;;; MISCELLANEOUS NODE-TREE AND DEFINITION UTILITES
;;;============================================================================

;;; Useful utility, not used by any code

(define (node-base node)
  (do ((p node (node-parent p)))
      ((not (node? (node-parent p)))
       p)))

;;; Get the type, if any, associated with NODE.

(define (any-known-type node)
  (cond ((not (reference-node? node))
         (get-node-definition-type node))
        (else
         (let* ((var (reference-variable node))
                (binder (variable-binder var)))
           (cond ((get-variable-definition var)
                  => (lambda (def)
                       (get-definition-type def node)))
                 ((not binder)
                  nil)
                 ((eq? var (lambda-rest-var binder))
                  'literal)
                 ((eq? call-proc (node-role binder))
                  (any-known-type ((call-arg (fx+ -1 (variable-number var)))
                                   (node-parent binder))))
                 (else nil))))))

(define (get-definition-type def node)
  (cond ((definition->primop def)
         => (lambda (p)
              (fix-primop-type (primop.type p node))))
        ((definition-type def)
         => identity)
        (else
         nil)))

;;; Get the 'real' value of NODE, i.e. the definition value if NODE is a
;;; variable reference.

(define (known-value node)
  (cond ((and (reference-node? node)
              (get-variable-definition (reference-variable node)))
         => definition-value)
        (else node)))

;;; The same as KNOWN-VALUE except that it only returns primops.

(define (known-primop node)
  (cond ((primop-node? node)
         (primop-value node))
        ((and (reference-node? node)
              (get-variable-definition (reference-variable node)))
         => definition->primop)
        (else nil)))

;;; The same as KNOWN-VALUE except that it checks for objects and returns the
;;; whole definition instead of just the value.

(define (known-object-definition node)
  (cond ((and (reference-node? node)
              (get-variable-definition (reference-variable node)))
         => (lambda (def)
              (if (definition->object def) def nil)))
        (else nil)))

;;; Are all references to VAR in call position?

(define (all-refs-are-calls? var)               
  (every? (lambda (ref)
            (eq? (node-role ref) call-proc))
          (variable-refs var)))

;;; Returns T if REF is being referred to as an L-value.

(define (nonvalue-reference? ref)
  (and (eq? (node-role ref) (call-arg 2))
       (primop-node? (call-proc (node-parent ref)))
       (primop.uses-L-value? (primop-value (call-proc (node-parent ref))))))

;;; Walk (or map) a tree modifying procedure down a variable's references.

(define (walk-refs-safely proc var)
  (let ((refs (free-copy-list (variable-refs var))))
    (walk proc refs)
    (return-list-to-freelist refs)
    (return)))

(define (map-refs-safely proc var)
  (let* ((refs (free-copy-list (variable-refs var)))
         (res (map proc refs)))
    (return-list-to-freelist refs)
    res))

;;; The value a thunk will return when it is called.

(define (thunk-value l-node)
  (let ((refs (variable-refs (lambda-cont-var l-node))))
    (cond ((or (fxn= 1 (length refs))
               (neq? call-proc (node-role (car refs))))
           nil)
          ((fxn= 1 (length (call-args (node-parent (car refs)))))
           (bug "thunk returns multiple values"))
          (else   
           ((call-arg 1) (node-parent (car refs)))))))

;;; Does THUNK just return a lambda or object node?

(define (simple-thunk? thunk)
  (let ((node (thunk-value thunk)))
    (and (node? node)
         (eq? (node-parent (node-parent node)) thunk)
         (or (lambda-node? node)
             (object-node? node)))))

;;; Replaces the call node CALL with VALUE.
;;; (<proc> <exit> . <args>) => (<exit> <value>)

(define (replace-call-with-value call value)
  (cond ((fxn= 1 (call-exits call))
         (bug "can only substitute for call with one exit ~S" call))
        (else
         (let ((cont (detach ((call-arg 1) call))))
           (walk (lambda (node)
                   (if (node? node)
                       (erase-all (detach node))))
                 (cdr (call-args call)))
           (set (call-exits call) 0)
           (replace (call-proc call) cont)
           (relate-new-call-args call (if value `(,value) '()))))))

;;;=============================================================================
;;;                             Child Variables
;;;=============================================================================

;;; These are used as aliases for global variables.  They typically have
;;; slightly different declaration information than their parents.

(lset *child-vars* '())

(define (remove-child-vars)
  (walk (lambda (v)
          (cond ((get-child-variable v 'parent)
                 => (lambda (p)
                      (walk-refs-safely (lambda (n)
                                          (set (reference-variable n) p))
                                        v)
                      (modify (variable-refs p)
                              (lambda (l) (append! (variable-refs v) l)))))
                (else
                 (bug "child-variable ~S has no parent"))))
        *child-vars*)
  (set *child-vars* '()))

(define (get-child-variable var type)
  (iterate loop ((l (variable-flags var)))
    (cond ((null? l)
           nil)
          ((and (pair? (car l))
                (eq? (caar l) type))
           (cdar l))
          (else
           (loop (cdr l))))))

(define (add-child-variable parent child type)
  (push *child-vars* child)
  (push (variable-flags parent) (cons type child))
  (push (variable-flags child) (cons 'parent parent))
  (push (variable-flags child) (cons 'type type)))

;;;============================================================================
;;; SUBSTITUTING VALUES FOR VARIABLES
;;;============================================================================

;;; Substitute VAL for VAR.  If DETACH? is true then VAL should be detached
;;; and so can be used instead of a copy for the first substitution.

(define (substitute var val detach?)
  (let ((refs (variable-refs var)))
;    (orbit-debug "substituting: ~A := ~A~%" var (pp-cps-2 val))
    (set (variable-refs var) '())
    (if (and (reference-node? val)               ;Keep LET variable names
             (eq? 'v (variable-name (reference-variable val))))
        (set (variable-name (reference-variable val))
             (variable-name var)))
    (cond (refs
           (walk (lambda (ref)
                   (replace ref (copy-node-tree val)))
                 (if detach? (cdr refs) refs))
           (if detach? (replace (car refs) (detach val)))
           (return-list-to-freelist refs))
          (detach?
           (erase-all (detach val))))))

;;; Replace every reference of OLD-VAR in NODE with a reference to NEW-VAR.
;;; Return T if any change is made.

(define (substitute-in-node-tree node old-var new-var)
  (let ((count (length (variable-refs new-var))))
    (substitute-vars-in-node-tree node (list old-var) (list new-var))
    (fxn= count (length (variable-refs new-var)))))

;;; Walk the tree NODE replacing references to variables in OLD-VARS with
;;; the corresponding variables in NEW-VARS.  Uses VARIABLE-FLAG to mark
;;; the variables being replaced.

(define (substitute-vars-in-node-tree node old-vars new-vars)
  (walk (lambda (old new)
          (if (used? old)
              (set (variable-flag old) new)))
        old-vars new-vars)
  (iterate tree-walk ((node node))
    (cond ((lambda-node? node)
           (walk tree-walk (call-proc+args (lambda-body node))))
          ((call-node? node)
           (walk tree-walk (call-proc+args node)))
          ((object-node? node)
           (walk tree-walk (object-operations node))
           (walk tree-walk (object-methods node))
           (tree-walk (object-proc node)))
          ((and (reference-node? node)
                (variable-flag (reference-variable node)))
           => (lambda (new)
                (replace node (create-reference-node new))))))
  (walk (lambda (old) 
          (if (used? old)
              (set (variable-flag old) nil)))
        old-vars))

;;;============================================================================
;;; COPYING NODE TREES
;;;============================================================================

;;; Copy the node-tree NODE.  This dispatches on the type of NODE.

(define (copy-node-tree node)
  (let ((new (xselect (node-variant node)
               ((leaf-node?)
                (copy-leaf node))
               ((lambda-node?)
                (copy-lambda node))
               ((call-node?)
                (copy-call node))
               ((object-node? node)
                (copy-object node)))))
     new))

;;; Copying leaves.  Variables which have been copied have the copy in the
;;; NODE-FLAG field.

(define (copy-leaf node)
  (xcase (leaf-variant node)
    ((literal)
     (create-literal-node (literal-value node)))
    ((primop)
     (create-primop-node (primop-value node)))
    ((reference)
     (let ((var (reference-variable node)))
       (cond ((and (variable-binder var)
                   (variable-flag var))
              => create-reference-node)
             (else
              (create-reference-node var)))))))

;;; Copy a lambda node and its variables.  The variables' copies are put in
;;; their VARIABLE-FLAG while the lambda's body is being copied.

(define (copy-lambda node)
  (let* ((vars (free-map (lambda (var)
                           (if var
                               (set (variable-flag var)
                                    (create-variable (variable-name var)))
                               nil))
                         (lambda-rest+variables node)))
         (new-node (create-lambda-node (variable-name (lambda-self-var node))
                                       vars)))
    (relate lambda-body new-node (copy-node-tree (lambda-body node)))
    (walk (lambda (var)
            (if var (set (variable-flag var) nil)))
          (lambda-rest+variables node))
    new-node))

(define (copy-call node)
  (let ((new-node (create-call-node (length (call-proc+args node))
                                    (call-exits node))))
    (relate call-proc new-node (copy-node-tree (call-proc node)))
    (relate-call-args new-node (free-map copy-node-tree (call-args node)))
    new-node))

(define (copy-object node)
  (let ((new-node (create-object-node (object-operation? node)
                                      (length (object-operations node)))))
    (relate object-proc new-node (copy-node-tree (object-proc node)))
    (relate-object-ops new-node
                      (free-map copy-node-tree (object-operations node)))
    (relate-object-methods new-node
                           (free-map copy-node-tree (object-methods node)))
    new-node))

;;;============================================================================
;;; STORING NODE TREES IN VECTORS
;;;============================================================================

;;;    Convert a node into a vector
;;;
;;;  primop        => <primop>
;;;  literal       => QUOTE <literal>
;;;  reference     => <index of the variable's name in vector> if lexical
;;;                   LOCALE <variable-name> if not lexical
;;;                   KNOWN <variable> if this variable was
;;;                       originally LOCALE but has been statically bound
;;;  lambda        => LAMBDA #vars <variable names...> <call>
;;;  call          => <exits> <number of args> <args>
;;;  object        => (OBJECT <proc> <ops> <methods>) if at top level
;;;                => INTERNAL-OBJECT <proc> <ops> <methods> if not                                                     


;;; This returns a vector if NODE has no free references to lexically bound
;;; variables, otherwise it returns NIL.  If NODE is an object node the vector
;;; is actually a list of vectors.  This is done so that the procedure and
;;; methods may be reconstructed seperately.

(define (node->vector node)
  (cond ((object-node? node)
         (list 'object
               (object-operation? node)
               (node->vector (object-proc node))
               (map node->vector (object-operations node))
               (map node->vector (object-methods node))))
        (else
         (let* ((exp-vec (make-infinite-vector 100 false))
                (vec (cons exp-vec 0))
                (value (if (real-node->vector node vec)
                           (copy-node-vector exp-vec (cdr vec))
                           nil)))
           (recycle exp-vec)
           value))))

;;; Copies the expanding vector into a normal vector of the appropriate size.

(define (copy-node-vector exp-vec size)
  (let ((new (make-vector size)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i size))
      (set (vref new i) (exp-vec i)))
    new))

;;; Add another element to the vector (which is really a (<vector> . <index>)
;;; pair) keeping track of the current index.

(define-integrable (add-datum vec value)
  (set ((car vec) (cdr vec)) value)
  (set (cdr vec) (fx+ (cdr vec) 1)))

;;; The main dispatch

(define (real-node->vector node vec)
  (cond ((primop-node? node)
         (add-datum vec (primop-value node)))
        ((literal-node? node)
         (add-datum vec 'quote)
         (add-datum vec (literal-value node)))
        ((reference-node? node)
         (variable->vector (reference-variable node) vec))
        ((lambda-node? node)
         (lambda->vector node vec))
        ((object-node? node)
         (object->vector node vec))
        (else
         (bug "node->vector got funny node ~S" node))))

;;; VARIABLE-FLAGs are used to mark variables with their position in the
;;; vector.

(define (lambda->vector node vec)
  (add-datum vec 'lambda)
  (add-datum vec (variable-name (lambda-self-var node)))
  (add-datum vec (length (lambda-rest+variables node)))
  (walk (lambda (var)
          (cond (var
                 (set (variable-flag var) (cdr vec))
                 (add-datum vec (variable-name var)))
                (else
                 (add-datum vec nil))))
        (lambda-rest+variables node))
  (let ((ok? (call-node->vector (lambda-body node) vec)))
    (walk (lambda (var)
            (if var
                (set (variable-flag var) nil)))
          (lambda-rest+variables node))
    ok?))

;;; Return NIL if the variable is lexically bound and its binder has not
;;; been written into the vector.

(define (variable->vector var vec)
  (cond ((and (variable-binder var)
              (fixnum? (variable-flag var)))
         (add-datum vec (variable-flag var))
         t)
        ((variable-binder var)
         nil)
        (else
         (add-datum vec 'locale)
         (add-datum vec (variable-name var))
         t)))

(define (call-node->vector node vec)
  (add-datum vec (call-exits node))
  (add-datum vec (length (call-proc+args node)))
  (node-list->vector (call-proc+args node) vec))

(define (object->vector node vec)
  (add-datum vec 'internal-object)
  (add-datum vec (object-operation? node))
  (add-datum vec (length (object-operations node)))
  (and (real-node->vector (object-proc node) vec)
       (node-list->vector (object-operations node) vec)
       (node-list->vector (object-methods node) vec)))

;;; Write the nodes in list NODES into VEC.

(define (node-list->vector nodes vec)
  (iterate loop ((children nodes))
    (cond ((null? children)
           t)
          ((real-node->vector (car children) vec)
           (loop (cdr children)))
          (else
           nil))))

;;;============================================================================
;;; TURNING VECTORS BACK INTO NODES
;;;============================================================================

;;; Reconstructing nodes requires an environment in addition to the vector.
;;; Free variables are looked up in the environment.  The vectors and
;;; environments are usually in definitions.

;;; Get the value of DEF if it is a variable.

(define (definition->variable def)
  (vector->variable (definition-value def) (definition-env def)))

;;; Get the value of VECTOR if it is a variable.

(define (vector->variable vector env)
  (cond ((not (vector? vector))
         nil)
        (else
         (case (vref vector 0)
           ((locale)
            (obtain-locale-bound-variable (vref vector 1) env))
           ((early-bound)
            (vref vector 1))
           (else nil)))))

;;; The same thing for primops and objects.

(define (definition->primop def)
  (let ((vector (definition-value def)))
    (if (and (vector? vector)
             (primop? (vref vector 0)))
        (vref vector 0)
        nil)))

(define (definition->object def)
  (let ((vector (definition-value def)))
    (if (and (pair? vector)
             (eq? (car vector) 'object))
        vector
        nil)))

;;; Definitions and vectors are made back into nodes.

(define (definition->node def)
  (vector->node (definition-value def) (definition-env def)))

(define (vector->node vector env)
  (cond ((vector? vector)
         (real-vector->node (cons vector -1) env))
        ((and (pair? vector)
              (eq? (car vector) 'object))
         (list->object-node (cdr vector) env))
        (else
         (bug "VECTOR->NODE got funny value ~S~%" vector))))

;;; Pop the next thing off of the vector (which is really a (<vector> . <index>)
;;; pair).

(define-integrable (get-datum vec)
  (set (cdr vec) (fx+ (cdr vec) 1))
  (vref (car vec) (cdr vec)))

;;; Vector used to contain any lexical variables required in reconstructing
;;; a node.

(lset *vector->lexical-var*
  (make-infinite-vector 0 false '*vector->lexical-var*))

;;; Dispatch on the next thing in VEC.

(define (real-vector->node vec env)
  (let ((exp (get-datum vec)))
    (cond ((primop? exp)
           (create-primop-node exp))
          ((variable? exp)
           (create-reference-node exp))
          ((fixnum? exp)
           (create-reference-node (*vector->lexical-var* exp)))
          (else
           (case exp
            ((lambda)
             (vector->lambda-node vec env))
            ((quote)
             (create-literal-node (get-datum vec)))
            ((locale)
             (let ((name (get-datum vec)))
               (create-reference-node (obtain-locale-bound-variable name env))))
            ((known)
             (let ((var (get-datum vec)))
               (create-reference-node var)))
            ((internal-object)
             (vector->object-node vec env))
            ((object)
             (bug '"vector->node got an OBJECT in ~S" vec))
            (else
             (bug '"vector->node got an unknown form ~S" exp)))))))

(define (vector->lambda-node vec env)
  (let* ((self-name (get-datum vec))
         (count (get-datum vec)))   
    (do ((i 0 (fx+ i 1))
         (v '() (cons-from-freelist (vector->bound-variable vec) v)))
        ((fx>= i count)
         (let ((node (create-lambda-node self-name (reverse! v))))
           (relate lambda-body node (vector->call-node vec env))
           node)))))        

;;; Replace a variable name with a new variable.

(define (vector->bound-variable vec)
  (let ((name (get-datum vec)))
    (cond (name
           (let ((var (create-variable name)))
             (set (*vector->lexical-var* (cdr vec)) var)
             var))
          (else nil))))

(define (vector->call-node vec env)
  (let* ((exits (get-datum vec))
         (count (get-datum vec))
         (node (create-call-node count exits)))
    (vector->node-list vec node call-arg count env)))

(define (vector->node-list vec node relation count env)
  (do ((i 0 (fx+ i 1)))
      ((fx>= i count)
       node)
    (relate (relation i) node (real-vector->node vec env))))

;;; There are two ways of encoding objects depending on the context.

(define (vector->object-node vec env)
  (let* ((op? (get-datum vec))
         (count (get-datum vec))
         (node (create-object-node op? count)))
    (relate object-proc node (real-vector->node vec env))
    (vector->node-list vec node object-op count env)
    (vector->node-list vec node object-method count env)
    node))

(define (list->object-node list env)
  (destructure (((#f op? proc ops methods) list))
    (let ((node (create-object-node op? (length ops))))
      (relate object-proc node (vector->node proc env))
      (relate-object-ops node (free-map (lambda (v)
                                          (vector->node v env))
                                        ops))
      (relate-object-methods node (free-map (lambda (v)
                                              (vector->node v env))
                                            methods))
      node)))




