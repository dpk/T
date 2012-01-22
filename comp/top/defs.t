(herald (orbit_top defs))

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

;;;  Structure definitions
;;;  Node creation; interconnect manipulation

;;; Reckless (little type checking)
;;; Reclaims node and variable storage

;;; VARIABLES
;;;===========================================================================
;;; Structures to represent variables.

(define-structure-type variable
  name          ; Source code name for variable (temporary, for debugging only)
  id            ; Unique numeric identifier
  binder        ; LAMBDA node which binds this variable
  definition    ; Support information for this variable
  number        ; K: var = (NTH (LAMBDA-ALL-VARIABLES (VARIABLE-BINDER var)) K)
  refs          ; List of leaf nodes n for which (REFERENCE-VARIABLE n) = var.
  type          ; The type of the variable's value at point of binding
  rep           ; Representation for variable's value
  flag          ; Useful slot, used by shapes, COPY-NODE, NODE->VECTOR, etc.
  flags         ; For various annotations, e.g. IGNORABLE
  (((print self stream)
    (format stream "#{Variable~_~S~_~A}"
            (object-hash self)
            self))
   ((display self stream)        ; hack for ~A (?!)
    (format stream "~S_~S"
            (cond ((primop? (variable-name self))
                   (identification (variable-name self)))
                  (else
                   (variable-name self))) 
            (variable-id self)))))

(lset *variable-id* 0)

(define variable-pool
        (make-pool 'variable-pool make-variable 20 variable?))

(define (create-variable name)
  (let ((var (obtain-from-pool variable-pool)))
    (set (variable-name       var) name)
    (set (variable-id         var) *variable-id*)
    (set (variable-binder     var) nil)
    (set (variable-definition var) nil)
    (set (variable-refs       var) '())
    (set (variable-type       var) type/top)
    (set (variable-rep        var) 'rep/pointer)       
    (set (variable-flag       var) nil)
    (set (variable-flags      var) '())
    (set *variable-id* (fx+ 1 *variable-id*))
    var))

(define (used? var)
  (and var
       (variable-refs var)))

;;; NODES
;;;============================================================================
;;; There are three node types:
;;;  - LAMBDA
;;;  - CALL
;;;  - LEAF
;;; Calls have a nonzero number of children, lambda nodes have a single child,
;;; leaf node have none.

(define-structure-type node
  variant           ; Node type, a predicate (e.g. LAMBDA-NODE?)
  parent            ; Parent node
  role              ; node == ((NODE-ROLE node) (NODE-PARENT node))
  simplified?       ; True if it has already been simplified.
  instructions
  stuff-0           ; Variant components
  stuff-1
  stuff-2
  stuff-3
  stuff-4
  stuff-5
  (((print node stream)
    (print-node (node-variant node) node stream))))
;   ((disclose node)
;    (express node))))           ; For PP, and for CRAWL's P command.

(define node-pool (make-pool 'node-pool
                             (lambda ()
                               (let ((new (make-node)))
                                 (set (node-role new) '<new>)
                                 new))
                             30
                             node?))

(lset *node-count* 0)
(lset *node-return-count* 0)

;;; EMPTY
;;;==========================================================================
;;; EMPTY is used to mark empty parent and child slots in nodes.

(define empty
  (object nil ((print self stream) (writes stream "#{Empty}"))))

(define-integrable (empty? obj) (eq? obj empty))

(define *empty* empty) ; compatibility

(define (proclaim-empty probe)
  (cond ((not (empty? probe))
         (bug "not empty - ~S" probe))))

;;; NODE VARIANTS
;;;==========================================================================
;;; A "node variant" is a predicate which answers true to nodes which
;;; belong to this variant node type.

(define-operation (print-node variant node stream))

(define (create-node-variant id)
  (labels ((self
            (object (lambda (obj)
                      (eq? (node-variant obj) self))
                    ((print self stream)
                     (format stream "#{Node-variant~_~S}" id))
                    ((print-node self node stream)
                     (format stream "#{~S-node~_~S}" id (object-hash node))))))
    self))

(define (create-node variant)
  (let ((node (obtain-from-pool node-pool)))
    (if (not (or (eq? '<erased> (node-role node))
                 (eq? '<new> (node-role node))))
        (bug "new node already in use ~S" node))
    (set *node-count* (fx+ 1 *node-count*))
    (set (node-variant      node) variant)
    (set (node-parent       node) empty)
    (set (node-role         node) '<free>)
    (set (node-simplified?  node) nil)
    (set (node-instructions node) '())
    (set (node-stuff-0      node) nil)
    (set (node-stuff-1      node) nil)
    (set (node-stuff-2      node) nil)
    (set (node-stuff-3      node) nil)
    (set (node-stuff-4      node) nil)
    (set (node-stuff-5      node) nil)
    node))

(define (make-empty-node-list size)
  (do ((i 0 (fx+ 1 i))
       (l '() (cons-from-freelist empty l)))
      ((fx>= i size) l)))

;;; NODE FIELDS
;;;===========================================================================
;;;  These are used to rename the NODE-STUFF fields of particular node
;;; variants.

;;; Ugh.  Done to get node fields integrable in the quick version.
;;; (define-local-syntax (node-field variant field id) field)

(define-constant (node-field variant field id)
  (object (lambda (node)
            (field (check-arg variant node id)))
    ((setter self)
     (lambda (node val)
       (set (field (check-arg variant node id)) val)))
    ((identification self) id)))

;;; RELATIONS
;;;=========================================================================
;;; A "relation" is a selector procedure - something appropriate to put in
;;; the ROLE slot of a node.

(define (make-relation id variant slot)
  (object (lambda (node)
            (slot node))
    ((setter self) (setter slot))
    ((relation-variant self) variant)
    ((print self stream)
     (format stream "#{Relation~_~S}" id))))

(define (make-list-relation id variant slot index pred)
  (object (lambda (node)
            (nth (slot node) index))
    ((setter self)
     (lambda (node value) (set (nth (slot node) index) value)))
    ((relation-index self) index)
    ((relation-variant self) variant)
    ((pred self) t)               ; hack
    ((print self stream)
     (format stream "#{Relation~_~S}" id))))

(define-operation (relation-variant relation))
(define-operation (relation-index relation))

(define-integrable (relate relation parent child)
  (proclaim-empty (node-parent child))  ; Could be flushed
  (proclaim-empty (relation parent))    ; Could be flushed
  (set (relation parent) child)
  (set (node-parent child) parent)
  (set (node-role child) relation))

(define (relate-list relations start parent list children)
  (do ((i start (fx+ 1 i))
       (l list (cdr l))
       (children children (cdr children)))
      ((null? children))
    (let ((child (car children)))
      (proclaim-empty (node-parent child))     ; Could be flushed
      (proclaim-empty (car l))                 ; Could be flushed
      (set (node-role child) (relations i))
      (set (node-parent child) parent)
      (set (car l) child))))

;;; RECLAIMING NODES
;;;============================================================================
;;;     Erase node structure.  Updates the REFS slot of variables free to this
;;; node and returns the node to the pool.  There is some safety question
;;; here about erasing references to variables that have already been erased.
;;; I don't think it is a problem but it could be checked.

(define (erase node)
  (cond ((empty? node)
         nil)
        (else
         (cond ((eq? (node-role node) '<erased>)
                (bug "node erased twice ~S" node))
               ((reference-node? node)
                (let ((var (reference-variable node)))
                  (set (variable-refs var)
                       (free-delq! node (variable-refs var)))))
               ((lambda-node? node)
                (walk (lambda (v)
                        (if v (return-to-pool variable-pool v)))
                      (lambda-all-variables node))
                (return-list-to-freelist (lambda-all-variables node))
                (set (lambda-all-variables node) '())))
         (set (node-role node) '<erased>)
         (set *node-return-count* (fx+ 1 *node-return-count*))
         (return-to-pool node-pool node))))

(define (erase-all node)
  (iterate label ((node node))
    (cond ((empty? node)
           nil)
          (else
           (select (node-variant node)
             ((lambda-node?)
              (label (lambda-body node)))
             ((call-node?)
              (walk label (call-proc+args node))
              (return-list-to-freelist (call-proc+args node)))
             ((object-node?) 
              (label (object-proc node))
              (return-to-freelist (object-proc-pair node))
              (walk label (object-operations node))
              (return-list-to-freelist (object-operations node))
              (walk label (object-methods node))
              (return-list-to-freelist (object-methods node))))
           (erase node)))))

;;; CONNECTING AND DISCONNECTING NODES
;;;===========================================================================

;;; Disconnect node from its parent.

(define-integrable (detach node)
  (set ((node-role node) (node-parent node)) empty)
  (set (node-role node) nil)
  (set (node-parent node) empty)
  node)

;;; Replace node in tree with value of applying proc to node.
;;; Note the fact that a change has been made, for the simplifier.

(define (move node proc)
  (let ((parent (node-parent node))
        (role (node-role node)))
    (mark-changed node)
    (detach node)
    (relate role parent (proc node))))

;;; Add a new call into the node tree after lambda-node PARENT.

(define (insert-call call cont parent)
  (move (lambda-body parent)
        (lambda (old-body)
          (relate lambda-body cont old-body)
          call)))

;;; Replace old-node with new-node.
;;; Note the fact that a change has been made, for the simplifier.

(define (replace old-node new-node)
  (let ((role (node-role old-node))
        (parent (node-parent old-node)))
    (mark-changed old-node)
    (set (node-parent old-node) empty)
    (erase-all old-node)
    (set (role parent) new-node)
    (set (node-simplified? new-node) nil)
    (set (node-parent new-node) parent)
    (set (node-role new-node) role)))

(define (mark-changed node)
  (do ((p (node-parent node) (node-parent p)))
      ((or (empty? p)
           (not (node-simplified? p))))
    (set (node-simplified? p) nil)))

;;; LEAF NODES
;;;=========================================================================
;;;   There are three kinds of leaf nodes - PRIMOP, LITERAL, REFERENCE
;;;
;;; Fields:
;;;   variant  - 'literal, 'primop, or 'reference
;;;   value    - Either a primop, a variable, or a literal value
;;;   type     - the type of the object this refers to (e.g. integer)

(define leaf-node? (create-node-variant 'leaf))

(define leaf-value
  (node-field leaf-node? node-stuff-0 'leaf-value))

(define leaf-variant
  (node-field leaf-node? node-stuff-1 'leaf-variant))

(define leaf-type
  (node-field leaf-node? node-stuff-2 'leaf-type))

(define leaf-flags
  (node-field leaf-node? node-stuff-3 'leaf-flag))

(define (create-leaf-node value variant)
  (let ((node (create-node leaf-node?)))
    (set (leaf-value   node) value)
    (set (leaf-variant node) variant)
    (set (leaf-type    node) nil)
    node))

;;; PRIMOP NODES
;;;=========================================================================

(define-integrable (create-primop-node primop)
  (create-leaf-node primop 'primop))

(define-integrable (primop-node? node)
  (and (leaf-node? node)
       (eq? (leaf-variant node) 'primop)))

;;; Checks to see if NODE is a reference to on of the primops in PRIMOPS.
(define-integrable (primop-ref? node primop)
  (and (primop-node? node)
       (eq? (primop-value node) primop)))

(define-constant primop-value leaf-value)

;;; LITERAL NODES
;;;=========================================================================

(define-integrable (create-literal-node value)
  (create-leaf-node value 'literal))

(define-integrable (literal-node? node)
  (and (leaf-node? node)
       (eq? (leaf-variant node) 'literal)))

(define-constant literal-value leaf-value)
  
;;; REFERENCE NODES
;;;=========================================================================

(define-integrable (create-reference-node variable)
  (let ((node (create-leaf-node variable 'reference)))
    (push (variable-refs variable) node)
    node))

(define-integrable (reference-node? node)
  (and (leaf-node? node)
       (eq? (leaf-variant node) 'reference)))

(define-integrable (variable-ref? node . variables)
  (and (reference-node? node)
       (memq? (reference-variable node) variables)))

(define-constant reference-variable leaf-value)

(define-constant reference-flags leaf-flags)

;;; LAMBDA NODES
;;;============================================================================
;;; Fields:
;;;   variables - list of variables which are bound by this lambda.  The first
;;;        variable gets bound to the procedure itself; the second is a 'rest'
;;;        variable, if it is non-null the lambda is n-ary.
;;;   body      - the node for the body (after CPS, always a call node)
;;;   env       - list of variables live on entry to this lambda
;;;   strategy  - label, stack, or heap (where are closures over this lambda?)
;;;   live      - the variables live in the body of the lambda.
;;;   db        - trace information.


(define lambda-node? (create-node-variant 'lambda))

(define lambda-body
  (make-relation 'lambda-body lambda-node? node-stuff-0))

(define lambda-all-variables
  (node-field lambda-node? node-stuff-1 'lambda-all-variables))

(define lambda-env
  (node-field lambda-node? node-stuff-2 'lambda-env))

(define lambda-strategy
  (node-field lambda-node? node-stuff-3 'lambda-strategy))

(define lambda-live
  (node-field lambda-node? node-stuff-4 'lambda-live))

(define lambda-db
  (node-field lambda-node? node-stuff-5 'lambda-db))

;;; Selecting various subsets of a lambda's variables.

(define-integrable (lambda-variables node)
  (cddr (lambda-all-variables node)))

(define-integrable (lambda-rest+variables node)
  (cdr (lambda-all-variables node)))

(define-integrable (lambda-self-var node)
  (car (lambda-all-variables node)))

(define-integrable (lambda-rest-var node)
  (cadr (lambda-all-variables node)))

(define-integrable (lambda-cont-var node)
  (caddr (lambda-all-variables node)))

;;;    Creates a lambda node.  NAME is used as the name of the lambda node's
;;; self variable.   VARS is a list of variables.  The VARIABLE-BINDER and
;;; VARIABLE-NUMBER slots of the variables are set.

(define (create-lambda-node name vars)
  (let ((node (create-node lambda-node?))
        (vars (cons-from-freelist (create-variable name)
                                  vars)))
    (set (lambda-all-variables node) vars)
    (set (lambda-strategy      node) nil) 
    (set (lambda-live          node) nil)
    (set (lambda-env           node) nil)
    (set (lambda-body          node) empty)
    (do ((vars vars (cdr vars))
         (n 0 (fx+ n 1)))
        ((null? vars))
      (let ((var (car vars)))
        (cond (var
               (set (variable-binder var) node)
               (set (variable-number var) n)))))
    node))

;;; CALL NODES
;;;==========================================================================
;;; Fields:
;;;   exits      - the number of initial arguments that are continuations
;;;   complexity - no longer used...
;;;   hoisted-cont - continuation to be consed on stack
;;;   proc+args  - list of child nodes

(define call-node? (create-node-variant 'call))
                                                       
(define call-proc+args
  (node-field call-node? node-stuff-0 'call-proc+args))

(define call-exits
  (node-field call-node? node-stuff-1 'call-exits))

(define call-complexity
  (node-field call-node? node-stuff-2 'call-complexity))

(define call-hoisted-cont
  (node-field call-node? node-stuff-3 'call-hoisted-cont))

;;; Selecting various subsets of the children of a call node.

(define-integrable (call-args node)
  (cdr (call-proc+args node)))

(define-integrable (call-exit-args node)
  (sublist (call-args node) 0 (call-exits node)))

(define-integrable (call-non-exit-args node)
  (nthcdr (call-args node) (call-exits node)))

;;;   T if NODE is an exit of a call node, NIL otherwise.

(define (call-exit? node)
  (let ((role (node-role node)))
    (cond ((not (call-arg? role))
           nil)
          ((lambda-node? (call-proc (node-parent node)))
           (eq? role call-proc))
          ((eq? role call-proc)
           (fx= 0 (call-exits (node-parent node))))
          (else
           (fx<= (call-arg-number role)
                 (call-exits (node-parent node)))))))

;;; Create a call node with N children and EXITS exits.  Add new argument
;;; relations if their aren't enough.

(define (create-call-node n exits)
  (let ((node (create-node call-node?)))
    (set (call-proc+args node) (make-empty-node-list n))
    (set (call-exits node) exits)
    (set (call-complexity node) nil)
    (set (call-hoisted-cont node) nil)
    node))

;;; ARGUMENT RELATIONS
;;;========================================================================
;;; Argument relations are created on demand whenever a newly created
;;; call node is going to have more arguments than any previous call node
;;; has had.

(define-predicate call-arg?)

(define (make-arg-relation i)
  (make-list-relation `(call-arg ,i) call-node? call-proc+args i call-arg?))

(define-constant call-arg-number relation-index)

(define-constant call-proc 
  (make-list-relation 'call-proc call-node? call-proc+args 0 call-arg?))

(define call-arg-relations
  (make-infinite-vector 20 make-arg-relation 'call-arg-relations))

(set (call-arg-relations 0) call-proc)

(define-integrable (call-arg i)
  (call-arg-relations i))

;;; Make ARGS the arguments of call node PARENT.

(define (relate-call-args node args)
  (relate-list call-arg-relations 1 node (cdr (call-proc+args node)) args))

;;; Replace the arguments of call node NODE with NEW-ARGS.

(define (replace-call-args node new-args)
  (walk (lambda (n)
          (if (not (empty? n))
              (erase (detach n))))
        (call-args node))
  (relate-new-call-args node new-args))

;;; Replace the arguments of call node NODE with (possibly shorter) NEW-ARGS.

(define (relate-new-call-args node new-args)
  (modify (cdr (call-proc+args node))
          (lambda (l)
            (let ((n (fx- (length l) (length new-args))))
              (nthcdr l n))))        ; pairs lost...
  (relate-call-args node new-args))

; Avoiding n-ary procedures

(define (relate-two-call-args node a1 a2)
  (let ((l (flist2 a1 a2 '())))
    (relate-call-args node l)
    (return-list-to-freelist l)))

(define (relate-three-call-args node a1 a2 a3)
  (let ((l (flist3 a1 a2 a3 '())))
    (relate-call-args node l)
    (return-list-to-freelist l)))

(define (relate-four-call-args node a1 a2 a3 a4)
  (let ((l (flist4 a1 a2 a3 a4 '())))
    (relate-call-args node l)
    (return-list-to-freelist l)))

(define (relate-five-call-args node a1 a2 a3 a4 a5)
  (let ((l (flist5 a1 a2 a3 a4 a5 '())))
    (relate-call-args node l)
    (return-list-to-freelist l)))


;;; OBJECT NODES
;;;===========================================================================
;;;  NUMBER      unique number for this object, used by PP-CPS
;;;  PROC-PAIR   CAR is the procedure node, must be a pair for SIMPLIFY
;;;  OPERATIONS  list of operation nodes
;;;  METHODS     list of method nodes, these are all LAMBDAs
;;;  OPERATION?  T if this object is an operation

(define object-node? (create-node-variant 'object))

(define-constant object-number
  (make-relation 'object-number object-node? node-stuff-0))

(define-constant object-proc-pair
  (make-relation 'object-proc-pair object-node? node-stuff-1))

(define object-operations
  (node-field object-node? node-stuff-2 'object-operations))

(define object-methods
  (node-field object-node? node-stuff-3 'object-methods))

(define object-operation?
  (node-field object-node? node-stuff-4 'object-operation?))

;;; Object proc

(define object-proc
  (make-list-relation 'object-proc object-node? object-proc-pair 0 nil))

;;; Object operations

(define-predicate object-op?)

(define (make-op-relation i)
  (make-list-relation
     `(object-op ,i) object-node? object-operations i object-op?))

(define-constant object-op-number relation-index)

(define object-op-relations
  (make-infinite-vector 20 make-op-relation 'object-op-relations))

(define-integrable (object-op i)
  (object-op-relations i))

(define (relate-object-ops node ops)
  (relate-list object-op-relations 0 node (object-operations node) ops))

;;; Object methods

(define-predicate object-method?)

(define (make-method-relation i)
  (make-list-relation
     `(object-method ,i) object-node? object-methods i object-method?))

(define-constant object-method-number relation-index)

(define object-method-relations
  (make-infinite-vector 20 make-method-relation 'object-method-relations))

(define-integrable (object-method i)
  (object-method-relations i))

(define (relate-object-methods node methods)
  (relate-list object-method-relations 0 node (object-methods node) methods))

(define (create-object-node operation? n)
  (let ((node (create-node object-node?)))
    (set (object-number     node) *variable-id*)
    (set (object-proc-pair  node) (flist1 empty '()))
    (set (object-operations node) (make-empty-node-list n))
    (set (object-methods    node) (make-empty-node-list n))
    (set (object-operation? node) operation?)
    (set *variable-id* (fx+ 1 *variable-id*))
    node))

(define (maybe-replace-object-with-proc node)
  (cond ((object-node? node)
         (let ((proc (object-proc node)))
           (replace node proc)
           proc))
        (else node)))













