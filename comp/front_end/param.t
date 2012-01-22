(herald (front_end param)
  (env t (orbit_top defs)))
 
;;; Parameterizing procedures so that they can be easily copied
;;;
;;;    (LET ((F (LAMBDA <vars>
;;;               <stuff>
;;;               (FOO <exit1> <exit2> ... <arg1> <arg2> ...))))
;;;       <body>)
;;;  If any of the <args> are LAMBDAs they cannot reference any of <vars>
;;;  or they must be small enough to be duplicated.  <stuff> cannot reference
;;;  any of the <vars> or contain any side-effects or references to global
;;;  variables.
;;;
;;;  =>
;;;
;;;    <stuff>
;;;      (LET ((E1 (LAMBDA <vars> <exit1>))  ; These LAMBDAs are only needed if
;;;            (E2 (LAMBDA <vars> <exit2>))  ; the exits reference <vars>.
;;;            ...
;;;            (A1 <arg1>)  ; These go here only if they are LAMBDAs that do not
;;;            (A2 <arg2>)  ; reference <vars>
;;;            ...)
;;;        (LET ((F (LAMBDA <vars>
;;;                   (FOO (LAMBDA <e-vars1> 
;;;                          (E1 <vars>))  
;;;                        (LAMBDA <e-vars2>
;;;                          (E2 <vars>))  
;;;                        ...
;;;                        A1
;;;                        A2 ...))))
;;;          <body>))
;;;  Thus LAMBDA bound to F is cheap to copy as its call contains only small
;;;  pieces of node tree.

;;; If <stuff> didn't obey the above restrictions but also didn't bind any
;;; variables that the FOO call used, the following transformation could be
;;; used:
;;;
;;;      (LET ((B (LAMBDA <vars> <stuff>))
;;;            (E1 (LAMBDA <vars> <exit1>))  ; These LAMBDAs are only needed if
;;;            (E2 (LAMBDA <vars> <exit2>))  ; the exits reference <vars>.
;;;            ...
;;;            (A1 <arg1>)  ; These go here only if they are LAMBDAs that do not
;;;            (A2 <arg2>)  ; reference <vars>
;;;            ...)
;;;        (LET ((F (LAMBDA <vars>
;;;                   (B <vars>)
;;;                   (FOO (LAMBDA <e-vars1> 
;;;                          (E1 <vars>))  
;;;                        (LAMBDA <e-vars2>
;;;                          (E2 <vars>))  
;;;                        ...
;;;                        A1
;;;                        A2 ...))))
;;;          <body>))

;;; Top level call.

(define (parameterize l-node call)
  (cond ((can-parameterize? l-node call)
         (really-parameterize l-node call)
         t)
        (else nil)))

;;; Can CALL be parameterized at L-NODE?  Walks up the node tree from CALL
;;; collecting all of the non-exit lambda-nodes as it goes.  Once all of the
;;; lambdas are collected they are checked to make sure they do not reference
;;; any of L-NODE's variables.
;;;
;;; Each call between CALL and L-NODE is checked to make sure it it can be
;;; moved.

(define (can-parameterize? l-node call)
  (and (every? hoistable-exit? (call-exit-args call))
       (direct-descendent? l-node call)
       (iterate loop ((top (node-parent call))
                      (nodes (filter (lambda (n) (not (leaf-node? n)))
                                     (call-non-exit-args call))))
         (cond ((eq? top l-node)
                (walk (lambda (l) (set (node-flag l) t))
                      nodes)
                (mark-reference-parents l-node)
                (let ((res (every? node-flag nodes)))
                  (walk (lambda (l) (set (node-flag l) nil))
                        nodes)
                  res))
               ((and (call-node? (node-parent top))
                     (hoistable-call? (node-parent top) top))
                (loop (node-parent (node-parent top)) 
                      (append! (filter (lambda (n) (neq? n top))
                                       (call-proc+args (node-parent top)))
                              nodes)))
               (else nil)))))

(define (hoistable-exit? node)
  (or (reference-node? node)
      (and (lambda-node? node)
           (not (variable? (lambda-rest-var node))))
      (and (object-node? node)
           (or (lambda-node? (object-proc node))
               (object-node? (object-proc node)))
           (hoistable-exit? (object-proc node)))))

(define (direct-descendent? ancestor descendent)
  (iterate loop ((node descendent))
    (let ((parent (node-parent node)))
      (cond ((eq? parent ancestor) t)
            ((or (not (call-node? parent))
                 (and (fx> 2 (call-exits parent))
                      (call-exit? node)))
             (loop parent))
            (else nil)))))

(define (mark-reference-parents l-node)
  (walk (lambda (var)
          (cond ((variable? var)
                 (walk (lambda (ref)
                         (do ((n ref (node-parent n)))
                             ((eq? n l-node))
                           (set (node-flag n) nil)))
                       (variable-refs var)))))
        (lambda-rest+variables l-node)))

(define (hoistable-call? call from)
  (let ((proc (call-proc call)))
    (cond ((lambda-node? proc)
           (and (eq? proc from)
                (not (any? global-reference? (call-args call)))))
          ((primop-node? proc)
           (and (fx= '1 (call-exits call))
                (eq? from (car (call-args call)))
                (not (primop.side-effects? (primop-value proc)))
                (not (any? global-reference? (call-args call)))))
          (else
           nil))))
            
(define (global-reference? node)
  (and (reference-node? node)
       (not (variable-binder (reference-variable node)))))

;;; Use NODE-INSTRUCTIONS field to mark nodes

(define-constant node-flag
  (object (lambda (n)
            (node-instructions n))
    ((setter self) (setter node-instructions))))

;;;                    PARAMETERIZING A CALL
;;;============================================================================

;;; If L-NODE is not the parent of CALL the block of code between them must
;;; be moved above L-NODE.
;;; Then the arguments to CALL are parameterized and any large ones are
;;; put into a LET above CALL.

(define (really-parameterize l-node call)
  (if (neq? (node-parent call) l-node)
      (move-block l-node call))
  (receive (vars vals)
           (parameterize-args call (lambda-rest+variables l-node))
    (if (not (null? vars))
        (insert-let vars vals (node-parent (node-parent l-node))))
    (return)))

;;; Move the code between L-NODE and CALL to above L-NODE

(define (move-block l-node call)
  (let ((new-top-call (detach (lambda-body l-node))))
    (move (node-parent l-node)
          (lambda (old-top-call)
            (move call
                  (lambda (call)
                    (relate lambda-body l-node call)
                    old-top-call))
            new-top-call))))

;;; VARS are bound to VALS in a let-node just below PARENT.

(define (insert-let vars vals parent)
  (let ((new-proc (create-lambda-node 'l (cons-from-freelist nil vars)))
        (new-call (create-call-node (fx+ '1 (length vals)) '0)))
    (relate-call-args new-call vals)
    (relate call-proc new-call new-proc)
    (move (lambda-body parent)
          (lambda (call)
            (relate lambda-body new-proc call)
            new-call))))

;;; Walk down the arguments to CALL, replacing non-leaf nodes with variables.
;;; Returns a list of the new variables and the values they should be bound
;;; to.  Exits require a little more work as they may contain references to
;;; SCOPE-VARS.

(define (parameterize-args call scope-vars)
  (iterate loop ((args (copy-list (call-args call))) (exits (call-exits call))
                 (vars '()) (vals '()))
    (cond ((null? args)
           (return (reverse! vars) (reverse! vals)))
          ((leaf-node? (car args))
           (loop (cdr args) (fx- exits '1) vars vals))
          ((fx< '0 exits)
           (let ((role (node-role (car args))))
             (receive (new var val)
                      (parameterize-exit (detach (car args)) scope-vars)
               (relate role call new)
               (mark-changed new)
               (loop (cdr args) (fx- exits '1)
                     (cons-from-freelist var vars) (cons val vals)))))
          (else
           (let ((let-var (create-variable 'c)))
             (move (car args)
                   (lambda (#f)
                     (create-reference-node let-var)))
             (loop (cdr args) '0
                   (cons let-var vars) (cons (car args) vals)))))))

;;;    (LAMBDA (<cvars>) <cont>) + <vars>
;;; => 
;;;   If <cont> doesn't reference <vars>:
;;;
;;;     v                                     {new continuation}
;;;     v                                     {variable for LET}
;;;     (LAMBDA (<cvars>) <cont>)             {value for LET}
;;;
;;;   IF <cont> references <vars> and <cvars> is empty:
;;;
;;;     (LAMBDA () (c <vars>))                {new continuation}
;;;     c                                     {variable for LET}
;;;     (LAMBDA (<vs>) <cont>[<vs>/<vars>])   {value for LET}
;;;
;;;   Otherwise:
;;;
;;;     (LAMBDA (<cvs0>) (c <cvs0> <vars>))   {ditto}
;;;     c
;;;     (LAMBDA (<cvs1> <vs>) <cont>[<vs>/<vars>][<cvs1>/<cvars>])

(define (parameterize-exit node vars)
  (let ((exit-vars (proc-variables node)))
    (receive (new-vars old-vars)
             (replace-needed-vars node vars)
      (cond ((null? new-vars)
             (let ((let-var (create-variable 'c)))
               (return (create-reference-node let-var) let-var node)))
            ((null? exit-vars)
             (let ((let-var (create-variable 'c))
                   (old-refs (map create-reference-node old-vars))
                   (let-body (detach (lambda-body node))))
               (let-nodes ((new-cont (()) ((* let-var) '0 . old-refs))
                           (let-value (() . new-vars) let-body))
                 (erase-all node)
                 (return new-cont let-var let-value))))
            (else
             (messy-parameterize-exit node exit-vars new-vars old-vars))))))

;;; LAMBDA-VARIABLES that works on object-nodes as well.

(define (proc-variables node)
  (cond ((lambda-node? node)
         (lambda-variables node))
        ((object-node? node)
         (proc-variables (object-proc node)))
        (else
         (bug "PROC-VARIABLES: node ~S has no variables."))))

;;; Replace any of VARS that are referenced in NODE with references to new
;;; variables, returning a list of any new variables and a corresponding list
;;; of the VARS they replaced.

(define (replace-needed-vars node vars)
  (let ((new-vars (map (lambda (var) (if (used? var) (create-variable 'w) nil))
                       vars)))
    (substitute-vars-in-node-tree node vars new-vars)
    (let ((l (filter! (lambda (p) (used? (car p)))
                      (map cons new-vars vars))))
      (return (free-map car l) (map cdr l)))))

;;; Full blown PARAMETERIZE-EXIT.
;;;    (LAMBDA (<cvars>) <cont>) + <vars>
;;; => 
;;;    (LAMBDA (<cvs0>) (c1 <cvs0> <vars>))                       {new-cont}
;;;    c1                                                         {let-var}
;;;    (LAMBDA (<cvs1> <vs>) <cont>[<vs>/<vars>][<cvs1>/<cvars>]) {let-value}

(define (messy-parameterize-exit node exit-vars new-vars old-vars)
  (let* ((let-var (create-variable 'c))
         (new-exit-vars0 (copy-var-list exit-vars))
         (new-exit-vars1 (copy-var-list exit-vars))
         (refs (map create-reference-node (append new-exit-vars0 old-vars)))
         (let-value-vars (append new-exit-vars1 new-vars))
         (let-value-body (detach (lambda-body node))))
    (substitute-vars-in-node-tree let-value-body exit-vars new-exit-vars1)
    (let-nodes ((new-cont (() . new-exit-vars0) ((* let-var) 0 . refs))
                (let-value (() . let-value-vars) let-value-body))
      (erase-all node)
      (return new-cont let-var let-value))))

(define (copy-var-list l)
  (free-map (lambda (v)
              (if (used? v)
                  (create-variable (variable-name v))
                  nil))
            l))







