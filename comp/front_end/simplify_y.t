(herald (front_end simplify_y)
  (env t (orbit_top defs)))

;;; SIMPLIFY-Y finds all of the recursively bound values that do not reference
;;; any of the recursive variables and binds them using a normal lambda. Lots
;;; more could be done.

;;; (PRIMOP/Y <cont>
;;;           (LAMBDA (C V1 ... Vn)                ; y-lambda
;;;             (C (LAMBDA (C0)                    ; body-lambda
;;;                  <body>)
;;;                (LAMBDA (C1) <value code>)      ; value thunks
;;;                ...                             ;
;;;                (LAMBDA (Cn) <value code>))))   ;
;;;
;;; V1 to Vn are bound to the results of calling the value thunks.  The
;;; body-lambda is then called on <cont>.  

;;; This works by marking all of the recursively bound values and then walking
;;; up the tree from the references to the corresponding variables, unmarking
;;; nodes until Y-LAMBDA is reached.  The net effect is to mark all of the
;;; values that do not refer to the variables.  HOIST-NONRECURSIVE-VALUES then
;;; binds these values with a LET.

(define (simplify-y call-node)
  (simplify (call-args call-node))
  (let* ((y-lambda ((call-arg '2) call-node))
         (value-call (lambda-body y-lambda)))
    (walk (lambda (n)
            (set (node-flag n) t))
          (cdr (call-args value-call)))
    (mark-reference-parents y-lambda)
    (cond ;((not (node-flag (car (call-args value-call))))
          ; (if (not (null? (cdr (call-args value-call))))
          ;     (user-message '"Unused variables ..."))
          ; (replace-y-with-body call-node))
          (else
           (hoist-nonrecursive-values call-node y-lambda value-call)))))

;;; REMOVE-LOOP-VALUES is removes the marked values from VALUE-CALL.  These
;;; are then bound to their corresponding variables by INSERT-LABLE-LETS.
;;; If there are no remaining recursively bound values the call to PRIMOP/Y
;;; is removed.

(define (hoist-nonrecursive-values call-node y-lambda value-call)
  (let* ((removed (remove-loop-values y-lambda
                                      value-call
                                      (lambda (n) (not (node-flag n)))))
;                                      (lambda (n) (loop-value? n y-lambda))
         (empty? (null? (cdr (lambda-variables y-lambda))))
         (removed? (not (null? removed))))
    (cond (removed?
           (walk (lambda (n)
                   (set (node-flag (cdr n)) nil))
                 removed)
           (insert-label-lets removed (node-parent call-node))))
    (cond (empty?
           (replace (call-proc call-node)
                    (detach (car (call-args value-call))))
           (replace-call-args call-node
                              (list (detach (car (call-args call-node)))))))
    (or removed? empty?)))

;;; Remove the arguments to CALL that do not answer true to PRED and return
;;; them and their corresponding variables from L-NODE.

(define (remove-loop-values l-node call pred)
  (iterate loop ((vars (lambda-variables l-node))
                 (args (call-args call))
                 (removed '())
                 (n '3))
    (cond ((null? (cdr vars))
           removed)
          ((not (used? (cadr vars)))
           (set (cdr vars) (cddr vars)) ; Evil
           (erase-all (cadr args))
           (set (cdr args) (cddr args)) ; Extremely Evil
           (loop vars args removed n))
          ((not (pred (cadr args)))
           (set (node-role (cadr args)) (call-arg (fx- n '1)))
           (let ((r (cons (cadr vars) (detach (cadr args)))))
             (set (cdr vars) (cddr vars)) ; Evil
             (set (cdr args) (cddr args)) ; Extremely Evil
             (loop vars args (cons r removed) n)))
          (else
           (set (variable-number (cadr vars)) n)
           (set (node-role (cadr args)) (call-arg (fx- n '1)))
           (loop (cdr vars) (cdr args) removed (fx+ n '1))))))

;(define (loop-value? thunk y-lambda)
;  (let ((node (thunk-value thunk)))
;    (or (not node)
;        (lambda-node? node)       ;;; Could check live vars...
;        (object-node? node)
;        (and (reference-node? node)
;             (bound-below? node y-lambda))))))

;(define (bound-below? ref top)  ; NIL if REF is bound by TOP
;  (let ((binder (variable-binder (reference-variable ref))))
;    (if (not binder)
;        nil
;        (iterate loop ((node ref))
;          (cond ((eq? node top) nil)
;                ((eq? node binder) t)
;                (else
;                 (loop (node-parent node))))))))

(define (insert-label-lets vars-and-thunks parent)
  (walk (lambda (vt)
          (var-gets-thunk-value (car vt) (cdr vt) parent))
        vars-and-thunks))

(define (var-gets-thunk-value var thunk parent)
  (let ((new-call (create-call-node '2 '1))
        (cont (create-lambda-node 'p (flist2 nil var '()))))
    (relate call-proc new-call thunk)
    (relate (call-arg '1) new-call cont)
    (move (lambda-body parent)
          (lambda (call)
            (relate lambda-body cont call)
            new-call))))

;;; If THUNK is more than just a simple return splice its body between PARENT
;;; and PARENT's LAMBDA-BODY.

(define (maybe-splice-thunk thunk parent)
  (let* ((v (thunk-value thunk))
         (p (node-parent v))
         (v (detach v)))
    (cond ((neq? p (lambda-body thunk))
           (splice-thunk thunk parent))
          (else
           (erase-all thunk)))
    v))

;;; Put the body of THUNK except its return between lambda-node PARENT and
;;; its call.

(define (splice-thunk thunk parent)
  (move (lambda-body parent)
        (lambda (old-body)
          (replace (node-parent (car (variable-refs (lambda-cont-var thunk))))
                   old-body)
          (detach (lambda-body thunk))))
  (erase-all thunk))



                

