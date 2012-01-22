(herald (front_end fixup)
  (env t (orbit_top defs)))

;;; (lambda (k) <body>) => (lambda (v) ((lambda (k) <body>) v))

(define (fixup-node-tree top-node)
  (fixup-call-node (lambda-body top-node))
  (set (node-parent top-node) nil)
  top-node)

(define (fixup-value-node node)
;  (if (not (node-simplified? node))
;      (bug "~S has not been simplified" node))
  (cond ((lambda-node? node)
         (fixup-call-node (lambda-body node)))
        ((object-node? node)
         (fixup-value-node (object-proc node))
         (walk fixup-value-node (object-operations node))
         (walk fixup-value-node (object-methods node))
         (fix-object-node node))))

(define (fixup-call-node node)
  (walk fixup-value-node (call-proc+args node))
  (let ((proc (call-proc node)))
    (cond ((lambda-node? proc)
           (walk (lambda (var val)
                   (if (lambda-node? val)
                       (check-continuation-var var val)))
                 (lambda-variables proc)
                 (call-args node)))
          ((primop-node? proc)
           (select (primop-value proc)
             ((primop/conditional)
              (if (reference-node? ((call-arg 1) node))
                  (replace-with-lambda ((call-arg 1) node) 0))
              (if (reference-node? ((call-arg 2) node))
                  (replace-with-lambda ((call-arg 2) node) 0)))
             ((primop/undefined-effect)
              (simplify-undefined-effect node))
             ((primop/y)
              (fixup-y node)))))))

(define (check-continuation-var var val)
  (cond ((any? (lambda (ref)
                 (eq? (node-role ref) call-proc))
               (variable-refs var))
         (walk-refs-safely (lambda (ref)
                             (if (call-exit? ref)
                                 (fix-exit-reference var ref val)))
                           var))
        ((fxn= 2 (variable-number var))
         (walk-refs-safely (lambda (ref)
                             (if (and (call-exit? ref)
                                      (not (primop-node?
                                            (call-proc (node-parent ref)))))
                                 (fix-exit-reference var ref val)))
                           var))))

(define (fix-exit-reference var node value)
  (let ((proc (call-proc (node-parent node))))
    (cond ((eq? node proc)
           (return))
          ((not (primop-node? proc))
           (introduce-exit-lambda var node value '#t))
          ((eq? primop/y (primop-value proc))
           (introduce-exit-lambda var node value '#f))
          (else
           (replace-with-lambda
            node
            (primop.values-returned
             (primop-value (call-proc (node-parent node)))))))))

(define (introduce-exit-lambda var node value args?)
  (if (and args? (used? (lambda-rest-var value)))
      (bug '"don't know how to fixup n-ary exit ~S" value))
  (let* ((new-vars (free-map (lambda (var)
                               (if var
                                   (create-variable (variable-name var))
                                   nil))
                             (lambda-rest+variables value)))
         (cont (create-lambda-node 'c new-vars))
         (args (if (not args?)
                   '()
                   (map (lambda (v) (if v
                                        (create-reference-node v)
                                        (create-literal-node '#f)))
                        (cdr new-vars))))
         (call (create-call-node (fx+ '1 (length args)) '0)))
    (relate call-proc call (create-reference-node var))
    (relate-call-args call args)
    (relate lambda-body cont call)
    (replace node cont)))

(define (real-fix-exit-reference var node value)
  (let* ((new-vars (free-map (lambda (var)
                               (if var
                                   (create-variable (variable-name var))
                                   nil))
                             (lambda-rest+variables value)))
         (cont (create-lambda-node 'c new-vars))
         (call (create-call-node (length new-vars) 0)))
    (relate call-proc call (create-reference-node var))
    (relate-call-args call (map (lambda (var)
                                  (if var
                                      (create-reference-node var)
                                      (create-literal-node '#f)))
                                (cdr new-vars)))
    (relate lambda-body cont call)
    (replace node cont)))

(define (replace-with-lambda node count)
  (let* ((vars (do ((i 0 (fx+ i 1))
                    (v '() (cons (create-variable 'v) v)))
                   ((fx>= i count) v)))
         (l-node (create-lambda-node 'x `(#f . ,vars)))
         (c-node (create-call-node (fx+ 1 count) 0)))
    (move node
          (lambda (node)
            (relate lambda-body l-node c-node)
            (relate call-proc c-node node)
            (relate-call-args c-node (map create-reference-node vars))
            l-node))))                         

(define (replace-with-one-arg-lambda node)
  (let* ((v (create-variable 'v))
         (l-node (create-lambda-node 'x `(#f ,v)))
         (c-node (create-call-node 2 0)))
    (move node
          (lambda (node)
            (relate lambda-body l-node c-node)
            (relate call-proc c-node node)
            (relate (call-arg 1) c-node (create-reference-node v))
            l-node))))

;;; Remove any continuation of UNDEFINED-EFFECT (now done by simplifier)
;
;(define (fixup-undefined-effect node)
;  (cond ((fx= (call-exits node) 1)
;         (set (call-exits node) 0)
;         (erase-all (detach ((call-arg 1) node)))
;         (relate-new-call-args node (map detach (cdr (call-args node))))
;         t)
;        ((and (fx= 1 (length (call-args node)))
;              (literal-node? ((call-arg 1) node))
;              (pair? (literal-value ((call-arg 1) node))))
;         (modify (literal-value ((call-arg 1) node)) cdr))
;        (else 
;         nil))) 

;;; Fixing up a call to PRIMOP/Y so that all values are dethunked lambdas.

(define (fixup-y node)
  (let* ((y-lambda ((call-arg 2) node))
         (value-call (lambda-body y-lambda))
         (body-lambda ((call-arg 1) value-call))
         (removed (remove-loop-values y-lambda value-call simple-thunk?)))
    (if removed
        (introduce-labels-cells node value-call removed))
    (cond ((not (null? (cdr (lambda-variables y-lambda))))
           (walk (lambda (thunk)
                   (replace thunk (detach (thunk-value thunk))))
                 (cdr (call-args value-call))))
          (else
           (substitute-y-continuation body-lambda ((call-arg 1) node))
           (replace node (detach (lambda-body body-lambda)))))))

(define (substitute-y-continuation b-lambda cont)
  (let ((c-var (car (lambda-variables b-lambda))))
    (cond ((null? (variable-refs c-var)))
          ((or (null? (cdr (variable-refs c-var)))
               (reference-node? cont))
           (substitute c-var cont t))
          (else
           (move b-lambda
                 (lambda (old)
                   (let-nodes ((new (#f) ((! old) 0 (! (detach cont)))))
                     new)))))))

(define (introduce-labels-cells node value-call removed)
  (let ((body-lambda ((call-arg 1) value-call))
        (parent (node-parent node)))
    (walk (lambda (r)
            (let* ((var (car r))
                   (new-var (create-variable (variable-name var))))
              (walk-refs-safely (lambda (ref)
                                  (hack-reference ref new-var))
                                var)
              (add-label-cell new-var parent)
              (add-label-assigner new-var (cdr r) body-lambda)))
          removed)))

(define (add-label-assigner var thunk parent)
  (cond ((thunk-value thunk)
         => (lambda (value)
              (add-simple-label-assigner var (detach value) parent)
              (splice-thunk thunk parent)))
        (else
         (let* ((c-var (create-variable 'k))
                (value (create-reference-node c-var)))
           (add-simple-label-assigner var value parent)
           (var-gets-thunk-value c-var thunk parent)))))

(define (add-simple-label-assigner var value parent)
  (let ((call (create-call-node 5 1))
        (cont (create-lambda-node 'c (flist1 (create-variable 'ignore) '()))))
    (relate call-proc call (create-primop-node primop/set-location))
    (relate-four-call-args call
                           cont
                           (create-primop-node primop/cell-value)
                           value
                           (create-reference-node var))
    (insert-call call cont parent)))

(define (add-label-cell var parent)
  (let ((call (create-call-node 3 1))
        (cont (create-lambda-node 'c (flist2 nil var '()))))
    (relate call-proc call (create-primop-node primop/make-cell))
    (relate-two-call-args call cont (create-literal-node 'uninitialized-labels))
    (insert-call call cont parent)))

;;; (object <proc> (<op1> ... <opN>) (<meth1> ... <methN>))
;;; =>
;;; (lambda (V1)
;;;   (primop/proc+handler V1
;;;                        <proc>
;;;                        (lambda (V2)
;;;                          (V2 <op1> ... <opN>))
;;;                        <meth1>
;;;                        ...
;;;                        <methN>)))

(define (fix-object-node node)
  (let* ((ops (object-operations node))
         (meths (object-methods node))
         (obj-lambda-cont (create-variable 'v))
         (obj-lambda (create-lambda-node 'c (flist2 nil obj-lambda-cont '())))
         (obj-call (create-call-node (fx+ 4 (length meths)) 1))
         (ops-lambda-cont (create-variable 'v))
         (ops-lambda (create-lambda-node 'c (flist2 nil ops-lambda-cont '())))
         (ops-call (create-call-node (fx+ 1 (length ops)) 0)))
    (relate lambda-body obj-lambda obj-call)
    (relate call-proc obj-call (create-primop-node primop/proc+handler))
    (relate-call-args obj-call
                      `(,(create-reference-node obj-lambda-cont)
                        ,(fix-object-proc (object-proc node))
                        ,ops-lambda
                        . ,(map detach meths)))
    (relate lambda-body ops-lambda ops-call)
    (relate call-proc ops-call (create-reference-node ops-lambda-cont))
    (relate-call-args ops-call (map detach ops))
    (replace node obj-lambda)))

;;;  FOO => (LAMBDA (K) (APPLY FOO K))

(define (fix-object-proc node)
  (cond ((lambda-node? node)
          (detach node))
        ((leaf-node? node)
         (let ((apply-var (get-system-variable 'apply)))
           (let-nodes ((l (k v)
                         ((* apply-var) 1 (* v) (! (detach node)) (* k))))
             l)))             
        (else
         (bug '"object-proc of ~S is not a leaf or lambda node"
              (node-parent node)))))




