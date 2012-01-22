(herald safe)

;;; A procedure is type-safe if type checking is done for all primop calls
;;; it contains.  This code is used to produce closed compiled versions of
;;; primops.  It is not guarenteed to produce code that is 100% type-safe.

;;; Turn NODE into a type-safe procedure.

(define (make-type-safe node)
  (cond ((lambda-node? node)
         (real-make-type-safe node '())
         (simplify-call node))
        ((object-node? node)
         (real-make-type-safe node '()))
        (else
         (bug '"MAKE-TYPE-SAFE called on ~S" node))))

;;; TYPES is a list of (<variable> . <type>) pairs indicating that <variable>
;;; is known to have type <type>.  This is used to prevent unnecessary type
;;; tests.

(define (real-make-type-safe node types)
  (cond ((lambda-node? node)
         (make-call-type-safe (lambda-body node) types))
        ((object-node? node)
         (real-make-type-safe (object-proc node) types)
         (walk (lambda (m)
                 (real-make-type-safe m types))
               (object-methods node)))
        (else nil)))

;;; Make a call type-safe.

(define (make-call-type-safe call types)
  (cond ((primop-node? (call-proc call))
         (make-primop-type-safe call (primop-value (call-proc call)) types))
        (else
         (walk (lambda (n)
                 (real-make-type-safe n types))
               (call-proc+args call)))))

;;; Make a call to a primop type-safe.  This gets the necessary tests and
;;; corresponding known variable types and then makes the arguments type-safe
;;; and inserts the tests.

(define (make-primop-type-safe call primop types)
  (receive (tests types)
           (get-primop-arg-typing (primop.type primop call)
                                  (call-args call)
                                  types)
    (walk (lambda (n)
            (real-make-type-safe n types))
          (call-args call))
    (walk (lambda (p)
            (insert-test (node-parent call) (car p) (cdr p)))
          tests)))

;;; TYPE is the type of the primop, ARGS are the actual arguments, and TYPES
;;; are the known variable types.  Each argument is checked to see if it
;;; requires testing.

(define (get-primop-arg-typing type args types)
  (cond ((not (proc-type? type))
         (return '() types))
        (else
         (let ((arg-types (proc-type-args type)))
           (iterate loop ((args args) (i 1) (tests '()) (types types))
             (if (null? args)
                 (return tests types)
                 (receive (ntest ntype)
                          (new-typing (car args) (vref arg-types i) types)
                   (loop (cdr args) (fx+ i 1)
                         (append ntest tests)
                         (append ntest ntype types)))))))))

;;; Dispatch on TYPE and the node type of ARG.

(define (new-typing arg type types)
  (cond ((type-top? type)
         (return '() '()))
        ((and (lambda-node? arg)
              (proc-type? type))
         (return '() (add-procedure-types type arg)))
        ((and (reference-node? arg)
              (not (proc-type? type)))
         (return (new-variable-typing (reference-variable arg) type types)
                 '()))
        (else
         (return '() '()))))

;;; Returns the known types of ARG's variables given that it is called by
;;; by a primop expecting something of type TYPE.

(define (add-procedure-types type arg)
  (let ((arg-types (proc-type-args type)))
    (iterate loop ((vars (lambda-variables arg)) (i 1) (types '()))
      (cond ((null? vars)
             types)
            ((or (not (used? (car vars)))
                 (or (type-top? (vref arg-types i))
                     (proc-type? (vref arg-types i))))
             (loop (cdr vars) (fx+ i 1) types))
            (else
             (loop (cdr vars) (fx+ i 1)
                   `((,(car vars) . ,(vref arg-types i)) . ,types)))))))

;;; Produce any additional type test needed to guarentee that VAR is of type
;;; TYPE.

(define (new-variable-typing var type types)
  (let ((known (assq var types)))
    (cond ((and known
                (type-subset? type (cdr known)))
           '())
          (else
           (let ((type (if (not known)
                           type
                           (type-subtract type (cdr known)))))
             `((,var . ,type)))))))

;;; Insert a test to guarentee that VAR is of type TYPE in the body of L-NODE.
;;;
;;; (LAMBDA <vars> <body>) + <var> + <type>
;;;   =>
;;; (LAMBDA <vars>
;;;   (LET ((W (IF (<type-test> <var>)
;;;                <var>
;;;                (*ENFORCE <type-test> <var>))))
;;;     <body>[W/<var>]))

(define (insert-test l-node var type)
  (let ((old-body (detach (lambda-body l-node)))
        (test (type->system-variable type))
        (*enforce (get-free-variable '*enforce))) ; Not the right thing
    (let-nodes ((call ((* test) 1 cont (* var)))
                  (cont (#f v) ((^ cond) 1 (^ join)))
                    (cond (#f j) (($ primop/conditional)
                                  2
                                  yes
                                  no
                                  ($ primop/test)
                                  ($ primop/true?)
                                  (* v)))
                      (yes () ((* j) 0 (* var)))
                      (no () ((* *enforce) 1 (* j) (* test) (* var)))
                    (join (#f w) old-body))
      (substitute-vars-in-node-tree old-body (list var) (list w))
      (relate lambda-body l-node call)
      (mark-all-changed (call-proc call)))))

;;; Get the name of the system predicate that tests for TYPE.

(define (type->system-variable type)
  (let ((pred (type-predicate type)))
    (if pred
        (get-system-variable pred)
        (bug '"no predicate for type ~S" (type-name type)))))

;;; Mark all ancestors of NODE as having been modified.

(define (mark-all-changed node)
  (do ((p (node-parent node) (node-parent p)))
      ((not (node? p)))
    (set (node-simplified? p) nil)))




