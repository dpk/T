(herald (front_end simplifiers)
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

;;; Procedures to simplify calls to various primops

(define (simplify-undefined-effect node)
  (cond ((fx= (call-exits node) 1)
         (set (call-exits node) 0)
         (erase-all (detach ((call-arg 1) node)))
         (relate-new-call-args node (map detach (cdr (call-args node))))
         t)
        (else 
         nil)))

;;; (<location> . <args>)
;;; => (CONTENTS-LOCATION <location> . <args>)
;;; Get (CAR X) to actually return a value.

(define (simplify-location node)
  (let* ((args (map detach (call-proc+args node)))
         (new-call (create-call-node (fx+ 1 (length args)) 1)))
    (relate call-proc new-call (create-primop-node primop/contents-location))
    (let ((loc (car args)))
      (set (car args) (cadr args))
      (set (cadr args) loc))
    (relate-call-args new-call args)
    (replace node new-call)
    t))

;;;                       SETTER
;;;============================================================================

;;; If setter is called on an object, see if it has a SETTER method, otherwise
;;; try to integrate it.  If it integrates, remove the call from the tree and
;;; simplify all the calls to the setter method (see SIMPLIFY-LOCATION-SET).

(define (simplify-setter node)
  (cond ((known-object-definition ((call-arg 2) node))
         => (lambda (def)
              (simplify-operation-dispatch node (definition-value def) def)))
        ((not (integrate-setter? (call-proc node)))
         nil)
        (else
         (let ((p (known-primop ((call-arg 2) node))))
           (mark-reference-used ((call-arg 2) node))
           (mark-reference-used (call-proc node))
           (walk-refs-safely (lambda (ref)
                               (primop.simplify-setter p (node-parent ref)))
                             (car (lambda-variables ((call-arg 1) node))))
           (replace node (detach (lambda-body ((call-arg 1) node))))
           t))))

;;; SETTER should be integrated at NODE if it is called on a known primop and
;;; the result is only called.

(define (integrate-setter? node)
  (and (eq? (node-role node) call-proc)
       (destructure (((#f cont arg . rest)
                      (call-proc+args (node-parent node))))
         (let ((p (known-primop arg)))
           (and (null? rest)
                p
                (primop.settable? p)
                (lambda-node? cont)   ;;; should do nargs check on lambda here
                (all-refs-are-calls? (car (lambda-variables cont))))))))

;;; ((SETTER <location primop>) @<args> <value>)
;;;  =>
;;; (PRIMOP/SET-LOCATION <cont> <location primop> <value> . <args>)

(define (simplify-location-set location call nargs)     ; needs arg checking
  (ignore nargs)
  (let ((args (reverse! (map detach (cdr (call-args call)))))
        (new (create-call-node (fx+ 2 (length (call-args call))) 1)))
    (relate call-proc new (create-primop-node primop/set-location))
    (relate-call-args new
                      `(,(detach ((call-arg 1) call))
                        ,(create-primop-node location)
                        ,(car args)
                        . ,(reverse! (cdr args))))
      (replace call new)))

;;;                   Multiple Value Returns
;;;============================================================================

;;; RETURN (what used to be VALUES)
;;;
;;; (return cont a b c)
;;;    ==>  (cont a b c)

(define (simplify-values node)
  (let ((cont (any-known-type ((call-arg 1) node))))
    (cond ((or (not cont)
               (and (callable-type? cont)
                    (arg-check-of-return-type cont node)))
           (let ((args (map detach (call-args node))))
             (set (call-exits node) 0)
             (replace-call-args node (cdr args))
             (replace (call-proc node) (car args))
             t))
          ((not (callable-type? cont))
           (bug '"contination to VALUES is not callable ~S" node))
          (else
           (fix-call-to-values node cont)))))

;;; RECEIVE is syntax that expands into a call to RECEIVE-VALUES
;;;
;;; (RECEIVE <args> <form> . <body>)
;;; => (RECEIVE-VALUES (LAMBDA <args> . <body>) (LAMBDA () <form>))
;;;
;;; After CPS conversion RECEIVE-VALUES can often be removed completely.
;;;
;;; (RECEIVE-VALUES <cont> (LAMBDA <c . rest> . <body>) <sender>)
;;; => (<sender> (LAMBDA <rest>
;;;                ((LAMBDA (<c>) . <body>)
;;;                 <cont>)))
;;; If the receiver is an object instead of a lambda its procedure's variable
;;; is used for <c>.

;;; This checks the node tree and replaces SENDER with its procedure if it is
;;; an object.

(define (simplify-receive-values node)
  (destructure (((cont receiver sender) (call-args node)))
    (let ((sender (maybe-replace-object-with-proc sender)))
      (cond ((or (fxn= '3 (length (call-args node)))
                 (not (check-receiver? receiver))
                 (not (check-sender? sender)))
             (not (node-simplified? node)))        ;SENDER may have changed
            (else
             (really-simplify-receive-values node cont receiver sender))))))

;;; The receiver must be a lambda or an object and have a continuation variable.

(define (check-receiver? receiver)
  (or (and (lambda-node? receiver)
           (variable? (lambda-cont-var receiver)))
      (and (object-node? receiver)
           (and (lambda-node? (object-proc receiver))
                (variable? (lambda-cont-var (object-proc receiver)))))))

;;; The sender must be a lambda with only a continuation variable.

(define (check-sender? sender)
  (and (lambda-node? sender)
       (variable? (lambda-cont-var sender))
       (not (lambda-rest-var sender))
       (null? (cdr (lambda-variables sender)))))

;;; Make the change.  RECEIVER's continuation variable must be excised from
;;; its variable list so that it can be bound by a separate lambda.

(define (really-simplify-receive-values node cont receiver sender)
  (let* ((r-proc (if (object-node? receiver)
                     (object-proc receiver)
                     receiver))
         (c-var (lambda-cont-var r-proc))
         (r-body (detach (lambda-body r-proc))))
    (set (cdr (lambda-rest+variables r-proc))
         (cdr (lambda-variables r-proc)))
    (check-receive-calls (lambda-cont-var sender)
                         (get-node-definition-type r-proc))
    (walk (lambda (v)
            (if (variable? v)
                (modify (variable-number v)
                        (lambda (n) (fx- n 1)))))
          (lambda-variables r-proc))
    (let-nodes ((new-call ((! (detach sender)) 1 (! (detach receiver))))
                (l1 (#f (c c-var)) r-body)
                (new-r-body ((! l1) 1 (! (detach cont)))))
      (relate lambda-body r-proc new-r-body)
      (set (node-simplified? receiver) nil) ;receiver has a new body 
      (replace node new-call)
      t)))

;;; Check the number of arguments to the receive so that appropriate error
;;; messages can be issued if necessary.

(define (check-receive-calls var type)
  (walk-refs-safely (lambda (r)
                      (if (and (eq? (node-role r) call-proc)
                               (not (arg-check-of-type type (node-parent r))))
                          (fix-call-to-receive-values (node-parent r) type)))
                    var))

;;;                Predicates and Conditionals
;;;============================================================================

;;; (<type>? cont x) =>
;;; (primop/conditional (lambda () (cont #t))
;;;                     (lambda () (cont #f))
;;;                     primop/test
;;;                     <type>?
;;;                     x)

(define (presimplify-predicate node)
  (really-presimplify-predicate node ((call-arg 2) node)))

(define (presimplify-no-argument-predicate node)
  (really-presimplify-predicate node (create-literal-node '#f)))

(define (really-presimplify-predicate call arg)
  (let ((pred (call-proc call))
        (cont ((call-arg 1) call)))
    (let ((primop (if (primop-node? pred)
                      pred
                      (create-primop-node (known-primop pred))))
          (test (create-primop-node primop/test)))
      (construct-conditional call test cont primop arg)
      (if (reference-node? pred) (erase pred))
      t)))

;;; (<cond> cont arg1 arg2) =>
;;; (primop/conditional (lambda () (cont #t))
;;;                     (lambda () (cont #f))
;;;                      <cond> arg1 arg2)
;;; where <cond> is one of PRIMOP/TEST, EQ?, FX<, etc.

(define (presimplify-to-conditional node)
  (destructure (((pred cont arg1 arg2) (call-proc+args node)))
    (construct-conditional node pred cont arg1 arg2)))

(define (construct-conditional node pred cont arg1 arg2)
  (let ((primop (if (primop-node? pred)
                    pred
                    (create-primop-node (known-primop pred)))))
    (walk detach (call-proc+args node))
    (if (reference-node? pred) (erase pred))
    (let-nodes ((c1 ((^ l1) 1 cont))
                 (l1 (#f v1)
                     (($ primop/conditional) 2 (^ l2) (^ l3) primop arg1 arg2))
                   (l2 (#f) ((* v1) 0 ''#t))
                   (l3 (#f) ((* v1) 0 ''#f)))
      (replace node c1))))

;;; (PRIMOP/CONDITIONAL <exit1> <exit2> PRIMOP/TEST PRIMOP/TRUE? #F)
;;; => <exit2>
;;;
;;; (PRIMOP/CONDITIONAL <exit1> <exit2> PRIMOP/TEST PRIMOP/TRUE? not-#F)
;;; => <exit1>
;;;
;;; *VALUE-TABLE* contains values known from tests that occur above this
;;; one in the tree.

(define (simplify-test node)
  (destructure (((exit-1 exit-2 #f test val) (call-args node)))
    (cond ((not (primop-ref? test primop/true?))
           nil)
          ((and (reference-node? val)
                (table-entry *value-table* (reference-variable val)))
           =>(lambda (value)
               (if (eq? value 'true)
                   (replace-test node exit-1)
                   (replace-test node exit-2))
               t))
          ((not (literal-node? val))
           nil)
          ((eq? '#f (primop-value val))
           (replace-test node exit-2)
           t)
          (else
           (replace-test node exit-1)
           t))))

(define (replace-test call-node new-node)
  (let ((new-call (create-call-node 1 0)))
    (detach new-node)
    (relate call-proc new-call new-node)
    (replace call-node new-call)))

;;;                     Creating Primops
;;;============================================================================

;;; Just replace the call with the primop.

(define (simplify-*primop primop call)
  (ignore primop)
  (replace-call-with-new-primop call (primop-value ((call-arg 2) call)))
  t)

;;; CALL is a call node whose procedure is PRIMOP.  This checks to see that
;;; there are the right number of arguments and that they are all literals.
;;; If so, the values are attached the primop and the call is replaced by the
;;; new primop.

(define (simplify-parameterized-primop primop call)
  (cond ((or (fxn= (length (primop.formals primop)) ; Could be improper?
                   (fx+ -1 (length (call-args call))))
             (not (every? literal-node? (cdr (call-args call)))))
         nil)
        (else
         (let ((args (map literal-value (cdr (call-args call)))))
           (replace-call-with-new-primop call (construct-primop primop args))
           t))))

;;; Structure accessors cannot be plain parameterized primops as the STYPE
;;; will not be a literal node.

(define (simplify-parameterized-structure-accessor primop call)
  (destructure (((cont stype offset slot) (call-args call)))
    (cond ((or (fxn= 4 (length (call-args call)))
               (not (reference-node? stype))
               (not (literal-node? offset))
               (not (literal-node? slot)))
           nil)
          (else
           (let ((new-primop (construct-primop primop
                                               `(,(literal-value offset)))))
             (add-call-value-to-definition call new-primop t)
             (replace (call-proc call)
                      (create-reference-node
                       (get-system-variable 'stype-selector)))
             (walk detach (call-args call))
             (erase offset)
             (relate-new-call-args call (list cont stype slot))
             t)))))

;;; This does two things.  CALL is replace by the node-tree for the closed
;;; compiled form for PRIMOP.  The PRIMOP becomes the value of any locale
;;; variables which are defined to be the value of CALL.

(define (replace-call-with-new-primop call primop)
  (let ((thunk-node (subexpression->code-tree (primop.make-closed primop))))
    (add-call-value-to-definition call primop nil)
    (replace (call-proc call) thunk-node)
    (walk (lambda (n) (erase (detach n)))
          (cdr (call-args call)))
    (relate-new-call-args call (list (detach ((call-arg 1) call)))))
  (return))

;;; Try to find if the value of CALL is used as the value of any locale
;;; variable.  This works for (DEFINE FOO (PRIMOP ...)) and not for anything
;;; much more complicated.  If it cannot find a place to put the value a
;;; warning is issued.

(define (add-call-value-to-definition call primop parameterized?)
  (let ((cont ((call-arg 1) call)))
    (cond ((or (not (lambda-node? cont))
               (fxn= 1 (length (lambda-variables cont))))
           (dropped-primop-warning call primop parameterized?))
          (else
           (iterate loop ((refs (variable-refs (car (lambda-variables cont))))
                          (hit? nil))
             (cond ((null? refs)
                    (cond ((not hit?)
                           (dropped-primop-warning call primop parameterized?))
                          ((primop.variant-id primop)
                           (add-new-primop *shape* primop))))
                   ((add-primop-to-definition (car refs) primop)
                    (loop (cdr refs) t))
                   (else
                    (loop (cdr refs) hit?)))))))
  (return))

;;; See if REF is part of (DEFINE FOO <REF>).  If it is, the value of FOO is
;;; declared to be PRIMOP.
;;; Why is NODE created before the COND??

(define (add-primop-to-definition ref primop)
  (let ((parent (node-parent ref))
        (node (create-primop-node primop)))
    (cond ((or (not (call-arg? (node-role ref)))
               (not (variable-definition? parent)))
           (erase node)
           nil)
          (else
           (let ((defined-var (reference-variable ((call-arg 2) parent))))
             (and defined-var
                  (real-add-definition-value defined-var node)))))))

(define (dropped-primop-warning call primop parameterized?)
  (user-message 'warning
                (if parameterized?
                    "parameterized primop ~S not added to support"
                    "primop ~S not added to support")
                nil
                primop))

;;;          closed compiled definitions for primops
;;;============================================================================
;;;   Wrap a LAMBDA with the necessary ENFORCEs around primop P.  (ENFORCE
;;; used to be called PROCLAIM).  There are three procedures, one for normal
;;; primops, one for predicates, and one for conditionals.

(define (make-closed-primop p)
  (let ((type (primop.type p nil)))
    (cond ((not (proc-type? type))
           primop/undefined-effect)
          ((proc-type-n-ary? type)
           (bug "cannot make a closed compiled for n-ary primop ~S" p))
          (else
           (receive (vars proclaims)
                    (make-proclaims (cddr (vector->list (proc-type-args type))))
             `(,syntax/lambda ,vars
                 ((,syntax/lambda ,vars (,p . ,vars))
                  . ,proclaims)))))))

(define (make-closed-predicate p)
  (receive (names proclaims)
           (make-proclaims (cddr (vector->list
                                  (proc-type-args (primop.type p nil)))))
    (let ((vars (map create-variable names)))
     (let-nodes ((node (#f k (v0 (car vars)))
                   (($ p) 1 (* k) (* v0))))
       (presimplify-predicate (lambda-body node))
       `(,syntax/lambda ,names
           (,node . ,proclaims))))))

(define (make-closed-conditional p)
  (receive (names proclaims)
           (make-proclaims (cddr (vector->list
                                  (proc-type-args (primop.type p nil)))))
    (let ((vars (map create-variable names)))
      (let-nodes ((node (#f k (v0 (car vars)) (v1 (cadr vars)))
                    (($ p) 1 (* k) (* v0) (* v1))))
        (presimplify-to-conditional (lambda-body node))
        `(,syntax/lambda ,names
            (,node . ,proclaims))))))

;;; Make PROCLAIM forms for TYPES.

(define (make-proclaims types)
  (iterate loop ((types types) (vars '()) (procs '()))
    (cond ((null? types)
           (return (reverse! vars) (reverse! procs)))
          ((type-top? (car types))
           (let ((var (generate-symbol 't)))
             (loop (cdr types) (cons var vars) (cons var procs))))
          ((type-predicate (car types))
           => (lambda (predicate)
                (let ((var (generate-symbol 't)))
                  (loop (cdr types)
                        (cons var vars)
                        (cons `(enforce ,predicate ,var) procs)))))
          (else
           (bug "there is no predicate for ~S" (car types))))))


(lset *n-ary->binary-arg-limit* '2)   ; Limit code explosion

(define (n-ary->binary call proc)
  (let ((args (cdr (call-args call)))
        (var (get-system-variable proc)))
    (cond ((or (null? args) (null? (cdr args)))
           (bug "not enough arguments in ~S for N-ARY->BINARY" call))
          ((null? (cddr args))
           (replace (call-proc call) (create-reference-node var))
           '#t)
          ((fx< *n-ary->binary-arg-limit* (length args))
           '#f)
          (else
           (let ((top (node-parent call))
                 (cont (detach ((call-arg 1) call))))
             (iterate loop ((args args) (cont cont))
               (cond ((null? (cddr args))
                      (let-nodes ((c1 ((* var) 1 cont
                                       (! (detach (car args)))
                                       (! (detach (cadr args))))))
                        (replace call c1))
                      '#t)
                     (else
                      (let-nodes ((l1 (#f v) ((* var) 1 cont 
                                              (! (detach (car args)))
                                              (* v))))
                        (loop (cdr args) l1))))))))))

(define (presimplify-to-funny-conditional node count)
  (destructure (((pred cont arg1 arg2) (call-proc+args node)))
    (construct-funny-conditional node pred cont arg1 arg2 count)))

(define (construct-funny-conditional node pred cont arg1 arg2 count)
  (let ((primop (if (primop-node? pred)
                    pred
                    (create-primop-node (known-primop pred)))))
    (walk detach (call-proc+args node))
    (if (reference-node? pred) (erase pred))
    (let-nodes ((c1 ((^ l1) 1 cont))
                 (l1 (#f v1)
                     (($ primop/conditional) 2
                      (! (construct-conditional-cont count v1 '#t))
                      (! (construct-conditional-cont count v1 '#f))
                      primop arg1 arg2)))
      (replace node c1))))

(define (construct-conditional-cont arg-count var value)
  (let* ((vars (make-vars arg-count))
         (args (map create-reference-node vars)))
    (let-nodes ((l1 (#f . vars) ((* var) 0 'value . args)))
      l1)))

(define (make-vars count)
  (do ((i 0 (fx+ i 1))
       (v '() (cons (create-variable 'v) v)))
      ((fx>= i count) v)))
