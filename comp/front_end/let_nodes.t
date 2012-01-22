(herald let_nodes)

;;;   This is a backquote-like macro for building node trees.  The format is
;;; meant to look like the output of PP-CPS.  There is as yet no way to
;;; construct OBJECT nodes.
;;;
;;;   The goal is to produce code that is as efficient as possible.
;;;
;;; (LET-NODES (<spec1> ... <specN>) . <body>)
;;;
;;; <spec> ::= (<ident> <real-call>) |                            ; call node
;;;            (<ident> (<var1> ... <varN>) <call>) |             ; lambda node
;;;            (<ident> (<var1> ... <varN> . <last-vars>) <call>) ; lambda node
;;;
;;; <call> ::= <ident> | <real-call>
;;;
;;; <real-call> ::= (<arg0> <exits> <arg1> ... <argN>) |
;;;                 (<arg0> <exits> <arg1> ... <argN> . <last-args>))
;;;
;;; <var>  ::= () |               ; Ignored variable position
;;;            <ident> |          ; Create a variable with this name
;;;            (<ident> <value>)  ; Evaluate <value> to get the variable
;;;
;;; <last-args> ::= <ident>
;;;
;;; <last-vars> ::= <ident>
;;;
;;; <arg>  ::= ($ foo)      primop node for foo (which evaluates to a primop)
;;;            'foo         literal node containing the value of foo
;;;            (* foo)      reference to foo (which evaluates to a variable)
;;;            (! foo)      foo evaluates to a node
;;;            name         short for (! name) when foo is an atom
;;;            (^ name)     same as name (used for lambda nodes)
;;;
;;; Example:
;;;
;;; (let-nodes ((c1 ((^ l1) 1 cont))
;;;              (l1 (() v1)
;;;                  (($ primop/conditional) 2 (^ l2) (^ l3) primop arg1 arg2))
;;;                (l2 (()) ((* v1) 0 ''#t))
;;;                (l3 (()) ((* v1) 0 ''#f)))
;;;   (replace node c1))
;;;
;;;  ==>
;;;
;;; (LET ((V1 (CREATE-VARIABLE 'V1)))
;;;   (LET ((C1 (CREATE-CALL-NODE 2 1))
;;;         (C_1 (CREATE-CALL-NODE 6 2))
;;;         (L1 (CREATE-LAMBDA-NODE 'C (FLIST2 '() V1 '())))
;;;         (C_2 (CREATE-CALL-NODE 2 0))
;;;         (L2 (CREATE-LAMBDA-NODE 'C (FLIST1 '() '())))
;;;         (C_3 (CREATE-CALL-NODE 2 0))
;;;         (L3 (CREATE-LAMBDA-NODE 'C (FLIST1 '() '()))))
;;;     (RELATE CALL-PROC C1 L1)
;;;     (RELATE (CALL-ARG 1) C1 CONT)
;;;     (RELATE CALL-PROC C_1 (CREATE-PRIMOP-NODE PRIMOP/CONDITIONAL))
;;;     (RELATE-FIVE-CALL-ARGS C_1 L2 L3 PRIMOP ARG1 ARG2)
;;;     (RELATE LAMBDA-BODY L1 C_1)
;;;     (RELATE CALL-PROC C_2 (CREATE-REFERENCE-NODE V1))
;;;     (RELATE (CALL-ARG 1) C_2 (CREATE-LITERAL-NODE '#t))
;;;     (RELATE LAMBDA-BODY L2 C_2)
;;;     (RELATE CALL-PROC C_3 (CREATE-REFERENCE-NODE V1))
;;;     (RELATE (CALL-ARG 1) C_3 (CREATE-LITERAL-NODE '()))
;;;     (RELATE LAMBDA-BODY L3 C_3)
;;;     (REPLACE NODE C1)))
;;;

(define-syntax (let-nodes specs . body)
  (receive (vars nodes code)
           (parse-node-specs specs)
    `(let ,vars
       (let ,nodes
         ,@code
         ,@body))))

;;; Parse the specs, returning a list of variable specs, a list of node specs,
;;; and a list of construction forms.  An input spec is either a call or a
;;; lambda, each is parsed by an appropriate procedure.

(define (parse-node-specs specs)
  (iterate loop ((specs (reverse specs)) (vars '()) (nodes '()) (codes '()))
    (if (null? specs)
        (return vars nodes codes)
        (destructure ((((name . spec) . rest) specs))
          (cond ((null? (cdr spec))
                 (receive (node code)
                          (construct-call name (car spec))
                   (loop rest vars
                         `((,name ,node) . ,nodes) (append code codes))))
                ((fx= 2 (length spec))
                 (receive (vs node new-spec call)
                          (construct-lambda (car spec) (cadr spec))
                   (loop (if new-spec (cons new-spec rest) rest)
                         (append vs vars)
                         `((,name ,node) . ,nodes)
                         (if call 
                             `((relate lambda-body ,name ,call) . ,codes)
                             codes))))
                (else
                 (error "illegal spec in LET-NODES ~S" (cons name spec))))))))

;;; The names of the call-arg relation procedures, indexed by the number of
;;; arguments handled.

(define call-relate-names
  '#(#f
     #f
     relate-two-call-args
     relate-three-call-args
     relate-four-call-args
     relate-five-call-args))

;;; Return the node spec and construction forms for a call.  This dispatches
;;; on whether the argument list is proper or not.
;;;
;;; <real-call> ::= (<arg0> <exits> <arg1> ... <argN>) |
;;;                 (<arg0> <exits> <arg1> ... <argN> . <last-args>))

(define (construct-call name specs)
  (destructure (((proc exits . args) specs))
    (receive (list last)
             (decouple-improper-list args)
      (cond ((null? last)
             (construct-proper-call name proc exits args))
            (else
             (construct-improper-call name proc exits list last))))))

;;; Return proper part of the list and its last-cdr seperately.

(define (decouple-improper-list list)
  (do ((list list (cdr list))
       (res '() (cons (car list) res)))
      ((atom? list)
       (return (reverse! res) list))))

;;; Returns
;;;   (CREATE-CALL-NODE <length of args + 1> <exits>)
;;; and
;;;   ((RELATE-CALL-PROC <name> <procedure code>)
;;;    . <argument relation code>)
;;; The form of <argument relation code> depends on the number of arguments.

(define (construct-proper-call name proc exits args)
  (let* ((args (map construct-node args)) 
         (arg-code (cond ((null? args) '())
                         ((null? (cdr args))
                          `((relate (call-arg 1) ,name ,(car args))))
                         ((fx< (length args) 6)
                          `((,(vref call-relate-names (length args))
                             ,name ,@args)))
                         (else
                          `((relate-call-args ,name (list ,args)))))))
      (return `(create-call-node ,(fx+ 1 (length args)) ,exits)
              `((relate call-proc ,name ,(construct-node proc))
                . ,arg-code))))

;;; Returns
;;;   (CREATE-CALL-NODE (fx+ (LENGTH <last-arg>) <length of args + 1>) <exits>)
;;; and
;;;   ((RELATE-CALL-PROC <name> <procedure code>)
;;;    (RELATE-CALL-ARGS <name> (APPEND <args> <last-arg>)))

(define (construct-improper-call name proc exits args last-arg)
  (let* ((args (map construct-node args))
         (arg-code (if (null? args)
                       last-arg
                       `(append! (list . ,args) ,last-arg))))
      (return `(create-call-node (fx+ ,(fx+ 1 (length args))
                                      (length ,last-arg))
                                 ,exits)
              `((relate call-proc ,name ,(construct-node proc))
                (relate-call-args ,name ,arg-code)))))

;;; Dispatch on the type of the SPEC and return the appropriate code.
;;;
;;; <arg>  ::= ($ foo)      primop node for foo (which evaluates to a primop)
;;;            'foo         literal node containing the value of foo
;;;            (* foo)      reference to foo (which evaluates to a variable)
;;;            (! foo)      foo evaluates to a node
;;;            name         short for (! name) when foo is an atom
;;;            (^ name)     same as name (used for lambda nodes)

(define (construct-node spec)
  (cond ((atom? spec) spec)
        (else
         (case (car spec)
           (($) `(create-primop-node ,(cadr spec)))
           ((^) (cadr spec))
           ((*) `(create-reference-node ,(cadr spec)))
           ((quote) `(create-literal-node ,(cadr spec)))
           ((!) (cadr spec))
           (else
            (error "CONSTRUCT-NODE confused by ~S in LET-NODE" spec))))))

;;; Parse a lambda spec.  This returns a list of variable specs, code to
;;; construct the lambda node, a spec for the body if necessary, and
;;; the code needed to put it all together.

(define (construct-lambda vars call)
  (receive (vars node)
           (construct-vars vars)
    (cond ((null? call)
           (return vars node nil nil))
          ((atom? call)
           (return vars node nil call))
          (else
           (let ((sym (generate-symbol 'c)))
             (return vars node `(,sym ,call) sym))))))

;;; Returns the code needed to construct the variables and the code to make
;;; the lambda node that binds the variables.
;;;
;;; <var>  ::= () |               ; Ignored variable position
;;;            <ident> |          ; Create a variable with this name
;;;            (<ident> <value>)  ; Evaluate <value> to get the variable

(define (construct-vars vars)
  (iterate loop ((vs vars) (vlist '()) (code '()))
    (cond ((null? vs)
           (return code
                   `(create-lambda-node 'c ,(flistify (reverse! vlist) ''()))))
          ((atom? vs)
           (return code
                   `(create-lambda-node 'c ,(flistify (reverse! vlist) vs))))
          ((null? (car vs))         
           (loop (cdr vs) (cons ''() vlist) code))
          ((list? (car vs))
           (loop (cdr vs) (cons (caar vs) vlist)
                 `((,(caar vs) ,(cadar vs)) . ,code)))
          (else
           (loop (cdr vs) (cons (car vs) vlist)
                 `((,(car vs) (create-variable ',(car vs))) . ,code))))))

;;; The names of the free-list constructing procedures, indexed by the
;;; number of arguments they take.

(define flist-names '#(#f flist1 flist2 flist3 flist4 flist5))

;;; Returns code to construct a free-list of the SPECS, with the last-cdr
;;; of LAST.

(define (flistify specs last)
  (cond ((null? specs)
         last) 
        ((fx< (length specs) 6)
         `(,(vref flist-names (length specs)) ,@specs ,last))
        (else
         (let ((rest (cddddr specs)))
           `(flist5 ,(car specs)
                    ,(cadr specs)
                    ,(caddr specs)
                    ,(cadddr specs)
                    ,(car rest)
                    ,(flistify (cdr rest) last))))))



