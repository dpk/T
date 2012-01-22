(herald fx_const)

;;; Constant folding and reduction in strength for fixnum arithmetic.

;;; To Do
;;; fixnum-equal?, fixnum-less?
;;; char=, char<
;;; char->ascii, ascii->char

;;;                       UTILITIES
;;;============================================================================

;;; Get the literal value of a node.  FIXNUM is a misnomer here.

(define (fixnum-value node)
  (if (literal-node? node)
      (literal-value node)
      nil))

;;; (PROC NODE ARG) => NODE if ARG is the arithmetic identity element for
;;; PROC.

(define (reduce-arith-identity call arg node ident)
  (cond ((fx= arg ident)
         (replace-call-with-value call (detach node))
         t)
        (else nil)))
                                      
;;; Replace CALL with VAL if TEST is true.

(define (reduce-arith-no-op call test val)
  (cond (test
         (replace-call-with-value call (create-literal-node val))
         t)
        (else nil)))
                                      
;;; If A1 and A2 are fixnums, then replace CALL with (PROC A1 A2).

(define (fixnum-constant-fold call a1 a2 proc)
  (cond ((and a1 (not (fixnum? a1)))
         (fix-user-error call
                         '"type conflict, got ~S when expecting a fixnum"
                         a1)
         t)
        ((and a2 (not (fixnum? a2)))
         (fix-user-error call
                         '"type conflict, got ~S when expecting a fixnum"
                         a2)
         t)
        ((and a1 a2)
         (replace-call-with-value call (create-literal-node (proc a1 a2)))
         t)
        (else nil)))

;;; Are N1 and N2 references to the same variable?

(define (duplicate-args? n1 n2)
  (and (reference-node? n1)
       (reference-node? n2)
       (eq? (reference-variable n1) (reference-variable n2))))

;;;               Fixnum Constant Folding
;;;============================================================================

;;;  (fx+ <fix>     0) => <fix>
;;;  (fx+     0 <fix>) => <fix>

(define (simplify-fixnum-add call)
  (let* ((n1 ((call-arg 2) call))
         (n2 ((call-arg 3) call))
         (a1 (fixnum-value n1))
         (a2 (fixnum-value n2)))
    (cond ((fixnum-constant-fold call a1 a2 fixnum-add)
           t)
          (a1
           (reduce-arith-identity call a1 n2 0))
          (a2
           (reduce-arith-identity call a2 n1 0))
          (else nil))))

;;;  (fx- <fix>     0) => <fix>
;;;  (fx- <fix> <fix>) => 0

(define (simplify-fixnum-subtract call)
  (let* ((n1 ((call-arg 2) call))
         (n2 ((call-arg 3) call))
         (a1 (fixnum-value n1))
         (a2 (fixnum-value n2)))
    (cond ((fixnum-constant-fold call a1 a2 fixnum-subtract)
           t)
          (a2
           (reduce-arith-identity call a2 n1 0))
          ((duplicate-args? n1 n2)
           (replace-call-with-value call (create-literal-node 0)))
          (else nil))))

;;;  (fx*  <fix>      0) => 0
;;;  (fx*      0  <fix>) => 0
;;;  (fx*  <fix>      1) => <fix>
;;;  (fx*      1  <fix>) => <fix>
;;;  (fx* <2**N>  <fix>) => (fixnum-ashl <fix> N)
;;;  (fx*  <fix> <2**N>) => (fixnum-ashl <fix> N)

;;;  (fx* <small fix> <fix>) => shift and add <fix>

(define (simplify-fixnum-multiply call)
  (let* ((n1 ((call-arg 2) call))
         (n2 ((call-arg 3) call))
         (a1 (fixnum-value n1))
         (a2 (fixnum-value n2)))
    (cond ((fixnum-constant-fold call a1 a2 fixnum-multiply)
           t)
          (a1
           (or (reduce-arith-identity call a1 n2 1)
               (reduce-arith-no-op call (fx= a1 0) 0)
               (reduce-multiply-by-2**n call a1 'fixnum-ashl n2)))
          (a2
           (or (reduce-arith-identity call a2 n1 1)
               (reduce-arith-no-op call (fx= a2 0) 0)
               (reduce-multiply-by-2**n call a2 'fixnum-ashl n1)))
          (else nil))))

;;;  (fx/ <fix>  <fix>) => 1   NO! <fix> may be 0
;;;  (fx/ <fix>      1) => <fix>
;;;  (fx/     0  <fix>) => 0 
;;;  (fx/ <fix>      0) => error
;;;  (fx/ <fix> <2**N>) => (fixnum-ashr <fix> N)

(define (simplify-fixnum-divide call)
  (let* ((n1 ((call-arg 2) call))
         (n2 ((call-arg 3) call))
         (a1 (fixnum-value n1))
         (a2 (fixnum-value n2)))
    (cond ((and a2 (eq? a2 0))                ; Must check first
           (fix-user-error call "fixnum divide by 0")
           t)
          ((fixnum-constant-fold call a1 a2 fixnum-divide)
           t)
          (a1
           (reduce-arith-no-op call (fx= a1 0) 0))
          (a2
           (or (reduce-arith-identity call a2 n1 1)
               (reduce-multiply-by-2**n call a2 'fixnum-ashr n1)))
          (else nil))))
                                      
;;; (<op> <arg> 2**n) => (<proc> <arg> n)

(define (reduce-multiply-by-2**n call arg proc node)
  (cond ((fixnum-power-of-two? arg)
         (replace-call call
                       proc
                       (detach node)
                       (create-literal-node (fx- (fixnum-howlong arg) 1)))
         t)
        (else nil)))

(define (fixnum-power-of-two? x)
  (and (fx> x 1)
       (fx= x (fixnum-ashl 1 (fx- (fixnum-howlong x) 1)))))

(define (obtain-system-primop name)                                      
  (let* ((v (get-system-variable name))
         (p (if v (definition->primop (variable-definition v)) nil)))
    (if p p (bug '"can't find system primop ~S" name))))

(define (replace-call call proc-name . args)
  (let ((new (create-call-node (fx+ '2 (length args)) '1))
        (var (get-system-variable proc-name)))
    (relate call-proc new (create-reference-node var))
    (relate-call-args new (cons (detach ((call-arg '1) call)) args))
    (replace call new)
    t))

;;;  (fx-ior <fix>     0) => <fix>
;;;  (fx-ior <fix>    -1) => -1
;;;  (fx-ior     0 <fix>) => <fix>
;;;  (fx-ior    -1 <fix>) => -1
;;;  (fx-ior <fix> <fix>) => <fix>

(define (simplify-fixnum-logior call)
  (let* ((n1 ((call-arg 2) call))
         (n2 ((call-arg 3) call))
         (a1 (fixnum-value n1))
         (a2 (fixnum-value n2)))
    (cond ((fixnum-constant-fold call a1 a2 fixnum-logior)
           t)
          (a1
           (or (reduce-arith-identity call a1 n2 0)
               (reduce-arith-no-op call (fx= a1 -1) -1)))
          (a2
           (or (reduce-arith-identity call a2 n1 0)
               (reduce-arith-no-op call (fx= a2 -1) -1)))
          ((duplicate-args? n1 n2)
           (replace-call-with-value call (detach n1)))
          (else nil))))

;;;  (fx-xor <fix>     0) => <fix>
;;;  (fx-xor 0     <fix>) => <fix>
;;;  (fx-xor <fix> <fix>) => 0

(define (simplify-fixnum-logxor call)
  (let* ((n1 ((call-arg 2) call))
         (n2 ((call-arg 3) call))
         (a1 (fixnum-value n1))
         (a2 (fixnum-value n2)))
    (cond ((fixnum-constant-fold call a1 a2 fixnum-logxor)
           t)
          (a1
           (reduce-arith-identity call a1 n2 0))
          (a2
           (reduce-arith-identity call a2 n1 0))
          ((duplicate-args? n1 n2)
           (replace-call-with-value call (create-literal-node 0)))
          (else nil))))

;;;  (fx-and <fix>     0) => 0
;;;  (fx-and <fix>    -1) => <fix>
;;;  (fx-and     0 <fix>) => 0
;;;  (fx-and    -1 <fix>) => <fix>
;;;  (fx-and <fix> <fix>) => <fix>

(define (simplify-fixnum-logand call)
  (let* ((n1 ((call-arg 2) call))
         (n2 ((call-arg 3) call))
         (a1 (fixnum-value n1))
         (a2 (fixnum-value n2)))
    (cond ((fixnum-constant-fold call a1 a2 fixnum-logand)
           t)
          (a1
           (or (reduce-arith-identity call a1 n2 -1)
               (reduce-arith-no-op call (fx= a1 0) 0)))
          (a2
           (or (reduce-arith-identity call a2 n1 -1)
               (reduce-arith-no-op call (fx= a2 0) 0)))
          ((duplicate-args? n1 n2)
           (replace-call-with-value call (detach n1)))
          (else nil))))

;;;  (fx-ash~ <fix>      0) => <fix>
;;;  (fx-ash~ 0      <fix>) => 0
;;;  (fx-ash~ <fix> *>=30*) => 0

(define (simplify-fixnum-shift call op)
  (let* ((n1 ((call-arg 2) call))
         (n2 ((call-arg 3) call))
         (a1 (fixnum-value n1))
         (a2 (fixnum-value n2)))
    (cond ((fixnum-constant-fold call a1 a2 op)
           t)
          (a1
           (reduce-arith-no-op call (fx= a1 0) 0))
          (a2
           (or (reduce-arith-identity call a2 n1 0)
               (reduce-arith-no-op call (fx>= a2 *bits-per-fixnum*) 0)))
          (else nil))))

