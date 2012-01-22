(herald genarith
  (env tsys (t3_primops open) (t3_primops aliases)))

(define-constant (add . args)         
  (iterate loop ((args args) (res '0))
    (if (null? args)
        res
        (loop (cdr args) (%add (car args) res)))))

(declare simplifier add
  (lambda (call)
    (let ((args (cdr (call-args call))))
      (cond ((null? args)
             (replace-call-with-value call (create-literal-node '0))
             '#t)
            ((null? (cdr args))  ; (+ 'a) => 'a in compiled code - no checking
             (replace-call-with-value call (detach (car args)))
             '#t)
            (else
             (n-ary->binary call '%add))))))

(define-constant (%add x y)
  (let ((generic (lambda (x y) (%%add x y))))
    (receive (ok? arg1 arg2)
             (two-fixnums x y)
      (if ok?
          (receive (over? result)
                   (fixnum-add-with-overflow arg1 arg2)
            (if over? (generic x y) result))
          (generic x y)))))

;;; SUBTRACT

(define-constant (subtract x y)
  (let ((generic (lambda (x y) (%%subtract x y))))
    (receive (ok? arg1 arg2)
             (two-fixnums x y)
      (if ok?
          (receive (over? result)
                   (fixnum-subtract-with-overflow arg1 arg2)
            (if over? (generic x y) result))
          (generic x y)))))

;;; MULTIPLY

(define-constant (multiply . args)    ; must be CONSTANT to get simplifier
  (iterate loop ((args args) (res '1))
    (if (null? args)
        res
        (loop (cdr args) (%multiply (car args) res)))))

(declare simplifier multiply
  (lambda (call)
    (let ((args (cdr (call-args call))))
      (cond ((null? args)
             (replace-call-with-value call (create-literal-node '1))
             '#t)
            ((null? (cdr args))  ; (* 'a) => 'a in compiled code - no checking
             (replace-call-with-value call (detach (car args)))
             '#t)
            (else
             (n-ary->binary call '%multiply)))))) 

(define-constant (%multiply x y)
  (let ((generic (lambda (x y) (%%multiply x y))))
    (receive (ok? arg1 arg2)
             (two-fixnums x y)
      (if ok?
          (receive (over? result)
                   (fixnum-multiply-with-overflow arg1 arg2)
            (if over? (generic x y) result))
          (generic x y)))))

;;; DIVIDE

(define-constant fixnum-remainder-with-hack
  (primop fixnum-remainder-with-hack ()
    ((primop.generate self node)
     (generate-hack-dr node 'remainder))
    ((primop.make-closed self) primop/undefined-effect)
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))


(define-constant (remainder x y)
  (receive (ok? arg1 arg2)
           (two-fixnums x y)
    (if ok?
        (fixnum-remainder-with-hack arg1 arg2)   ; no overflow possible
        (%%remainder x y))))

(define-constant (logand x y)
  (receive (ok? arg1 arg2)
           (two-fixnums x y)
    (if ok?
        (fixnum-logand arg1 arg2)   ; no overflow possible
        (%%logand x y))))

(define-constant (logior x y)
  (receive (ok? arg1 arg2)
           (two-fixnums x y)
    (if ok?
        (fixnum-logior arg1 arg2)   ; no overflow possible
        (%%logior x y))))

(define-constant (logxor x y)
  (receive (ok? arg1 arg2)
           (two-fixnums x y)
    (if ok?
        (fixnum-logxor arg1 arg2)   ; no overflow possible
        (%%logxor x y))))

(define-constant (lognot x)
  (logxor x -1))

;;; LESS?

(define-constant (less? x y)
  (if (two-fixnums-for-compare? x y)
      (fx< x y)
      (%%less? x y)))

;;; NUMBER-EQUAL?

(define-constant (number-equal? x y)
  (if (two-fixnums-for-compare? x y)
      (fx= x y)
      (%%equal? x y)))

;;; Thousands of random ways to call the above

(define-constant (negate x) (subtract 0 x))

(define-constant + add)

(define-constant (- x . y)            ; must be CONSTANT to get simplifier
  (cond ((null? y) (negate x))
        ((null? (cdr y)) (subtract x (car y)))
        (else (error "wrong number of arguments to procedure~%  ~S"
		     `(- ,x . ,y)))))

(declare simplifier -
  (lambda (call)
    (let ((args (cdr (call-args call))))
      (cond ((null? args)
             (user-message 'warning "- called with no arguments" '#f)
             '#f)       ; Error at runtime
            ((null? (cdr args))
             (replace (call-proc call)
                      (create-reference-node (get-system-variable 'negate)))
             '#t)
            ((null? (cddr args))
             (replace (call-proc call)
                      (create-reference-node (get-system-variable 'subtract))))
            (else
             (user-message 'warning "more than two arguments in a call to -" '#f)
             '#f)))))   ; Error at runtime


(define-constant * multiply)

(define-constant (add1      x) (%add     x 1))
(define-constant (subtract1 x) (subtract x 1))

(define-constant  1+ add1)
(define-constant -1+ subtract1)
(define-constant (=1? x) (= x 1))

(define-constant (not-less? x y)         (not (less? x y)))
(define-constant (number-not-equal? x y) (not (number-equal? x y)))
(define-constant (greater? x y)          (less? y x))
(define-constant (not-greater? x y)      (not (less? y x)))

(define-constant <  less?)
(define-constant <= not-greater?)
(define-constant =  number-equal?)
(define-constant N= number-not-equal?)
(define-constant >  greater?)
(define-constant >= not-less?)

(define-constant (negative? x)     (< x 0))
(define-constant (zero? x)         (= x 0))
(define-constant (positive? x)     (> x 0))
(define-constant (not-negative? x) (>= x 0))
(define-constant (not-zero? x)     (N= x 0))
(define-constant (not-positive? x) (<= x 0))

(define-constant <0?  negative?)
(define-constant =0?  zero?)
(define-constant >0?  positive?)
(define-constant >=0? not-negative?)
(define-constant n=0? not-zero?)
(define-constant <=0? not-positive?)
