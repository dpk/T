(herald big_util
  (env tsys (osys fixnum) bignum))

;;; Low level bignum stuff

;;; (MAKE-BIGNUM <number of slots>)
(define (make-bignum length)
  (make-vector-extend header/bignum
                     (enforce acceptable-vector-size? length)
                     length))

(lset *bignum-cons-counter* 0)
(lset *bignum-cons-size-counter* 0)

(define (create-bignum j)
  (set *bignum-cons-counter* (fx+ *bignum-cons-counter* 1))
  (set *bignum-cons-size-counter* (fx+ *bignum-cons-size-counter* j))
  (make-bignum j))

(define-integrable (bignum-sign num)
  (if (bignum-positive? num) 1 -1))

(define-integrable (set-bignum-sign! num sign)
  (if (if (bignum-positive? num)
          (fx> 0 sign)
          (fx< 0 sign))
      (bignum-negate! num))
  num)

;;; Random general utilities:

(define (make-and-replace-bignum size old i1 i2 count)
  (let ((new (create-bignum size)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i count)
         (set-bignum-sign! new (bignum-sign old))
         new)
      (set (bignum-digit new (fx+ i1 i)) (bignum-digit old (fx+ i2 i))))))

(define (copy-bignum old)
  (let* ((len (bignum-length old))
         (new (make-and-replace-bignum len old 0 0 len)))
    (set-bignum-sign! new (bignum-sign old))
    new))

;;; Given a bignum of length at least l, truncates that bignum,
;;; eliminating the leading zeros.  This is destructive.

(define (bignum-trim! num)
  (do ((i (fx- (bignum-length num) 1) (fx- i 1)))
      ((or (fx<= i 0)
           (not (fx= (bignum-digit num i) 0)))
       (set-bignum-length! num (fx+ i 1))
       num)))

;;; Primops for bignums

;;; (BIGNUM-POSITIVE? <bignum>)
;;; (BIGNUM-NEGATE! <bignum>)
;;; (BIGNUM-LENGTH <bignum>)
;;; (SET-BIGNUM-LENGTH! <bignum> <length>)

;;; Primops for 30 bit hyperdigits

;;; Add two hyperdigits and a carry bit, returning sum and new carry.

(define (%digit-add u v carry)
  (let ((sum (fx+ (fx+ (low-bits u) (low-bits v)) carry)))
    (xcase (fx+ (high-bit u) (high-bit v))
      ((0) (return sum 0))
      ((1) (if (fx= (high-bit sum) 0)
               (return (set-high-bit sum 1) 0)
               (return (set-high-bit sum 0) 1)))
      ((2) (return sum 1)))))

(define-integrable (high-bit x)
  (fixnum-bit-field x 29 1))

(define-integrable (set-high-bit x b)
  (set-fixnum-bit-field x 29 1 b))

(define-integrable (low-bits x)
  (fixnum-bit-field x 0 29))

;;; Subtract two hyperdigits and carry, returning difference and new carry.

(define (%digit-subtract u v carry)
  (let ((sum (fx- (fx- (low-bits u) (low-bits v)) carry)))
    (xcase (fx- (high-bit u) (high-bit v))
      ((-1) (if (fx= (high-bit sum) 0)
               (return (set-high-bit sum 1) 1)
               (return (set-high-bit sum 0) 1)))
      ((0) (return sum (high-bit sum)))
      ((1) (if (fx= (high-bit sum) 0)
               (return (set-high-bit sum 1) 0)
               (return (set-high-bit sum 0) 0))))))

;;; Multiply two hyperdigits, returning low and high digits of product.

(define (%digit-multiply u v)
  (let ((low-u (low-half u))
        (low-v (low-half v))
        (high-u (high-half u))
        (high-v (high-half v)))
    (let ((low (fx* low-u low-v))
          (middle-a (fx* low-u high-v))
          (middle-b (fx* high-u low-v))
          (high (fx* high-u high-v)))
      (receive (low c1)
               (%digit-add low (fixnum-ashl middle-a 15) 0)
        (receive (low c2)
                 (%digit-add low (fixnum-ashl middle-b 15) 0)
          (receive (high #f)
                   (%digit-add high
                               (fx+ (fx+ (high-half middle-a)
                                         (high-half middle-b))
                                    c1)
                               c2)
            (return high low)))))))

(define-integrable (low-half x)
  (fixnum-bit-field x 0 15))

(define-integrable (high-half x)
  (fixnum-bit-field x 15 15))

(define-integrable (fixnum-lshr x d)
  (fixnum-logand (fixnum-lognot (fixnum-ashl -1 (fx- 30 d)))
                 (fixnum-ashr x d)))

(define (%digit-greater? x y)
  (receive (dort carry)
           (%digit-subtract y x 0)
    (fx= carry 1)))

(define (fixnum-add-with-overflow-xxx x y)
  (let ((sum (fx+ x y)))
    (return sum (if (fx>= x 0)
                    (and (fx> y 0) (fx< sum 0))
                    (and (fx< y 0) (fx>= sum 0))))))

(define (fixnum-subtract-with-overflow-xxx x y)
  (let ((diff (fx- x y)))
    (return diff
            (if (fx>= x 0)
                (and (fx< y 0) (fx< diff 0))
                (and (fx> y 0) (fx> diff 0))))))



