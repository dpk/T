(herald big_fixnum
  (env tsys))

;;; This file contains fixnum routines which require other number
;;; types to be present.

(define (fixnum-negative-expt x y)
  (ratio 1 (fixnum-expt x (fx- 0 y))))

;;; Careful fixnum routines:

;;; Assume N bit arithmetic, machine result is N bits (what the hardware
;;; gives).  The "true result" may be up to N+1 bits sign of result on
;;; overflow.
;;;
;;;   if (A-B) overflows, A and B have different signs,
;;;            true result is sign of A
;;;   if (A+B) overflows, A and B have the same sign,
;;;            true result is the same sign
;;;
;;;   When the sign of the true result is positive, the magnitude of the
;;;     true result is the machine result.
;;;
;;;   When the sign of the true result is negative, the magnitude of the
;;;     true result is the magnitude of the N+1 bit number
;;;     formed by the carry bit and the machine result.
;;;
;;; Subtle issue:
;;; Taking the arithmetic complement of an N+1 bit number requires N+1 bits
;;; in all cases except for the most negative number which requires N+2 bits
;;; to complement.  Fortunately we only want the magnitude, which is
;;; the arithmetic complement of the hardware result, except when the
;;; N+1 bit number is the most negative N+1 bit number (carry bit = 1,
;;; fixnum = 0), in which case the magnitude is that same N+1 bit number.
;;;
;;; Aren't you glad I figured this out?
;;;
;;;  I'd be happier if you had also figured out the most negative number can
;;; only occur on an add overflow and not on a subtraction.  Also, a
;;; subtraction cannot overflow and give a result of 0.  Thus the CARRY?
;;; argument to FIXNUM-AFTERMATH->BIGNUM was unnecessary.

;;; This routine takes wreckage from an overflowed ADD or SUB and
;;; makes a bignum out of it.  SIGN is computed as described above.

(define (fixnum-aftermath->bignum sign fixnum)
  (cond ((fx= sign -1)
         (cond ((fx= fixnum 0)     ; min N+1 bit fixnum
                (sign&magnitude->bignum -1 t 0))
               (else
                (sign&magnitude->bignum -1 nil (fixnum-negate fixnum)))))
        (else
         (sign&magnitude->bignum 1 nil fixnum))))

(define (fixnum-add-carefully x y)
  (receive (sum overflow?)
           (fixnum-add-with-overflow-xxx x y)
    (cond (overflow?
           (fixnum-aftermath->bignum (if (fx< x 0) -1 1)  ; sign of either
                                     sum))
          (else sum))))

(define (fixnum-subtract-carefully x y)
  (receive (diff overflow?)
           (fixnum-subtract-with-overflow-xxx x y)
    (cond (overflow?
           (fixnum-aftermath->bignum (if (fx< x 0) -1 1)  ; sign of first
                                     diff))
          (else diff))))

(define (fixnum-multiply-carefully x y)
  (let ((res (fx+ (fixnum-howlong (fixnum-abs x))
                  (fixnum-howlong (fixnum-abs y)))))
    (cond ((fx>= res *bits-per-fixnum*)
           (bignum-multiply (fixnum->bignum x) (fixnum->bignum y)))
          (else (fx* x y)))))

(define fixnum-divide-carefully ratio)

;(define (fixnum-negate-carefully fixnum)
;  (cond ((fx= fixnum most-positive-fixnum)  ;;; Huh? most-negative-fixnum is the problem.
;         (fixnum->bignum fixnum))
;        (else (fixnum-negate fixnum))))
