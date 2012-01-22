(herald arith (env tsys))

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

;;; Copyright (c) 1983, 1984 Yale University

;;;; Arithmetical and mathematical and generical

;;; Basic predicates

(define (integer? x)
  (or (fixnum? x) (bignum? x)))

(define-constant (number? x)
  (or (fixnum? x)
      (true? (extended-number-type x))))

(define float? double-float?)

(define real? number?)

(define short-float? false)	;Fix later for T3

;;; (define complex? number?)

(define-constant %%fixnum-number-type 0)
(define-constant %%flonum-number-type 1)
(define-constant %%bignum-number-type 2)
(define-constant %%ratio-number-type  3)

(define-constant %%number-of-number-types 4)

(define-operation (extended-number-type obj) nil)

(define-operation (unguarded-version proc) proc)

(define-integrable (number-type obj op)
  (cond ((fixnum? obj) %%fixnum-number-type)
        ((and (extend? obj)
              (extended-number-type obj))
         => identity)
        (else (losing-number-type obj op))))

(define (losing-number-type obj op)
  (number-type (error "non-numeric argument~%  (~S ... ~S ...)" op obj)
                op))

(define-operation (set-dispatch generic-op-frob type1 type2 procedure))



(define (make-two-arg-number-routine identifier)
  (let ((table (make-vector %%number-of-number-types)))
    (let ((lose
           (lambda (n1 n2)
             (error "generic number routine not defined for this ~
                     combination of types~%  (~S~_~S~_~S)"
                    identifier n1 n2))))
      (do ((i 0 (fx+ i 1)))
          ((fx>= i %%number-of-number-types)
           (object (lambda (n1 n2)
                     ((vref (vref table (number-type n1 identifier))
                            (number-type n2 identifier))
                      n1 n2))
                   ((set-dispatch self type1 type2 proc)
                    (vset (vref table type1) type2 (unguarded-version proc)))
                   ((identification self) identifier)))
        (let ((v (make-vector %%number-of-number-types)))
          (vset table i v)
          (vector-fill v lose))))))

;;; The dispatch tables

(define %%add      (make-two-arg-number-routine 'add))
(define %%subtract (make-two-arg-number-routine 'subtract))
(define %%multiply (make-two-arg-number-routine 'multiply))
(define %%divide   (make-two-arg-number-routine 'divide))
(define %%quotient (make-two-arg-number-routine 'quotient))
(define %%less?    (make-two-arg-number-routine 'less?))
(define %%equal?   (make-two-arg-number-routine 'number-equal?))

(define (set-dispatches type1 type2 + - * / quotient < =)
  (set-dispatch %%add       type1 type2 +)
  (set-dispatch %%subtract  type1 type2 -)
  (set-dispatch %%multiply  type1 type2 *)
  (set-dispatch %%divide    type1 type2 /)
  (set-dispatch %%quotient  type1 type2 quotient)
  (set-dispatch %%less?     type1 type2 <)
  (set-dispatch %%equal?    type1 type2 =))

;;; QUOTIENT
(define-constant divide %%divide)
(define-constant / divide) 
(define-constant div2 quotient&remainder)
(define-constant quotient %%quotient)

(define (quotient&remainder x y)
  (let ((div2-kludgily (lambda (x y)
                         (let ((q (quotient x y)))
                           (return q (subtract x (%multiply q y)))))))
    (cond ((fixnum? y)
           (cond ((fixnum? x)
                  (return (fx/ x y) (fixnum-remainder x y)))
                 ((bignum? x) (b-f-div2 x y))
                 (else
                  (div2-kludgily x y))))
          ((bignum? y)
           (cond ((fixnum? x) (return 0 x))
                 ((bignum? x) (bignum-div2 x y))
                 (else        (div2-kludgily x y))))
          (else (div2-kludgily x y)))))

;++ Kludge - add to dispatch table later.

(define (%%remainder x y)
  (let ((remainder-kludgily  (lambda (x y)
                               (subtract x (%multiply (quotient x y) y)))))
    (cond ((fixnum? y)
           (cond ((fixnum? x) (fixnum-remainder x y))
                 ((bignum? x) (b-f-remainder x y))
                 (else        (remainder-kludgily x y))))
          ((bignum? y)
           (cond ((fixnum? x)
                  (if (and (fx= x most-negative-fixnum)       ;Thanks to Joe Stoy!
                           (= y (negate most-negative-fixnum)))
                      0
                      x))
                 ((bignum? x) (bignum-remainder x y))
                 (else        (remainder-kludgily x y))))
          (else (remainder-kludgily x y)))))

(define-constant (odd? x) 
  (odd?-aux x odd?))

(define-constant (even? x) 
  (not (odd?-aux x even?)))

(define (odd?-aux x who)
  (let ((x (check-arg integer? x who)))
    (cond ((fixnum? x) (fixnum-odd? x))
          (else        (bignum-odd? x)))))

(define-integrable (nonnegative-integer? n)
  (and (integer? n) (not-negative? n)))

(define-integrable (fixnum-length x)
  (if (fx>= x 0)
      (fixnum-howlong x)
      (fixnum-howlong (fx- -1 x))))

(define (integer-length x) 
  (let ((x (enforce integer? x)))
    (cond ((fixnum? x)
	   (fixnum-length x))
          (else
	   (if (>= x 0)
	       (bignum-howlong x)
	       (bignum-howlong (subtract -1 x)))))))

(define (ash num amount)
  (let ((num    (enforce nonnegative-integer? num))
        (amount (enforce fixnum? amount)))
    (%ash num amount)))

(define (%ash num amount)           ; See PRINT-FLONUM
  (cond ((fixnum? num)
         (cond ((fx> amount 0)
                (let ((num-length (integer-length num)))
                  (let ((result-length (fx+ num-length amount)))
                    (cond ((fx> result-length *u-bits-per-fixnum*)
                           (fixnum-ashl-bignum num amount))
                          (else
                           (fixnum-ashl num amount))))))
               (else
                (fixnum-ashr num (fx- 0 amount)))))
        (else
         (cond ((fx> amount 0)
                (bignum-ashl num amount))
               ((fx= amount 0)
                num)
               (else
                (let ((amount (fx- 0 amount))
                      (num-length (integer-length num)))
                  (let ((result-length (fx- num-length amount)))
                    (cond ((fx> result-length *u-bits-per-fixnum*)
                           (bignum-ashr num amount))
                          ((fx<= result-length 0) 0)
                          (else
                           (bignum-ashr-fixnum num amount))))))))))

(define (bignum-lossage op)
  (lambda (x y)
    (error "~S not yet implemented for bignums"
	   (list op x y))))

(define %%logand  (bignum-lossage 'logand))
(define %%logior  (bignum-lossage 'logior))
(define %%logxor  (bignum-lossage 'logxor))

(define bit-field fixnum-bit-field)
(define set-bit-field set-fixnum-bit-field)

(define (max2 n1 n2)
  (if (greater? n1 n2) n1 n2))

(define (max number . numbers)
  (do ((n numbers (cdr n))
       (result number (max2 result (car n))))
      ((null? n) result)))

(define (min2 n1 n2)
  (if (less? n1 n2) n1 n2))

(define (min number . numbers)
  (do ((n numbers (cdr n))
       (result number (min2 result (car n))))
      ((null? n) result)))

(define (abs n) (if (less? n 0) (negate n) n))

;;; Raise any number to a fixnum power > 1.

(define (raise-to-fixnum-power base power)
  (do ((bit (fixnum-ashl 1 (fx- (fixnum-howlong power) 2))
            (fixnum-ashr bit 1))
       (result base (let ((result^2 (%multiply result result)))
                      (if (fx= (fixnum-logand power bit) 0)
                          result^2
                        (%multiply result^2 base)))))
      ((fx= bit 0) result)))

;;; (define (foo base power)
;;;   (do ((p power (fixnum-ashr p 1))
;;;        (temp base (* temp temp))
;;;        (result 1 (if (fixnum-odd? p) (* temp result) result)))
;;;       ((fx= p 0) result)))

;;; Has to deal with flonums

(define (expt x y)
  (let ((x (enforce number? x))
        (y (enforce fixnum? y)))
    (cond ((fx= y 1) x)
          ((fx< y 0) (/ 1 (expt x (fx- 0 y))))
          ((fx= y 0) 1)                 ; ??? if x is float, should return 1.0?
          ((not (fixnum? x)) (raise-to-fixnum-power x y))
          ((fx= x 0) 0)
          ((fx= x 1) 1)
          ((fx= x -1) (if (fixnum-odd? y) -1 1))
          (else (raise-to-fixnum-power x y)))))

;;; Euclid's algorithm.  Binary GCD would probably be better, esp. on machines
;;; like the 68000 that lack divide instructions.

(define (gcd p q)
  (do ((p (abs p) q)
       (q (abs q) (remainder p q)))
    ((number-equal? q 0) p)))

(define-integrable (signum x)
  (let ((x (enforce number? x)))
    (if (zero? x) x (/ x (abs x)))))

(define (modulo x y)
  (let ((x (enforce integer? x))
        (y (enforce integer? y)))
    (cond ((= (signum x) (signum y))
           (remainder x y))
          (else
           (let ((r (remainder x y)))
             (cond ((= r 0) 0)
                   (else
                    (+ y r))))))))

(define-constant mod modulo)

;;; Return largest multiple N of Y such that N <= X
;;; Awful, awful kludgey definition.

(define (floor x y)
  (subtract x (mod x y)))

;;; Return smallest multiple N of Y such that N >= X
;;; Awful, awful kludgey definition.

(define (ceiling x y)
  (floor (%add x (subtract y 1)) y))

;;; Coerce to integer.
;;; Awful, awful kludgey definition.

(define truncate ->integer)

(define (->integer x)
  (cond ((integer? x) x)
        ((float? x) (flonum->integer x))
        ((ratio? x) (quotient (numerator x) (denominator x)))
        (else (->integer (error "can't coerce to integer~%  (~S ~S)"
                                '->integer x)))))

;;; Coerce to floating point number.
;;; Awful, awful kludgey definition.

(define (->float x)
  (cond ((float? x) x)
        ((fixnum? x) (fixnum->flonum x))
        ((bignum? x) (bignum->flonum x))
        ((ratio? x) (flonum-divide (->float (numerator x))
                                   (->float (denominator x))))
        (else
         (->float (error "can't coerce to floating point number~%  (~S ~S)"
                         '->float x)))))
(define-constant most-positive-fixnum-as-flonum 
  (fixnum->flonum most-positive-fixnum))

(define-constant most-negative-fixnum-as-flonum 
  (fixnum->flonum most-negative-fixnum))

(define (flonum->integer x)
  (cond ((fl> x most-positive-fixnum-as-flonum)
         (receive (sig mag exp) (integer-decode-float x)
           (ash mag exp)))
        ((fl< x most-negative-fixnum-as-flonum)
         (receive (sig mag exp) (integer-decode-float x)
           (let ((n (ash mag exp)))
             (cond ((bignum? n) 
                    (bignum-negate! n) 
                    n)
                   (else
                    (fx-negate n))))))
        (else (flonum->fixnum x))))

(define (flonum-quotient x y)
  (flonum->integer (flonum-divide x y)))

(define integer->flonum fixnum->flonum)
