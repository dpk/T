(herald vax_float (env tsys))


;;; Flonum dismemberment.

;;; Returns sign, and normalized mantissa and exponent  
;;; PRECISION is number of bits desired in the mantissa 
;;; EXCESS is the exponent excess
;;; HIDDEN-BIT-IS-1.? is true if the hidden bit preceeds the
;;;  binary point (it does in Apollo IEEE, does not on the VAX).

(define (normalize-float-parts sign m e precision excess hidden-bit-is-1.?)
  (let* ((have (integer-length m))
         (need (fx- precision have))
         (normalized-m (%ash m need))
         (normalized-e (- (+ e 
                             precision 
                             excess
                             (if hidden-bit-is-1.? -1 0))
                           need)))
     (return (if (= sign 1) 0 1) normalized-m normalized-e)))

;;; <n,s> means bit field of length s beginning at bit n of the first
;;; WORD (not longword)
;;;                    sign      exponent   MSB       fraction
;;; VAX11 flonum (D)   <15,1>    <7,8>      hidden    <0,7>+next 3 words
;;; VAX11 flonum (D)   - binary point precedes hidden MSB, 56 bits of
;;;     precision, if hidden bit is included 

(define-constant %%vax-d-size 56)
(define-constant %%vax-d-excess 128)

(define (integer-decode-float x)
  (let ((a (mref-16-u x 0)))
    (return (if (fl<= 0.0 x) 1 -1)
            (+ (mref-16-u x 6)
               (%ash (+ (mref-16-u x 4)
                        (%ash (fx+ (mref-16-u x 2)
                                   (fixnum-ashl (fx+ (fixnum-bit-field a 0 7) 128)
                                                16))
                              16))
                     16))
            (fx- (fixnum-bit-field a 7 8) (fx+ 128 56)))))                      

(define (integer-encode-float sign m e)
  (let ((float (make-flonum)))
    (receive (sign mantissa exponent)
             (normalize-float-parts sign
                                    m
                                    e
                                    %%vax-d-size
                                    %%vax-d-excess 
                                    '#f)
      (set (mref-16-u float 0) (fx+ (fixnum-ashl sign 15)
                                    (fx+ (fixnum-ashl exponent 7)
                                         (bignum-bit-field mantissa 48 7))))
      (set (mref-16-u float 2) (bignum-bit-field mantissa 32 16)) 
      (set (mref-16-u float 4) (bignum-bit-field mantissa 16 16)) 
      (set (mref-16-u float 6) (bignum-bit-field mantissa 0  16)) 
      float)))

(define (string->flonum s)
  (kludgy-string->flonum s))

(lset *print-flonums-kludgily?* t)

(define-handler double-float
  (object nil
    ((extended-number-type self) %%flonum-number-type)
    ((print self stream)
     (if *print-flonums-kludgily?*
         (print-flonum-kludgily self stream)
         (print-flonum self stream)))))
                                               
