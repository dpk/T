(herald pfloat
  (env tsys))

;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************


;;;
;;; Spice Lisp printer.
;;; Written by Neal Feinberg, Spice Lisp Group.
;;; Currently maintained by Skef Wholey.
;;; 
;;; *******************************************************************

;;; *** Adapted for use in T by J. Rees, Nov 1983 ***

;;; *** Rewritten without SET by R. Kelsey, April 1986 ***

;;;; Floating Point printing
;;;
;;;  Written by Bill Maddox
;;;
;;;
;;;
;;; FLONUM->STRING (and its subsidiary function FLOAT-STRING) does most of
;;; the work for all printing of floating point numbers in the printer and in
;;; FORMAT.  It converts a floating point number to a string in a free or 
;;; fixed format with no exponent.  The interpretation of the arguments is as 
;;; follows:
;;;
;;;     X        - The floating point number to convert, which must not be
;;;                negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;                of fraction digits to produce if the FDIGITS parameter
;;;                is unspecified or NIL.  If the non-fraction digits and the
;;;                decimal point alone exceed this width, no fraction digits
;;;                will be produced unless a non-NIL value of FDIGITS has been
;;;                specified.  Field overflow is not considered an error at
;;;                this level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;                trailing zeroes may be introduced as needed.  May be
;;;                unspecified or NIL, in which case as many digits as possible
;;;                are generated, subject to the constraint that there are no
;;;                trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-NIL, then the number
;;;                printed is (* x (expt 10 scale)).  This scaling is exact,
;;;                and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-NIL, is the minimum
;;;                number of fraction digits which will be produced, regardless
;;;                of the value of WIDTH or FDIGITS.  This feature is used by
;;;                the ~E format directive to prevent complete loss of
;;;                significance in the printed value due to a bogus choice of
;;;                scale factor.
;;;
;;; Most of the optional arguments are for the benefit of FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (return DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;                       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;                       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;                       point.  Zero indicates point before first digit.
;;;
;;; NOTE:  FLONUM->STRING goes to a lot of trouble to guarantee accuracy.
;;; Specifically, the decimal number printed is the closest possible 
;;; approximation to the true value of the binary number to be printed from 
;;; among all decimal representations  with the same number of digits.  In
;;; free-format output, i.e. with the number of digits unconstrained, it is 
;;; guaranteed that all the information is preserved, so that a properly-
;;; rounding reader can reconstruct the original binary number, bit-for-bit, 
;;; from its printed decimal representation. Furthermore, only as many digits
;;; as necessary to satisfy this condition will be printed.
;;;
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.  The
;;; algorithm is essentially that of algorithm Dragon4 in "How to Print 
;;; Floating-Point Numbers Accurately" by Steele and White.  The current 
;;; (draft) version of this paper may be found in [CMUC]<steele>tradix.press.
;;; DO NOT EVEN THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING 
;;; THE PAPER!

;;; E.g. (%ceiling 27 10) => 3

(define (%ceiling x y)          ; YUCKO !!
  (receive (q r) (quotient&remainder x y)
    (if (= r 0) q (+ q 1))))

;;; Returns a STRING-BUFFER, which must be freed at some point.

(define (flonum->string x width fdigits scale fmin)
  (cond ((zero? x)
         ;; zero is a special case which float-string cannot handle
         (let ((buffer (get-string-buffer)))
           (string-writec buffer #\.)
           (return buffer 1 t t 0)))
        (else
         (receive (#f mag exp)
                  (integer-decode-float x)
           (float-string mag exp (integer-length mag)
                         width fdigits scale fmin)))))

(define (float-string fraction exponent precision width fdigits scale fmin)
  (receive (r s m- m+ log-r roundup? cutoff)
           (rationalize-float fraction exponent precision
                              width fdigits scale fmin)
    (let ((buffer (get-string-buffer)))
      (write-preceding-zeros log-r buffer)
      (write-digits r s m- m+ log-r buffer cutoff roundup?)
      (let ((decpnt (string-posq #\. buffer)))
        ;;add trailing zeroes to pad fraction if fdigits specified
        (if fdigits
            (add-asked-for-zeroes fdigits buffer decpnt))
        (let ((digits (string-buffer-length buffer)))
          (return buffer
                  digits
                  (fx= decpnt 0)
                  (fx= decpnt (fx- digits 1))
                  decpnt))))))

(define  (add-asked-for-zeroes fdigits buffer decpnt)
  (let ((needed-zeros (fx- fdigits (fx- (string-buffer-length buffer)
                                        decpnt))))
    (do ((i needed-zeros (fx- i 1)))
        ((fx<= i -1)) ; off by one?
      (string-writec buffer #\0))))
        
;; Represent fraction as r/s, error bounds as m+/s and m-/s.
;; Rational arithmetic avoids loss of precision in subsequent calculations.

(define (rationalize-float fraction exponent precision width fdigits scale fmin)
  (receive (r s m- m+)
           (really-rationalize-float fraction exponent precision)
    (receive (r s m- m+)
             (scale-up r s m- m+ scale)
      (receive (r s m- m+ log-r)
               (compute-log-r r s m- m+)
        (iterate loop ((r r) (s s) (m- m-) (m+ m+) (log-r log-r) (roundup? nil)) 
          (receive (r s m- m+ log-r)
                   (recompute-log-r r s m- m+ log-r)
            (let ((cutoff (get-digit-cutoff width fdigits fmin log-r)))
              (receive (m- m+ new-ru?)
                       (if cutoff
                           (adjust-error m- m+ s (fx- cutoff log-r))
                           (return m- m+ nil))
                (if (< (+ (%ash r 1) m+) (%ash s 1))
                    (return r s m- m+ log-r (or roundup? new-ru?) cutoff)
                    (loop r s m- m+ log-r (or roundup? new-ru?)))))))))))

(define (really-rationalize-float fraction exponent precision)
  (receive (r s m- m+)
           (cond ((fx> exponent 0)
                  (return (%ash fraction exponent)
                          1
                          (%ash 1 exponent)
                          (%ash 1 exponent))) 
                 ((fx< exponent 0)
                  (return fraction (%ash 1 (fx- 0 exponent)) 1 1))
                 (else
                  (return fraction 1 1 1)))
    ;; adjust the error bounds m+ and m- for unequal gaps
    (if (= fraction (%ash 1 precision))
        (return (%ash r 1) (%ash s 1) m- (%ash m+ 1))
        (return r s m+ m-))))

;; scale value by requested amount, and update error bounds

(define (scale-up r s m- m+ scale)
  (cond ((not scale)
         (return r s m- m+))
        ((fx< scale 0)
         (let ((scale-factor (expt 10 (fx- 0 scale))))
           (return r (* s scale-factor) m- m+)))
        (else
         (let ((scale-factor (expt 10 scale)))
           (return (* r scale-factor)
                   s
                   (* m- scale-factor)
                   (* m+ scale-factor))))))

(define (compute-log-r r s m- m+)
  (let ((c (%ceiling s 10)))
    (do ((k 0 (fx- k 1))
         (d 1 (* d 10)))
        ((>= (* r d) c)
         (return (* r d) s (* m- d) (* m+ d) k)))))

(define (recompute-log-r r s m- m+ log-r)
  (let ((z (+ (%ash r 1) m+)))
    (do ((s s (* s 10))
         (k log-r (fx+ k 1)))
        ((< z (%ash s 1))
         (return r s m- m+ k)))))

;;determine number of fraction digits to generate

(define (get-digit-cutoff width fdigits fmin log-r)
  ;;don't allow less than fmin fraction digits
  (let ((fix-fmin (lambda (maybe)
                    (if (and fmin (fx> maybe (fx- 0 fmin)))
                        (fx- 0 fmin)
                    maybe))))
    (cond (fdigits
           ;;use specified number of fraction digits
           (fix-fmin (fx- 0 fdigits)))
          ((not width)
           nil)
          ((fx< log-r 0)
           ;;use as many fraction digits as width will permit
           (fix-fmin (fx- 1 width)))
          (else
           (fix-fmin (fx+ 1 (fx- log-r width)))))))

;;If we decided to cut off digit generation before precision has
;;been exhausted, rounding the last digit may cause a carry propagation.
;;We can prevent this, preserving left-to-right digit generation, with
;;a few magical adjustments to m- and m+.  Of course, correct rounding
;;is also preserved.

(define (adjust-error m- m+ s count)
  (let ((y (if (fx>= count 0)
               (* s (expt 10 count))
               (do ((i count (fx+ i 1))
                    (y s (%ceiling y 10)))
                   ((fx>= i 0) y)))))
    (return (max y m-) (max y m+) (>= y m+))))

;;zero-fill before fraction if no integer part

(define (write-preceding-zeros log-r buffer)
  (cond ((fx>= log-r 0)
         0)
        (else
         (string-writec buffer #\.)
         (do ((i log-r (fx+ i 1)))
             ((fx>= i 0)
              (fx- 0 log-r))
           (string-writec buffer #\0)))))

;;generate the significant digits

(define (write-digits r s m- m+ log-r buffer cutoff roundup?)
  (let ((ash-s-1 (%ash s 1)))
    (iterate loop ((r r) (m- (* m- 10)) (m+ (* m+ 10)) (log-r (fx- log-r 1)))
      (if (fx= log-r -1) (string-writec buffer #\.))
      ;;(multiple-value-set (u r) (truncate (* r 10) s))
      (receive (u r)
               (quotient&remainder (* r 10) s)
        (let* ((ash-r-1 (%ash r 1))
               (low (< ash-r-1 m-))
               (high (if roundup?
                         (>= ash-r-1 (- ash-s-1 m+))
                         (>  ash-r-1 (- ash-s-1 m+)))))
      ;;stop when either precision is exhausted or we have printed as many
      ;;fraction digits as permitted
          (cond ((or low high (and cutoff (fx<= log-r cutoff)))
                 ;;if cutoff occured before first digit, then no digits generated
                 ;;at all.  last digit may need rounding
                 (if (or (not cutoff) (fx>= log-r cutoff))
                     (string-writec buffer (last-digit u high low r s)))
                 ;;zero-fill after integer part if no fraction
                 (if (fx>= log-r 0)
                     (add-decimal-point log-r buffer)))
                (else
                 (string-writec buffer (digit->char u 10))
                 (loop r (* m- 10) (* m+ 10) (fx- log-r 1)))))))))

(define (last-digit u high low r s)
  (digit->char (fx+ u (cond ((and low (not high)) 0)
                            ((and high (not low)) 1)
                            ((<= (%ash r 1) s)    0)
                            (else                 1)))
               10))

(define (add-decimal-point log-r buffer)
  (do ((i log-r (fx- i 1)))
      ((fx<= i 0))
    (string-writec buffer #\0))
  (string-writec buffer #\.))


;;; Given a non-negative floating point number, SCALE-EXPONENT returns a
;;; new floating point number Z in the range (0.1, 1.0] and an exponent
;;; E such that Z * 10^E is (approximately) equal to the original number.
;;; There may be some loss of precision due the floating point representation.

;(defconstant short-log10-of-2 #~F0.30103s0)
(define-constant %sp-l-float fixnum->flonum)
(define-constant %long-float-ten (fixnum->flonum 10))
(define-constant %long-float-one-tenth (/ 1 %long-float-ten))
(define (log10 x) (fl/ (log x) (log %long-float-ten)))
(define-constant long-log10-of-2 (log10 (fixnum->flonum 2)))
(define-constant zero (fixnum->flonum 0))

(define (scale-exponent x)
      (scale-expt-aux x (%sp-l-float 0) (%sp-l-float 1) %long-float-ten
                      %long-float-one-tenth long-log10-of-2))


(define (scale-expt-aux x zero one ten one-tenth log10-of-2)
  (receive (#f #f exponent)
           (integer-decode-float x)
    (if (fl= x zero)
        (return zero 1)
        (let* ((e (flonum->fixnum (fl* (fixnum->flonum exponent) log10-of-2)))
               (x (if (fx< e 0)                ;For the end ranges.
                      (* (* x ten) (expt ten (fx- -1 e)))
                      (/ (/ x ten) (expt ten (fx-  e 1))))))
          (do ((d ten (* d ten))
               (y x (/ x d))
               (e e (fx+ 1 e)))
              ((< y one)
               (do ((m ten (* m ten))
                    (z y (* z m))
                    (e e (fx- e 1)))
                   ((>= z one-tenth) (return z e)))))))))

;;; Entry point for the float printer as called by PRINT, PRIN1, PRINC,
;;; etc.  The argument is printed free-format, in either exponential or 
;;; non-exponential notation, depending on its magnitude.
;;;
;;; NOTE:  When a number is to be printed in exponential format, it is scaled
;;; in floating point.  Since precision may be lost in this process, the
;;; guaranteed accuracy properties of FLONUM->STRING are lost.  The
;;; difficulty is that FLONUM->STRING performs extensive computations with
;;; integers of similar magnitude to that of the number being printed.  For
;;; large exponents, the bignums really get out of hand.  When we switch to
;;; IEEE format for long floats, this will significantly restrict the magnitude
;;; of the largest allowable float.  This combined with microcoded bignum
;;; arithmetic might make it attractive to handle exponential notation with
;;; the same accuracy as non-exponential notation, using the method described
;;; in the Steele and White paper.

(define %long-float1l-3 (/ (fixnum->flonum 1) 1000)) ;1.0e-3
(define %long-float1l7  (fixnum->flonum 10000000))   ;1.0e7

(define (print-flonum x stream)         ;Entry point from PRINT
  (output-float-aux x %long-float1l-3 %long-float1l7 stream))

;;; There is (was?) a TC bug lurking in here.  Beware.

(define (output-float-aux x e-min e-max stream)
  (cond ((nan? x) (write-string stream "NaN"))
	((fl= x 0.0) (write-string stream "0.0")
                   ;(if (not (typep x *read-default-float-format*))
                   ;    (write-string (if (typep x 'short-float) "s0" "L0")))
                   )
        (else
         (let ((x (cond ((fl< x 0.0)
                         (write-char stream #\-)
                         (fl- 0.0 x))
                        (else x))))
           (if (and (fl>= x e-min) (fl< x e-max))
               (output-free-float x stream)
               (output-exponential-float x stream))))))

(define (output-free-float x stream)
  (receive (str len lpoint tpoint ())
           (flonum->string x nil nil nil nil)
    (if lpoint (write-char stream #\0))
    (write-string stream str)
    (release-string-buffer str)
    (if tpoint (write-char stream #\0))
    ;(if (not (typep x *read-default-float-format*))
    ;    (write-string (if (typep x 'short-float) "s0" "L0")))
    ))

(define (output-exponential-float x stream)
  (receive (f e)
           (scale-exponent x)
    (receive (str len lpoint tpoint ())
             (flonum->string f nil nil 1 nil)
      (if lpoint (write-char stream #\0))
      (write-string stream str)
      (release-string-buffer str)
      (if tpoint (write-char stream #\0))
      (write-char stream
                  ;(if (typep x *read-default-float-format*)
                      #\E
                      ;(if (typep x 'short-float) #\S #\L))
      )
      ;; must subtract 1 from exponent here, due to
      ;; the scale factor of 1 in call to FLONUM->STRING
      (if (not (fx< (fx- e 1) 0)) (write-char stream #\+))
      (print (fx- e 1) stream))))

(set *print-flonums-kludgily?* nil)
