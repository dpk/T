(herald bignum
  (env tsys (osys fixnum) bignum))

;;; (c) Copyright 1983, 1984 Yale University

;;; To do:
;;;   destructive routines
;;;   rewrite print-bignum & associates (format nil ... etc)
;;;   pool bignums
;;;   bignum templates, positve & negative ?
;;;   pack densely - use hardware multiply, etc

;;; WARNING
;;;  Parts of this depend on (>= *bits-per-hyperdigit* *bits-per-fixnum*) => T.

;;; Constants:

(define-constant *bits-per-hyperdigit* 30)

(define-constant *max-hyperdigit* -1)

(define-constant *half-max-hyperdigit*
  (fx- (fixnum-ashl 1 (fx- *bits-per-hyperdigit* 1)) 1))

(lset *bignums-print-nicely?* nil)

(define-handler bignum
  (object nil
    ((extended-number-type self) %%bignum-number-type)
    ((print self port)
     (if *bignums-print-nicely?*
         (print-bignum self port)
         (print-bignum-guts self port)))))

(define (print-bignum-guts self port)
  (format port "{Bignum ~D ~A["
          (object-hash self) 
          (if (bignum-positive? self) "+" "-"))
  (format port "~X" (bignum-digit self 0))
  (do ((i 1 (fx+ i 1)))
      ((fx>= i (bignum-length self)))
    (format port "~_~X" (bignum-digit self i)))
  (format port "]}"))

;;; Comparison:

(define-integrable (bignum-magnitude-less? u v)
  (fx< (bignum-compare-magnitudes u v) 0))

;;; Returns a fixnum whose sign is the same as (- u v).

(define (bignum-compare u v)
  (let ((u-sign (bignum-sign u)))
    (cond ((fxn= u-sign (bignum-sign v)) u-sign)
          ((fx> u-sign 0) (bignum-compare-magnitudes u v))
          (else           (bignum-compare-magnitudes v u)))))

(define-integrable (bignum-less? u v)
  (fx< (bignum-compare u v) 0))

(define-integrable (bignum-equal? u v)
  (fx= (bignum-compare u v) 0))

;;; Sign negotiation and normalization:

;;; The BIGNUM-FOO routines negotiate a sign for the result, then
;;; dispatch to the appropriate FOO-MAGNITUDES routine.  The result
;;; is then normalized.

(define (bignum-add u v)
  (let ((u-sign (bignum-sign u))
        (v-sign (bignum-sign v)))
    (normalize-integer
     (cond ((fx= u-sign v-sign)
            (set-bignum-sign! (add-magnitudes u v) u-sign))
           ((bignum-magnitude-less? u v)
            (set-bignum-sign! (subtract-magnitudes v u) v-sign))
           (else
            (set-bignum-sign! (subtract-magnitudes u v) u-sign))))))

(define (bignum-subtract u v)
  (let ((u-sign (bignum-sign u))
        (v-sign (bignum-sign v)))
    (normalize-integer
     (cond ((fxn= u-sign v-sign)
            (set-bignum-sign! (add-magnitudes u v) u-sign))
           ((bignum-magnitude-less? v u)
            (set-bignum-sign! (subtract-magnitudes u v) u-sign))
           (else
            (set-bignum-sign! (subtract-magnitudes v u)
                             (fixnum-negate u-sign)))))))

(define-integrable (bignum-multiply-sign u v)
  (if (fx= (bignum-sign u) (bignum-sign v)) 1 -1))

(define (bignum-multiply u v)
  (normalize-integer
   (set-bignum-sign! (multiply-magnitudes u v) (bignum-multiply-sign u v))))

;;; Used only by BIGNUM-DIVIDE, BIGNUM-REMAINDER, and B-F-DIV2

(define (bignum-div2 u v)
  (let ((m (bignum-compare-magnitudes u v)))
    (cond ((fx= m 0)
           (return (bignum-multiply-sign u v) 0))
          ((fx< m 0)
           (return 0 u))
          (else
           (receive (q r)
                    (div2-magnitudes u v)
             (return (normalize-integer
                      (set-bignum-sign! q (bignum-multiply-sign u v)))
                     (normalize-integer
                      (set-bignum-sign! r (bignum-sign u)))))))))

(define (bignum-divide u v)    (receive (q r) (bignum-div2 u v) q))
(define (bignum-remainder u v) (receive (q r) (bignum-div2 u v) r))

(define (b-f-add u v)      (bignum-add u (fixnum->bignum v)))
(define (b-f-subtract u v) (bignum-subtract u (fixnum->bignum v)))

(define (b-f-divide u v)    (receive (q r) (b-f-div2 u v) q))
(define (b-f-remainder u v) (receive (q r) (b-f-div2 u v) r))

(define (fixnum-ashl-bignum num amount)
  (bignum-ashl (fixnum->bignum num) amount))   ; Fix later

(define (bignum-ashr-fixnum src amount)
  (normalize-integer (bignum-ashr src amount)))

;;; Total randomness: negate, odd?, howlong.

(define (bignum-negate num)
  (let ((new (copy-bignum num)))
    (bignum-negate! new)
    (normalize-integer new)))

(define (bignum-odd? num)
  (fixnum-odd? (bignum-digit num 0)))

(define (bignum-howlong num)
  (let ((last (fx- (bignum-length num) 1)))
    (fx+ (fixnum-howlong (bignum-digit num last))
         (fx* last *bits-per-hyperdigit*))))

;;; MAGN is a fixnum interpreted as an unsigned  integer that is 
;;; *bits-per-fixnum* long.  EXTRA-BIT? is a handy N+1st bit for those
;;; times that you have (+ *bits-per-fixnum* 1) bits of magnitude.

(define (sign&magnitude->bignum sign extra-bit? magn)
  (let ((num (create-bignum (if extra-bit? 2 1))))
    (set (bignum-digit num 0) magn)
    (if extra-bit? (set (bignum-digit num 1) 1))
    (set-bignum-sign! num sign)
    num))

;;; Normalization:

;;; Convert an integer to normal form.  That is, if it is a bignum within
;;; the fixnum range, convert it to a fixnum.

(define (normalize-integer n)
  (cond ((fixnum? n) n)
        ((if (bignum-positive? n)
             (bignum-less? most-positive-fixnum-as-bignum n)
             (bignum-less? n most-negative-fixnum-as-bignum))
         n)
        (else (bignum->fixnum n))))

(define-constant most-positive-fixnum-as-bignum
  (sign&magnitude->bignum  1 nil most-positive-fixnum))

(define-constant most-negative-fixnum-as-bignum
  (sign&magnitude->bignum -1 nil most-negative-fixnum))


;;; Coercion routines:

(define (fixnum->bignum fx)
  (cond ((fx= fx most-negative-fixnum) most-negative-fixnum-as-bignum)
        (else (sign&magnitude->bignum (if (fx< fx 0) -1 1) 
				      nil
				      (fixnum-abs fx)))))

(define (bignum->fixnum bn)
  (cond ((bignum-equal? most-negative-fixnum-as-bignum bn) most-negative-fixnum)
        ((bignum-positive? bn) (bignum-digit bn 0))
        (else
         (fx- 0 (bignum-digit bn 0)))))

(define (bignum->flonum b)
  (error "integer to float conversion not yet implemented~%  (~S ~S)"
         '->float b))

;;; Input and output:
;++ This can be speeded up if necessary.

(define (print-bignum num port)
  (let ((new-num (normalize-integer num)))
    (cond ((neq? num new-num)
           (format port "#{Unnormalized-bignum~_~S}"
                   new-num))
          (else
           (let ((buffer (bignum->buffer num)))
             (cond ((not (bignum-positive? num)) (writec port negative-sign-char)))
             (do ((i (fx- (buffer-length buffer) 1) (fx- i 1)))
                 ((not (char= (buffer-elt buffer i) #\0))
                  (do ((i i (fx- i 1)))
                      ((fx< i 0) (release-buffer buffer))
                    (writec port (buffer-elt buffer i))))))))))

;;; Convert a bignum to a sequence of characters.
;;; Characters are generated in reverse order by successive divisions.

(define (bignum->buffer num)
  (let* ((radix (rt-radix *print-table*))
         (k (\#chars-in-bit-field radix *bits-per-hyperdigit*))
         (radix^k (fixnum-expt radix k))
         (buffer (get-buffer)))
    (iterate loop ((num num))
      (receive (q r)
               (b-f-div2-unnormalized num radix^k)
        (output-bignum-digit (fixnum-abs r) k buffer radix)
        (cond ((fx> (bignum-length q) 1)
               (loop q))
              (else
               (iterate loop ((n (bignum-digit q 0)))
                 (if (fx= n 0)
                     buffer
                     (loop (output-bignum-digit n k buffer radix))))))))))

;;; Generate k digits of output.  Returns the k+1'th digit.

(define (output-bignum-digit digit k buffer radix)
  (iterate loop ((n digit)
                 (i k))
    (cond ((fx> i 0)
           (receive (q r)
                    (%digit-divide 0 n radix)
             (vm-write-char buffer (digit->char r radix))
             (loop q (fx- i 1))))
          (else n))))

;;; Number of characters in RADIX that can surely fit in FIELD-SIZE bits.
;;; Make this more accurate.  How does BIGNUM-STRINGIFY work and did
;;; I screw it up by changing this routine?

(define (\#chars-in-bit-field radix field-size)
  (fx/ field-size (fixnum-howlong radix)))

;;; Convert string to fixnum or bignum, as appropriate.

(define (string->integer string radix)
  (cond ((char= (char string) negative-sign-char)
         (string->integer-aux string 1 t radix))
        ((char= (char string) positive-sign-char)
         (string->integer-aux string 1 nil radix))
        (else
         (string->integer-aux string 0 nil radix))))

;;;  We grab a bunch of digits at a whack, convert them to fixnum, and
;;;  do multiplications just with them.  
;;; grabsize:  number of digits we can grab (whack size)
;;; shift:     radix of grabsize considered as a hyperdigit
;;; leftovers: number of digits that don't fit into an even number of grabs -
;;;            convert these first

;;; Fast enough?  Clean enough?
;;; Hack sign inside loop rather than after so that we read most-negative-fixnum
;;; as a fixnum and not a bignum.
;;; Note that any compiler worth its salt will integrate the definitions
;;; of my+ and my*.

(define (string->integer-aux string start neg? radix)
  (let ((length (string-length string))
        (grabsize (\#chars-in-bit-field radix *u-bits-per-fixnum*))
        (my* (lambda (x y) (cond ((fixnum? x) (fixnum-multiply-carefully x y))
                                 (else (b-f-multiply x y)))))
        (my+ (lambda (x y) (cond ((fixnum? x) (fixnum-add-carefully x y))
                                 (else (b-f-add x y))))))
    (let ((shift (fixnum-expt radix grabsize))
          (leftovers (fixnum-remainder (fx- length start) grabsize)))
      (let ((sum (string->fixnum string start leftovers radix)))
        (do ((sum (if neg? (fixnum-negate sum) sum)
                  (my+ (my* sum shift) 
                       (let ((x (string->fixnum string strpos grabsize radix)))
                         (if neg? (fixnum-negate x) x))))
             (strpos (fx+ start leftovers)
                     (fx+ strpos grabsize)))
            ((fx>= strpos length) sum))))))


;;; This belongs elsewhere

(define (string->fixnum string start count radix)
  (let ((limit (fx+ start count)))
    (do ((i start (fx+ i 1))
         (sum 0 (fx+ (fx* sum radix) (%char->digit (nthchar string i) radix))))
        ((fx>= i limit) sum))))


;;; Debugging utility:

;(define-syntax (bignum-pig x)
;  `(',*bignum-pig (lambda () ,x)))

(define (*bignum-pig x)
  (let ((b1 *bignum-cons-counter*)
        (b2 *bignum-cons-size-counter*))
    (let ((val (x))
          (a1 *bignum-cons-counter*)
          (a2 *bignum-cons-size-counter*))
      `(count = ,(fx- a1 b1) total = ,(fx- a2 b2) value = ,val))))

(set *bignums-print-nicely?* t)
