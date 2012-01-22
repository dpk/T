(herald big_arith
  (env tsys (osys fixnum) bignum))

;;; Addition:

;;; Places the modular sum of u, v, and carry, in element i of
;;; vector destv.  Returns the new carry.

(define-integrable (add-step u v destv i carry)
  (receive (sum carry)
           (%digit-add u v carry)
    (set (bignum-digit destv i) sum)
    carry))

;;; add-magnitudes takes two bignums, adds their magnitudes, and returns
;;; the magnitude of their sum.

(define (add-magnitudes u v)
  (cond ((fx< (bignum-length u) (bignum-length v))
         (really-add-magnitudes v u))
        (else
         (really-add-magnitudes u v))))

;;; Assumes that first number is longer than second number.

(define (really-add-magnitudes u v)
  (let* ((u-size (bignum-length u))
         (v-size (bignum-length v))        ; u-size >= v-size
         ;; The following test tries to predict whether there will be carryout.
         (lc (if (or (%digit-greater? (bignum-digit u (fx- u-size 1))
                                      *half-max-hyperdigit*)
                     (and (fx= u-size v-size)
                          (%digit-greater? (bignum-digit v (fx- v-size 1))
                                           *half-max-hyperdigit*)))
                 (fx+ 1 u-size)
                 u-size))
         (sum (create-bignum lc)))
    (do ((carry 0 (add-step (bignum-digit u i) (bignum-digit v i) sum i carry))
         (i 0 (fx+ 1 i)))
        ((fx= i v-size)
         ;; Propagate carry
         (do ((carry carry
                     (add-step (bignum-digit u i) 0 sum i carry))
              (i i (fx+ 1 i)))
             ((or (fx= i u-size) (fx= carry 0))
              ;; Copy remaining digits from u
              (do ((i i (fx+ 1 i)))
                  ((fx= i u-size)
                   (cond ((fx= 0 carry)
                          (set-bignum-length! sum u-size))
                         (else
                          (set (bignum-digit sum i) 1)))
                   sum)
                (set (bignum-digit sum i) (bignum-digit u i)))))))))

;;; Subtraction:

(define-integrable (subtract-step u v destv i carry)
  (receive (diff carry)
           (%digit-subtract u v carry)
    (set (bignum-digit destv i) diff)
    carry))

;;; Returns the difference of the magnitudes of two bignums.
;;; Assumes that first number is at least as long as than second.

(define (subtract-magnitudes u v)
  (let* ((la (bignum-length u))
         (lb (bignum-length v))
         (diff (create-bignum la)))
    (do ((carry 0 (subtract-step (bignum-digit u i)
                                 (bignum-digit v i)
                                 diff i carry))
         (i 0 (fx+ 1 i)))
        ((fx= i lb)
         (do ((carry carry (subtract-step (bignum-digit u i) 0 diff i carry))
              (i i (fx+ 1 i)))
             ((or (fx= la i) (fx= carry 0))
              (do ((i i (fx+ 1 i)))
                  ((fx= la i)
                   (bignum-trim! diff))
                (set (bignum-digit diff i) (bignum-digit u i)))))))))


;;; Multiplication:

;;; Multiplies a bignum by a scalar, and adds the product into an
;;; accumulator beginning at the specified start index.
;;; Assumes that accum is at least of length v-size + start + 1.

(define (multiply-step scalar v v-size accum start)
  (do ((carry 0 (receive (d1 d0)
                         (%digit-multiply scalar (bignum-digit v j))
                  (add-step (bignum-digit accum (fx+ 1 i))
                            (fx+ d1 carry)  ; Note: d1 < *max-hyperdigit*
                            accum
                            (fx+ 1 i)
                            (add-step (bignum-digit accum i) d0 accum i 0))))
       (i start (fx+ 1 i))
       (j 0 (fx+ 1 j)))
      ((fx>= j v-size)
       (cond ((fxn= carry 0)
              (add-step (bignum-digit accum (fx+ 1 j))
                        0 accum (fx+ 1 j) carry)))
       accum)))

;;; Similar in action to ADD-MAGNITUDES only with product.

(define (multiply-magnitudes u v)
  (let* ((u-size (bignum-length u))
         (v-size (bignum-length v))
         (l-product (fx+ (fx+ u-size v-size) 1))       ;Why + 1?
         (product (create-bignum l-product)))
    (do ((i 0 (fx+ 1 i)))
        ((fx= i u-size) (bignum-trim! product))
      (multiply-step (bignum-digit u i) v v-size product i))))

;;; Division:

;;; The New Division Routine by J.R.
;;; (In a division u/v, u is the dividend and v is the divisor.)

(define (div2-magnitudes u v)
  (let* ((m+n (bignum-length u))
         (n (bignum-length v))
         (d (fx- *bits-per-hyperdigit*
                 (fixnum-howlong (bignum-digit v (fx- n 1)))))
         (new-u (make-and-replace-bignum (fx+ m+n 1) u 0 0 m+n)))
    ;; Normalize
    (cond ((fx> d 0)
           (really-div2-magnitudes (bignum-ashl! new-u d)
                                   (bignum-ashl v d)
                                   m+n n d))
          (else
           (really-div2-magnitudes new-u v m+n n 0)))))

;;; Knuth equivalences:
;;;   u[j]     ->   (bignum-digit u m+n-j)
;;;   u[j+1]   ->   (bignum-digit u (- m+n-j 1))
;;;   u[j+n]   ->   (bignum-digit u (- m-j))

(define (really-div2-magnitudes u v m+n n d)
  (let* ((v1 (bignum-digit v (fx- n 1)))
         (v2 (if (fx> n 1) (bignum-digit v (fx- n 2)) 0))
         (m (fx- m+n n))
         (q (create-bignum (fx+ m 1))))
    (do ((m-j   m   (fx- m-j   1))
         (m+n-j m+n (fx- m+n-j 1)))
        ((fx< m-j 0)
         (return (bignum-trim! q)
                 (bignum-trim! (bignum-ashr! u d))))

      ;; Calculate one digit of the quotient.
      (let ((q^ (compute-q^ (bignum-digit u m+n-j)
                            (if (fx> m+n-j 0) (bignum-digit u (fx- m+n-j 1)) 0)
                            (if (fx> m+n-j 1) (bignum-digit u (fx- m+n-j 2)) 0)
                            v1
                            v2)))
        (set (bignum-digit q m-j) q^)

        ;; Multiply and subtract: (uj...uj+n)  -:=  q^ * (v1...vn)
        (iterate loop ((prev 0)
                       (borrow 0)
                       (i m-j)
                       (k 0))
          (cond ((fx< k n)
                 (receive (d1 d0)
                          (%digit-multiply q^ (bignum-digit v k))
                   (receive (sum carry)
                            (%digit-add d0 prev 0)
                     (let ((d1 (fx+ d1 carry)))
                       (receive (diff borrow)
                                (%digit-subtract (bignum-digit u i) sum borrow)
                         (set (bignum-digit u i) diff)
                         (loop d1 borrow (fx+ i 1) (fx+ k 1)))))))
                (else
                 (receive (diff borrow)
                          (%digit-subtract (bignum-digit u i) prev borrow)
                   (set (bignum-digit u i) diff)
                   (cond ((fxn= borrow 0)
                          (add-back u v q q^ n m-j))
                         ((fxn= diff 0)
                          (error "inconsistency in bignum division!~%  ~S"
                                 `(div ,u ,v)))
                         )))))))))

;;; In the loop, q^ starts out being 0, 1, or 2 larger than the actual
;;; first digit of the quotient of the bignums u and v.
;;; The loop may decrement q^, eliminating all cases where q^ is
;;; two larger, and most cases where it is one larger.

;;; The initial guess for q^ is obtained by dividing u's first two digits
;;; by v's first digit.
;;; r^ is initially the remainder of the division, but as q^ is
;;; decremented, r^ maintains as its value the actual difference between
;;; the dividend  {u[j] u[j+1]}  and the product  q^*v[1].
;;; We stop pruning q^ as soon as its product with the second digit
;;; of v, exceeds r^ adjoined with the third digit of u.

(define (compute-q^ uj uj+1 uj+2 v1 v2)
  (labels (((loop q^ r^)
            ;; Test to see whether, in Knuth's notation,
            ;;  q^*v[2] > r^*b + u[j+2]
            ;;     where r^ = u[j]*b + u[j+1] - q^*v[1]
            ;; We use the same tricks he does in his MIX code.
            (receive (a1 a0)
                     (%digit-multiply q^ v2)
              ;; {a1 a0}  =  v[2]*q^
              (cond ((and (not (%digit-greater? a1 r^))
                          (or (fxn= a1 r^)
                              (not (%digit-greater? a0 uj+2))))
                     q^)
                    (else
                     (test (fx- q^ 1) r^)))))
           ((test q^ r^)
            ;; Adjust r^.  q^ must get no smaller if r^ overflows here.
            (receive (sum carry)
                     (%digit-add r^ v1 0)
              (if (fxn= carry 0)
                  q^
                  (loop q^ sum)))))
    ;; uj <= v1
    (cond ((fx= uj v1)
           ;; Note that in this case, uj+1 <= v2 also.  E.g. in
           ;; (/ 8123,4567,... 8123,xxxx,...) -> q= FFFF, r= C68A (=8123+4567)
           ;; we know that xxxx >= 4567.
           (test *max-hyperdigit* uj+1))
          (else
           (receive (q^ r^)
                    (%digit-divide uj uj+1 v1)
             (loop q^ r^))))))

;;; We come here if compute-q^ screwed up and gave us a bogus guess for
;;; the quotient digit.  The probability of this happening is about
;;;  2 / b  where b = (1+ *max-hyperdigit*).

(define (add-back u v q q^ n m-j)
  (*let-us-know* u v q^)
  (set (bignum-digit q m-j) (fx- q^ 1))
  (iterate loop ((carry 0)
                 (i m-j)
                 (k 0))
    (cond ((fx< k n)
           (receive (sum carry)
                    (%digit-add (bignum-digit u i) (bignum-digit v k) carry)
             (set (bignum-digit u i) sum)
             (loop carry (fx+ i 1) (fx+ k 1))))
          (else
           (set (bignum-digit u i) 0)))))

(lset *let-us-know*
  (lambda (u v q^)
    (if (experimental?)
        (format (terminal-output)
                '("~&;Please send the following numbers to the T implementors.~%"
                  ";This is not an error.~%; q^ = ~s~%; u  = ~s~%; v  = ~s~%")
                q^ u v))
    (set *let-us-know* false)))

;;; This depends on (>= *bits-per-hyperdigit* *bits-per-fixnum*)
;;; => T and, in %B-F-DIV2, on (FIXNUM-ABS most-negative-fixnum)
;;; => most-negative-fixnum which is true in T2.9 and T3.0 and should
;;; be true in general.

(define (b-f-multiply u v)
  (let* ((u-size (bignum-length u))
         (product (create-bignum (fx+ 1 u-size))))
    (multiply-step (fixnum-abs v) u u-size product 0)
    (set-bignum-sign! product
                      (if (fx= (bignum-sign u)
                               (if (fx< v 0) -1 1))
                          1
                          -1))
    (normalize-integer (bignum-trim! product))))

;;; Division of a bignum by a fixnum.

(define (b-f-div2 u v)
  (cond ((fx= v 0)
         (error "division by zero~%  (DIV ~s ~s)" u v))
        (else
         (%b-f-div2 u v t))))

;;; An unnormalized result is required by the printer and INTEGER->POINTER.

(define-integrable (b-f-div2-unnormalized u v)
  (%b-f-div2 u v nil))

(define (%b-f-div2 u v normalize?)
  (let* ((v-abs (fixnum-abs v))
         (d (fx- *bits-per-hyperdigit*
                 (fixnum-howlong v-abs)))
         (sign (if (fx= (bignum-sign u)
                        (if (fx< v 0) -1 1))
                   1
                   -1)))
    (cond ((fx= d 0)
           (b-f-div2-aux u v-abs normalize? d sign))
          (else
           (b-f-div2-aux (bignum-ashl u d)
                         (fixnum-ashl v-abs d)
                         normalize?
                         d
                         sign)))))

(define (b-f-div2-aux u v normalize? d sign)
;  (format t "~&0> ~D ~D ~A ~D ~D~%" u v normalize? d sign)
  (let* ((u-size (bignum-length u))
         (q (create-bignum u-size)))
;    (format t "~&2> ~D ~D ~%" u-size q)
    (iterate loop ((i (fx- u-size 1))
                   (r 0))
      (cond ((fx< i 0)
             ;; Last time through, r is the remainder.
;             (format t "~&2> ~D ~%" q)
             (set-bignum-sign! q sign)
;             (format t "~&3> ~D ~%" q)
             (bignum-trim! q)
;             (format t "~&4> ~D ~%" q)
             (return (if normalize? (normalize-integer q) q)
                     (let ((r (fixnum-lshr r d)))
                       (if (bignum-positive? u) r (- 0 r)))))
            (else
             (let ((word0 (bignum-digit u i)))
               (receive (qi r)
                        (%digit-divide r word0 v)
                 (set (bignum-digit q i) qi)
                 (loop (fx- i 1) r))))))))

;;; Returns a fixnum whose sign is the same as (- |u| |v|).

(define (bignum-compare-magnitudes u v)
  (let ((u-size (bignum-length u))
        (v-size (bignum-length v)))
    (cond ((fxn= u-size v-size)
           (fx- u-size v-size))
          (else
           (iterate loop ((i (fx- u-size 1)))
             (receive (diff carry)
                      (%digit-subtract (bignum-digit u i) (bignum-digit v i) 0)
               (cond ((and (fxn= i 0) (fx= diff 0) (fx= carry 0))
                      (loop (fx- i 1)))
                     ((fxn= carry 0) -1)
                     ((fxn= diff 0) 1)
                     (else 0))))))))

;;; Bitfields

;;; only works on positive bignums
(define (bignum-bit-field bn start count)
  (let ((end (fx- (fx+ start count) 1)) )
    (let ((first-hd (fx/ start *bits-per-hyperdigit*))   
          (last-hd  (fx/ end *bits-per-hyperdigit*))
          (bits-in-last (fx+ (fx-rem end *bits-per-hyperdigit*) 1))
          (start-in-hd  (fx-rem start *bits-per-hyperdigit*)) )
      (cond ((or (not (bignum? bn))
                 (not (bignum-positive? bn))
                 (fx< start 0)
                 (fx< count 0)
                 (fx> first-hd (fx+ (bignum-length bn) 1)) 
                 (fx> last-hd  (fx+ (bignum-length bn) 1)) )
             (error "inconsistent arguments in~%  (bignum-bit-field ~s ~s ~s)"
                    bn start count))
            ((fx= first-hd last-hd) 
             (fixnum-bit-field (bignum-digit bn first-hd) start-in-hd count))
            (else
             (let* ((new-last-hd (fx- last-hd first-hd))
                    (new-bn (create-bignum (fx+ new-last-hd 1))))
               (do ((i 0 (fx+ i 1)))
                   ((fx> i new-last-hd)
                    (modify (bignum-digit new-bn new-last-hd)
                            (lambda (hd) (fixnum-bit-field hd 0 bits-in-last)))
                    (set-bignum-sign! new-bn 1)
                    (normalize-integer 
                     (bignum-trim! (bignum-ashr! new-bn start-in-hd)))
                    )
                 (set (bignum-digit new-bn i) (bignum-digit bn (fx+ first-hd i)))
                 )
               ))))))


;;; Arithmetic shift entry points:

;;; Left shift bignum.

(define (bignum-ashl num amount)
  (receive (q r)
           (%digit-divide 0 amount *bits-per-hyperdigit*)
    (let* ((old-size (bignum-length num))
           (new-size (fx+ (fx+ old-size q) 1)))
      (bignum-trim!
       (bignum-ashl! (make-and-replace-bignum new-size num q 0 old-size)
                     r)))))

;;; Destructive shift.  0 <= amount < *bits-per-hyperdigit*

(define (bignum-ashl! num amount)
  (let ((j (fx- (bignum-length num) 1))
        (z (fx- *bits-per-hyperdigit* amount)))
    (iterate loop ((prev (bignum-digit num j))
                   (j j))
      (let ((high (fixnum-ashl prev amount)))
        (cond ((fx<= j 0)
               (set (bignum-digit num j) high)
               num)
              (else
               (let ((foo (bignum-digit num (fx- j 1))))
                 (set (bignum-digit num j) (fx+ high (fixnum-lshr foo z)))
                 (loop foo (fx- j 1)))))))))

;;; Right shift bignum yielding bignum.

(define (bignum-ashr src amount)
  (receive (q r)
           (%digit-divide 0 amount *bits-per-hyperdigit*)
    (let* ((old-size (bignum-length src))
           (new-size (fx- old-size q)))
      (bignum-trim!
       (bignum-ashr! (make-and-replace-bignum new-size src 0 q new-size)
                     r)))))

;;; Destructive right shift.

(define (bignum-ashr! num amount)
  (let* ((end (fx- (bignum-length num) 1))
         (z (fx- *bits-per-hyperdigit* amount)))
    (iterate loop ((prev (bignum-digit num 0))
                   (j 0))
      (let ((high (fixnum-lshr prev amount)))
        (cond ((fx>= j end)
               (set (bignum-digit num j) high)
               num)
              (else
               (let ((foo (bignum-digit num (fx+ j 1))))
                 (set (bignum-digit num j)
                      (fx+ high (fixnum-ashl foo z)))
                 (loop foo (fx+ j 1)))))))))
