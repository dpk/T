(herald ratio (env tsys))

;;; copyright (c) 1983, 1984 yale university

(define (ratio p q)
  (let ((p (enforce integer? p))
        (q (enforce integer? q))
        (normal (lambda (p q)
                  (let ((g (gcd p q)))
                    (let ((p (quotient p g))
                          (q (quotient q g)))
                      (cond ((= q 1) p)
                            (else (object nil
                                       ((extended-number-type self)
                                        %%ratio-number-type)
                                       ((ratio? self) t)
                                       ((numerator self) p)
                                       ((denominator self) q)
                                       ((print self port)
                                        (format port "~s~c~s"
                                                p ratio-char q))))))))))
    ;; ... put p & q in lowest terms ...
    (cond ((= q 0) (error "attempt to divide by zero~%  (/ ~s ~s)" p q))
          ((< q 0) (normal (- 0 p) (- 0 q)))
          (else (normal p q)))))

(define-predicate ratio?)

(define (rational? x)
  (or (integer? x) (ratio? x)))

(define-operation (numerator x)
  (cond ((integer? x) x)
        (else (error "cannot take numerator of non-integer ~s" x))))

(define-operation (denominator x)
  (cond ((integer? x) 1)
        (else (error "cannot take denominator of non-integer ~s" x))))

(define (rational-parts-add n1 d1 n2 d2)
  (ratio (+ (* n1 d2) (* n2 d1))
         (* d1 d2)))

(define (rational-parts-subtract n1 d1 n2 d2)
  (ratio (- (* n1 d2) (* n2 d1))
         (* d1 d2)))

(define (rational-parts-multiply n1 d1 n2 d2)
  (ratio (* n1 n2) 
         (* d1 d2)))

(define-integrable (rational-parts-divide n1 d1 n2 d2)
  (rational-parts-multiply n1 d1 d2 n2))

(define (rational-parts-quotient n1 d1 n2 d2)
  (quotient (* n1 d2)
            (* n2 d1)))

;;; hacked for consistency

(define (rational-add r1 r2) 
  (rational-op rational-parts-add r1 r2))

(define (rational-subtract r1 r2) 
  (rational-op rational-parts-subtract r1 r2))

(define (rational-multiply r1 r2) 
  (rational-op rational-parts-multiply r1 r2))

(define (rational-divide r1 r2) 
  (rational-op rational-parts-divide r1 r2))

(define (rational-quotient r1 r2)
  (rational-op rational-parts-quotient r1 r2))

(define (rational-less? r1 r2)
  (< (* (numerator r1) (denominator r2)) (* (numerator r2) (denominator r1))))

(define (rational-equal? r1 r2)
  ;; assume normalization.
  (and (= (numerator r1)   (numerator r2))
       (= (denominator r2) (denominator r1))))

(define (rational-op proc r1 r2)
  (proc (numerator r1)
        (denominator r1)
        (numerator r2)
        (denominator r2)))

;;; coercers

(define (ratio->flonum r)
  (flonum-divide (integer->flonum (numerator r))
                 (integer->flonum (denominator r))))
