(herald random
  (env tsys))

;;; (MAKE-RANDOM <seed>) takes a fixnum seed and returns a procedure of no
;;; arguments that returns a new pseudo-random number each time it is called.

(define (make-random seed)
  (receive (vec a b)
           (make-random-vector seed)
    (object (lambda ()
              (set a (randomize a 314159265 271828189))
              (set b (randomize b 271828189 314159265))
              (receive (index #f)
                       (%digit-multiply a 64)
                (swap (vref vec index) b)))
      ((print self stream)
       (format stream "#{Random (~a) ~D}" (object-hash self) seed)))))

(define-constant (randomize x mult ad)
  (receive (#f low)
           (%digit-multiply x mult)
    (fx+ low ad)))

(define (make-random-vector seed)
  (let ((vec (make-vector 64)))
    (do ((i 0 (fx+ i 1))
         (b seed (randomize b 271828189 314159265)))
        ((fx>= i 64)
         (return vec seed b))
      (set (vref vec i) b))))



