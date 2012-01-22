(herald weak
  (env tsys))

;;; No more 'simultaneous access on weak' errors.

;;; Weak sets and weak alists

;;; (MAKE-WEAK-SET . ELTS)
;;; (WEAK-SET-MEMBER? S X)
;;; (MAP-WEAK-SET F S)
;;; (WALK-WEAK-SET F S)
;;; (ADD-TO-WEAK-SET! S X)
;;; (REMOVE-FROM-WEAK-SET! S X)
;;; (WEAK-SET-POP! S)
;;; (WEAK-SET-EMPTY? S)
;;; (WEAK-SET->LIST S)

;;; (MAKE-WEAK-ALIST)
;;; (WEAK-ALIST-PUSH! S X Y)
;;; (WEAK-ALIST-MOVE FROM TO PREDICATE)
;;; (WEAK-ALIST-MERGE FROM TO)

;;; Weak Sets

(define (make-weak-set . elts)
  (let* ((elts (copy-list elts))   ; Safety...
         (wk (make-vector-extend header/weak-set 0 1)))
    (set (weak-set-elements wk) elts)
    (clear-weak-semaphore wk)
    wk))

(define-local-syntax (define-weak-set-proc spec set-var elts-var body)
  `(define ,spec
     (let* ((already-set? (test-and-set-semaphore ,set-var))
            (,elts-var (weak-set-elements ,set-var))
            (res ,body))
       (if (not already-set?) (clear-weak-semaphore ,set-var))
       res)))

(define-weak-set-proc (weak-set-member? set member) set elts
  (memq? member elts))

(define-weak-set-proc (weak-set-empty? set) set elts
  (null? elts))

(define-weak-set-proc (map-weak-set f set) set elts
  (map1 f elts))

(define-weak-set-proc (walk-weak-set f set) set elts
  (walk1 f elts))

(define-weak-set-proc (weak-set->list set) set elts
  (copy-list elts))

(define-local-syntax (define-weak-set-modifier spec set-var elts-var body)
  `(define ,spec
     (let* ((already-set? (test-and-set-semaphore ,set-var))
            (,elts-var (weak-set-elements ,set-var)))
       (set (weak-set-elements ,set-var) ,body)
       (if (not already-set?) (clear-weak-semaphore ,set-var))
       ,set-var)))

(define-weak-set-modifier (add-to-weak-set! set member) set elts
  (if (memq? member elts) elts (cons member elts)))

(define-weak-set-modifier (remove-from-weak-set! set member) set elts
  (delq! member elts))

(define (weak-set-pop! set)
  (let* ((already-set? (test-and-set-semaphore set))
         (elts (weak-set-elements set)))
    (cond ((null? elts)     
           (if (not already-set?) (clear-weak-semaphore set))
           nil)
          (else
           (let ((elt (car elts)))
             (set (weak-set-elements set) (cdr elts))
             (if (not already-set?) (clear-weak-semaphore set))
             elt)))))

(define-handler weak-set
  (object nil
;++    ((print self stream)
;++     (format stream "#{Weak-set~_~D}" (object-hash self)))
    ((crawl-exhibit self)
     (exhibit-standard-extend self 1 0 0))
    ((maybe-crawl-component self command)
     (cond ((and (fixnum? command)
                 (fx= command 0))
            (crawl-push (extend-pointer-elt self command)))
           (else nil)))
    ((print-type-string self) "Weak-set")))


;;; Weak alists

(define (make-weak-alist)
  (let ((wk (make-vector-extend header/weak-alist 0 1)))
    (set (weak-alist-elements wk) empty-weak-alist-vector)
    (clear-weak-semaphore wk)
    wk))

(define (weak-alist-push! alist key val)
  (let* ((already-set? (test-and-set-semaphore alist))
         (vec (weak-alist-elements alist))
         (len (vector-length vec))
         (new (get-weak-alist-vector (fx+ len 2))))
    (vector-replace new vec len)
    (return-weak-alist-vector vec)
    (set (vref new len) key)
    (set (vref new (fx+ 1 len)) val)
    (set (weak-alist-elements alist) new)
    (if (not already-set?) (clear-weak-semaphore alist))
    (return)))

;;; Move pairs from FROM to TO if (PREDICATE <key>) is true.

(define (weak-alist-move from to predicate)
  (let ((from-already-set? (test-and-set-semaphore from))
        (to-already-set? (test-and-set-semaphore to)))
    (let ((from-vec (weak-alist-elements from))
          (to-vec (weak-alist-elements to)))
      (let ((new-to (get-weak-alist-vector (fx+ (vector-length to-vec)
                                                (vector-length from-vec))))) 
        (vector-replace new-to to-vec (vector-length to-vec))
        (return-weak-alist-vector to-vec)
        (iterate loop ((fi 0) (nfi 0) (ti (vector-length to-vec)))
          (cond ((fx>= fi (vector-length from-vec))
                 (set (weak-alist-elements from)
                      (maybe-shrink-weak-alist-vector from-vec nfi))
                 (set (weak-alist-elements to)
                      (maybe-shrink-weak-alist-vector new-to ti)))
                ((predicate (vref from-vec fi))
                 (set (vref new-to ti) (vref from-vec fi))
                 (set (vref new-to (fx+ 1 ti)) (vref from-vec (fx+ 1 fi)))
                 (loop (fx+ fi 2) nfi (fx+ ti 2)))
                ((fx= fi nfi)
                 (loop (fx+ fi 2) (fx+ nfi 2) ti))
                (else
                 (set (vref from-vec nfi) (vref from-vec fi))
                 (set (vref from-vec (fx+ 1 nfi)) (vref from-vec (fx+ 1 fi)))
                 (loop (fx+ fi 2) (fx+ nfi 2) ti))))
        (if (not from-already-set?) (clear-weak-semaphore from))
        (if (not to-already-set?) (clear-weak-semaphore to))
        (return)))))

;;; Move all pairs from FROM to TO

(define (weak-alist-merge from to)
  (let ((from-already-set? (test-and-set-semaphore from))
        (to-already-set? (test-and-set-semaphore to)))
    (let ((from-vec (weak-alist-elements from))
          (to-vec (weak-alist-elements to)))
      (let ((new-vec (get-weak-alist-vector (fx+ (vector-length from-vec) 
                                                 (vector-length to-vec)))))
        (vector-replace new-vec to-vec (vector-length to-vec))
        (do ((i 0 (fx+ i 1)))
            ((fx>= i (vector-length from-vec)))
          (set (vref new-vec (fx+ i (vector-length to-vec)))
               (vref from-vec i)))
        (set (weak-alist-elements from) empty-weak-alist-vector)
        (set (weak-alist-elements to) new-vec)
        (return-weak-alist-vector from-vec)
        (return-weak-alist-vector to-vec)
        (if (not from-already-set?) (clear-weak-semaphore from))
        (if (not to-already-set?)   (clear-weak-semaphore to))
        (return)))))

(define (maybe-shrink-weak-alist-vector vec size)
  (if (fx> (vector-length vec) size)
      (let ((new (get-weak-alist-vector size)))
        (vector-replace new vec size)
        (return-weak-alist-vector vec)
        new)
      vec))

(define-handler weak-alist
  (object nil
;++    ((print self stream)
;++     (format stream "#{Weak-alist~_~D}" (object-hash self)))
    ((crawl-exhibit self)
     (exhibit-standard-extend self 1 0 0))
    ((maybe-crawl-component self command)
     (cond ((and (fixnum? command)
                 (fx= command 0))
            (crawl-push (extend-pointer-elt self command)))
           (else nil)))
    ((print-type-string self) "Weak-alist")))

;;; weak-alist pools

(define empty-weak-alist-vector (make-vector 0))

;(define impure-weak-alist-pool-vec
;  (vector-fill (make-vector 256) nil))  ;;; bigger than number of modules
;                                        ;;; linked or suspended
;
;(define impure-weak-alist-pool
;  (object (lambda (x)
;            (vref impure-weak-alist-pool-vec x))
;    ((setter self)
;     (lambda (x val)
;       (set (vref impure-weak-alist-pool-vec x) val)))
;    ((identification self) 'impure-weak-alist-pool)))
;
;(define impure-weak-alist-pool-vec
;  (make-vector 256))
;
;(define (heap-weak-alist-pool x)
;  (vref heap-weak-alist-pool-vec x))
;
;(define (initialize-weak-alist-pool) 
;  (do ((i 0 (fx+ i 1)))
;      ((fx>= i 256))
;    (set (vref impure-weak-alist-pool i)
;         (make-weak-alist-vector-pool i)))
;  (return))
;
;(define (make-weak-alist-vector-pool size)
;  (make-pool `(weak-alist-vector-pool ,size)
;             (lambda () (make-vector (fx* size 2)))
;             1
;             vector?))
;
;(define (get-weak-alist-vector size)
;  (cond ((fx= 0 size)
;         empty-weak-alist-vector)
;        ((impure-weak-alist-pool (fx/ size 2))
;         => (lambda (vec)
;              (set (impure-weak-alist-pool (fx/ size 2))
;                   (vref vec 0))
;              vec))
;        (else
;         (obtain-from-pool (heap-weak-alist-pool (fx/ size 2))))))
;
;(define (return-weak-alist-vector vec)
;  (let ((len (vector-length vec)))
;    (cond ((fx= 0 len))
;          ((points-to-initial-impure-memory? vec)
;           (vector-fill vec 0)
;           (modify (impure-weak-alist-pool (fx/ len 2))
;                   (lambda (old)
;                     (set (vref vec 0) old)
;                     vec)))
;          (else
;           (return-to-pool (heap-weak-alist-pool (fx/ len 2)) vec))))
;    (return)))

(define (initialize-weak-alist-pool) 
  (return))

(define (get-weak-alist-vector size)
  (cond ((fx= 0 size)
         empty-weak-alist-vector)
        (else
         (make-vector size))))

(define (return-weak-alist-vector vec)
  (let ((len (vector-length vec)))
    (cond ((fx= 0 len))
          (else
           (vector-fill vec 0)))
    (return)))

;;; Weak Cells

(define (make-weak-cell contents)
  (let ((wk (make-vector-extend header/weak-cell 0 1)))
    (set (weak-cell-contents wk) contents)
    wk))

(define-handler weak-cell
  (object nil
;++    ((print self stream)
;++     (format stream "#{Weak-cell~_~D}" (object-hash self)))
    ((crawl-exhibit self)
     (exhibit-standard-extend self 1 0 0))
    ((maybe-crawl-component self command)
     (cond ((and (fixnum? command)
                 (fx= command 0))
            (crawl-push (extend-pointer-elt self command)))
           (else nil)))
    ((print-type-string self) "Weak-cell")))


;;; A population is like a list except that its members go away
;;; if they aren't copied out by the GC.  That is, at any point
;;; when the only way to get to an object is via a population, then
;;; the system is free to delete the object from the population,
;;; thus making it completely inaccessible.

;;; The only valid operations on populations are:
;;; ADD-TO-POPULATION      - add an object to a population.
;;; REMOVE-FROM-POPULATION - remove an object from a population.
;;; POPULATION->LIST       - make a list of the members of a population.
;;;     (This has the effect of making the population's members
;;;     accessible via the returned list.)

(define (make-population id)
  (ignore id)
  (make-weak-set))

(define population? weak-set?)

(define add-to-population add-to-weak-set!)

(define remove-from-population remove-from-weak-set!)

(define population->list weak-set->list)

;;; Auxiliary.  This is provided because it might for whatever reason
;;; be more efficient than (WALK PROC (POPULATION->LIST P)); e.g.
;;; there might be fewer pointers around if PROC does a GC.

;++ shouldn't it be (WALK-POPULATION PROC P)
(define (walk-population p proc)
  (walk-weak-set proc p))
