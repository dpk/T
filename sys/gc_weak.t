(herald gc_weak
  (env tsys (osys table))) ; TABLE is important

(define-constant gc-weak-set-list
  (object (lambda ()
            (process-global task/gc-weak-set-list))
    ((setter self)
     (lambda (k)
       (set (process-global task/gc-weak-set-list) k)))))

(define-constant gc-weak-alist-list
  (object (lambda ()
            (process-global task/gc-weak-alist-list))
    ((setter self)
     (lambda (k)
       (set (process-global task/gc-weak-alist-list) k)))))

(define-constant gc-weak-table-list
  (object (lambda ()
            (process-global task/gc-weak-table-list))
    ((setter self)
     (lambda (k)
       (set (process-global task/gc-weak-table-list) k)))))

(define (pre-gc-fix-weak-sets)
  (set (gc-weak-set-list) '()))

(define (pre-gc-fix-weak-alists)
  (set (gc-weak-alist-list) '()))

(define (pre-gc-fix-weak-tables)
  (set (gc-weak-table-list) '()))

;;; Update the weak sets.  If any of the pointed to objects were copied
;;; then put the new pointer into the weak set.

(define (post-gc-fix-weak-sets)
  (let ((sample (make-weak-set)))
    (clear-weak-semaphore sample)
    (iterate loop ((ptr (gc-weak-set-list)))
      (cond ((null? ptr) nil)
            (else
             (let ((next (extend-header ptr)))
               (set (extend-header ptr) (extend-header sample))
               (modify (extend-elt ptr 0) clean-weak-set-list)
               (loop next)))))))

;;; The pairs used in weak sets and weak alists need to be clobbered in case
;;; they are not in the heap.  It may be that this only needs to be done for
;;; weaks that are not in the heap, in which case there can be two versions
;;; of this code.

(define (clean-weak-set-list lst)
  (cond ((not (list? lst))
         (gc-write-line ";*** weak-set list is not a list")
         '())
        (else
         (really-clean-weak-set-list lst))))

(define (really-clean-weak-set-list lst)
  (iterate loop ((lst lst) (new '()))
    (cond ((null? lst)
           (reverse! new))
          ((atom? lst)
           (gc-write-line ";*** weak-set list is not a pair")
           (reverse! new))
          (else
           (receive (traced? new-loc)
                    (get-new-copy lst)
             (cond ((not traced?)
                    (receive (traced? new-loc)
                             (get-new-copy (car lst))
                      (let ((next (cdr lst)))
                        (set (car lst) 0)
                        (set (cdr lst) 0)
                        (loop next
                              (if traced? (cons new-loc new) new)))))
                   (else
                    (let ((copy (copy-list new-loc)))
                      (if (eq? new-loc lst)
                          (iterate loop ((lst lst))
                            (cond ((not (null? lst))
                                   (let ((n (cdr lst)))
                                     (set (car lst) 0)
                                     (set (cdr lst) 0)
                                     (loop n))))))
                      (append! new copy)))))))))

(define (post-gc-fix-weak-alists)
  (let ((sample (make-weak-alist)))
    (clear-weak-semaphore sample)
    (iterate loop ((ptr (gc-weak-alist-list)))
      (cond ((null? ptr) nil)
            (else
             (let ((next (extend-header ptr)))
               (set (extend-header ptr) (extend-header sample))
               (modify (extend-elt ptr 0) clean-weak-alist-vector)
               (loop next)))))))

;;; The values in weak alists (as opposed to the keys) are always fixnums and
;;; thus don't need to be traced.

(define (clean-weak-alist-vector vec)
  (cond ((points-to-initial-impure-memory? vec)
         (really-clean-weak-alist-vector vec))
        (else
         (receive (traced? new-loc)
                  (get-new-copy vec)
           (if traced?
               new-loc
               (really-clean-weak-alist-vector (copy-vector vec)))))))

(define (really-clean-weak-alist-vector vec)
  (let ((len (vector-length vec)))
    (iterate loop ((i 0) (ni 0))
      (cond ((fx>= i len)
             (maybe-shrink-weak-alist-vector vec ni))
            (else
             (receive (traced? new-loc)
                      (get-new-copy (vref vec i))
               (cond ((not traced?)
                      (loop (fx+ i 2) ni))
                     ((fx= i ni)
                      (set (vref vec i) new-loc)
                      (loop (fx+ i 2) (fx+ ni 2)))
                     (else
                      (set (vref vec ni) new-loc)
                      (set (vref vec (fx+ 1 ni)) (vref vec (fx+ 1 i)))
                      (loop (fx+ i 2) (fx+ ni 2))))))))))

;;; Weak tables

;;; This must iterate as fixing a weak-table may cause others to be copied.

(define (post-gc-fix-weak-tables)
  (iterate loop ((ptr (gc-weak-table-list)))
    (set (gc-weak-table-list) '())
    (cond ((null? ptr)
           nil)
          (else
           (post-gc-fix-weak-table-list ptr)
           (loop (gc-weak-table-list))))))

(define sample-weak-table (%make-weak-table))

(define (post-gc-fix-weak-table-list ptr)
  (iterate loop ((ptr ptr))
    (cond ((null? ptr) nil)
          (else
           (let ((next (extend-header ptr)))
             (post-gc-clean-table ptr weak-table-update)
             (loop next))))))

(define (post-gc-clean-table table update)
  (set (extend-header table) (extend-header sample-weak-table))
  (exchange (weak-table-vector table)
            (%table-vector (weak-table-table table)))
  (receive (traced? new-loc)
           (get-new-copy (%table-vector (weak-table-table table)))
    (cond (traced?
           (set (%table-vector (weak-table-table table)) new-loc))
          ((vector? (%table-vector (weak-table-table table)))
           (post-gc-clean-and-shrink-table (weak-table-table table) update))
          (else
           (gc-write-line "; *** nonvector in weak-table")
           (set (%table-vector (weak-table-table table)) empty-vec)))) 
  (clear-weak-semaphore table))

(define (weak-table-update key value)
  (receive (traced? new-loc)
           (get-new-copy key)
    (cond ((not traced?)
           (return nil nil))
          (else
           (return new-loc (gc-copy-object value))))))

;;;; Object hash table - a normal weak table except that it must retain pointers
;;;; to symbols.
;;;; Just call MOVE-OBJECT on OBJECT-HASH-TABLE and remove it from the
;;;; list?  ... No, it may copy others as well ...
;;;; This must be called after GC-FLIP.

(lset gc-clean-object-unhash-table? nil)

(define (object-unhash-pre-gc)
  (cond ((not (weak-semaphore-set? object-unhash-table))
         (set-weak-semaphore object-unhash-table)
         (set gc-clean-object-unhash-table? t)
         (exchange (weak-table-vector object-unhash-table)
                   (%table-vector (weak-table-table object-unhash-table)))
         (let ((new (gc-copy-extend object-unhash-table weak-table-slots)))
           (move-object (make-pointer new 0))))
        (else
         (set gc-clean-object-unhash-table? nil))))

(define (object-unhash-post-gc)
  (if gc-clean-object-unhash-table?
      (post-gc-clean-table object-unhash-table
                           object-unhash-table-update)))

;;; This should be elsewhere

(define-constant weak-table-slots 2)

;;; Check if the value has been copied.  The key is a fixnum and
;;; doesn't need to be copied.

(define (object-unhash-table-update key value)
  (receive (traced? new-loc)
           (get-new-copy value)
    (cond (traced?
           (return key new-loc))
          (else
           (return nil nil)))))
