(herald table_util
  (env tsys))

;;;                      UTILITIES
;;;============================================================================

;;; Do (PROC <key> <value>) for every (<key> <value>) in TABLE.

(define (table-walk table proc)
  (let ((table (enforce %table? table)))
    (let* ((vec (%table-vector table))
           (len (%table-next table)))
      (do ((i 0 (fx+ i 2)))
          ((fx>= i len))
        (cond ((vref vec i)
               => (lambda (v)
                    (proc (vref vec (fx+ i 1)) v)))))
      (return))))

(define-integrable (walk-table proc table)
    (table-walk table proc))

;;; This returns the first KEY and VALUE for which (PRED KEY VALUE) => true.

(define (find-table-entry table pred)
  (let* ((table (enforce %table? table))
         (vec (%table-vector table))
         (len (%table-next table)))
    (iterate loop ((i 0))
      (cond ((fx>= i len)
             (return nil nil))
            ((vref vec i)
             => (lambda (v)
                  (if (pred (vref vec (fx+ i 1)) v)
                      (return (vref vec (fx+ i 1)) v)
                      (loop (fx+ i 2)))))
            (else
             (loop (fx+ i 2)))))))

;;; Copy a table.  This gets its %table from the pool.

(define (copy-table table id . copy-proc)
  (let* ((table (enforce %table? table))
         (vec (%table-vector table))
         (len (vector-length vec))
         (copy-proc (if (null? copy-proc) identity (car copy-proc)))
         (new (copy-structure! (obtain-from-pool *table-pool*) table)))
    (let ((new-vec (if (fx= len '2) 
                       empty-vec
                       (obtain-from-pool (table-vector-pool len)))))
      (set (%table-id     new) id)
      (set (%table-vector new) new-vec)
      (cond ((eq? copy-proc identity)
             (vector-replace new-vec vec (vector-length vec)))
            (else
             (iterate loop ((i 0))
               (cond ((fx>= i len) nil)
                     ((vref vec i)
                      => (lambda (v)
                           (set (vref new-vec i) (copy-proc v))
                           (set (vref new-vec (fx+ 1 i)) (vref vec (fx+ i 1)))
                           (loop (fx+ i 2))))
                     (else
                      (set (vref new-vec i) nil)
                      (set (vref new-vec (fx+ i 1)) (vref vec (fx+ i 1)))
                      (loop (fx+ i 2)))))))
      new)))

;;; This stuff is used by the post-gc-hook for weak tables.

;;; Same as CLEAN-AND-SHRINK-TABLE except the vector is not reused

(define (clean-and-shrink-table table update)
  (really-clean-and-shrink-table table update t))

(define (post-gc-clean-and-shrink-table table update)
  (really-clean-and-shrink-table table update nil))

(define (really-clean-and-shrink-table table update recycle?)
  (let* ((table (enforce %table? table))
         (new-count (clean-table-vector! (%table-vector table) update)))
    (set (%table-count table) new-count)
    (if recycle?
        (table-rehash table new-count)
        (really-table-rehash table new-count))
    table))

(define (clean-table-vector! vec update)
  (let ((len (vector-length vec)))
    (iterate loop ((i 0) (count 0))
      (cond ((fx>= i len)
             count)
            (else
             (let ((v (vref vec i)))
               (receive (k v)
                        (if (not v)
                            (return nil nil)
                            (receive (k v)
                                     (update (vref vec (fx+ i 1)) v)
                              (if v (return k v) (return nil nil))))
                 (set (vref vec i) v)
                 (set (vref vec (fx+ i 1)) k)
                 (loop (fx+ i 2) (if v (fx+ 1 count) count)))))))))




