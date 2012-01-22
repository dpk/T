(herald table_entry
  (env tsys (osys table)))

;;;                           Hashing
;;;============================================================================

;;; This procedure attempts to improve on the user supplied hashing procedure.
;;; Testing show that it slightly improves DESCRIPTOR-HASH.

(define-integrable (second-hash n)
  (let ((low (fixnum-logand n 255)))
    (fixnum-logand (fixnum-logxor (fixnum-logxor low (fixnum-ashl low 3))
                       (fixnum-logxor (fixnum-ashr n 7)
                           (fixnum-logxor (fixnum-ashl n 6)
                                          (fixnum-ashr n 16))))
                   most-positive-fixnum)))

(define (descriptor-hash p)
  (descriptor->fixnum p))

;;; Rehash and cut with MASK

(define-integrable (really-table-hash key hash mask)
  (fixnum-logand (fixnum-ashl (second-hash (hash key))
                              3)
                 mask))

;;; Keep hashing until HASH and TABLE are of the same GC generation.  Tables
;;; with mask 0 have only a single bucket and thus do not need hashing.

(define (table-hash table key)
  (cond ((fx= (%table-mask table) 0)
         0)
        ((not (%table-gc-stamp table))
         (really-table-hash key (%table-hash table) (%table-mask table)))
        (else
         (iterate loop ()
           (let* ((hash (really-table-hash key
                                           (%table-hash table)
                                           (%table-mask table)))
                  (stamp (%table-gc-stamp table)))
             (cond ((neq? (gc-stamp) stamp)
                    (table-rehash table (%table-count table))
                    (loop))
                   (else
                    hash)))))))

;;;                     TABLE-ENTRY and friends
;;;===========================================================================

(define table-entry
  (object (lambda (table key)
            (let* ((table (enforce %table? table))
                   (hash (table-hash table key))
                   (comparator (%table-compare table))
                   (vec (%table-vector table)))
              (receive (found? index)
                       (find-table-index key vec hash comparator)
                (if found? (vref vec index) nil))))
    ((setter self) %set-table-entry)))

;;; Speed hack for common case - get EQ? tests generated inline (at some cost
;;; in code size).

(define-integrable (find-table-index key vec hash comparator)
  (if (eq? comparator eq?)
      (really-find-table-index key vec hash eq?)
      (really-find-table-index key vec hash comparator)))

;;; Look for KEY in the hash bucket of VEC that starts at HASH.  Returns a
;;; flag and an index.  If the flag is #T then the index is the index of the
;;; key in the vector.  If the flag is #F then the key was not found and the
;;; index is the index of the end of the bucket.

(define-integrable (really-find-table-index key vec hash comparator)
  (iterate loop ((i hash))
    (cond ((vref vec i)
           (if (comparator key (vref vec (fx+ i 1)))
               (return t i)
               (loop (fx+ i 2))))
          ((vref vec (fx+ i 1))
           => loop)
          (else
           (return nil i)))))

;;;                     SET-TABLE-ENTRY and friends
;;;===========================================================================

;;; If the table is full, rehash with a slightly larger size.  If it is still
;;; full, there are hashing problems so go a lot bigger.

(define (%set-table-entry table key value)
  (let ((table (enforce %table? table)))
    (cond ((not value)
           (if (%remove-table-entry key table)
               (modify (%table-count table)
                       (lambda (x) (fx- x 1)))))
          (else
           (iterate loop ((new-size (fx+ 1 (%table-count table))))
             (cond ((not (really-set-table-entry key value table))
                    (table-rehash table new-size)
                    (loop (fx-ashl new-size 1)))))))
    value))

;;; Add KEY and VALUE to TABLE, incrementing the count.

(define (set-table-vec table vec index key value)
  (modify (%table-count table) (lambda (c) (fx+ c 1)))
  (set (vref vec index) value)
  (set (vref vec (fx+ index 1)) key)
  t)

;;; Try to set the value of KEY in TABLE to be VALUE.  Returns #t is successful
;;; and #f if the table is too full.
;;;
;;; FIND-TABLE-INDEX looks for KEY in the table.  If it is already there the
;;; value is changed.  Otherwise there are three possibilities:
;;;   The table has only one bucket:
;;;      If it's full return #F, otherwise add VALUE at the end.
;;;   There is room in the current bucket:
;;;      Add VALUE at the end.
;;;   Otherwise:
;;;      If there are no free overflow buckets return #F, otherwise
;;;      allocate an overflow bucket and put put VALUE in it.
;;;
;;; This is integrable because it is only called in one place.

(define-integrable (really-set-table-entry key value table)
  (let* ((hash (table-hash table key))
         (comparator (%table-compare table))
         (vec (%table-vector table)))
    (receive (found? index)
             (find-table-index key vec hash comparator)
      (cond (found?
             (set (vref vec index) value)
             t)
            ((fx= 0 (%table-mask table))
             (if (fx>= (fx+ index 2) (vector-length vec))
                 nil
                 (set-table-vec table vec index key value)))
            ((fx> 6 (fixnum-logand index 7))
             (set-table-vec table vec index key value))
            (else
             (let ((next (%table-next table)))
               (cond ((fx>= next (vector-length vec))
                      nil)
                     (else
                      (set (%table-next table) (fx+ next 8))
                      (set (vref vec (fx+ 1 index)) next)
                      (set-table-vec table vec next key value)))))))))

;;; Speed hack for common case - get EQ? tests generated inline (at some cost
;;; in code size).
;;;
;;; This is integrable because it is only called in one place.

(define-integrable (%remove-table-entry key table)
  (let* ((i (table-hash table key))
         (comparator (%table-compare table))
         (vec (%table-vector table)))
    (if (eq? comparator eq?)
        (really-remove-table-entry vec key i eq?)
        (really-remove-table-entry vec key i comparator))))

;;; Look for KEY and remove it if you find it.

(define-integrable (really-remove-table-entry vec key hash comparator)
  (iterate loop ((i hash))
    (cond ((vref vec i)
           (if (comparator key (vref vec (fx+ i 1)))
               (remove-entry vec i)
               (loop (fx+ i 2))))
          ((vref vec (fx+ i 1))
           => loop)
          (else nil))))     ; must return NIL here

;;; Remove the vector entry at I and slide the rest of the bucket down one.
;;; This is the real cost of using vectors instead of lists.

(define (remove-entry vec i)
  (iterate loop ((i i))
    (set (vref vec i) (vref vec (fx+ i 2)))
    (set (vref vec (fx+ i 1)) (vref vec (fx+ i 3)))
    (cond ((vref vec i)
           (loop (fx+ i 2)))
          ((vref vec (fx+ i 1))
           => (lambda (j)
                (set (vref vec i) (vref vec j))
                (set (vref vec (fx+ i 1)) (vref vec (fx+ j 1)))
                (loop j)))
          (else t))))       ; must return T here


;;;                           TABLE-REHASH
;;;============================================================================

;;;   Rehash TABLE because of a GC or a change in size.  Get a new vector, move
;;; all the values into it, and release the old vector.  Some calls to this
;;; do not want the vector to be reused.

(define (table-rehash table new-size)
  (release-table-vector (really-table-rehash table new-size))
  table)

(define (really-table-rehash table new-size)
  (let ((vec (%table-vector table))
        (old-mask (%table-mask table)))
    (receive (new mask next)
             (get-table-vector new-size)
      (set (%table-mask   table) mask)
      (set (%table-vector table) new)
      (if (%table-gc-stamp table)                    ; This must be before any
          (set (%table-gc-stamp table) (gc-stamp)))  ; hashing.
      (cond ((fx= 0 mask)
             (simple-rehash-values vec new (%table-next table))
             (set (%table-next table) next))
            ((rehash-values vec
                            new
                            (%table-hash table)
                            mask
                            next
                            (%table-next table))
             => (lambda (next)
                  (set (%table-next table) next)))
            (else             ; This is extremely unlikely
             (set (%table-mask table) old-mask)
             (set (%table-vector table) vec)
             (really-table-rehash table (fx* new-size 2)))))
    vec))

;;;   Move all the values from OLD to NEW.  MASK is the mask of NEW, NEXT is
;;; the first overflow bucket in NEW, LEN how far we have to go in OLD.  Most
;;; of this procedure is a version of REALLY-FIND-TABLE-INDEX and
;;; REALLY-SET-TABLE-ENTRY.

(define (rehash-values old new hasher mask next len)
  (iterate i-loop ((i 0) (next next))
    (cond ((fx>= i len)
           next)
          ((not (vref old i))
           (i-loop (fx+ i 2) next))
          (else
           (let ((k (vref old (fx+ i 1))))
             (iterate j-loop ((j (really-table-hash k hasher mask)))
               (cond ((vref new j)
                      (j-loop (fx+ 2 j)))
                     ((vref new (fx+ 1 j))
                      => j-loop)
                     ((fx> 6 (fixnum-logand j 7))
                      (set (vref new j) (vref old i))
                      (set (vref new (fx+ 1 j)) k)
                      (i-loop (fx+ i 2) next))
                     ((fx>= next (vector-length new))
                      nil)
                     (else
                      (set (vref new (fx+ 1 j)) next)
                      (set (vref new next) (vref old i))
                      (set (vref new (fx+ 1 next)) k)
                      (i-loop (fx+ i 2) (fx+ next 8))))))))))

(define (simple-rehash-values old new len)
  (iterate loop ((i 0) (next 0))
    (cond ((fx>= i len)
           (return))
          ((not (vref old i))
           (loop (fx+ i 2) next))
          (else
           (set (vref new next) (vref old i))
           (set (vref new (fx+ 1 next)) (vref old (fx+ i 1)))
           (loop (fx+ i 2) (fx+ 2 next))))))


