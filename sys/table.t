(herald table 
  (env tsys))

;;; What a lot of code!!

;;; This needs testing to determine the best size for various constants:
;;;
;;;  How big should a table become before it starts hashing its entries?
;;;  How big should a hash bucket be?
;;;  What percentage of a table's vector should be used for overflow buckets?
;;;

;;; Interface:
;;;  MAKE-TABLE
;;;  MAKE-TABLE-OF-SIZE
;;;  MAKE-STRING-TABLE
;;;  MAKE-STRING-TABLE-OF-SIZE
;;;  MAKE-HASH-TABLE
;;;  MAKE-HASH-TABLE-OF-SIZE
;;;  HASH-TABLE?
;;;  TABLE?
;;;  STRING-TABLE?
;;;  TABLE-ENTRY
;;;  TABLE-WALK
;;;  WALK-TABLE
;;;  FIND-TABLE-ENTRY
;;;  COPY-TABLE
;;;  CLEAN-TABLE
;;;  RETURN-TABLE-TO-POOL

;;; Obsolete interface:

(define (remove-table-entry table key)
  (set (table-entry table key) nil))

(define (set-table-entry table key value)
  (set (table-entry table key) value))

;;; Hash buckets built into table vectors.   The first part of the vector
;;; contains the hash buckets, the rest is divided into overflow buckets.
;;; Each bucket looks like:
;;;
;;;            val0
;;;            key0
;;;            ...
;;;            valN
;;;            keyN
;;;            #f
;;;            #f
;;;
;;; where N < 4.  If there are more than three entries in the bucket an
;;; overflow bucket is added:
;;;
;;;            val0
;;;            key0
;;;            ...
;;;            val3
;;;            key3
;;;            #f  
;;;            index of the start of the overflow bucket
;;;
;;; The size of the buckets is power of two to make it easy to convert hash
;;; numbers into vector indices.  It isn't really necessary.
;;;
;;; Tables below a certain size are kept in a single large bucket and no
;;; hashing is done.

(define (make-table . maybe-id)
  (create-%table (if maybe-id (car maybe-id) nil)
                 0 t true descriptor-hash eq?))

(define (make-table-of-size start-size . maybe-id)
  (create-%table (if maybe-id (car maybe-id) nil)
                 start-size t true descriptor-hash eq?))

(define make-table-with-size make-table-of-size)

(define (make-string-table . maybe-id)
  (create-%table (if maybe-id (car maybe-id) nil)
                 0 nil string? string-hash string-equal?))

(define (make-string-table-of-size start-size . maybe-id)
  (create-%table (if maybe-id (car maybe-id) nil)
                 start-size nil string? string-hash string-equal?))

(define make-string-table-with-size make-string-table-of-size)

(define (make-symbol-table . maybe-id)
  (create-%table (if maybe-id (car maybe-id) nil)
                 0 nil symbol? symbol-hash eq?))

(define (make-symbol-table-of-size start-size . maybe-id)
  (create-%table (if maybe-id (car maybe-id) nil)
                 start-size nil symbol? symbol-hash eq?))

(define make-symbol-table-with-size make-symbol-table-of-size)

(define (make-hash-table type hash comparison gc-sensitive? . maybe-id)
  (let ((type       (enforce procedure? type))
        (hash       (enforce procedure? hash))
        (comparison (enforce procedure? comparison)))
    (create-%table (if maybe-id (car maybe-id) nil)
                   0
                   gc-sensitive?
                   type
                   hash
                   comparison)))

(define (make-hash-table-of-size start-size type hash
                                 comparison gc-sensitive? . maybe-id)
  (let ((start-size (enforce nonnegative-fixnum? start-size))
        (type       (enforce procedure? type))
        (hash       (enforce procedure? hash))
        (comparison (enforce procedure? comparison)))
    (create-%table (if maybe-id (car maybe-id) nil)
                   start-size
                   gc-sensitive?
                   type
                   hash
                   comparison)))

(define (hash-table? x)
  (%table? x))

;;; The following predicates are not very exact.

(define (table? x)
  (and (%table? x)
       (eq? (%table-type x) true)
       (eq? (%table-compare x) eq?)))

(define (string-table? x)
  (and (%table? x)
       (eq? (%table-type x) string?)
       (eq? (%table-compare x) string-equal?)))

(define (symbol-table? x)
  (and (%table? x)
       (eq? (%table-type x) symbol?)
       (eq? (%table-compare x) eq?)))


;;;                  %TABLE structures
;;;==========================================================================

;;;   #F is used as the end-of-bucket marker in the table's vector and thus it
;;; cannot be used as a key in the vector.  %TABLE structures have a slot to
;;; store the value of #F if it is used as a key.  %TABLE-COUNT is the number
;;; of values in the vector.  It doesn't count the value (if any) of #F.

(define-structure-type %table
  id            ; Identification
  count         ; Number of entries
  vector        ; Vector of entries
  mask          ; mask used to cut hashes to correct size
  next          ; index of next overflow bucket
  type          ; type predicate for keys
  hash          ; Hash procedure
  compare       ; Comparison procedure
  gc-stamp      ; Stamp of last GC this table was hashed after.  Nil if hash
                ;   is not GC sensitive.
  (((recycle self)
     (return-table-to-pool self))
    ((print self port)
     (format port "#{Table~_~S~_~S}"
             (object-hash self)
             (%table-id self)))
    ((identification self) (%table-id self))
    ((set-identification self id)
     (if (not (%table-id self)) (set (%table-id self) id)))))

(define (create-%table id size gc? type hash compare)
  (let ((table (obtain-from-pool *table-pool*)))
    (set (%table-id       table) id)
    (set (%table-count    table) 0)
    (set (%table-type     table) type)
    (set (%table-hash     table) hash)   
    (set (%table-compare  table) compare)
    (set (%table-gc-stamp table) (if gc? (gc-stamp) nil))
    (receive (vec mask next)
             (get-table-vector size)
      (set (%table-mask   table) mask)
      (set (%table-vector table) vec)
      (set (%table-next   table) next)
      table)))

(lset *table-pool* nil)

(define (initialize-table-pool)
  (set *table-pool* (make-pool '*table-pool* make-%table 1 %table?)))

;;;                   Storage allocation for tables
;;;============================================================================

;;; Numbers:
;;;   Start at one entry per bucket
;;;   size of vector = table-count * 8 => one per bucket
;;;
;;;   Overflow buckets / Hash buckets => 1 / 1
;;;

;;; A vector for empty tables.

(define empty-vec (make-vector 2))

;;; The number of elements at which we start hashing:

(define-constant *minimum-hashing-size* 32)

;;; Get a vector appropriate for a table with COUNT entries.  Returns the
;;; vector, a mask to turn hash numbers into bucket indices, and the start of
;;; the first overflow bucket.

(define table-grow-factor 4)

(define (get-table-vector count)
  (let* ((size (enforce fixnum? count)))
    (cond ((fx>= count *minimum-hashing-size*)
           (let* ((vec (obtain-from-pool
                        (table-vector-pool (fx* table-grow-factor count))))
                  (size (fixnum-ashr (vector-length vec) 1)))
             (vector-fill vec nil)
             (return vec (fixnum-logand (fixnum-lognot 7) (fx- size 1)) size)))
          ((fx> count 0)
           (let ((vec (obtain-from-pool
                       (table-vector-pool (fx* 2 (fx+ 1 size))))))
             (vector-fill vec nil)
             (return vec 0 (vector-length vec))))
          (else
           (return empty-vec 0 0)))))

(define (get-table-next vec-length mask)
  (if (fx= mask 0)
      vec-length
      (fixnum-ashr vec-length 1)))

;++ is there another way to do this?
;++ Should an error be returned is a table is larger then the maximum size? 

;;; Vector sizes are currently of the form 2**n.

(define-constant *minimum-table-vector-size* 7)
(define-constant *number-of-table-vector-pools* 16)

(define *table-vector-pools*
  (make-vector *number-of-table-vector-pools*))

(define (initialize-table-vector-pool)
  (do ((i 0 (fx+ i 1))
       (size (fx+ *minimum-table-vector-size* 1) (fixnum-ashl size 1)))
      ((fx>= i *number-of-table-vector-pools*) t)
    (set (vref *table-vector-pools* i)
         (make-pool `(*table-vector-pool* ,i)
                     (lambda () (make-vector size))
                     1
                     vector?))))

;++ Coalesce this code with that in buffer.t.
;-- Only if buffers want vectors of size 2**n.

(define table-vector-pool
  (let ((flag nil))
    (lambda (size)
      (cond ((fx< size *minimum-table-vector-size*)
             (vref *table-vector-pools* 0))
            (else
             (let ((i (fx- (fixnum-howlong (fx- size 1)) 3)))
               (cond ((fx<= i 15)
                      (vref *table-vector-pools* i))
                     (flag
                      (vref *table-vector-pools* 15))
                     (else
                      (warning "table size exceeds maximum - using maximum.")
                      (warning "~t Please inform implementors.~%")
                      (vref *table-vector-pools* 15)))))))))

;;; Return a vector to the appropriate pool.

(define (release-table-vector vec)
  (let ((vec (enforce vector? vec)))
    (if (neq? vec empty-vec)
        (return-to-pool (table-vector-pool (vector-length vec)) vec))))

;;; Remove all the entries from a table.

(define (clean-table table)
  (let ((table (enforce %table? table)))
    (vector-fill (%table-vector table) nil)
    (set (%table-count table) 0)
    (set (%table-next table)
         (get-table-next (vector-length (%table-vector table))
                         (%table-mask table)))
    table))

;;; Return storage used by a table.

(define (return-table-to-pool table)
  (let ((table (enforce %table? table)))
    (release-table-vector (%table-vector table))
    (return-to-pool *table-pool* table)))

(initialize-table-pool)
(initialize-table-vector-pool)
(vector-fill empty-vec nil)
