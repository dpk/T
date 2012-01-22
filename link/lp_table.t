(herald lp-table
  (env tsys))

;;; a simple table package using a linear probing hash algorithm.

;;; Evens are objects; odds are assocs.


;;; Nil is not a possible entry in a table.

;;; (vref t *jtable-len*) = (gc-stamp) of last time


(lset *max-chain-size* 100)

(define-integrable (lp-hash obj)
  (let ((n (descriptor->fixnum obj)))
    (fx-xor (fx-xor (fx-and n 255)
                    (fx-ashl (fx-and n 255) 3))
            (fx-xor (fixnum-ashr n 7)
                    (fx-xor (fx-ashl n 6) (fx-ashr n 16))))))

(define-structure-type %lp-table
  id
  vector
  stamp
  count
  (((setter self)
    (lambda (obj val) (set-lp-table-entry table obj val)))
   ((get-vector self)
    (%lp-table-vector self))
   ((print-type-string self) "LP-table")
   ((identification self) 'make-lp-table)))


(define (make-lp-table size . id)
  (let* ((len   (fx- (fixnum-expt 2 (fixnum-howlong size)) 2))
         (table (make-%lp-table)))
    (set (%lp-table-id table) (car id))
    (set (%lp-table-count table) 0)
    (set (%lp-table-stamp table) (gc-stamp))
    (set (%lp-table-vector table) (vector-fill (make-vector len) nil))
    table))

(define (lp-table-entry table obj)
  (let* ((v   (%lp-table-vector table))
         (len (vector-length v)))
    (iterate loop ((index (fx-and (lp-hash obj) len)))
      (let ((slot (vref v index)))
        (cond ((null? slot) nil)
              ((eq? slot obj)
               (vref v (fx+ index 1)))
              (else
               (let ((next (fx+ index 2)))
                 (loop (if (fx>= next len) 0 next)))))))))

(define (set-lp-table-entry table obj val)
  (let* ((v     (%lp-table-vector table))
         (len   (vector-length v))
         (start (fx-and (lp-hash obj) len)))
    (iterate loop ((index start) (chain-size 0))
      (let* ((index (if (fx>= index len) 0 index))
             (slot  (vref v index)))
        (cond ((null? slot)
               (set (vref v index) obj)
               (set (vref v (fx+ index 1)) val))
              ((eq? slot obj)
               (error "resetting slot (~s ~s)" obj val))
               ;++(set (vref v (fx+ index 1)) val))
              ((fx> chain-size *max-chain-size*)
               (rehash-lp-table table)
               (set-lp-table-entry table obj val))
              (else
               (loop (fx+ index 2) (fx+ chain-size 1))))))
    (set (%lp-table-count table) (fx+ 1 (%lp-table-count table)))))


(define (rehash-lp-table table)
  (format t "** Warning: rehashing table ~a ..." table)
  (let* ((ov   (%lp-table-vector table))
         (olen (vector-length ov))
         (len  (fx- (fixnum-expt 2 (fx+ (fixnum-howlong olen) 1)) 2))
         (v    (vector-fill (make-vector len) nil)))
    (set (%lp-table-vector table) v)
    (iterate loop ((i 0))
      (cond ((fx>= i olen))
            (else
             (let ((obj (vref ov i)))
               (if obj (set-lp-table-entry table
                                           obj
                                           (vref ov (fx+ i 1))))
               (loop (fx+ i 2))))))
    (format t "(~a ~a ~a ~a) done.~%" olen ov len v)))
