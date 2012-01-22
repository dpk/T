(herald weak_table
  (env tsys (osys table) (osys table_entry)))

;;;
;;;  Weak Tables
;;;

;;;  MAKE-WEAK-TABLE
;;;  MAKE-WEAK-TABLE-OF-SIZE
;;;  MAKE-WEAK-HASH-TABLE
;;;  MAKE-WEAK-HASH-TABLE-OF-SIZE
;;;  WEAK-TABLE?
;;;  WEAK-TABLE-ENTRY

; Not done yet
;;;  WEAK-TABLE-WALK
;;;  WALK-WEAK-TABLE
;;;  FIND-WEAK-TABLE-ENTRY
;;;  COPY-WEAK-TABLE
;;;  CLEAN-WEAK-TABLE
;;;  RETURN-WEAK-TABLE-TO-POOL

;;; A weak table has:
;;;
;;; -1 Header
;;;  0 table     ; A %TABLE
;;;  1 vector    ;

;;; ID should be gotten from the TABLE

;;; HEADER/WEAK-TABLE should have the semaphore set!  This is important.

;;; This should be elsewhere
(define (%make-weak-table)
  (make-vector-extend header/weak-table 0 2))

(define (create-weak-table start-size type hash comparison gc-sensitive? id)
  (let ((table (create-%table id
                              start-size
                              gc-sensitive?
                              type
                              hash
                              comparison))
        (new (%make-weak-table)))
    (set (weak-table-table  new) table)
;    (set (weak-table-vector new) (%table-vector table))
;    (set (%table-vector table) nil)
    (set (weak-table-vector new) nil)
    (clear-weak-semaphore new)
    new))

(define (make-weak-hash-table type hash comparison gc-sensitive? . maybe-id)
  (let ((type       (enforce procedure? type))
        (hash       (enforce procedure? hash))
        (comparison (enforce procedure? comparison))
        (id (if (null? maybe-id) nil (car maybe-id))))
    (create-weak-table 0 type hash comparison gc-sensitive? id)))

(define (make-weak-hash-table-of-size start-size type hash
                                      comparison gc-sensitive? . maybe-id)
  (let ((start-size (enforce nonnegative-fixnum? start-size))
        (type       (enforce procedure? type))
        (hash       (enforce procedure? hash))
        (comparison (enforce procedure? comparison))
        (id (if (null? maybe-id) nil (car maybe-id))))
    (create-weak-table start-size type hash comparison gc-sensitive? id)))

(define (make-weak-table . maybe-id)
  (create-weak-table 0 true descriptor-hash eq? t 
                     (if (null? maybe-id) nil (car maybe-id))))

;(define-integrable (weak-table-exchange table)
;  (exchange (weak-table-vector table)
;            (%table-vector (weak-table-table table))))

;(define-integrable (set-table-semaphore table)
;  (set-weak-semaphore table))
;  (weak-table-exchange table))

;(define-integrable (clear-table-semaphore table)
;  (weak-table-exchange table)
;  (clear-weak-semaphore table))

(define weak-table-entry
  (object (lambda (table key)
            (let* ((already-set? (test-and-set-semaphore table))
                   (value (table-entry (weak-table-table table) key)))
              (if (not already-set?) (clear-weak-semaphore table))
              value))
    ((setter self) set-weak-table-entry)))

(define (set-weak-table-entry table key val)
  (let ((already-set? (test-and-set-semaphore table)))
    (set (table-entry (weak-table-table table) key) val)
    (if (not already-set?) (clear-weak-semaphore table))
    (return)))

(define (find-weak-table-entry table test)
  (let ((already-set? (test-and-set-semaphore table)))
    (receive (key value)
             (find-table-entry (weak-table-table table) test)
      (if (not already-set?) (clear-weak-semaphore table))
      (return key value))))

(define-handler weak-table
  (object nil
    ((print self stream)
     (format stream "#{Weak-table~_~S}" (weak-table-table self)))
    ((crawl-exhibit self)
     (exhibit-standard-extend self 2 0 0))
    ((maybe-crawl-component self command)
     (cond ((and (nonnegative-fixnum? command)
                 (fx< command 2))
            (crawl-push (extend-pointer-elt self command)))
           (else nil)))
    ((identification self)
     (identification (weak-table-table self)))))
