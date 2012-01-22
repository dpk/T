(herald pool
  (env tsys))

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer 
;;; Science Department.  Permission to copy this software, to redistribute it, 
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;; Pools are implemented as simple structures.  They keep statistics
;;; which are very handy for finding storage leaks.  We should add
;;; tombstones someday.

;++ In order to do tombstoned pools the compiler probably needs
;++ to know something about them.  The architecture probably also
;++ needs to support pointers into objects.
;++ should pools be using vectors?

(define-operation (POOL-STATISTICS POOL))

(define-structure-type %pool
  id
  generator
  generate-count
  pred
  weak
  created
  allocated
  freed
  (((print-type-string self) (ignore self) "pool")
   ((identification self) (%pool-id self))
   ((pool-statistics self)
    ;; backquote is not available in the VM.
    (list (list 'pool-statistics (%pool-id self))
          (list 'created         (%pool-created   self))
          (list 'allocated       (%pool-allocated self))
          (list 'freed           (%pool-freed     self))))))

;++ Change when structure package handles defaults
(set (%pool-generate-count  (stype-master %pool-stype)) 1)
(set (%pool-pred            (stype-master %pool-stype)) true)
(set (%pool-created         (stype-master %pool-stype)) 0)
(set (%pool-allocated       (stype-master %pool-stype)) 0)
(set (%pool-freed           (stype-master %pool-stype)) 0)

;++ addition of optional count and predicate argument.  This will
;++ not be optional in 3.1.

(define (make-pool id generator . args)
  (destructure (((count pred) args))
    (if (null? count) (warning "No COUNT argument to MAKE-POOL."))
    (if (null? pred)  (warning "No PREDICATE argument to MAKE-POOL."))
    (%make-pool id generator (if count count 1) (if pred pred true))))

(define (%make-pool id generator count pred)
  (let ((pool (make-%pool)))
    (set (%pool-id             pool) id)
    (set (%pool-generator      pool) generator)
    (set (%pool-generate-count pool) count)
    (set (%pool-pred           pool) pred)
    (set (%pool-weak           pool) (make-weak-cell '#f))
    pool))

(define pool-freelist
  (make-freelist))

;;; Release the storage associated with an object.
(define-operation (recycle obj) (return))

(define (obtain-from-pool pool)
  (set (%pool-allocated pool) (fx+ 1 (%pool-allocated pool)))
  (let* ((weak (%pool-weak pool))
         (elts (weak-cell-contents weak)))
    (receive (obj elts)
             (if (not elts)
                 (return ((%pool-generator pool)) (cons-a-pool-freelist pool))
                 (let ((a (car elts))
                       (d (cdr elts)))
                   (return-to-this-freelist pool-freelist elts)
                   (return a d)))
      (set (weak-cell-contents weak) elts)
      obj)))

(define (cons-a-pool-freelist pool)
  (let ((generator (%pool-generator pool))
        (count (%pool-generate-count pool)))
    (do ((i 1 (fx+ i 1))
         (l '() (cons-from-this-freelist pool-freelist (generator) l)))
        ((fx>= i count) 
         (set (%pool-created pool) (fx+ i (%pool-created pool)))
         l))))

(lset zeroed-storage 0)

(define (return-to-pool pool obj)
  (set (%pool-freed pool) (fx+ 1 (%pool-freed pool)))
  (cond ((points-to-initial-impure-memory? obj)
         (if (extend? obj) (zero-out-storage obj))
         (return))
        (else
         (let* ((weak (%pool-weak pool))
                (obj  (enforce (%pool-pred pool) obj)))
           (modify (weak-cell-contents weak)
                   (lambda (l)
                     (cons-from-this-freelist pool-freelist obj l)))
           (return)))))

(define (zero-out-storage obj)
  (let ((count (extend-pointers obj)))
    (modify zeroed-storage (lambda (x) (fx+ x count)))
    (do ((i (fx- count 1) (fx- i 1)))
        ((fx< i 0))
      (set (extend-elt obj i) 0))))

(define (extend-pointers ptr)
  (let ((header (extend-header ptr)))
    (receive (ptr #f #f)
             (cond ((template? header)
                    (scan-closure ptr header))
                   (else
                    ((vref *scan-dispatch-vector* (header-type header)) ptr)))
      ptr)))

