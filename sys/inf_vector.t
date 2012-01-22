(herald infinite_vector)

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

(define (make-infinite-vector start-size init . maybe-id)
  (let ((start-size (enforce nonnegative-fixnum? start-size)))
    (receive (vec size)
             (expand-vector nil 0 start-size init)
      (let ((id (if maybe-id (car maybe-id) nil))
            (expand (lambda (index)
                      (let ((index (enforce nonnegative-fixnum? index)))
                        (receive (new-vec new-size)
                                 (expand-vector vec size (fx+ 1 index) init)
                          (set vec new-vec)
                          (set size new-size))))))
        (object (lambda (index)
                  (if (fx>= index size) (expand index))
                  (vref vec index))
          ((setter self) 
           (lambda (index new)
             (if (fx>= index size) (expand index))
             (set (vref vec index) new)))
          ((recycle self)
           (return-vector vec)
           (set vec nil)       ;Safety
           (set size -1))      ;
          ((identification self) id)
          ((set-identification self new-id)
           (set id new-id))
          ((print-type-string self) "infinite vector"))))))

(define (expand-vector vec size index init)
  (let* ((new-vec (obtain-from-pool (vector-pool index)))
         (new-size (vector-length new-vec)))
    (cond (vec
           (vector-replace new-vec vec size)
           (return-vector vec)))
    (let ((init (if init init (lambda (x) (ignore x) 0))))
      (do ((i size (fx+ i 1)))
          ((fx>= i new-size))
        (set (vref new-vec i) (init i))))
    (return new-vec new-size)))

;;; Stolen from TABLE

;++ is there another way to do this?  Should an error be returned
;++ is a vector is larger then the maximum size?
;;; Vector sizes are currently of the form 2**n
;;; There are 16 pools, for vectors of various sizes.
;;;    0  1  2  3   4   5   6    7    8    9   10    11    12 ...
;;;    8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 ...

(define-constant *minimum-vector-size* 8)
(define-constant *minimum-vector-bits*
  (fx- (fixnum-howlong *minimum-vector-size*) 1))
(define-constant *number-of-vector-pools* 16)

(define *vector-pools*
  (make-vector *number-of-vector-pools*))

(define (initialize-vector-pool)
  (do ((i 0 (fx+ i 1))
       (size *minimum-vector-size* (fixnum-ashl size 1)))
      ((fx>= i *number-of-vector-pools*) t)
    (set (vref *vector-pools* i)
         (make-pool `(*vector-pool* ,i)
                    (lambda () (make-vector size))
                    1
                    vector?))))

(define (vector-pool size)
  (let ((i (fx- (fixnum-howlong (fx- size 1))
                *minimum-vector-bits*)))
    (cond ((or (fx= 0 size)
               (fx>= 0 i))
           (vref *vector-pools* 0))
          ((fx< i 15)
           (vref *vector-pools* i))
          (else
           (error "expanding vector size exceeds implementation maximum")))))

;;; Return a vector to the appropriate pool.

(define (return-vector vec)
  (let ((vec (enforce vector? vec)))
    (return-to-pool (vector-pool (vector-length vec)) vec)))

(initialize-vector-pool)
