(herald free
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

;;; Freelist utilities
;;; Thanks and apologies to Bob Nix.

;;; Could also have
;;;   FREE-LIST
;;;   FREE-APPEND
;;;   FREE-APPEND!
;;;   FREE-DEL
;;;   FREE-DELQ
;;;   FREE-` (this could be tough)
;;;   etc.

;;; The last CDR of a freelist is #F and not () so as not to have to check
;;; for both.

(define-constant %%pair-pool-generate-count 1000)

(define-structure-type %freelist
  weak
  (((print-type-string self) "Freelist")))

(define-constant freelist? %freelist?)

(define (make-freelist)
  (let ((n (make-%freelist)))
    (set (%freelist-weak n) (make-weak-cell '#f))
    n))

(define (cons-from-this-freelist f a d)
  (let* ((weak (%freelist-weak f))
         (pair (weak-cell-contents weak)))
    (cond ((not pair)
           (set (weak-cell-contents weak) (cons-up-a-freelist))
           (cons a d))
          (else
           (set (weak-cell-contents weak) (cdr pair))
           (set (car pair) a)
           (set (cdr pair) d)
           pair))))

(define (cons-up-a-freelist)
  (iterate loop ((i 1) (freelist '#f))   ; #F instead of () is important
    (cond ((fx= i %%pair-pool-generate-count)
           freelist)
          (else
           (loop (fx+ i 1) (cons 'free freelist))))))

(define (return-to-this-freelist f cell)
  (really-return-to-freelist (%freelist-weak f) cell)
  nil)

(define (return-list-to-this-freelist f l)
  (let ((weak (%freelist-weak f)))
    (do ((l l (let ((n (cdr l))) (really-return-to-freelist weak l) n)))
        ((atom? l)
         nil))))

(define-constant (really-return-to-freelist weak pair)
  (set (car pair) 'free)
  (modify (weak-cell-contents weak)
          (lambda (x)
            (set (cdr pair) x)
            pair)))

;;; Compatibility junk

(define system-freelist
  (make-freelist))

(define (cons-from-freelist a d)
  (cons-from-this-freelist system-freelist a d))

(define (return-to-freelist cell)
  (return-to-this-freelist system-freelist cell))

(define (return-list-to-freelist list)
  (return-list-to-this-freelist system-freelist list))

;;; Only works on one list...
;;; The N-list version is too gross.

(define (free-map proc l)
  (cond ((null-list? l) '())
        (else
         (let ((result (cons-from-freelist (proc (car l)) '())))
           (iterate loop ((l (cdr l)) (r result))
             (cond ((null-list? l)
                    result)
                   (else
                    (let ((q (cons-from-freelist (proc (car l)) '())))
                      (set (cdr r) q)
                      (loop (cdr l) q)))))))))


(define (free-del! pred obj list)
  (iterate loop ((list list))
    (cond ((null-list? list) '())
          ((pred obj (car list))
           (let ((d (cdr list)))
             (return-to-freelist list)
             (loop d)))
          (else
           (set (cdr list) (loop (cdr list)))
           list))))

(define (free-delq! obj list)
  (iterate free-delq! ((list list))
    (cond ((null-list? list) '())
          ((eq? obj (car list))
           (let ((d (cdr list)))
             (return-to-freelist list)
             (free-delq! d)))
          (else
           (set (cdr list) (free-delq! (cdr list)))
           list))))

