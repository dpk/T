(herald string_buffer (env tsys))

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

;;; describe buffers

;++ what should be integrated?
;++ pooled structures

(define-integrable (string-buffer-limit b)
  (text-length (string-text b)))

(define-integrable (string-buffer-space-remaining b)
  (fx- (string-buffer-limit b) (string-length b)))

(define-integrable string-buffer->string copy-string)
(define-integrable string-buffer-length  string-length)

;++ Should this be lap? or primop. This uses indexing, on a machine
;++ with tags it would use pointers into objects.

;(define-integrable (MOVE-TEXT SRC S-OFF DST D-OFF N)
;  (do ((n n (fx- n 1))
;       (s-off s-off (fx+ s-off 1))
;       (d-off d-off (fx+ d-off 1)))
;      ((fx<= n 0) (no-value))
;    (set (text-elt dst d-off) (text-elt src s-off))))

;;; Makes sure that buffer can hold at least N additional characters.
;;; If not the buffers size in increased by allocating a buffer
;;; of the appropriate size, copying the contents of the old buffer
;;; to the new, and finally exchanging the text pointers of the
;;; two buffers, hopefully, creating a transparent side effect.

(define (ensure-string-buffer-size b n)
  (cond ((fx< (string-buffer-space-remaining b) n)
         (let* ((old-size (string-length b))
                (temp     (get-string-buffer-of-size (fx+ old-size n))))
           (move-text (string-text b) 0 (string-text temp) 0 old-size)
           (exchange (string-text b) (string-text temp))
           (release-string-buffer temp))))
  (no-value))

;;; Write a character to a buffer.

(define (string-writec b ch)
  (let ((len (string-length b)))
    (let ((new-len (fx+ len 1)))
      (if (fx>= new-len *min-string-buffer-size*) ; horrible speed hack
          (ensure-string-buffer-size b 1))
      (set (string-length b) new-len)
      (set (text-elt (string-text b) len) ch)))
  (no-value))

;;; Write a string to a buffer.

(define (string-writes b s)
  (let* ((slen    (string-length s))
         (blen    (string-length b))
         (new-len (fx+ blen slen)))
    (if (fx>= new-len *min-string-buffer-size*) ; horrible speed hack
        (ensure-string-buffer-size b slen))
    (move-text (string-text s) 0 (string-text b) blen slen)
    (set (string-length b) new-len))
  (no-value))

;;; Obtain a buffer.

(define (get-string-buffer)
  (let ((b (obtain-from-pool (vref *string-buffer-pools* 0))))
    (set (string-length b) 0)
    b))

;;; Obtain a buffer whose size is >= N.

(define (get-string-buffer-of-size n)
  (let ((b (obtain-from-pool (string-buffer-pool n))))
    (set (string-length b) 0)
    b))

;;; Release a buffer.

(define (release-string-buffer b)
  (let ((b (enforce string? b)))
    (return-to-pool (string-buffer-pool (string-buffer-limit b)) b)))


;++ This should be an abstraction in pool.
;;; There are ten pools, for buffers of various sizes.
;;;    0    1    2    3     4     5     6     7      8      9
;;;   64  128  256  512  1024  2048  4096  8192  16834  32768

(define *string-buffer-pools* (make-vector 10))

(define-constant *min-string-buffer-size* 64)
(define-constant *max-string-buffer-size* 32768)

(define (initialize-string-buffer-pool)
  (do ((i 0 (fx+ i 1))
       (n *min-string-buffer-size* (fixnum-ashl n 1)))
      ((fx> i 9))
    (set (vref *string-buffer-pools* i)
         (make-pool `(*string-buffer-pool* ,i)
                     (lambda () (make-string n))
                     1
                     string?))))
                                                        
;;; Return a pool from which one can obtain a buffer whose size
;;; is >= N.

(define (string-buffer-pool n)
  (cond ((fx<= n *min-string-buffer-size*)
         (vref *string-buffer-pools* 0))       ; speed hack for common case
        (else
         (let ((i (fixnum-howlong (fixnum-ashr (fx- n 1) 6))))
           (if (fx> n *max-string-buffer-size*)
               (error "cannot allocate buffer of size ~a~%" n)
               (vref *string-buffer-pools* i))))))


(initialize-string-buffer-pool)
