(herald fixnum (env tsys))

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

(define-constant *bits-per-fixnum* 30)
(define-constant *u-bits-per-fixnum* (fx- *bits-per-fixnum* 1))

(define-constant most-negative-fixnum (fixnum-ashl (no-op 1) *u-bits-per-fixnum*))
(define-constant most-positive-fixnum (fx- most-negative-fixnum 1))

(define-constant positive-sign-char #\+)
(define-constant negative-sign-char #\-)
(define-constant ratio-char         #\/)

(define-integrable (exponent-introducer? ch)
  (or (char= ch #\e) (char= ch #\E)))

(define-integrable (sign-char? ch)
  (or (char= ch positive-sign-char)
      (char= ch negative-sign-char)))

;;; Fixnum hackery

(define (fixnum-expt x y)
  (labels (((real-fixnum-expt x y)
            (do ((y y (fixnum-ashr y 1))
                 (x x (fx* x x))
                 (z 1 (if (fixnum-odd? y) (fx* z x) z)))
                ((fx<= y 0) z))))
    (cond ((fx< y 0) (fixnum-negative-expt x y))
          ((fx= y 0) 1)
          (else (real-fixnum-expt x y)))))

(define (fixnum-bit? fx bit)
  (let ((shifted (fixnum-ashr fx bit)))
    (fxn= shifted (fixnum-ashl (fixnum-ashr shifted 1) 1))))


(define (fixnum-bit-field fixnum start count)
  (fx-and (fx-not (fx-ashl -1 count)) (fx-ashr fixnum start)))

;;; chop off VAL high bits, OR field into target, move field to
;;; appropriate pos

(define (set-fixnum-bit-field fixnum start count val)
  (let ((val (fixnum-bit-field val 0 count)))
    (fx-ior (fx-ashl val start)
            (fx-and (fx-ior (fx-not (fx-ashl -1 start))
                            (fx-ashl -1 (fx+ start count)))
                    fixnum))))

;;; Aliases

(define fx-odd?   fixnum-odd?)
(define fx-even?  fixnum-even?)
(define fx-length fixnum-length)
(define fx-expt   fixnum-expt)
(define fx-bit?   fixnum-bit?)

(define handle-fixnum
  (object nil
    ((hash self) self)
    ((crawl-exhibit n)
     (let ((port (standard-output)))
       (format port " ~d = #x~x = #o~o = #b~b" n n n n)
       (cond ((and (fx>= n 0) (fx<= n number-of-char-codes))
              (format port " = (char->ascii ~s)" (ascii->char n))))
       (newline port)))
    ((print n port)
     (let ((rdx (rt-radix *print-table*)))
       (labels (((write-fx n)
               (cond ((fxN= n 0)
                      (write-fx (fx/ n rdx))
                      (let ((c (digit->char (fx-abs (fx-rem n rdx)) rdx)))
                        (write-char port c))))))
         (cond ((iob? port) (vm-write-fixnum port n rdx))
               ((fx= n 0) (write-char port #\0))
               (else
                (if (fx< n 0) (write-char port negative-sign-char))
                (write-fx n))))))))
