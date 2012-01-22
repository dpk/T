(herald port
        (env tsys (osys buffer)))

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

;;; Given the filename of a local file, return a port for it.

;;; old   - '( in | out | append)
;;; major - in | out | append | update
;;;         '(major . [minor ...])

(define (read-objects-from-string string)
  (let ((iob (string->buffer string)))
    (iterate loop ((l '()))
      (let ((obj (read iob)))
        (cond ((eof? obj)
               (release %buffer-pool iob)
               (reverse! l))
              (else
               (loop (cons obj l))))))))

;;; Hack for LOAD-TRANSDUCE.

(define (cons-port obj port)
  (let ((flag nil))
    (join (object nil
            ((read self)
             (cond (flag (read port))
                   (else (let ((val obj))
                           (set flag t)
                           (set obj nil)
                           val)))))
          port)))

;++ make this handle PORT-POSITION
(define (make-output-width-port)
  (let ((count 0))
    (object nil
      ((write-char self c)
       (ignore c)
       (set count (fx+ count 1)))
      ((close self) count)               ;++ pretty random
      ((output-width-port? self) '#t)
      ((output-port? self) '#t)
      ((port? self) '#t)
      ((print-type-string self) "Output-port"))))

(define (make-broadcast-port . ports)
  (labels (((doit proc)
            (iterate loop ((s ports))
              (cond ((null? s) (no-value))
                    (else
                     (proc (car s))
                     (loop (cdr s))))))
           (port (object nil
                   ((write-char self c)
                    (doit (lambda (port) (write-char port c))))
                   ((hpos self) (hpos (car ports)))
                   ((set-hpos self pos)
                    (doit (lambda (port) (set (hpos port) pos))))
                   ((newline self)
                    (doit newline))
                   ((fresh-line self)
                    (doit fresh-line))
                   ((force-output self)
                    (doit force-output))
                   ((print-type-string self) "Broadcast-port")
                   ((output-port? self) t)
                   ((port? self) t))))
    port))

;;; This is kind of yicky also.

(define (make-echo-port iport oport)
  (join (object nil
          ((read-char self)
           (let ((c (read-char iport)))
             (write-char oport c)
             c)))
        iport))

; random utility used by crawl, debug, eval

(define (print-one-line obj port)
  (catch abort
    (print obj
           (object nil
             ((writec self char) (writec port char))
             ((writes self string) (writes port string))
             ((hpos self) (hpos port))
             ((port? self) '#t)
             ((newline self)       ; called from space
              (writes self " ---")
              (abort nil)))))
  (no-value))
