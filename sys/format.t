(herald format
        (env tsys (osys pool) (osys port_op) (osys port)))

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

;;;; Format

;;; for robustness' sake, we put a stub definition of format in
;;; the kernel.  It jumps here if it thinks the world is in a
;;; consistent state.

(define (t-format-aux port f stuff force?)
  (cond ((eq? port nil)
         (with-output-to-string s (format-internal s f stuff '#f)))
        (else
         (format-internal port f stuff force?))))

(define *format-dispatch-table*
  (vector-fill (make-vector number-of-char-codes)
               nil))

(define-integrable (format-proc char)
  (vref *format-dispatch-table* (char->ascii char)))

(define (set-format-proc char proc)
  (vset *format-dispatch-table* (char->ascii (char-downcase char)) proc)
  (vset *format-dispatch-table* (char->ascii (char-upcase char)) proc))

(define (format-internal port f stuff force?)
  (let ((foo (format-internal-loop port f stuff f)))
    (if force? (force-output port))
    (cond ((not (null? foo))
           (error "too many arguments in call to ~s~%  ~s"
                  'format
                  (cons* 'format port f stuff))))
      (no-value)))

(define (format-internal-loop port fmt stuff f)
  (cond ((pair? fmt)
         (do ((fmt fmt (cdr fmt))
              (stuff stuff (format-internal-loop port (car fmt) stuff f)))
             ((null-list? fmt) stuff)))
        (else
         (let ((fmt (chopy (enforce string? fmt))))
           (iterate loop ((stuff stuff)) 
             (cond ((string-empty? fmt) stuff)
                   ((char= (char fmt) #\~)
                    (chdr! fmt)
                    (let* ((arg (cond ((or (sign-char? (char fmt))
                                           (digit? (char fmt) 10))
                                       (format-get-number fmt))  ; speed hack (?)
                                      (else nil)))
                           (op (cond ((string-empty? fmt)
                                      (error " bad format string ~%  ~s"
                                             (cons* 'format port f stuff)))
                                     (else (char fmt)))))
                      (chdr! fmt)
                      (let ((proc (format-proc op)))
                        (cond ((null? proc)
                               (error "~c is an unknown code~%  ~s"
                                      op
                                      (cons* 'format port f stuff))
                               (loop stuff))
                              (else (loop (proc port fmt stuff arg)))))))
                   (else
                    (writec port (char fmt))
                    (chdr! fmt)
                    (loop stuff))))))))

;;; gnaw a number off the string.  clobber string header.

;++ this doesn't really work - fix it.

(define (format-get-number f)           ; nwm
  (let ((f2 (chopy f)))
    (do ((f f (chdr! f))
         (i 0 (fx+ i 1)))
        ((not (or (sign-char? (char f)) (digit? (char f) 10)))
         (string->integer (string-slice f2 0 i) 10)))))

;;; peel off one object from argument list

(define (format-car port fmt stuff)
  (cond ((null? stuff)
         (error "too few arguments in call to ~s~%  (~s ~s ~s ...)"
                'format 'format
                port fmt))
        (else (car stuff))))

;;; kludge format: take the next argument as a format string.

(define (format-kludge port fmt stuff arg)
  (ignore arg)
  (format-internal-loop port (format-car port fmt stuff) (cdr stuff) fmt))
(set-format-proc #\k format-kludge)

;;; fresh line followed by <arg>-1 newlines

(define (format-fresh-line port fmt stuff arg)
  (ignore fmt)
  (fresh-line port)
  (let ((count (if (fixnum? arg) arg 1)))
    (do ((i 1 (fx+ i 1)))
        ((fx>= i count) stuff)
      (newline port))))
(set-format-proc #\& format-fresh-line)

;;; <arg> new lines

(define (format-newline port fmt stuff arg)
  (ignore fmt)
  (let ((count (if (fixnum? arg) arg 1)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i count) stuff)
      (newline port))))
(set-format-proc #\% format-newline)

;;; space over <arg> spaces

(define (format-space port fmt stuff arg)
  (ignore fmt)
  (let ((count (if (fixnum? arg) arg 1)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i count) stuff)
      (space port))))
(set-format-proc #\_ format-space)

;;; tab to column <arg>

(define (format-tab port fmt stuff arg)
  (ignore fmt)
  (cond ((fixnum? arg)
         (set-hpos port arg))
        (else
         (writec port #\tab)))
  stuff)
(set-format-proc #\t format-tab)

;;; utility for printing within fixed-width field.

(define (format-write-field port width writer)
  (let ((buffer (get-buffer)))
    (writer buffer)
    (let ((count (buffer-length buffer)))
      (cond ((fx>= width 0)
             ;; pad on right.
             (writes port (buffer->string! buffer))
             (if (fx< count width) (vm-write-spaces port (fx- width count))))
            (else
             ;; pad on left.
             (let ((width (fx-negate width)))
               (if (fx< count width) (vm-write-spaces port (fx- width count))))
             (writes port (buffer->string! buffer)))))
    (release-buffer buffer)
    (no-value)))

;;; print

(define (format-print port fmt stuff arg)     ; hack field width
  (let ((obj (format-car port fmt stuff)))
    (cond ((fixnum? arg)
           (format-write-field port
                               arg
                               (lambda (port) (print obj port))))
          (else
           (print obj port))))
  (cdr stuff))
(set-format-proc #\s format-print)

;;; display

(define (format-display port fmt stuff arg)   ; hack field width
  (let ((obj (format-car port fmt stuff)))
    (cond ((fixnum? arg)
           (format-write-field port
                               arg
                               (lambda (port) (display obj port))))
          (else
           (display obj port))))
  (cdr stuff))
(set-format-proc #\a format-display)

;;; pretty-print

(define (format-pretty-print port fmt stuff arg)     ; hack field width
  (ignore arg)
  (let ((obj (format-car port fmt stuff)))
    (pretty-print obj port))
  (cdr stuff))
(set-format-proc #\g format-pretty-print)

;;; pluralize - this is a hack

(define (format-plural port fmt stuff arg)
  (ignore arg)
  (let ((obj (enforce number? (format-car port fmt stuff))))
    (if (n= obj 1) (writec port #\s))
    (cdr stuff)))

(set-format-proc #\p format-plural)

;;; number in various radices

(define (make-radical-formatter radix)
  (lambda (port fmt stuff arg)
    (let ((obj (format-car port fmt stuff)))
      (bind ((*print-table* (rt-with-radix *print-table* radix)))
        (cond ((fixnum? arg)
               (format-write-field port
                                   arg
                                   (lambda (port) (print obj port))))
              (else
               (print obj port))))
      (cdr stuff))))
(set-format-proc #\d (make-radical-formatter 10))
(set-format-proc #\x (make-radical-formatter 16))
(set-format-proc #\o (make-radical-formatter  8))
(set-format-proc #\b (make-radical-formatter  2))

;;; number in radix <arg>

(define (format-radical port fmt stuff arg)
  (bind ((*print-table*
	  (rt-with-radix *print-table*
			 (enforce acceptable-radix? arg))))
    (write port (format-car port fmt stuff))
    (cdr stuff)))
(set-format-proc #\r format-radical)

;;; character

(define (format-char port fmt stuff arg)
  (ignore arg)
  (cond ((control? (format-car port fmt stuff))
         (writec port #\^)
         (writec port (uncontrolify (format-car port fmt stuff))))
        (else
         (writec port (format-car port fmt stuff))))
  (cdr stuff))
(set-format-proc #\c format-char)

;;; ~~ prints a tilde

(define (format-tilde port fmt stuff arg)
  (ignore fmt arg)
  (writec port #\~)
  stuff)
(set-format-proc #\~ format-tilde)

;;; ~<whitespace> is ignored

(define (format-skip-whitespace port fmt stuff arg)
  (ignore port arg)
  (iterate skip ()
    (cond ((whitespace? (char fmt))
           (chdr! fmt) (skip))
          (else stuff))))
(set-format-proc #\linefeed format-skip-whitespace)
(set-format-proc #\return   format-skip-whitespace)
(set-format-proc #\space    format-skip-whitespace)
(set-format-proc #\tab      format-skip-whitespace)
