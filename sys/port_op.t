(herald port_op
        (env tsys (osys buffer) (osys pool))) 

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

;;;; I/O port operations

;;; General port operations.

(define-predicate port?)

(define-operation (CLOSE port) (no-value))

;;; Opening lists and strings "wins" by opening the named file.

(define-operation (MAYBE-OPEN FILESPEC MODE)
  (maybe-open (->filename filespec) mode))

(define-operation (OPEN FILESPEC MODE)
  (iterate loop ((fname filespec))
    (cond ((maybe-open fname mode))
          (else
           (receive vals (error "(OPEN '~s '~s) failed ~%~
                                **~10tType (RET) or (RET filespec) to retry."
                                fname
                                mode)
             (if (null? vals) 
                 (loop filespec)
                 (loop (car vals))))))))

(define-operation (port-open? port) (ignore port) '#t) ;??

(define-operation (re-open port))

;;; Input operations.  All input ports are expected to handle
;;; READC and UNREADC.

(define-predicate INPUT-PORT?)
(define-operation (INTERACTIVE? port) nil)
(define INTERACTIVE-PORT? INTERACTIVE?)

(define-operation (READ-CHAR PORT))           ; MUST BE HANDLED.
(define readc read-char)

(define-operation (char-ready? port)
  (let ((val (maybe-read-char port)))
    (cond (val
           (unread-char port val)
           t)
          (else nil))))

(define-operation (maybe-read-char port))     ;++ should default be readc?
(define maybe-readc maybe-read-char)

(define-operation (UNREAD-CHAR PORT))         ; MUST BE HANDLED.
(define unreadc unread-char)

(define-operation (PEEK-CHAR PORT)
  (cond ((iob? port)
         (vm-peek-char port))
        (else
         (let ((val (read-char port)))
           (unread-char port)
           val))))

(define PEEKC peek-char)

(define-operation (READ PORT)
  (read-object port (port-read-table port)))

(define-settable-operation (PORT-READ-TABLE PORT))

(define SET-PORT-READ-TABLE (setter port-read-table))

;;; WITH-BUFFERS is not available in VM.

(define-operation (READ-LINE port)
  (with-buffers ((buffer))
    (let ((readc (if (iob? port) vm-read-char read-char))
          (done  (lambda () (buffer->string buffer))))    ; go to.
      (iterate loop ()
        (let ((ch (readc port)))
          (cond ((eof? ch)
                 (if (buffer-empty? buffer) eof (done)))
                (else
                 (cond ((newline? ch) (done))
                       (else
                        (vm-write-char buffer ch)
                        (loop))))))))))

(define-operation (read-block port extend size))

(define-operation (clear-input port) (no-value))

;;; Output operations.  All output ports are expected to handle
;;; WRITEC.

(define-predicate OUTPUT-PORT?)

(define-operation (print obj port)
  (print-random obj port))

(define-operation (display obj port)
  (print obj port))

(define-operation (print-type-string obj)
  (cond ((bogus-entity? obj)
         (if (procedure? (bogus-entity-procedure obj)) "Procedure" "Object"))
        ((procedure? obj) "Procedure")
        ((frame?     obj) "Continuation")
        ((extend?    obj) "Object")
        ;; should never fall through past reasonable? check.
        (else "Random")))

(define-operation (print-info obj) '#f)

;;; What about robustness problem, i.e. errors within the printer?
;;; We have to be able to do something intelligent with pair pointer
;;; into outer space, etc.  Can we hook the print operation to do
;;; reasonableness check on arg before dispatching?

(lset *print-table* nil)

(define (print-random obj port)
  (cond ((not (reasonable? obj))
         (format port "#{Unreasonable~_#x~x}" (descriptor->fixnum obj)))
        (else
         (let ((type (print-type-string obj))
               (h    (object-hash obj))
               (id   (or (print-info obj) (identification obj))))
           (if id
               (format port "#{~a~_~s~_~s}" type h id)
               (format port "#{~a~_~s}" type h))))))


(define-operation (WRITE-CHAR PORT CHAR))        ; must be handled.

(define WRITEC write-char)

(define-operation (WRITE-STRING PORT STRING)
  (let* ((string (enforce string? string))
         (writec (if (iob? port) vm-write-char write-char))
         (len    (string-length string)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i len) (no-value))
      (writec port (string-elt string i)))))

(define WRITES write-string)

;++ obsolete - flush later
(define-operation (WRITE-TEXT PORT TEXT)
  (display text port))

(define-operation (WRITE-LINE PORT STRING)
  (let ((string (enforce string? string)))
    (write-string port string)
    (newline port)))

(define-operation (WRITE-SPACES PORT N)
  (cond ((iob? port) (vm-write-spaces port n))
        (else
         (iterate loop ((i 0))
           (cond ((fx>= i n) (no-value))
                 (else
                  (space port)
                  (loop (fx+ i 1))))))))

(define-operation (WRITE PORT OBJ)
  (print obj port)
  (newline port))


(define-operation (SPACE PORT)
  (cond ((or (fx>= (hpos port) (line-length port))
             (fx>= (hpos port) (wrap-column port)))
         (newline port))
        (else
         (write-char port #\space))))


;;; This should be the only place in the system where
;;; #\NEWLINE is written to a port.

(define-operation (newline port)
  (write-char port #\newline))

(define NEW-LINE newline)

(define-operation (FRESHLINE port)
  (if (fx> (hpos port) 0) (newline port)))

(define FRESH-LINE freshline)

;;; Presumably ok to ignore.
(define-operation (FORCE-OUTPUT port) nil)
(define-settable-operation (PORT-NAME PORT) 'anonymous)
(define set-port-name (setter port-name))
(define-operation (port->IOB port) nil)

;++ What about FILEPOS and SET-FILEPOS?  last-modified time?
;++ Name some other operations that should be supported.

;;; Other random stuff.


(define-settable-operation (LINE-LENGTH port) standard-line-length)

(define SET-LINE-LENGTH (setter line-length))

(define-settable-operation (WRAP-COLUMN port) standard-wrap-column)

(define SET-WRAP-COLUMN (setter wrap-column))

;;; force fresh-line to do newline

(define hpos
  (operation (lambda (port) (ignore port) 1) ; force fresh-line to do newline
    ((setter self) set-hpos)))

(define-operation (set-hpos port pos)
  (let ((pos (enforce fixnum? pos))
        (p   (cond ((fx> (hpos port) pos) (newline port) 0)
                   (else (hpos port)))))
    (write-spaces port (fx- pos p))))

(define VPOS
  (operation (lambda (port) (ignore port) 1)
    ((setter self) set-vpos)))

(define-operation (SET-VPOS PORT POS)
  (let ((pos (enforce fixnum? pos))
        (v   (vpos port)))
    (do ((v v (fx+ v 1)))
        ((fx>= v pos) pos)
      (newline port))))

;;; Other random operations.

(define-settable-operation (FILE-POSITION PORT))

(define SET-FILE-POSITION (setter file-position))

(define-predicate OUTPUT-WIDTH-PORT?)

(define-operation (PRINT-WIDTH OBJ)
  (with-output-width-port port (print obj port)))

(define-operation (DISPLAY-WIDTH OBJ)
  (with-output-width-port port (display obj port)))

(define-unimplemented (WRITE-ELIDED PORT OBJ LIMIT))
