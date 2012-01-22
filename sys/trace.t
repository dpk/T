(herald trace (env tsys (osys hash)))

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

;;; trace

(lset *trace-level* 0)

(define (comment-indent msg-port n)       ;used also by pp?, and load
  (fresh-line msg-port)
  (writec msg-port #\semicolon)
  (set-hpos msg-port (fx+ n 1)))

(define (make-traced-object proc id origin)
  (let ((id (or id (identification proc) proc))
        (active? t)
        (traced-proc nil))
   (set traced-proc
    (join (object (lambda arglist
                    (cond (active?
                           (bind ((active? nil))
                             (let ((port (debug-output)))
                               (comment-indent port *trace-level*)
                               (format port "~d Calling ~s with arguments~_~s~%"
                                       *trace-level* id arglist)))
                           (let ((vals (bind ((*trace-level* (fx+ *trace-level* 1)))
                                          (receive vals 
                                            (if (operation? proc)
                               (apply-traced-operation traced-proc arglist)
                               (apply proc arglist))
                                             vals))))
                                    (bind ((active? nil))
                                      (let ((port (debug-output)))
                                        (comment-indent port *trace-level*)
                                        (format port "~d Returned from ~s with values~_~s~%"
                                                *trace-level* id vals)))
                                    (apply return vals)))
                          (else
                           (apply proc arglist))))
                  ((get-loaded-file self) (get-loaded-file proc))  ;not a no-op!
                  ((traced-location self) origin)
                  ((traced-id self) id)
                  ((*untrace self) proc)
                  ((traced? self) t)
                  ((print self port)
                   (format port "#{Traced~_~s~_~s}" (object-hash self) proc)))
          proc))
     traced-proc))

(define-operation (traced-location obj))
(define-operation (traced-id       obj))
(define-predicate traced?)

(define-operation (*trace proc id origin)       ; operations handle
  (make-traced-object proc id origin))

(define-operation (*untrace obj))

(define *traced-objects* (make-population '*traced-objects*))

(define (set-traced loc id)
  (let ((proc (contents loc)))
    (cond ((traced? proc)
           (format (debug-output) "~&~s already traced.~%" id))
          (else
           (let ((traced (*trace (contents loc) id loc)))
             (add-to-population *traced-objects* traced)
             (set (contents loc) traced)
             (format (debug-output) "~&~s traced.~%" id))))
    repl-wont-print))

(define (set-untraced loc)
  (let ((proc (contents loc)))
    (cond ((traced? proc)
           (remove-from-population *traced-objects* proc)
           (let ((probe (contents (traced-location proc))))
             (cond ((eq? probe proc)
                    (format (debug-output) "~&~s untraced.~%"
                            (set (contents loc) (*untrace proc))))
                   (else
                    (format (debug-output) "~&~s not untraceable.~%" probe)))))
          (else
           (format (debug-output) "~&~s not traced.~%" proc)))
    repl-wont-print))

(define (display-traced-objects)
  (format (debug-output) "~&Traced:~%")
  (walk-population
   *traced-objects*
   (lambda (obj)
     (cond ((eq? obj (contents (traced-location obj)))
            (format (debug-output) "  ~s~%" (traced-id obj))))))
    repl-wont-print)

(define (untrace-traced-objects)
  (walk-population
   *traced-objects*
   (lambda (obj)
     (set-untraced (traced-location obj))
     (remove-from-population *traced-objects* obj)))
  repl-wont-print)

;;; "user interface"

(define-syntax (trace . places)
  (cond ((null? places)
         '(display-traced-objects))
        (else
         (blockify (map (lambda (place)
                          `(set-traced (,(t-syntax 'locative) ,place)
                                       ',(if (symbol? place) place nil)))
                        places)))))

(define-syntax (untrace . places)
  (cond ((null? places)
         '(untrace-traced-objects))
        (else
         (blockify (map (lambda (place)
                          `(set-untraced (,(t-syntax 'locative) ,place)))
                        places)))))  

;;; measure consing performed in evaluating an expression.

(define-syntax (pig x)
  `(*pig (lambda () ,x)))

(define (*pig x)
  (let ((before (process-global task/area-frontier)))
    (let* ((val (x))
           (amount (fx- (process-global task/area-frontier) before)))
      (format (debug-output) "~&;consed ~s longwords, ~s (#x~x) bytes~%"
              amount (fx* amount 4) (fx* amount 4))
      val)))
