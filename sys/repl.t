(herald repl (env tsys))

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

;;;; system initialization and read-eval-print loop


;;; (breakpoint [message [env]]) simply punts to t-breakpoint.

(define (t-breakpoint . args)
  (cond ((null? args)
         (t-breakpoint-aux nil read-eval-print-loop))
        ((or (null? (cdr args)) (null? (cadr args)))
         (t-breakpoint-aux (car args) read-eval-print-loop))
        (else
         ;++ why not give repl a env arg?
         (bind (((repl-env) (enforce environment? (cadr args))))
           (t-breakpoint-aux (car args) read-eval-print-loop)))))

;;; The most weird (i.e. weirdest) control structure in the system.

(lset **up** luser-typed-eof-at-top-level)

(define (t-breakpoint-aux message repl)
  (catch ret
    (if message (format (repl-output) "~&~a" message))
    (catch up
      (let ((previous-up **up**))
        (bind ((*break-level* (fx+ *break-level* 1))
               (**up** up)
               (**ret** ret))
          (repl repl-input repl-output)
          (previous-up))))
    ;; a throw to up comes here.
    (t-breakpoint-aux nil repl)))

;;; read-eval-print loop.
;;; typing end-of-file (^z or ^d) is the only way this can ever return.

(define (read-eval-print-loop in out)
  (iterate loop ()
    (fresh-line (out))
    (prompt (out) ((repl-prompt) *break-level*))
    (let ((form ((repl-read) (in))))
      (cond ((eof? form) form)
            (else
             (receive vals
                      ;; evaluate the user's form.
                      ((repl-eval) form (repl-env))
               (cond ((null? vals)
                      (format (out) "~&;no value")
                      (loop))
                     ((not (null? (cdr vals)))
                      (set (repl-results) vals)
                      (format (out) "~&;multiple values:")
                      (do ((l vals (cdr l))
                           (i 0 (fx+ i 1)))
                          ((null? l) (loop))
                        (format (out) "~% [~s] " i)
                        ((repl-print) (car l) (out))))
                     ((not (repl-wont-print? (car vals)))
                      ;; single value
                      (set (repl-results) vals)
                      ((repl-print) (car vals) (out))
                      (loop))
                     (else (loop)))))))))

(define repl-results
  (let ((weak (make-weak-cell (list (undefined-value "##")))))
    (object (lambda () (weak-cell-contents weak))
      ((setter self)
       (lambda (val)
         (let ((val (enforce list? val)))
           (set (weak-cell-contents weak) val)
           val))))))

(define-simple-switch repl-prompt  procedure?   standard-prompt)
(define-simple-switch repl-read    procedure?   read)
(define-simple-switch repl-eval    procedure?   eval)
(define-simple-switch repl-print   procedure?   print)
(define-simple-switch repl-input   input-port?  (standard-input))
(define-simple-switch repl-output  output-port? (standard-output))
(define-simple-switch repl-env     environment? t-implementation-env)

(define (initialize-repl env)
  (set (repl-results)    (list (undefined-value "##")))
  (set (repl-prompt)     standard-prompt)
  (set (repl-read)       read)
  (set (repl-eval)       eval)
  (set (repl-print)      print)
  (set (repl-input)      (standard-input))
  (set (repl-output)     (standard-output))
  (set (repl-env)        env))

;;; random stuff.

(define (standard-prompt level)         ; arg is # of repls on stack.
  (case level
    ((0) "> ")
    ((1) ">> ")
    ((2) ">>> ")
    ((3) ">>>> ")
    (else
     (string-append (map-string! (always #\>)
                                 (make-string (fx+ level 1)))
                    " "))))

(define (alternate-prompt level)
  (case level
    ((0) "> ")
    ((1) "1: ")
    ((2) "2: ")
    ((3) "3: ")
    (else (format nil "~s: " level))))

;;; some commands.
;++ These belong elsewhere.  There should be a file command that implements
;++ command loops.

(define-integrable (current-frame)
  (escape-procedure-frame **ret**))

(define (backtrace)
  (*backtrace (current-frame)))

(define (crawl . rest)
  (apply *crawl (repl-env) rest))

(define (debug)
  (*crawl (repl-env) (current-frame)))

(define-syntax (pp form)
  (cond ((symbol? form)
         `(*pp-symbol ',form (repl-env)))
        (else
         `(*pp ,form))))
