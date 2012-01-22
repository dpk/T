(herald crawl (env tsys))

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

;;;; structure and stack crawler

(lset *crawl-env*   nil)
(lset *crawl-stack* nil)
(lset *crawl-quit*  nil)
(lset *crawl-args*  nil)              
(lset **cont** nil)

;;; top-level entry for crawl.

(define (*crawl env . objects)
  (catch quit
         (bind ((*print-level* 4)
                (*print-length* 6)
                (*crawl-quit* quit)     ; only for benefit of q command
                (*crawl-stack* '())
                (*crawl-args* nil)
                (*crawl-env* env))
           (walk crawl-push (reverse! objects))
           (t-breakpoint-aux nil crawl-command-loop))))

;;; prompt before reading new command line.
;;; don't prompt after an empty command line.
;;; do all commands read on one line.

(define (crawl-command-loop in out)
  (iterate next-command-line ((previous *crawling-nothing*) (hack t))
    (let ((obj (cond ((null? *crawl-stack*) *crawling-nothing*)
                     (else (car *crawl-stack*)))))
      (cond (hack
             (cond ((neq? obj previous)
                    (crawl-synopsis obj (out))))
             (fresh-line (out))
             (prompt (out) (crawl-prompt obj))))
      (let ((line (read-line (in))))
        (cond ((eof? line) repl-wont-print)       ; exit
              (else
               (let ((l (read-objects-from-string line)))
                 (cond ((null? l)
                        (next-command-line obj nil))
                       (else
                        (set *crawl-args* l)
                        (iterate loop ()
                          (cond ((null? *crawl-args*)
                                 (next-command-line obj t))
                                (else
                                 (let ((command (pop *crawl-args*))
                                       (next (car *crawl-stack*)))
                                   (cond ((get-crawl-command command)
                                          => (lambda (z) (z next)))
                                         ((maybe-crawl-component next command))
                                         (else
                                          (format (out) "illegal command.~%")))
                                   (loop))))))))))))))

(define *crawling-nothing*
  (object nil
          ((crawl-synopsis self out) (ignore out) nil)  ; do nothing
          ((identification self) '*crawling-nothing*)))

(define-operation (crawl-synopsis obj out)
  (print-one-line obj out)
  (fresh-line out)
  (cond ((frame? obj) 
         (frame-print-synopsis obj out))
        ((unit? obj)
         (format out " source = ~s  size = ~s"
                 (unit-source-filename obj)
                 (unit-length obj)))))

(define-operation (crawl-prompt obj)
  (cond ((frame? obj) "debug: ")
        (else         "crawl: ")))

;;; predicate: returns true if a component with the given name was
;;; pushed.

(define-operation (maybe-crawl-component obj command)
  (cond ((and (structure? obj)
              (any (lambda (x) (if (eq? command (selector-id x)) x nil))
                   (stype-selectors (structure-type obj))))
         => (lambda (sel) (crawl-push (sel obj))))
        ((frame? obj)
         (maybe-crawl-frame-component obj command))
        ((and (closure? obj) 
              (not (template-internal-bit? (extend-header obj))))
         (receive (pointer scratch) (closure-size-info obj)
           (cond ((and (fixnum? command)
                       (fx>= command 0)
                       (fx< command pointer))
                  (crawl-push (extend-pointer-elt obj command)))
                 (else nil))))
        (else nil)))
                                                  
;;; horrible stuff for T3 frames

(define (maybe-crawl-frame-component frame command)
  (if (and (fixnum? command) (fx>= command 0))
      (let ((prev (previous-continuation frame)))
        (iterate loop ((frame frame) (i 0))
          (cond ((eq? frame prev) nil)  
                ((not (frame? frame))
                 (loop (frame-previous frame) i))
                (else
                 (receive (pointer scratch) (closure-size-info frame)
                   (cond ((fx< command (fx+ i pointer))
                          (crawl-push (extend-pointer-elt frame (fx- command i))))
                         (else
                          (loop (frame-previous frame) (fx+ i pointer)))))))))
      nil))


(define (crawl-exhibit-frame frame)
  (let ((prev (previous-continuation frame)))
    (iterate loop ((frame frame) (i 0))      
      (cond ((eq? frame prev) t)
            ((not (frame? frame))
             (loop (frame-previous frame) i))
            (else
             (receive (pointer scratch) (closure-size-info frame)
               (exhibit-standard-extend frame pointer scratch i)
               (loop (frame-previous frame) (fx+ i pointer))))))))



;;; the commands:

(define-local-syntax (define-crawl-command pat symbol doc . body)
  `(block (define-operation ,pat . ,body)
          (*define-crawl-command  ',symbol
                                  ',doc
                                  ,(car pat))
          ',symbol))

(define (*define-crawl-command symbol doc op)
  (let ((com (join op (object nil
                       ((print-crawl-help self)
                        (format (terminal-output) "  ~a  ~a~%" symbol doc))))))
    (push *the-crawl-commands* com)
    (set (table-entry *crawl-command-table* symbol)
         com)))

(lset *the-crawl-commands* '())

(define *crawl-command-table* (make-table '*crawl-command-table*))

(define (get-crawl-command command)
  (if (symbol? command) (table-entry *crawl-command-table* command) nil))

(define-operation (print-crawl-help obj))

(define-crawl-command (crawl-help obj) ?
  "print summary of inspector commands."
  (walk print-crawl-help *the-crawl-commands*)
  (format (terminal-output)
          " the a, c, and e commands will prompt for an expression.~%"))

(define-crawl-command (crawl-apply obj) a
  "apply a procedure to the current object."
  (let ((z (crawl-read "call what procedure? ")))
    (cond ((eof? z) nil)
          (else
           (let ((proc (eval-in-crawled-env z obj)))
             (cond ((procedure? proc)
                    (receive-values crawl-receiver
                                    (lambda () (proc obj))))
                   (else
                    (format (terminal-output) "~s is inapplicable.~%"
                            proc))))))))

(define (crawl-receiver . vals)
  (cond ((null? vals)
         (format (terminal-output) "no values.~%"))
        ((not (null? (cdr vals)))
         (format (terminal-output)
                 "~s values.  successive u commands will inspect them.~%"
                 (length vals))))
  (walk crawl-push (reverse! vals)))

(define-crawl-command (crawl-break obj) b
  "enter a read-eval-print loop in an appropriate environment."
  (let ((env (or (get-environment obj)
                 (repl-env))))
    (format (terminal-output)
            "~&breakpoint in ~s~%  with *obj* = ~s~%"
            env
            obj)
    (breakpoint nil
                (eval `((lambda (*obj*) (the-environment)) ',obj)
                      env))))

(define-crawl-command (crawl-crawl obj) c
  "inspect another object."
  (let ((z (crawl-read "inspect what object? ")))
    (cond ((not (eof? z))
           (receive-values crawl-receiver
                           (lambda () (eval-in-crawled-env z obj)))))))

(define-crawl-command (crawl-down obj) d
  "go to next deeper continuation (i.e. stack frame)."
  (cond ((frame? obj)
         (let ((prev (previous-continuation obj)))
           (cond ((null? prev)
                  (format (terminal-output)
                          "you are at the bottom of the stack.~%"))
                 (else
                  (crawl-push prev)))))
        (else (bad-crawl-command))))
                         
(define-crawl-command (crawl-eval obj) e
  "evaluate an expression in current object's environment."
  (let ((z (crawl-read "evaluate what? ")))
    (cond ((not (eof? z))
           ((repl-print) (eval-in-crawled-env z obj) (terminal-output))
           (newline (terminal-output))))))

(define (cont) (**cont** nil))

(define-crawl-command (crawl-return-and-save obj) k
  "return a value to a continuation, and save debugger state."
  (let ((really-return
         (lambda (frame)
           (let ((val (crawl-read "return what value? (eof to abort) ")))
             (cond ((eof? val) nil)
                   (else 
                    (format t "Do (CONT) to return to debugger at this point.~%")
                    (call-with-current-continuation 
                     (lambda (k)
                       (set **cont** k)
                       (receive values (eval-in-crawled-env val obj)
                         (frame-throw frame values))))))))))
    (cond ((frame? obj)
           (really-return obj))
          ((escape-procedure? obj)
           (really-return (escape-procedure-frame obj)))
          (else
           (format (terminal-output) "k is meaningless here.~%")))))


(define-crawl-command (crawl-show-env obj) l
  "list values of lexical variables out to nearest locale."
  (crawl-exhibit-env (get-crawl-env obj)))

(define-operation (crawl-exhibit-env env)
  (format (terminal-output) "no local variables.~%"))

(define-crawl-command (crawl-macro-expand obj) m
  "macro-expand current object, and pretty-print the expansion."
  (let ((x (macro-expand obj (env-syntax-table (get-crawl-env obj)))))
    (crawl-pp x)
    (crawl-push x)))

(define (crawl-pp obj)
  (let ((out (terminal-output)))
    (fresh-line out)
    (pretty-print obj out)
    (fresh-line out)))

(define-crawl-command (crawl-next obj) n
  "go to next object on stack."
  (let ((prev (frame-previous obj)))
    (cond ((null? prev)
           (format (terminal-output)
                   "you are at the bottom of the stack.~%"))
          (else
           (crawl-push prev)))))


(define-crawl-command (crawl-print obj) p
  "pretty-print current object."
  (crawl-pp (or (cond ((frame? obj) (frame-disclose obj))
                      (else (disclose obj)))
                obj)))

(define-crawl-command (crawl-quit obj) q
  "exit the inspector."
  (*crawl-quit* repl-wont-print))

(define-crawl-command (crawl-return obj) r
  "return a value to a continuation, continuing execution at that point."
  (let ((really-return
         (lambda (frame)
           (let ((val (crawl-read "return what value? (eof to abort) ")))
             (cond ((eof? val) nil)
                   (else 
                    (receive values (eval-in-crawled-env val obj)
                      (frame-throw frame values))))))))
    (cond ((frame? obj)
           (really-return obj))
          ((escape-procedure? obj)
           (really-return (escape-procedure-frame obj)))
          (else
           (format (terminal-output) "r is meaningless here.~%")))))

(define-crawl-command (crawl-up obj) u
  "go back to inspecting previous object."
  (cond ((null? (cdr *crawl-stack*))
         (format (terminal-output) "you can't go up from here.~%"))
        (else
         (pop *crawl-stack*))))

(define-crawl-command (crawl-to-unit obj) v
  "inspect current object's unit (compiled module)."
  (cond ((template? obj)
         (crawl-push (template-unit obj)))
        ((closure? obj)
         (crawl-push (template-unit (extend-header obj))))
        (else (bad-crawl-command))))

(define-crawl-command (crawl-where-defined obj) w
  "give file name of current object's definition."
  (format (terminal-output) "~s~%" (where-defined obj)))

(define-crawl-command (crawl-show obj) x
  "display object's contents or other relevant information."
  (crawl-exhibit obj))

(define-crawl-command (crawl-display obj) =
  "print object, its hash, and its address."
  (format (terminal-output) " ~s~_=~_~s~_=~_(~s #x~x)~2_<~s>~%"
          obj
          `(unhash ,(object-hash obj))
          'fixnum->descriptor
          (descriptor->fixnum obj)
          (or (points-to-reasonable-memory obj)
              'random)))

;;; utilities.

(define (crawl-push obj)                ; must return true.
  (push *crawl-stack* obj)
  t)

(define (get-crawl-env obj)
  (or (and (not (structure? obj)) (get-environment obj)) *crawl-env*))

;;; careful!  this may return multiple values.

(define (eval-in-crawled-env form obj)
  ;; if stack frame, we should bind the dynamic context, yes?
  ((repl-eval) form (get-crawl-env obj)))

(define (crawl-read prmpt)
  (cond ((null? *crawl-args*)
         (prompt (terminal-output) prmpt)
         (read (terminal-input)))
        (else (pop *crawl-args*))))

;;; the moby x command.

(define-operation (crawl-exhibit obj)
  (cond ((not (closure? obj)) nil)
        ((frame? obj)
         (crawl-exhibit-frame obj))                      
        ((template-internal-bit? (extend-header obj))
         (format (terminal-output) '("Object is internal to a closure~a "
                                     "the v command will inspect it~%") 
                                   #\semicolon))  ; uluz 
        (else
         (receive (pointer scratch) (closure-size-info obj)
           (exhibit-standard-extend obj pointer scratch 0)))))

(define (crawl-print-component selector obj)
  (let ((out (terminal-output)))
    (format out " [~s] " selector)
    (print-one-line obj out)
    (newline out)))

(define (exhibit-standard-extend obj ptr-size scr-size start)
  (iterate loop ((i 0) (j start) (previous nil) (repeating? nil))
    (cond ((fx>= i ptr-size)
           (cond ((fx> scr-size 0)
                  (if (fx> ptr-size 0) (format (terminal-output) " and"))
                  (format (terminal-output) " ~s scratch slots~%" scr-size))))
          (else
           (let ((thing (extend-pointer-elt obj i)))
             (cond ((or (neq? thing previous)
                        (fx= i 0)
                        (fx= i (fx- ptr-size 1)))
                    (crawl-print-component j thing)
                    (loop (fx+ i 1) (fx+ j 1) thing nil))
                   ((not repeating?)
                    (format (terminal-output) " ...~%")
                    (loop (fx+ i 1) (fx+ j 1) thing t))
                   (else
                    (loop (fx+ i 1) (fx+ j 1) thing t))))))))

(define (bad-crawl-command)
  (format (terminal-output)
          "there is no way to go in that direction.~%"))

;;; reverse command list for prettiness in help.

(set *the-crawl-commands* (reverse! *the-crawl-commands*))
