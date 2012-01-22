(herald kernel (env tsys))

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

;;; Random low level global variables.

(lset *suspended-system*    nil)
(lset **reset**             nil)
(lset *top-level*           nil)

;;; *BREAK-LEVEL* is set to 0 during the evaluation of Z top level
;;; forms.  If we happen to incur some sort of error while doing
;;; system initialization the breakloop will start up at level one,
;;; rather than zero.
;++ we could set it up so that it printed the module if *break-level*
;++ is greater than n.

(lset *break-level*    0)
(lset **ret**          nil)
(lset *z?*             t)

;;; The entry and re-entry into the kernel after boot-strapping.

(define (TOP-LEVEL)
  (catch top
    (set *break-level* -1)
    (set **reset** top)
    (*top-level*))
  (top-level))

(define (RESET) (**reset**))

(define (breakpoint . args)
  (apply (if *z?* z-breakpoint t-breakpoint) args))

(define (ret . vals)
  (apply **ret** vals))

;;; Random low level procedures and objects.

(define-integrable (true . args)  (ignore args) '#t)
(define-integrable (false . args) (ignore args) '#f)
(define-integrable (true? x)      (if x '#t '#f))

;;; GC-STAMP must be present even if the GC isn't.

(define-integrable (gc-stamp)
  (system-global slink/gc-stamp))

(define (increment-gc-stamp)
  (defer-interrupts
    (set (system-global slink/gc-stamp)
         (fx+ (system-global slink/gc-stamp) 1))
    (system-global slink/gc-stamp)))

;;; "Reasonableness" predicate.  This should cons as little as
;;; possible in order to use it debugging gc. This may become hairier.

(define (reasonable? obj)
  (select (descriptor-tag obj)
    ((tag/fixnum) t)
    ((tag/immediate)
     (or (template-header? obj)
         (char? obj)
         (nonvalue? obj)
         (true-header? obj)))
    ((tag/pair)
     (points-to-reasonable-memory obj))
    (else
     (if (points-to-reasonable-memory obj)
         (cond ((template? obj) t)
               ((template? (extend-header obj))
                (points-to-reasonable-memory (extend-header obj)))
               (else                     
                (fx>= (vector-length obj) 0)))
         nil))))

(define (points-to-reasonable-memory obj)
  (let ((first (descriptor->fixnum obj)))
    (cond ((and (fx>= first (system-global slink/initial-pure-memory-begin))
                (fx< first (system-global slink/initial-pure-memory-end)))
           'initial-pure)
          ((and (fx>= first (system-global slink/initial-impure-memory-begin))
                (fx< first (system-global slink/initial-impure-memory-end)))
           'initial-impure)
          ((and (fx>= first (process-global task/area-begin))
                (fx< first (process-global task/area-frontier)))
           'heap)
          ;; remember the stack is high to low memory, but extends
          ;; are oriented in the same direction whether on the stack
          ;; or not.
          ((and (fx>= first (stack-pointer))
                (fx<= first (process-global task/stack)))
           'stack)
          (else nil))))

(define (points-to-initial-impure-memory? obj)
  (let ((first (descriptor->fixnum obj)))
    (and (fx>= first (system-global slink/initial-impure-memory-begin))
         (fx< first (system-global slink/initial-impure-memory-end)))))


;;; repl-wont-print

(define-predicate repl-wont-print?)

(define repl-wont-print
  (object nil
    ((repl-wont-print? self) t)
    ((identification self) 'repl-wont-print)))


;;; Undefined Value Note: It would be nice if undefined-value closed
;;; over the environment in which it was called.  Thus, for example,
;;; being able to print the name of the procedure in which the
;;; undefined value was created.  UNDEFINED-EFFECT is handled by
;;; the compiler.
 
;;; Enforce ;++ move to error?

(define (*enforce type obj)
  (catch caller
    (if (type obj) 
        obj
        (*enforce type
                   (error "(~s ~s ~s) failed in ~a"
                          'enforce
                          (or (identification type) type)
                          obj
                          (let* ((frame (escape-procedure-frame caller))
                                 (thing (cond ((interpreter-frame? frame)
                                               (interpreter-frame-code frame))
                                              (else
                                               (extend-header frame)))))
                            (or (get-proc-name thing)
                                "(anonymous)")))))))

(define (*check-arg predicate expression where)
  (if (predicate expression)
      expression
      (*check-arg predicate
        (error "some argument didn't answer true to ~S as expected~
                ~%  (~S ... ~S ...)"
                (or (identification predicate) predicate)
                (or (identification where) where) expression)
                where)))

(define value-of-assert (undefined-value 'assert))

(define (assert something)
  (cond ((not something)
         (error "assertion failed")))
         value-of-assert)

;;; NO-OP prevents compiler optimizations.  It must be an unknown
;;; procedure.

(define (no-op x) x)

;;; Modularity wins again.
;;; This is one of the places that having optional arguments be
;;; returned as a list loses, since we can't distinguish between
;;; no argument and '() as an argument.
;++ the arg order has changed from that of t2.  Release.

(define (make-simple-switch id type . val)
  (let ((s (lambda (new) (set val (check-arg type new id)))))
    (if (null? val) (set val (undefined-value id)) (s (car val)))
    (object (lambda () val)
      ((setter self) s)
      ((identification self) id)
      ((print-type-string self) "Switch"))))

;;; Low level switches

(define-simple-switch z-system-present?     boolean? '#f)
(define-simple-switch gc-present?           boolean? '#f)
(define-simple-switch loader-present?       boolean? '#f)
(define-simple-switch tables-present?       boolean? '#f)
(define-simple-switch file-system-present?  boolean? '#f)
(define-simple-switch syntax-present?       boolean? '#f)

(define (make-agenda id . agenda)
  (object (lambda ()
            (do ((l agenda (cdr l)))
                ((null? l))
              ((car l))))
    ((insert self item)
     (set agenda (append agenda (list item)))
     (no-value))
    ((delete self item)
     (set agenda (delq! item agenda)) 
     (no-value))
    ((identification self) id)
    ((print-type-string self) 'agenda)))

;;; Random global procedures

(define (format port fmt . args)
  (let ((port (if (eq? port t) (standard-output) port)))
    (cond (*z?*
           (z-format-aux port fmt args '#t))
          (else
           (t-format-aux port fmt args (interactive? port))))))

(define (prompt fd fmt . args)
  (cond (*z?*
         (z-format-aux fd fmt args '#t))
        (else
         (t-format-aux fd fmt args '#t))))

(define (dbg fmt . args)
  (cond (*z?*
         (z-format-aux (debug-output) fmt args '#t))
        (else
         (t-format-aux (debug-output) fmt args '#t))))
