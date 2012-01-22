(herald obsolete (env tsys))

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


;;; Definitions for backwards compatibility.

(define-local-syntax (define-obsolete old new)
  `(block
     (define ,old (%obsolete ',old ',new ,new))
     (declare-tsys-exports ',old)))

(define-local-syntax (define-obsolete-value old new)
  `(block
     (define ,old ,new)
     (declare-tsys-exports '(,old . ,new))))


(let ((msg "call to a system procedure using an obsolete name."))
  (define (%obsolete old-name new-name new)
    (let ((warned? nil))
      (object (lambda args
                (cond ((and (not warned?) *mention-obsolete-references?*)
                       (warning '("~a~%"
                                  ";**~12t ~S has been renamed to be ~S.~%")
                                msg
                                old-name
                                new-name)
                       (set warned? t)))
                (apply new args))
        ((print-type-string self) "Obsolete")
        ((identification self) old-name)
        ((setter self) (setter new))))))

(let ((msg0 "~s has changed in version ~s.~s (~s)~%~s~%")
      (msg1 "see the T3 manual or release notes for details."))
  (define (%system-change name proc message)
    (let ((warned? nil))
      (object (lambda args
                (cond ((and (not warned?) *mention-changes?*)
                       (warning msg0
                                name
                                (major-version t-system)
                                (minor-version t-system)
                                (link-edit  t-system)
                                message
                                msg1)
                       (set warned? t)))
                (apply proc args))
              ((print-type-string self) "System Change")
              ((identification self) name)
              ((setter self) (setter proc))))))

;;; DEFUNCT - adj. : having finished the course of life or existence -
;;; Webster's 9th.  It is used to inform users of defunct procedures.

(define-local-syntax (define-defunct old . info)
  `(block
     (define ,old (%defunct ',old ,@info))
     (declare-tsys-exports ',old)))

(let ((msg "Attempt to call a defunct procedure - ~a ~a"))
  (define (%defunct old-name . info)
    (object (lambda args
              (let ((aux (if info
                             (format nil "~%**~10t~a" (car info))
                             "")))
                (error msg `(,old-name ,@args) aux)))
        ((print-type-string self) "Defunct")
        ((identification self) old-name))))


(lset *mention-obsolete-references?* t)
(lset *mention-changes?* t)

;;; Add new entries to bottom of list, with date added.

;;; T3 changes - 18 March 1986

(define-obsolete-value *t-implementation-env*  t-implementation-env)
(define-obsolete-value *standard-read-table*   standard-read-table)
(define-obsolete-value *vanilla-read-table*    vanilla-read-table)
;(define-obsolete-value *standard-syntax-table* standard-syntax-table)
(define-obsolete-value *read-keyword-table*    read-keyword-table)
(define-obsolete-value *eof*                   eof)
(define-obsolete-value *repl-wont-print*       repl-wont-print)
(define-obsolete-value *number-of-char-codes*  number-of-char-codes)
(define-obsolete-value *min-fixnum*            most-negative-fixnum)
(define-obsolete-value *max-fixnum*            most-positive-fixnum)
(define-obsolete-value *t-version-number*      version-number)

(define-obsolete stream?                   port?)
(define-obsolete input-stream?             input-port?)
(define-obsolete output-stream?            output-port?)
(define-obsolete interactive-stream?       interactive-port?)
(define-obsolete stream-read-table         port-read-table)
(define-obsolete stream-filename           port-name)
(define-obsolete make-output-width-stream  make-output-width-port)
(define-obsolete make-broadcast-stream     make-broadcast-port)
(define-obsolete make-echo-stream          make-echo-port)
(define-defunct string->input-stream)
(define-defunct make-output-to-string-stream)

;;; Called from macro expansions.

(define-obsolete with-open-streams-handler with-open-ports-handler)

(define-obsolete fxrem                     fx-rem)
(define-obsolete div                       quotient)
(define-obsolete values                    return)

(define-defunct *the-symbol-table*)
(define-defunct make-symbol)
(define-defunct intern)
(define-defunct really-intern)
(define-defunct interned?)
(define-defunct walk-symbol-table "see WALK-ALL-SYMBOLS")
