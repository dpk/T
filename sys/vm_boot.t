(herald vm_boot (env tsys))

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

;;;; System startup

;;; This is where the startup sequence (GENESIS in e.g. aem68kernel)
;;; jumps; right into the eval-mungeables of this file.  We must
;;; not return - the kernel hasn't prepared for that possibility.
;;; All it has done at this point is set up the stack, heap memory,
;;; global constants, the root process, and the boot argument vector.
;;; *THE-INITIAL-MODULES* is defined by the linker.

;;; Initialize the kernel

;;; Unsuspended boot.  Assumes that evaluating top level forms will
;;; not blow out the initial area.  When a system is suspended *boot*
;;; is set to SUSPENDED-BOOT.  Note: *boot* must be defined and
;;; not lset or big_bang will not be able to jump to *boot*.  This
;;; is the only variable in the system with this constraint.

(define (*boot* root-process boot-args debug?)
  ;; Grock the initial modules.
  (ignore boot-args root-process)
  (set (system-global slink/boot-area-base) (make-vector 0))
  (set (system-global slink/initial-impure-base)
       (vref *the-initial-modules* 0))
  (let ((len (vector-length *the-initial-modules*)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i len))
      (let ((unit (vref *the-initial-modules* i)))
        (if debug? (print-module-name unit))
        ;++ fix this when modules work, or maybe when adjusting units
        (set (unit-env unit) tvm-env)
        ((unit-top-level-forms unit))))
    (if debug? (%vm-boot-write-tty '...Groked.))
    (initialize-systems)
    (top-level)))

(define (print-module-name unit)
  (let ((id (car (unit-herald unit))))
    (cond ((symbol? id)
           (%vm-boot-write-tty id))
          ((list? id)
           (%vm-boot-write-tty (cadr id)))
          (else
           (%vm-boot-write-tty '*****uluz****)))))

;*** The following procedure(s) is (are) in this file because they
;*** are only used when bootstrapping the VM.

;;; Move the initial symbols into the symbol table.


(define (initialize-symbol-table)
  (let ((tlen (vector-length the-symbols))
        (len  (vector-length *the-initial-symbols*)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i len))
      (let* ((symbol (vref *the-initial-symbols* i))
             (index  (fx-rem (symbol-hash symbol) tlen)))
        (set (vref the-symbols index)
             (cons symbol (vref the-symbols index)))))))

;;; BOOTSTRAP-ENV is a global environment for use before LOCALES are available.

(define (bootstrap-env id local? create?)
  (ignore local?)
  (cond ((assq id *boot-env*)
         => cdr)
        (create?    
         (let ((v (make-vcell id)))
           (push *boot-env* (cons id v))
           v))
        (else nil)))

;;; MAKE-BASE-ENVIRONMENT is used to build the T-IMPLEMENTATION-ENV
;;; which will replace the *z-repl-env*.  It cannot be called until
;;; environments are available.  Note: the *boot-env* can only
;;; contain symbols as identifiers.

(define (make-base-environment id)
  (let ((table (make-symbol-table-with-size (length *boot-env*) id)))
    (walk (lambda (entry) 
            (set (table-entry table (car entry)) (cdr entry)))
          *boot-env*)
    (really-make-locale nil id table)))

;;; BOOT-ADJUST-INITIAL-UNITS is used to set the herald and source
;;; filenames of the initial units.  It cannot be called until
;;; operations are enabled, adjust-unit-names, and filenames are
;;; available.

(define (boot-adjust-initial-units)
  (table-walk (weak-table-table code-unit-table)
              (lambda (code unit)
                (ignore code)
                (adjust-unit-names unit))))
