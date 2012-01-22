(herald vmfiles)

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

(block
  (define *vm-modules*
          '((osys aem68kernel)
            (osys m68kernel)
	    (osys m_dispatch)
            (t3_primops mconstants)
            (osys m68lap)
            (t3_primops locations) ;++ talk to RK about this
                                      ; operation needs locations
            (osys vm_boot)
            (osys kernel)

            ;; Continuation stuff
            (osys handler)   ; must occur before any DEFINE-HANDLER
            (osys frame)
            (osys throw)
            (osys operation)       ; kernel needs %operation

            ;; VM error system
            (osys error)
            (osys aegis_fault)

            ;; Primitive procedures
            (osys fixnum)
            (osys list)
            (osys map)        ; uses list
            (osys character)  ; uses list
            (osys string)     ; uses list, character
            (osys vector)     ; uses list 
            (osys symbol)     ; uses string
            (osys struct)     ; uses list

            ;; closed compiled primops
            (t3_primops m68primops)
            (t3_primops m68arith)
;++         (t3_primops locations)  ; setter is closed compiled
            (t3_primops m68low)
            (t3_primops predicates)
            (t3_primops open)
            (t3_primops aliases)
            (t3_primops carcdr)
            (t3_primops genarith)

            ;; Storage management
            (osys weak)
            (osys free)
            (osys pool)
            (osys string_buf)
            (osys table)
            (osys table_entry)
            (osys table_util)
            (osys weak_table)
            (osys hash)        ; must come after weak_table
            (osys inf_vector)

            ;; VM file system
            (osys buffer)
            (osys port_op)
            (osys ae_vmport)
            (osys vm_port)

            ;; VM system
            (osys environment)  ; must preceed any DEFINE-SYNTAX
            (osys vm_system)))

  ;;; Closed compiled versions of primops.  In a system which doesn't
  ;;; contain an interpreter these files are not necessary.

  (define *closed-compiled-primops*
          '((t3_primops m68constants)
            (t3_primops m68primops)
            (t3_primops m68arith)
;++            (t3_primops locations)  ; setter is closed compiled
            (t3_primops m68low)
            (t3_primops predicates)
            (t3_primops open)
            (t3_primops aliases)
            (t3_primops carcdr)
            (t3_primops garbage)
            ))

  ;;; Z system

  ;;; The Z system depends only on the VM
  (define *z-system-modules*
          '((osys zread)
            (osys zprint)
            (osys zeval)
            (osys zload)          ; not neaded if loader is not present
            (osys zsystem)))

  (define *bootstrap-z-system*
    (append *vm-modules*
            *z-system-modules*))

  ;;; Garbage collector, scanner, suspender

  (define *garbage-collector*
          '((osys scanner)
        ;    (osys gc_auxiliary)
        ;    (osys suspender)
            (osys aegis_gc)
            (osys gc_weak)
            (osys gc)
            (osys gc_top)
            ))

  ;;; Loader
  ;;; Storage management must be present in order to use the loader.

  (define *loader-modules*
          '((osys dump_codes)
            (osys retrieve)
;            (osys old_retrieve) ;++ flush later
            (osys m68_comex)
;            (osys aegis_foreign)
            (osys load_comex)
            ))             ; this could come later

  (define *operation-dispatch-modules*
        '(
          (osys condition)    ; Exceptions
          (osys location)
          ))

  (define *ZVM-system*     ; VM with Z system
    (append
            *vm-modules*
            *z-system-modules*
            *garbage-collector*
            *loader-modules*
            *operation-dispatch-modules*
            '((osys aegis)
              (osys z_undefined))
            ))



  (define *VM-system*      ; VM as a runtime library
    (append
            *vm-modules*
            *garbage-collector*
            *loader-modules*))

  ;;; Object and operation stuff


  ;;; Arithmetic - the order of arithmetic modules is important

  (define *arithmetic-modules*
          '((osys ratio)
            (osys aegis_float)
            (osys ieee_float)    
            (osys arith)
            (osys m68_bignum)
            (osys big_fixnum)
            (osys big_util)
            (osys bignum)
            (osys big_arith)
            (osys dispatch)
            (osys random)
            (osys equality)
            ;(osys single_float)
            ;(osys double-float)
            ;(osys complex)
            ))

  (define *file-system-modules*
        '(
         (osys port)
         (osys aegis_port)
         (osys sort)
         (osys herald)
         (osys load)
         (osys dump)
         (osys dump_comex)
         ))


  ;; Syntax system
  (define *syntax-system-modules*
          '(
            (osys syntax)
            (osys pattern)
            (osys sp_form)
            (osys macros)        ; ENV calls MAKE-TABLE which calls GC_STAMP
            (osys cond)
            (osys let)
            (osys quasiquote)
            (osys object)       ; object macro
            (osys modify)
            ))

  (define *t-repl-modules*
      '((osys readtable)
        (osys recognize)
        (osys read)
        (osys sym_printer)
        (osys format)
        (osys pfloat)
        (osys eval)
        (osys repl)
        (osys fs_parse)
        (osys fs)
        (osys tree)
        (osys sets)
        (osys combinators)
        (osys transcript)
        (osys  exports)
        (osys aegis_macro)  ; macro's for foreign calls; export's things
        (osys obsolete)
        (osys tsystem)
        ))


  (define *inspector-modules*
      '((osys crawl)
        (osys debug)
        (osys trace)
        (osys pp)
	(osys aegis_timer)
        ))

  (define *t-system*
    (append
            *vm-modules*
            *z-system-modules*
            *garbage-collector*
            *loader-modules*
            *operation-dispatch-modules*
            '((osys aegis))
            *arithmetic-modules*
            *file-system-modules*
            *syntax-system-modules*
            *inspector-modules*
            *t-repl-modules*
            ))

  (define *t-modules*
    (append *vm-modules*
            *z-system-modules*
            *garbage-collector*
            *loader-modules*
            *operation-dispatch-modules*
            *arithmetic-modules*
            *file-system-modules*
            *syntax-system-modules*
            *inspector-modules*
            *t-repl-modules*))


  (define *library-modules*
          '(
            (osys loop)
            (osys for)
            (osys moremaps)
            ))

)
