(herald (front_end oload))

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

(define *orbit-files*
    '((front_end free_stuff)
      (top       o_imports)
      ))

(define *top-files*
    '(
      (top defs)
      (top util)
      (top primop)
      (top o_syntax)        ;** changed 12 July 86
      (top top)
      ))

(define *front-files*
    '((front_end let_nodes)
      (front_end type_sets)
      (front_end types)
      (front_end type)
      (front_end envs)
      (front_end nodestuff)
      (front_end shape)
      (front_end expand)
      (front_end alpha)
      (front_end declare)
      (front_end node)
      (front_end assign)

      (front_end simplify)
      (front_end simpfy_call)
      (front_end simpfy_let)
      (front_end param)
      (front_end simplifiers)
      (front_end simplify_y)
      (front_end fx_const)

      (front_end safe)
      (front_end inf_files)
      (front_end gen_inter)
      (front_end fixup)
      (front_end user)
      (front_end user_error)
      (front_end module)
      (front_end analyze)
      (front_end front)
      ))

(define *back-end-files*
    '((back_end strategy)
      (back_end live)
      (back_end closure)
      (back_end type)
      (back_end comex)
      (back_end bookkeep)
      (back_end generate)
      (back_end generate_y)
      (back_end parassign)
      (back_end reg)
      (back_end lookup)))

(define *orbit-m68-files*
    '((back_end m68emit)
      (back_end m68bookkeep)
      (back_end m68gen)
      (back_end m68locgen)
      (back_end m68arithgen)
      (back_end m20arithgen)
      (back_end m68rep)
      ))

(define *orbit-vax-files*
    '((back_end vaxemit)
      (back_end vaxbookkeep)
      (back_end vaxgen)
      (back_end vaxlocgen)
      (back_end vaxarithgen)
      (back_end vaxrep)
      ))

(define *tas-files*
    '((assembler as_open)      ;; for assembler & machine descriptions
      (assembler as_utils)     ;; utilities for the assembler
      (assembler as)           ;; client compiler interface
      ;; the assembler
      (assembler fg)
      (assembler ib)
      (assembler count)
      (assembler mark)
      (assembler bits)
      (assembler listing)
      ;; lap user interface
      (assembler lap)
      ))

(define *tas-m68-files*
      '((assembler mini)
      (assembler as_m68)
      (assembler m68am)
      (assembler m68is1)
      (assembler m68is2)
      (assembler m682is)
      ))

(define *tas-vax-files*
    '((assembler vaxmini)
      (assembler as_vax)
      (assembler vmodes)
      (assembler vaxam)
      (assembler vaxi)
      (assembler vaxis)
      ))

nil)
