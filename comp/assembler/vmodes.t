(herald vmodes)

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

(define-constant *vax$ab* '(general 8 byte address))
(define-constant *vax$aw* '(general 16 word address))
(define-constant *vax$al* '(general 32 long address))
(define-constant *vax$aq* '(general 64 quad address))
(define-constant *vax$af* '(general 32 f-float address))
(define-constant *vax$ad* '(general 64 d-float address))
(define-constant *vax$ag* '(general 64 g-float address))
(define-constant *vax$ah* '(general 128 h-float address))
(define-constant *vax$ao* '(general 128 octa address))
(define-constant *vax$rb* '(general 8 byte read))
(define-constant *vax$rw* '(general 16 word read))
(define-constant *vax$rl* '(general 32 long read))
(define-constant *vax$rq* '(general 64 quad read))
(define-constant *vax$rf* '(general 32 f-float read))
(define-constant *vax$rd* '(general 64 d-float read))
(define-constant *vax$rg* '(general 64 g-float read))
(define-constant *vax$rh* '(general 128 h-float read))
(define-constant *vax$ro* '(general 128 octa read))
(define-constant *vax$mb* '(general 8 byte modify))
(define-constant *vax$mw* '(general 16 word modify))
(define-constant *vax$ml* '(general 32 long modify))
(define-constant *vax$mf* '(general 32 f-float modify))
(define-constant *vax$md* '(general 64 d-float modify))
(define-constant *vax$mg* '(general 64 g-float modify))
(define-constant *vax$mh* '(general 128 h-float modify))
(define-constant *vax$mo* '(general 128 octa modify))
(define-constant *vax$wb* '(general 8 byte write))
(define-constant *vax$ww* '(general 16 word write))
(define-constant *vax$wl* '(general 32 long write))
(define-constant *vax$wq* '(general 64 quad write))
(define-constant *vax$wf* '(general 32 f-float write))
(define-constant *vax$wd* '(general 64 d-float write))
(define-constant *vax$wg* '(general 64 g-float write))
(define-constant *vax$wh* '(general 128 h-float write))
(define-constant *vax$wo* '(general 128 octa write))
(define-constant *vax$bb* '(general 8 byte branch))
(define-constant *vax$bw* '(general 16 word branch))
(define-constant *vax$vb* '(general 8 byte field))
