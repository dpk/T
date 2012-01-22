(herald vaxlap (env tsys))

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
        
;;; lap code is of the form (lap free-vars . code)
;;; lap templates are (lap-template (pointer scratch nargs) . code)

(define local-processor
  (lambda ()
    (object nil
      ((processor-type self)     'vax11)
      ((vax-processor? self) '#t)
      ((print-type-string self)  "Processor"))))

(define (invoke-stack-continuation frame vals)
  (lap (return apply)
    (subl2 ($ 2) A1)
    (movl A1 SP)
    (cmpl A2 nil-reg)
    (beql (to no-values))
    (cmpl (d@r A2 -3) nil-reg)
    (bneq (to many-values))
    (movl (d@r A2 1) A1)
    (mnegl ($ 2) NARGS)
    (movl (@r sp) tp)
    (jmp (@r tp))
no-values
    (mnegl ($ 1) NARGS)
    (movl (@r sp) tp)
    (jmp (@r tp))
many-values
    (movl (d@r P (static 'return)) A1)
    (movl (d@r a1 2) a1)
    (movl (d@r P (static 'apply)) P)
    (movl (d@r p 2) p)
    (movl ($ 3) NARGS)
    (movl (d@r p -2) tp)
    (jmp (@r tp))))


(define (invoke-continuation sp stack val base-state current-state)
  (lap (rewind-state-and-continue)
    (movl A1 SP)                    ; set new continuation
    (movl (d@r TASK task/stack) S0) ; limit at stack base
    (addl2 ($ 2) A2)                  ; start at first word of stack in heap
    (brb (to copy-stack-test))
copy-stack-loop 
    (movl (@r+ A2) (@r+ A1))
copy-stack-test
    (cmpl A1 S0)
    (bleq (to copy-stack-loop))
    (movl A4 A1)
    (movl (d@r TASK 16) A2)
    (movl (d@r P (static 'rewind-state-and-continue)) P)
    (movl (d@r p 2) p)
    (movl ($ 4) NARGS)
    (movl (d@r p -2) tp)
    (jmp (@r tp))))




;;; (FIXNUM-HOWLONG n)
;;;   Returns the number of bits in N's binary representation.
;;;   Horrible name, after MACLISP function HAULONG.

(define (fixnum-howlong num)
 (lap ()
  (rotl ($ -2) A1 S0)        
  (clrl A1)
  (bitl ($ #xffff8000) S0)
  (beql (to howlong1))
  (bisl2 ($ (* 16 4)) A1)
  (ashl ($ -16) S0 S0)
howlong1
  (bitl ($ #x7f80) S0)
  (beql (to howlong2))
  (bisl2 ($ (* 8 4)) A1)
  (ashl ($ -8) S0 S0)
howlong2
  (bitl ($ #x78) S0)
  (beql (to howlong3))
  (bisl2 ($ (* 4 4)) A1)
  (ashl ($ -4) S0 S0)
howlong3
  (bitl ($ #x6) S0)
  (beql (to howlong4))
  (bisl2 ($ (* 2 4)) A1)
  (ashl ($ -2) S0 S0)
howlong4
  (bitl ($ #x1) S0)
  (beql (to howlong5))
  (bisl2 ($ (* 1 4)) A1)
howlong5  
  (mnegl ($ 2) NARGS)
    (movl (@r sp) tp)
    (jmp (@r tp))))


(define (*set x y)
  (lap ()  
    (movl A2 (d@r A1 2))
    (tstb (@r A1))
    (beql (to foo-set))
    (movl A1 (d@r TASK task/extra-pointer))
    (jsb (*d@r nil-reg slink/set))
foo-set
    (mnegl ($ 2) NARGS)
    (movl (@r sp) tp)
    (jmp (@r tp))))


(define (apply-traced-operation proc . args)
  (lap (*traced-op-template*)
    (movl (d@r P (static '*traced-op-template*)) TP)
    (movl (d@r tp 2) tp)
    (clrl (d@r TASK task/extra-scratch))
    (jmp (label entry))))

(define (apply proc . args)
 (lap (apply-too-many-args)                 
  (movl ($ 1) (d@r TASK task/extra-scratch))
entry
  (decl NARGS)                        ;; shift proc out
  (pushl P)                           ;; save env
  (movl A1 P)                         ;; first arg is proc
  (cmpl NARGS ($ 1))                  ;; no args to proc
  (beql (to apply-done))
  (decl NARGS)
  (cmpl NARGS ($ 1))
  (bneq (to next1))
  (movl A2 AN)
  (jmp (label apply-one-arg))
next1
  (cmpl NARGS ($ 2))
  (bneq (to next2))
  (movl A2 A1)
  (movl A3 AN)
  (jmp (label apply-two-args))
next2
  (cmpl NARGS ($ 3))
  (bneq (to next3))
  (movl A2 A1)
  (movl A3 A2)
  (movl A4 AN)                
  (jmp (label apply-three-args))
next3                         
  (cmpl NARGS ($ 4))
  (bneq (to next4))
  (movl A2 A1)
  (movl A3 A2)
  (movl A4 A3)
  (movl (d@r TASK 16) AN)           ;; first argument temp
  (jmp (label apply-four-args))
next4
  (movl A2 A1)
  (movl A3 A2)
  (movl A4 A3)
  (movl (d@r TASK 16) A4)            ;; first argument temp
  (subl3 ($ 5) NARGS S1)             ;; S1 counts sown to 0
  (addl3 TASK ($ 20) S2)             ;; set up S2 to point into rest vector
                                     ;; first 4 temps reserved, 1 done already
  (jmp (label apply-shift-test))
apply-shift-loop-top
  (movl (d@r S2 0) (d@r S2 -4))
  (decl S1)
  (addl2 ($ 4) S2)
apply-shift-test
  (cmpl S1 ($ 0))
  (bneq (to apply-shift-loop-top))
  (movl (d@r S2 0) AN)  
  (subl2 ($ 4) S2)
  (jmp (label apply-many-args))
apply-one-arg
  (cmpl AN nil-reg)   
  (beql (to apply-done))
  (movl (d@r AN 1) A1)                    
  (addl2 ($ 1) NARGS)
  (movl (d@r AN -3) AN)                   
apply-two-args
  (cmpl AN nil-reg)   
  (beql (to apply-done))
  (movl (d@r AN 1) A2)                    
  (addl2 ($ 1) NARGS)
  (movl (d@r AN -3) AN)                   
apply-three-args
  (cmpl AN nil-reg)   
  (beql (to apply-done))
  (movl (d@r AN 1) A3)                    
  (addl2 ($ 1) NARGS)
  (movl (d@r AN -3) AN)                   
apply-four-args
  (cmpl AN nil-reg)   
  (beql (to apply-done))
  (movl (d@r AN 1) A4)                    
  (addl2 ($ 1) NARGS)
  (movl (d@r AN -3) AN)      
  (addl3 TASK ($ 16) S0)
apply-spread-loop              
  (cmpl AN nil-reg)
  (beql (to apply-done))
  (movl (d@r AN 1) (d@r S0 0))
  (addl2 ($ 1) NARGS)
  (cmpl ($ (+ *pointer-temps* 1)) NARGS)
  (blss (to too-many))
  (addl2 ($ 4) S0)
  (movl (d@r AN -3) AN)
  (jmp (label apply-spread-loop))
too-many
  (movl (@r+ SP) P)
  (movl ($ 2) NARGS)
  (movl (d@r P (static 'apply-too-many-args)) P)
  (movl (d@r p 2) p)
  (movl (d@r p -2) tp)
  (jmp (@r tp))
apply-done                    
  (addl2 ($ 4) SP)
  (tstl (d@r TASK task/extra-scratch))
  (beql (to traced))
  (movl (d@r p -2) tp)
  (jmp (@r tp))
traced
  (jmp (@r TP))))




(define (string-hash string)
  ;; string in A1
  (lap ()
    ;; enter critical gc
    (addl3 (d@r A1 offset/string-text) ($ 2) A3);; raw string text in A3
    (addl2 (d@r A1 offset/string-base) A3)                              
hash
    (ashl ($ -8) (d@r A1 -2) S0)              ;; string-length in S0
    (clrl S1)                                 ;; conter in S1
    (clrl S2)                                 ;; hash value so far in S2
    (jmp (label hash-test))
hash-loop              
    (rotl ($ 1) S2 S2)
    (addb2 (@r+ A3) S2)
hash-test  
    (aobleq S0 S1 (to hash-loop))
    (rotl ($ 16) S2 S1)
    (xorl2 S1 S2)
    (bicl3 ($ #x80000003) S2 A1)              ;; positive-fixnumize
    ;; exit critical gc                       ;; blat bits 0,1,31
    (mnegl ($ 2) NARGS)
    (movl (@r sp) tp)
    (jmp (@r tp))))




;;;  magic frame is next-state
;;;                 winder
;;;                 previous-state
;;;                 unwinder
;;;                 *magic-frame-template*

(define (push-magic-frame unwinder stuff wind)   
 (lap (*magic-frame-template* bind-internal)
  (movl (d@r TASK task/dynamic-state) AN)
  (pushl nil-reg)                                      ; next state
  (pushl A3)                                           ; winder
  (pushl AN)                                           ; previous state
  (pushl A1)                                          ; unwinder
  (movl (d@r P (static '*magic-frame-template*)) a3)
  (pushl (d@r a3 2))
  (addl3 SP ($ 2) A1)                  ; first arg is the magic frame
  (cmpl AN nil-reg)                     ; is there a previous state?
  (beql (to magic-frame-exit))
  (movl A1 (d@r AN 14))                ; set next slot to this magic frame
magic-frame-exit
  (movl (d@r P (static 'bind-internal)) P)   ; second arg is stuff
  (movl (d@r p 2) p)
  (movl ($ 3) NARGS)
    (movl (d@r p -2) tp)
    (jmp (@r tp))))

                   
(define (make-structure-template size)
  (lap (*structure-template* *stype-template*)
    (movl (d@r P (static '*stype-template*)) AN)
    (movl (d@r an 2) an)
    (movl ($ 36) S1)                            ; 9 slots
    (jsb (*d@r nil-reg slink/make-extend))
    (movw ($ 32) (d@r AN 26))                     ; offset within closure
    (movb ($ 0) (d@r AN 28))                     ; 0 scratch slots
    (ashl ($ -2) A1 S0)                         ; pointer slots
    (movb S0 (d@r AN 29))
    (movw ($ header/template) (d@r AN 30))
    (movw ($ VAX-JUMP-ABSOLUTE) (d@r AN 32))
    (movl (d@r P (static '*structure-template*)) p)
    (movl (d@r p 2) (d@r AN 34)) ; auxilliary
    (moval (d@r AN 32) A1)                       ; template
    (movl AN A2)                                ; stype
    (mnegl ($ 3) NARGS)                         ; return two values
    (movl (@r sp) tp)
    (jmp (@r tp))))


