(herald m68lap
        (env tsys))

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

(define local-processor
  (lambda ()
    (object nil
      ((processor-type self)     'MC68000)
      ((mc68000-processor? self) '#t)
      ((print-type-string self)  "Processor"))))
        
;;; lap code is of the form (lap free-vars . code)
;;; lap templates are (lap-template (pointer scratch nargs) . code)

(define (invoke-stack-continuation frame vals)
  (lap (return apply)
    (sub .l ($ 2) A1)
    (move .l A1 SP)
    (cmp .l A2 nil-reg)
    (j= no-values)
    (cmp .l (d@r A2 -3) nil-reg)
    (jn= many-values)
    (move .l (d@r A2 1) A1)
    (move .l ($ -2) NARGS)
    (move .l (@r sp) tp)
    (jmp (@r tp))
no-values
    (move .l ($ -1) NARGS)
    (move .l (@r sp) tp)
    (jmp (@r tp))
many-values
    (move .l (d@static P (static 'return)) A1)
    (move .l (d@static P (static 'apply)) P)
    (move .l ($ 3) NARGS)
    (move .l (d@r p -2) tp)
    (jmp (@r tp))))

(define (invoke-continuation sp stack vals base-state current-state)
  (lap (rewind-state-and-continue)
    (bset ($ 6) (d@r task task/critical-count))
    (move .l A1 SP)                    ; set new continuation
    (move .l (d@r TASK task/stack) S0) ; limit at stack base
    (add .l ($ 2) A2)                  ; start at first word of stack in heap
    (jbr copy-stack-test)
copy-stack-loop 
    (move .l (@r+ A2) (@r+ A1))
copy-stack-test
    (cmp .l A1 S0)
    (j>= copy-stack-loop)
    (bclr ($ 6) (d@r task task/critical-count))
    (move .l (d@r TASK 12) A1)
    (move .l (d@r TASK 16) A2)
    (move .l (d@static P (static 'rewind-state-and-continue)) P)
    (move .l ($ 4) NARGS)
    (move .l (d@r p -2) tp)
    (jmp (@r tp))))


;;; (FIXNUM-HOWLONG n)
;;;   Returns the number of bits in N's binary representation.
;;;   Horrible name, after MACLISP function HAULONG.

(define (fixnum-howlong num)
  (lap ()
    (move .l a1 s0)
    (lsr  .l ($ 2) s0)             ; S0 hold num
    (move .l ($ 0) s1)             ; S1 holds result
    (move .l s0 s2)                ; S2 used as scratch
    (and  .l ($ #xffff8000) s2)
    (j= howlong1)
    (add  .w ($ 16) s1)
    (swap s0)
howlong1
    (move .w s0 s2)
    (and  .w ($ #x7f80) s2)
    (j= howlong2)
    (add  .w ($ 8) s1)
    (asr  .l ($ 8) s0)
howlong2
    (move .w s0 s2)
    (and  .b ($ #x78) s2)
    (j= howlong3)
    (add  .w ($ 4) s1)
    (asr  .l ($ 4) s0)
howlong3
    (move .w s0 s2)
    (and  .b ($ #x6) s2)
    (j= howlong4)
    (add  .w ($ 2) s1)
    (asr  .l ($ 2) s0)
howlong4
    (move .w s0 s2)
    (and  .b ($ #x1) s2)
    (j= howlong5)
    (add  .w ($ 1) s1)
howlong5
    (asl  .w ($ 2) s1)
    (move .l s1 a1)
    (move .l ($ -2) nargs)
    (move .l (@r sp) tp)
    (jmp (@r tp))))


              
(define (*set x y)
  (lap ()  
    (move .l A2 (d@r A1 2))
    (tst .b (@r A1))
    (j= foo-set)
    (move .l A1 (d@r TASK task/extra-pointer))
    (jsr (*d@nil slink/set))
foo-set    
    (move .l ($ -2) NARGS)
    (move .l (@r SP) TP)
    (jmp (@r TP))))


(define (apply-traced-operation proc . args)
  (lap (*traced-op-template*)
    (move .l (d@static P (static '*traced-op-template*)) TP)
    (clr .l S0)
    (jbr entry)))

(define (apply proc . args)
 (lap (apply-too-many-args)
  (move .l ($ 1) S0)
entry
  (sub .l ($ 1) NARGS)                   ;; shift proc out
  (move .l P (@-r SP))                   ;; save environment 
  (move .l A1 (@-r SP))                  ;; first arg is proc (save it)
  (cmp .l ($ 1) NARGS)                   ;; no args to proc
  (j= apply-done)
  (sub .l ($ 1) NARGS)
  (cmp .l ($ 1) NARGS)
  (jn= next1)
  (move .l A2 AN)
  (jbr apply-one-arg)
next1
  (cmp .l ($ 2) NARGS)
  (jn= next2)
  (move .l A2 A1)
  (move .l A3 AN)
  (jbr apply-two-args)
next2
  (cmp .l ($ 3) NARGS)
  (jn= next3)
  (move .l A2 A1)
  (move .l A3 A2)
  (move .l (d@r TASK 12) AN)           ;; first argument temp
  (jbr apply-three-args)
next3
  (move .l A2 A1)
  (move .l A3 A2)
  (move .l (d@r TASK 12) A3)            ;; first argument temp
  (move .l NARGS S1)
  (sub .l ($ 4) S1)                     ;; S1 counts sown to 0
  (lea (d@r TASK 16) P)                ;; set up P to point into rest vector
                                       ;; first 3 temps reserved, 1 done already
  (jbr apply-shift-test)
apply-shift-loop-top
  (move .l (@r P) (d@r P -4))
  (sub .l ($ 1) S1)
  (add .l ($ 4) P)
apply-shift-test
  (cmp .l ($ 0) S1)
  (jn= apply-shift-loop-top)
  (move .l (@r P) AN)  
  (sub .l ($ 4) P)
  (jbr apply-spread-loop)
apply-one-arg
  (cmp .l AN nil-reg)   
  (j= apply-done)
  (move .l (d@r AN 1) A1)                    
  (add .l ($ 1) NARGS)
  (move .l (d@r AN -3) AN)                   
apply-two-args
  (cmp .l AN nil-reg)   
  (j= apply-done)
  (move .l (d@r AN 1) A2)                    
  (add .l ($ 1) NARGS)
  (move .l (d@r AN -3) AN)                   
apply-three-args
  (cmp .l AN nil-reg)   
  (j= apply-done)
  (move .l (d@r AN 1) A3)                    
  (add .l ($ 1) NARGS)
  (move .l (d@r AN -3) AN)                   
  (lea (d@r TASK 12) P)
apply-spread-loop              
  (cmp .l AN nil-reg)
  (j= apply-done)
  (move .l (d@r AN 1) (@r P))
  (add .l ($ 1) NARGS)
  (cmp .l ($ (+ *pointer-temps* 1)) NARGS)
  (j> too-many)
  (add .l ($ 4) P)
  (move .l (d@r AN -3) AN)
  (jbr apply-spread-loop)
too-many
  (move .l (@r+ SP) A1)                    ; procedure is argument
  (move .l (@r+ SP) P)
  (move .l ($ 2) NARGS)
  (move .l (d@static P (static 'apply-too-many-args)) P)
  (move .l (d@r p -2) tp)
  (jmp (@r tp))
apply-done                                
  (move .l (@r+ SP) P)                     ; restore procedure
  (add .w ($ 4) SP)                        ; get rid of environment
  (tst .l S0)
  (j= traced)
  (jmp (*d@nil slink/icall))
traced            
  (jmp (@r TP))))


(define (string-hash string)
  ;; string in A1
  (lap ()
    ;; enter critical gc
    (move .l (d@r A1 offset/string-text) A3);; raw string text in A3
    (add .l (d@r A1 offset/string-base) A3)                              
    (add .l ($ 2) A3)
    (clr .l S1)                             ;; counter in S1
hash                
    (move .l (d@r A1 -2) S0)                ;; length in S0
    (asr .l ($ 8) S0)
    (clr .l S2)                             ;; hash value so far in S2
    (jmp (label hash-test))
hash-loop              
    (rol .l ($ 1) S2)                       ;++ change to 3 later
    (add .b (@r+ A3) S2)
hash-test
    (add .l ($ 1) S1)
    (cmp .l S1 S0)  
    (j>= hash-loop)
    (move .l S2 S1)
    (swap S1) 
    (eor .l S1 S2) 
    (and .l ($ #x7ffffffc) S2)              ;; positive-fixnumize
    (move .l S2 A1)
    ;; exit critical gc                       ;; blat bits 0,1,31
    (move .l ($ -2) NARGS)
    (move .l (@r sp) tp)
    (jmp (@r tp))))

            
;;;  magic frame is next-state
;;;                 winder
;;;                 previous-state
;;;                 unwinder
;;;                 *magic-frame-template*

(define (push-magic-frame unwinder stuff wind)   
 (lap (*magic-frame-template* bind-internal)
  (move .l (d@r TASK task/dynamic-state) AN)
  (move .l nil-reg (@-r SP))                           ; next state
  (move .l A3 (@-r SP))                                ; winder
  (move .l AN (@-r SP))                                ; previous state
  (move .l A1 (@-r SP))                                ; unwinder
  (move .l (d@static P (static '*magic-frame-template*)) (@-r SP))
  (lea (d@r SP 2) A1)                     ; first arg is the magic frame
  (cmp .l AN nil-reg)                     ; is there a previous state?
  (j= magic-frame-exit)
  (move .l A1 (d@r AN 14))                ; set next slot to this magic frame
magic-frame-exit
  (move .l (d@static P (static 'bind-internal)) P)   ; second arg is stuff
  (move .l ($ 3) NARGS)
  (move .l (d@r P -2) tp)
  (jmp (@r tp))))

(define (make-structure-template size)
  (lap (*structure-template* *stype-template*)
    (move .l (d@static P (static '*stype-template*)) AN)
    (move .l ($ 36) S1)                            ; 9 slots
    (jsr (*d@nil slink/make-extend))
    (move .w ($ 32) (d@r AN 28))                     ; offset within closure
    (move .b ($ 0) (d@r AN 27))                     ; 0 scratch slots
    (move .l A1 S0)
    (asr .l ($ 2) S0)                              ; pointer slots
    (move .b S0 (d@r AN 26))               
    (move .w ($ #x8000) (d@r AN 30))                ; high bit for template, 0 args
    (move .w ($ M68-JUMP-ABSOLUTE) (d@r AN 32))
    (move .l (d@static P (static '*structure-template*)) (d@r AN 34)) ; auxilliary
    (lea (d@r AN 32) A1)                           ; template
    (move .l AN A2)                                ; stype
    (move .l ($ -3) NARGS)                         ; return two values
    (move .l (@r sp) tp)
    (jmp (@r tp))))

