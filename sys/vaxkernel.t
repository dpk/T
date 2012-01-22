(herald unvaxkernel (env tsys))

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

(define (vax-big-bang)
  (lap (big_bang handle-stack-base
         icall-bad-proc icall-wrong-nargs
         handle-undefined-effect
        really-gc pc-code-vector
        heap-overflow-error call-fault-handler cont-wrong-nargs)


    (movl  nil-reg (d@r nil-reg -3))            ; (cdr '()) = '()
    (movl  nil-reg (d@r nil-reg 1))             ; (car '()) = '()
    (movl  P (d@r nil-reg slink/kernel))        ; save kernel pointer
    (moval (label %undefined-effect)  (d@r nil-reg slink/undefined-effect))
    (moval (label %make-pair)         (d@r nil-reg slink/make-pair))
    (moval (label %make-extend)       (d@r nil-reg slink/make-extend))
    (moval (label %nary-setup)        (d@r nil-reg slink/nary-setup))
    (moval (label %set)               (d@r nil-reg slink/set))
    (moval (label %icall)             (d@r nil-reg slink/icall))
    (moval (label %cont-wrong-nargs)  (d@r nil-reg slink/cont-wrong-nargs))
    (moval (label %kernel-begin)      (d@r nil-reg slink/kernel-begin))
    (moval (label %kernel-end)        (d@r nil-reg slink/kernel-end))

    ;; initialize root process, stored in outer space?  

    ;; zero out extra registers
    (movl ($ (fx/ temp-block-size 4)) S0)
initialize-loop     
    (clrl (@-r sp))  
    (decl S0)
    (j> initialize-loop)

    (movl   SP TASK)                            ; load task reg
    (addl2  ($ (fx+ %%task-header-offset 4)) sp); allocate task block
    (pushl  ($ header/task))                    ; task header
    (addl3  ($ 2) SP A3)                        ; extend pointer
    (movl   A3 (d@r NIL-REG slink/root-process)); ptr to root and
    (movl   A3 (d@r NIL-REG slink/current-task)); current process
 
    ;; initialize stack
    (pushl A3)                                 ; task block
    (pushl nil-reg)                             ; no parent
    (pushl ($ 0))                               ; active, no current sz
    (pushl ($ (fixnum-ashl %%stack-size 2)))    ; total stack size
    (pushl  ($ #xBADBAD))                       ; distinguished value
    (pushal (label stack-base-template))        ; stack base

    ;; initialize root process
;***** (addl3 ($ 2) SP (d@r TASK task/stack))
    (movl SP (d@r TASK task/stack))
    (clrl (d@r TASK task/extra-pointer))
    (clrl (d@r TASK task/extra-scratch))
    (movl nil-reg (d@r TASK task/dynamic-state))

    (movl nil-reg (d@r TASK task/doing-gc?))
    (clrl (d@r TASK task/foreign-call-cont))
    (clrl (D@r TASK task/critical-count))
    (movl nil-reg (d@r TASK task/k-list))
    (movl nil-reg (d@r TASK task/gc-weak-set-list))
    (movl nil-reg (d@r TASK task/gc-weak-alist-list))
    (movl nil-reg (d@r TASK task/gc-weak-table-list))
    (movl nil-reg (d@r nil-reg slink/snapper-freelist))
    (movl nil-reg (d@r nil-reg slink/pair-freelist))
    (movl (d@r P (static 'big_bang)) P)
    (movl (d@r p 2) p)
    (jmp (@r TP))
 
%make-pair
    ;; return pair in AN
    (bisb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
    (addl3 ($ 8) (d@r TASK task/area-frontier) AN)
    (cmpl AN (d@r TASK task/area-limit))
    (j> %make-pair-heap-overflow)
%make-pair-continue
    (movl AN (d@r TASK task/area-frontier))
    (subl2 ($ (fx- 8 tag/pair)) AN)
    (clrq (d@r AN -3))
    (bicb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
    (jn= %deferred-interrupts)
    (rsb)

%make-pair-heap-overflow
    (movl ($ header/true) (d@r TASK task/doing-gc?))
    (jsb (label %heap-overflow))
    (addl3 ($ 8) (d@r TASK task/area-frontier) AN)
    (cmpl AN (d@r TASK task/area-limit))
    (j> %horrible-heap-overflow)
    (bisb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
    (movl nil-reg (d@r TASK task/doing-gc?))
    (jmp (label %make-pair-continue))
    

%make-extend
    ;; receive descriptor in An, size in bytes S1,
    ;; return extend in AN.
    (bisb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
    (movl (d@r TASK task/area-frontier) S2) 
    (addl2 ($ 4) S1)                           
    (addl2 S2 S1)
    (cmpl S1 (d@r TASK task/area-limit))
    (j> %make-extend-heap-overflow)
%make-extend-continue  
    (movl S1 (d@r TASK task/area-frontier))
    (movl AN (@r+ S2))
    (cmpl S1 S2)
    (j= copy-done)
    (movl S2 AN)
extend-loop
    (clrl (@r+ AN))
    (cmpl S1 AN)
    (j> extend-loop)
copy-done
    (subl3 ($ tag/extend) S2 AN)
    (bicb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
    (jn= %deferred-interrupts)
    (rsb)
   
%make-extend-heap-overflow
    (movl ($ header/true) (d@r TASK task/doing-gc?))
    (subl2 S2 S1)
    (jsb (label %heap-overflow))
    (movl (d@r TASK task/area-frontier) S2) ; get area-frontier
    (addl2 S2 S1)                          ; one for the descriptor
    (cmpl S1 (d@r TASK task/area-limit))
    (j> %horrible-heap-overflow)
    (bisb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
    (movl nil-reg (d@r TASK task/doing-gc?))
    (jmp (label  %make-extend-continue))
                      
%heap-overflow
    (pushl S0)                  ; save scratch registers
    (pushl S1)
    (pushl S2)
    (pushl S3)
    (movl ($ (fx/ temp-block-size 4)) S0)
save-loop                                  ; save temps
    (pushl (index (d@r TASK -4) S0))
    (decl S0)
    (j>= save-loop)
    (pushl TP)
    (pushl AN)
    (pushl A4)
    (pushl A3)
    (pushl A2)
    (pushl A1)
    (pushl P)
    (movl (d@r SP (* (+ *no-of-registers* 3) 4)) A1)   ; one for TP 2 return
    (pushal (label pc-check-return))
    (movl (d@r nil-reg slink/kernel) P)
    (movl (d@r P (static 'pc-code-vector)) P)
    (movl (d@r p 2) p)
    (movl (d@r P -2) TP)
    (jmp (@r TP))                                ; call pc-code-vector
           
;;; the template header byte has high bit set if nary

%icall                     
  (bicb3 ($ #b11111100) P S0)
  (cmpb S0 ($ tag/extend))                     ; check proc is extend
  (jn= %icall-bad-proc)
  (movl (d@r P -2) TP)                         ; fetch header
  (bicb3 ($ #b11111100) TP S0)                 ; check header is extend
  (cmpb S0 ($ tag/extend))
  (jn= %icall-bad-proc)
  (cmpb (d@r TP -2) ($ header/template))       ; check header is template
  (jn= %icall-check-nary)
  (cmpb (d@r TP template/nargs) NARGS)         ; check number of args
  (j= %icall-ok)
  (jmp (label %icall-wrong-nargs))
%icall-check-nary
  (cmpb (d@r TP -2) ($ (fx+ header/template 128)))
  (jn= %icall-bad-proc)
  (cmpb (d@r TP template/nargs) NARGS)
  (j> %icall-wrong-nargs)
%icall-ok
  (jmp (@r TP))

%icall-bad-proc
  (movl a1 (d@r TASK task/t0))
  (movl a2 (d@r TASK (fx+ task/t0 4)))
  (movl a3 (d@r TASK (fx+ task/t0 8)))
  (movl a4 (d@r TASK (fx+ task/t0 12)))
  (clrl s0)
  (jsb (label %nary-setup))
  (movl an a2)
  (movl p a1)
  (movl (d@r nil-reg slink/kernel) P)
  (movl (d@r P (static 'icall-bad-proc)) P)
  (movl (d@r p 2) p)
  (movl (d@r P -2) TP)
  (jmp  (@r TP))

%icall-wrong-nargs
  (movl a1 (d@r TASK task/t0))
  (movl a2 (d@r TASK (fx+ task/t0 4)))
  (movl a3 (d@r TASK (fx+ task/t0 8)))
  (movl a4 (d@r TASK (fx+ task/t0 12)))
  (clrl s0)
  (jsb (label %nary-setup))
  (movl an a2)
  (movl p a1)
  (movl (d@r nil-reg slink/kernel) P)
  (movl (d@r P (static 'icall-wrong-nargs)) P)
  (movl (d@r p 2) p)
  (movl (d@r P -2) TP)
  (jmp  (@r TP))



%deferred-interrupts
    (pushl S3)
    (pushl S2)
    (pushl S1)
    (pushl S0)
    (movl ($ (fx/ (fx+ temp-block-size 4) 4)) S2)
%int-save-loop                              ; save temps and extra p and s
    (pushl (index (d@r TASK -8) S2))
    (decl S2)
    (j>= %int-save-loop)
    (pushl TP)
    (pushl AN)
    (pushl A4)
    (pushl A3)
    (pushl A2)
    (pushl A1)
    (pushl P)
    (pushl ($ 0))               ; pc
    (pushl (d@r SP (* (+ *pointer-temps* *scratch-temps* 14) 4)))
    (pushl ($ 0))               ; no pointers on top
    (pushl ($ (+ (fixnum-ashl (+ *pointer-temps* *scratch-temps* 16) 8)
                   header/fault-frame)))
    (pushal (label %int-return))
    (movl (d@r nil-reg slink/kernel) P)
    (movl (d@r P (static 'call-fault-handler)) P)
    (movl (d@r p 2) p)
    (movl (d@r P -2) TP)                      
    (jmp (@r TP))


%kernel-begin

%cont-wrong-nargs
  (mnegl nargs nargs)
  (movl a1 (d@r TASK task/t0))
  (movl a2 (d@r TASK (fx+ task/t0 4)))
  (movl a3 (d@r TASK (fx+ task/t0 8)))
  (movl a4 (d@r TASK (fx+ task/t0 12)))
  (clrl s0)
  (jsb (label %nary-setup))
  (movl an a2)
  (moval (d@r sp 2) a1)
  (movl (d@r nil-reg slink/kernel) P)
  (movl (d@r P (static 'cont-wrong-nargs)) P)
  (movl (d@r p 2) p)
  (movl (d@r P -2) TP)
  (jmp  (@r TP))
                
%post-gc-nary-setup
  (mnegl ($ 1) S1)
  (jmp (label %real-nary-setup))
  

%nary-setup                                 ; required args in S0
  (clrl S1)
%real-nary-setup
  (subl2 ($ 2) NARGS)                      
  (movl P (d@r TASK task/extra-pointer))
  (movl nil-reg AN)
  (bisb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
  (jmp (label %nary-test))
%nary-loop
  (movl AN P)                               ; accumulate in P
  (addl3 ($ 8) (d@r TASK task/area-frontier) AN)
  (cmpl AN (d@r TASK task/area-limit))
  (j> %nary-make-pair-heap-overflow)
%nary-make-pair-continue                        ; lose, lose
  (movl AN (d@r TASK task/area-frontier))
  (subl2 ($ (fx- 8 tag/pair)) AN)
  (clrq (d@r AN -3))
  (movl P (d@r AN -3))                      ; set cdr
  (movl (index (@r TASK) NARGS) (d@r AN 1))    ; set car
  (decl NARGS)
%nary-test
  (cmpl NARGS S0)                              ; redundant?
  (j>= %nary-loop)
  (tstl S1)
  (jn= nary-clear-extras)
  (movl (d@r TASK task/extra-pointer) P)                         ; restore P and return
  (bicb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
  (jn= %deferred-interrupts)
  (rsb)
nary-clear-extras
  (cmpl ($ 4) S0)
  (j<= foo45)
  (movl ($ 4) S0)
foo45
  (clrl (index (@r TASK) S0))
  (incl S0)
  (cmpl ($ (fx/ temp-block-size 4)) S0)
  (j> foo45)
  (moval (label %nary-setup) (d@r nil-reg slink/nary-setup))
  (movl (d@r TASK task/extra-pointer) P)                            
  (bicb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
  (jn= %deferred-interrupts)
  (rsb)     

%nary-make-pair-heap-overflow
    (movl ($ header/true) (d@r TASK task/doing-gc?))
    (jsb (label %heap-overflow))
    (movl (d@r TASK task/area-frontier) AN)
    (addl2 ($ 8) AN)
    (cmpl AN (d@r TASK task/area-limit))
    (j> %horrible-heap-overflow)
    (bisl2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
    (movl nil-reg (d@r TASK task/doing-gc?))
    (jmp (label %nary-make-pair-continue))

%set                                        ; a location is (unit  . index)
   ;;  vcell in extra-pointer
   (bisb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
   (pushl s1)
   (pushl s0)
   (pushl an)
   (pushl a3)
   (pushl a2)
   (pushl a1)
   (pushl p)
   (movl (d@r TASK task/extra-pointer) a3)
   (movl (d@r A3 6) A1)                  ; get locations
   (movl (d@r A1 2) A1)                  ; get the vector in A1
   (ashl ($ -8) (d@r A1 -2) S0)
   (jmp (label %set-test))
%set-loop
   (movl (d@r nil-reg slink/snapper-freelist) an)
   (cmpl an nil-reg)
   (j= cons-snapper)
   (movl (d@r an 1) p)
   (movl (d@r an -3) (d@r nil-reg slink/snapper-freelist))
   (movl (d@r nil-reg slink/pair-freelist) (d@r an -3))
   (movl an (d@r nil-reg slink/pair-freelist))
%real-top
   (movl (index (d@r A1 -6) S0) A2)      ; get unit
   (ashl ($ -2) (index (d@r A1 -2) S0) S1)      ; get index
   (movl (d@r a3 2) (d@r p 2))
   (movl a2 (d@r p 6))
   (ashl ($ 2) s1 (d@r p 10))
   (movl p (index (d@r A2 2) s1))
   (subl2 ($ 2) S0)
%set-test
   (tstl S0)
   (jn= %set-loop)
   (movl (@r+ sp) p)
   (movl (@r+ sp) a1)
   (movl (@r+ sp) a2)
   (movl (@r+ sp) a3)
   (movl (@r+ sp) an)
   (movl (@r+ sp) s0)
   (movl (@r+ sp) s1)
   (bicb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
   (jn= %deferred-interrupts)
   (rsb)
cons-snapper
   (movl (d@r TASK task/area-frontier) AN)
   (addl2 ($ 16) AN)
   (cmpl AN (d@r TASK task/area-limit))
   (j> %set-heap-overflow)
%set-continue                        ; lose, lose
   (movl AN (d@r TASK task/area-frontier))
   (moval (d@r an -14) p)
   (moval (label link-snapper) a2)
   (movl a2 (d@r p -2))
   (jmp (label %real-top))
%set-heap-overflow
    (movl ($ header/true) (d@r TASK task/doing-gc?))
    (pushl ($ (+ (fixnum-ashl 5 16) (fixnum-ashl 3 8) header/vframe )))
    (pushl (d@r sp 32))
    (jsb (label %heap-overflow))
    (movl (@r sp) (d@r sp 36))
    (addl2 ($ 8) sp)
    (movl (d@r TASK task/area-frontier) AN)
    (addl2 ($ 16) AN)
    (cmpl AN (d@r TASK task/area-limit))
    (j> %horrible-heap-overflow)
    (bisl2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
    (movl nil-reg (d@r TASK task/doing-gc?))
    (jmp (label %set-continue))

%kernel-end
        
%horrible-heap-overflow
  (addl2 ($ 4) SP)
  (bicb2 ($ #b10000000) (d@r TASK (fx+ task/critical-count 3)))
  (movl nil-reg (d@r TASK task/doing-gc?))
  (movl (d@r nil-reg slink/kernel) P)
  (movl (d@r P (static 'heap-overflow-error)) P)
  (movl (d@r p 2) p)
  (movl (d@r P -2) TP)
  (jmp (@r TP))
  


%undefined-effect
  (movl TP A2)
  (movl (d@r nil-reg slink/kernel) P)
  (movl (d@r P (static 'handle-undefined-effect)) P)
  (movl (d@r p 2) p)
  (movl (d@r P -2) TP)
  (jmp (@r TP))
))                         

(lap-template (0 0 -1 t stack %int-return-handler)
%int-return
    (movl (d@r SP 12) (d@r SP (* (+ *pointer-temps* *scratch-temps* 18) 4)))
    (addl2 ($ 20) sp)        ; pop template,header,pointers on stack,hack top,pc
    (movl (@r+ SP) P)
    (movl (@r+ SP) A1)
    (movl (@r+ SP) A2)
    (movl (@r+ SP) A3)
    (movl (@r+ SP) A4)
    (movl (@r+ SP) AN)
    (movl (@r+ SP) TP)
    (movl ($ -2) S0)
%int-return-restore-loop                                  ; restore temps
    (movl (@r+ SP) (index (@r TASK) S0))
    (incl S0)
    (cmpl ($ (fx/ temp-block-size 4)) S0)          
    (j> %int-return-restore-loop)
    (movl (@r+ SP) S0)
    (movl (@r+ SP) S1)
    (movl (@r+ SP) S2)
    (movl (@r+ SP) S3)
    (rsb)
%int-return-handler
    (movl nil-reg an)
    (rsb))

                              
        
(define (clear-extra-registers)
  (lap ()
    (mnegl ($ 1) S0)
zero-loop                                  ; restore temps
    (clrl (index (@r TASK) S0))
    (incl S0)
    (cmpl ($ (fx/ temp-block-size 4)) S0)
    (j> zero-loop)
    (mnegl ($ 2) NARGS)
    (movl (@r sp) tp)
    (jmp (@r tp))))


(lap-template (0 0 -1 t stack pc-check-return-handler) 
pc-check-return
    (addl2 ($ 4) SP)                            ; pop return address
    (pushl A1)                                  ; code vector of pc
    (pushal (d@r A1 -2))                            ; fixnumized code vector
    (pushal (label gc-template))
    (movl (d@r nil-reg slink/kernel) P)
    (movl (d@r P (static 'really-gc)) P)
  (movl (d@r p 2) p)
    (movl (d@r P -2) TP)
    (jmp (@r TP))
pc-check-return-handler
  (movl nil-reg AN)
  (rsb))

                 
;;; sizes of gc template:
;;; pointer -- n registers + n temps + 1 extra + 2 code vector + tp
;;; scratch -- gc return address + 1 other + n registers + n temps

(lap-template ((+ *pointer-temps* *pointer-registers* 4) 
               (+ *scratch-temps* *scratch-registers* 2) 
               -1 t stack gc-template-handler)       ;; see gc.t
gc-template
  (moval (label %post-gc-nary-setup) (d@r nil-reg slink/nary-setup))
  (addl2 ($ 4) SP)                                  ; pop template 
  (movl (@r+ SP) S0)                              ; pop old code (fixnum)
  (movl (@r+ SP) S1)                              ; pop relocated code
  (cmpl S1 nil-reg)
  (j= gc-continue)                                  ; not relocated
  (subl2 ($ 2) S1)                                  ; fixnumize new code
  (subl3 S0 (d@r SP (fx* (+ *no-of-registers* 3) 4)) S2) ; get old pc
  (addl3 S2 S1 (d@r SP (fx* (+ *no-of-registers* 3) 4)))     ; update pc
gc-continue
  (movl (@r+ SP) P)
  (movl (@r+ SP) A1)
  (movl (@r+ SP) A2)
  (movl (@r+ SP) A3)
  (movl (@r+ SP) A4)
  (movl (@r+ SP) AN)
  (movl (@r+ SP) TP)
  (mnegl ($ 1) S0)
restore-loop                                  ; restore temps
  (movl (@r+ SP) (index (@r TASK) S0))
  (incl S0)
  (cmpl ($ (fx/ temp-block-size 4)) S0)
  (j> restore-loop)
  (movl (@r+ SP) S3)
  (movl (@r+ SP) S2)
  (movl (@r+ SP) S1)
  (movl (@r+ SP) S0)
  (rsb)
gc-template-handler
  (movl nil-reg AN)
  (rsb))
                          

                                                            
(lap-template (0 0 0 nil stack stack-base-handler)
stack-base-template
  (jmp (*d@r nil-reg slink/undefined-effect))
stack-base-handler
  (movl (d@r nil-reg slink/kernel) AN)
  (movl (d@r AN (static 'handle-stack-base)) A1)
  (movl (d@r a1 2) a1)
  (jmp (*d@r nil-reg slink/dispatch-label)))
    

    
(define (lap-relocate frame old-tp new-tp offset)
  (lap ()                 
    (ashl ($ -2) A4 A4)                  ; offset
    (movl (index (d@r A1 2) A4) S1)   ; code
    (subl2 A2 S1)                       ; code-offset
    (addl2 S1 A3)                       ; new code
    (movl A3 (index (d@r A1 2) A4))
    (mnegl ($ 1) NARGS)
    (movl (@r sp) tp)
    (jmp (@r tp))))


(define (current-task)
 (lap ()
  (movl TASK A1)
  (addl2 ($ (fx+ %%task-header-offset 2)) A1)   ; offset is negative !
  (mnegl ($ 2) nargs)
    (movl (@r sp) tp)
    (jmp (@r tp))))


; debugger hacks

(define (@@ address)    ; randomness
  (lap ()
    (addl2 ($ 2) a1)
    (mnegl ($ 2) nargs)
    (movl (@r sp) tp)
    (jmp (@r tp))))

(define-foreign gc_interrupt (gc_interrupt) ignore)

(define (crawl-exhibit-fault-frame frame)
  (cond ((not (foreign-fault-frame? frame))       ; foreign
         (print-register frame 'p 3)
         (print-register frame 'a1 4)
         (print-register frame 'a2 5)
         (print-register frame 'a3 6)
         (print-register frame 'a4 7)
         (print-register frame 'an 8)
         (print-register frame 'tp 9))
        (else
         (format t " In foreign code; no information available~%"))))


(define (trace-fault-frame frame)
  (cond ((alt-bit-set? frame)          
         (move-object (make-pointer frame 0)))           ; foreign cont
        (else
         (let ((tp (extend-elt frame 9)))                ; old TP
           (trace-pointers (make-pointer frame 2) 
                           (fx+ *pointer-registers* 1))     ; trace registers
           (trace-pointers 
            (make-pointer frame (fx+ *pointer-registers* 4))  ; trace temps
            (fx+ *pointer-temps* 1))
           (let ((ptrs (extend-elt frame 0))             ; trace top of stack
                 (size (fault-frame-slots frame)))
             (trace-pointers (make-pointer frame (fx- size 1)) ptrs))
           (if (eq? (extend-elt frame 1) 0)              ; hack-top-of-stack?
               (relocate-random-code frame 2 tp)         ; relocate PC
               (relocate-random-code frame 1 tp))))))    ; relocate top-of-stack

(define (relocate-random-code frame offset old-tp)
  (if (in-old-space? (extend-elt frame offset))
      (lap-relocate frame 
                    old-tp 
                    (extend-elt frame (fx+ *pointer-registers* 3)) 
                    offset)))

(define (make-link-snapper value unit i)
  (lap ()
    (movl (d@r nil-reg slink/snapper-freelist) p)
    (cmpl p nil-reg)
    (j= cons-snapper-1)
    (movl (d@r p 1) an)
    (movl (d@r p -3) (d@r nil-reg slink/snapper-freelist))
    (movl (d@r nil-reg slink/pair-freelist) (d@r p -3))
    (movl p (d@r nil-reg slink/pair-freelist))
foobarfoo
    (movl a1 (d@r an 2))
    (movl a2 (d@r an 6))
    (movl a3 (d@r an 10))
    (movl an a1)
    (mnegl ($ 2) nargs)
    (movl (@r sp) tp)
    (jmp (@r tp))
cons-snapper-1    
    (moval (label link-snapper) an)
    (movl ($ 12) s1)
    (jsb (label %make-extend))
    (jmp (label foobarfoo))))

(define *link-snapper-template*
(lap-template (3 0 1 t heap handle-snapper)
link-snapper
  (movl p an)
  (movl (d@r p 2) p)
  (bicb3 ($ #b11111100) P S0)
  (cmpb S0 ($ tag/extend))                     ; check proc is extend
  (jn= %icall-bad-proc)
  (movl (d@r P -2) TP)                         ; fetch header
  (bicb3 ($ #b11111100) TP S0)                 ; check header is extend
  (cmpb S0 ($ tag/extend))
  (jn= %icall-bad-proc)
  (cmpb (d@r TP -2) ($ header/template))       ; check header is template
  (jn= %icall-check-nary)
  (cmpb (d@r TP template/nargs) NARGS)         ; check number of args
  (j= snap-link)
  (jmp (label %icall-wrong-nargs))
%icall-check-nary
  (cmpb (d@r TP -2) ($ (fx+ header/template 128)))
  (jn= %icall-bad-proc)
  (cmpb (d@r TP template/nargs) NARGS)
  (j> %icall-wrong-nargs)
snap-link
  (movl an (d@r task task/extra-pointer))
  (ashl ($ -2) (d@r an 10) s0)
  (movl (d@r an 6) an)
  (movl p (index (d@r an 2) s0))
  (movl (d@r nil-reg slink/pair-freelist) an)
  (cmpl an nil-reg)
  (j= cons-pair)
  (movl (d@r an -3) (d@r nil-reg slink/pair-freelist))
consed-pair
  (movl (d@r task task/extra-pointer) (d@r an 1))
  (movl (d@r nil-reg slink/snapper-freelist) (d@r an -3))
  (movl an (d@r nil-reg slink/snapper-freelist))
  (jmp (@r TP))
cons-pair
  (jsb (label %make-pair))
  (jmp (label consed-pair))
handle-snapper
  (movl nil-reg AN)
  (rsb)))

