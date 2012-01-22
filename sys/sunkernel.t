(herald sunkernel
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

;;; The procedure big_bang MUST come first in this file.     boot-arg-offset
;;;  When we enter Big_bang the stack looks as follows:
;;;              ________________
;;;              |   debug?      |   not a boot arg
;;;              |_______________|
;;;              |      argv     |    Command line argv
;;;              |_______________|
;;;              |      argc     |    Command line argc
;;;              |_______________|
;;;              |  heap-size    |    Size of the static storage area
;;;              |_______________|
;;;              |     heap2     | Base addresss of static
;;;              |_______________|        storage area
;;;              |     heap1     |
;;;              |_______________|
;;;       SP =>  |  return addr  |
;;;              |_______________|


;++ replace the numbers 1 and 3 below with boot/heap1 and boot/heap-size


(define (big_bang) 
  (lap (*the-slink* m68-big-bang *boot*)

    ;; set up global-constants
    (move .l  (d@static P (static '*the-slink*)) nil-reg)
    (asl .l ($ 2) S0)
    (move .l  S0 (d@nil slink/interrupt-handler))    ; interrupt_xenoid
    (move .l SP A1)  ; save argument pointer we have 6 boot-args
    (move .l ($ (fx+ (fixnum-ashl 6 8) header/general-vector)) (@-r SP))
    (lea (d@r SP 2) A2)                                    ; second arg to boot
    (move .l A2 (d@nil slink/boot-args))                 ; set up boot-args

    (move .l (d@static P (static 'm68-big-bang)) P)
    (move .l (d@r P -2) A2)
    (lea (label big-bang-return) TP)
;;; note that nil-reg is in AN and pointer to boot args in A1
    (jmp (@r A2))                  
big-bang-return
    ;; initialize area,area-frontier and area-limit
    (move .l (d@r A1 4) S0)         ; get address of heap
    (move .l S0 (d@r TASK task/area-begin))          
    (move .l S0 (d@r TASK task/area-frontier))       
    (add .l (d@r A1 12) S0)         ; add size to base
    (move .l S0 (d@r TASK task/area-limit))          

    ;; Set up the procedure register P and call boot,
    ;; never to return. (note: arg 2 (*boot-args*) setup above)
    (move .l nil-reg A3)
    (tst .b (d@r A1 24))
    (j= %debug)
    (move .l ($ header/true) A3)
%debug
    (lea (d@r TASK %%task-header-offset) A1)          ; root-process
    (move .l  ($ 4) NARGS)                            ; 3 args
    (move .l  (d@static P (static '*boot*)) P)
    (move .l  (d@r P -2) TP)
    (jmp   (@r TP))))

(define (call-fault-handler)
  (lap (signal-handler)

    (equate t-interrupt                     (fixnum-ashl 2 2))
    (equate t-virtual-timer                 (fixnum-ashl 26 2))

    (move .l ($ t-interrupt) A1)
    (btst ($ 1) (d@r TASK task/critical-count))                   
    (jn= %call-fault)
    (move .l ($ t-virtual-timer) A1)
%call-fault                                
    (lea (d@r SP 6) A2)
    (move .l (d@static P (static 'signal-handler)) P)
    (move .l (d@r P -2) TP)
    (clr .b (d@r TASK task/critical-count))
    (jmp (@r TP))))                                


;;;; Low-level exception handling

(lap-template (0 0 -1 t stack %fault-frame-handler)
%fault-frame-template
    (bset ($ 6) (d@r task task/critical-count))
    (move .l (d@r SP 4) S0)                    ; fault header
    (asr .l ($ 8) S0)
    (add .l ($ 2) S0)                          ; 2 for header and template
    (asl .l ($ 2) S0)
    (tst .l (d@r SP 12))
    (j= foobar)
    (move .l (d@r SP 12) (index (@r SP) S0))   ; restore hacked top of stack
foobar
    (add .w ($ 16) sp)        ; pop template,header,pointers on stack,hack top
    (move .l (d@r SP (* (+ *pointer-temps* *scratch-temps* 9) 4))
             A1)                           ; context
    (move .l (@r+ SP) (d@r A1 %%df_pc))
    (move .l (@r+ SP) (d@r A1 %%df_a0))    ; P
    (move .l (@r+ SP) (d@r A1 %%df_a1))    ; A1
    (move .l (@r+ SP) (d@r A1 %%df_a2))    ; A2
    (move .l (@r+ SP) (d@r A1 %%df_a3))    ; A3
    (move .l (@r+ SP) (d@r A1 %%df_a4))    ; AN
    (move .l (@r+ SP) (d@r A1 %%df_a5))    ; TP

    (move .l ($ -8) S0)
%fault-restore-loop                                  ; restore temps
    (move .l (@r+ SP) (index (@r TASK) S0))
    (add .l ($ 4) S0)
    (cmp .l ($ temp-block-size) S0)          
    (j< %fault-restore-loop)
    (add .w ($ 4) SP)                           ; pop context
    (bclr ($ 6) (d@r task task/critical-count))
    (rts)
%fault-frame-handler
    (move .l nil-reg an)
    (rts))

(lap-template (0 0 -1 nil stack handle-foreign-return)
%foreign-return
    (bset ($ 6) (d@r task task/critical-count))
    (add .w ($ 8) sp)                         ; pop template,header
    (move .l (@r+ SP) (d@r TASK task/foreign-call-cont))
    (bclr ($ 6) (d@r task task/critical-count))
    (rts)
handle-foreign-return
    (move .l nil-reg AN)
    (rts))
                 

(lap-template (0 0 -1 nil stack handle-enable-return)
%re-enabled
    (add .w ($ 4) sp)                         ; pop return address
    (rts)
handle-enable-return
    (move .l nil-reg AN)
    (rts))

(lap-template (0 0 -1 nil stack handle-doing-gc-return)
%doing-gc-return
    (add .w ($ 4) sp)                         ; pop return address
    (rts)
handle-doing-gc-return
    (move .l nil-reg AN)
    (rts))

;;; Interrupts can be deferred.   
;;; the task/critical count byte has
;;; bit 7 -- interrupts deferred
;;; bit 6 -- interrupts ignored
;;; bit 1 -- quit pending
;;; bit 0 -- timer interrupt pending

(define (interrupt_dispatcher)    ; code in S0, context in A1 
  (lap (signal-handler enable-signals gc_interrupt)

    (equate %%fault-sp-offset 8)               
    (equate %%df_a0 -20)
    (equate %%df_a1 -16)
    (equate %%df_a2 -128)
    (equate %%df_a3 -124)
    (equate %%df_a4 -120)
    (equate %%df_a5 -116)
    (equate %%df_pc       12)
    (equate fault-quit      3)
    (equate fault-interrupt                   2)
    (equate fault-virtual-timer               26)
                                             
    (move .l (d@static P (static '*the-slink*)) nil-reg)
    (move .l nil-reg AN)                          ; move slink to a-reg
    (move .l (d@r AN slink/current-task) task)    ; restore task
    (btst ($ 6) (d@r task task/critical-count))
    (jn= %ignore-interrupt)
    (cmp .l ($ fault-virtual-timer) S0)             ; is this a timer interrupt?
    (j= %timer)                                   
    (cmp .l ($ fault-interrupt) S0)                   ; is this a ^q?
    (jn= %fault)                                  ; if so ..
    (cmp .l (d@r TASK task/doing-gc?) nil-reg)    ; are we doing gc?
    (jn= %doing-gc)                               ; if not ...
    (tst .l (d@r TASK task/foreign-call-cont))
    (jn= %fault)
    (btst ($ 1) (d@r TASK task/critical-count))   ; is this the second one?                
    (j= %set-interrupt-flag)                      ; if not, defer interrupt
    (bclr ($ 1) (d@r TASK task/critical-count))
    (tst .b (d@r TASK task/critical-count))       ; are interrupts deferred?
    (j= %fault)             
%set-interrupt-flag                      ; if so ...
    (or .b ($ 2) (d@r TASK task/critical-count))  ; set quit bit 
    (jbr %ignore-interrupt)
%timer
    (cmp .l (d@r TASK task/doing-gc?) nil-reg)    ; are we doing gc?
    (jn= %ignore-interrupt)
    (tst .b (d@r TASK task/critical-count))
    (j= %fault) 
    (or .b ($ 1) (d@r TASK task/critical-count))  ; set timer bit 
%ignore-interrupt 
    (pea (label %re-enabled))                     ; re-enable interrupts
    (move .l (d@static p (static 'enable-signals)) p)    ; DON'T CONS!!!
    (move .l (d@r p -2) tp)
    (jmp (@r tp))                                                       

%doing-gc
    (pea (label %doing-gc-return))
    (move .l (d@static p (static 'gc_interrupt)) p)   
    (move .l (d@r p -2) tp)
    (jmp (@r tp))                                                       


;;; Interrupts should be disabled here.
%fault
    (move .l (d@r task task/foreign-call-cont) S1)
    (j=  %t-code-interrupt)

    ;; Interrupted out of foreign code.
    (clr .l (d@r task task/foreign-call-cont))     
    (move .l s1 (@-r sp))            ; push foreign continuation
    (sub .l sp s1)                   ; compute frame size
    (asl .l ($ 6) S1)
    (move .b ($ (fx+ header/fault-frame 128)) S1)
    (move .l s1 (@-r sp))            ; push frame size 
    (pea (label %foreign-return))
    (jbr %fault-done)
                                 
;;; registers s4=fault-sp  a1=context
%t-code-interrupt                    
    (move .l A1 (@-r SP))                  ; save context
    (move .l (d@r A1 %%fault-sp-offset) S4)        ; get fault SP in S4
    (move .l S4 AN)                        ; save fault sp

    (move .l ($ (fx+ temp-block-size 4)) S2)
%fault-save-loop                              ; save temps and extra p and s
    (move .l (index (d@r TASK -8) S2) (@-r SP))
    (sub .l ($ 4) S2)
    (j>= %fault-save-loop)
                                                                         
    (move .l (d@r A1 %%df_a5) (@-r SP))        ; TP (a5)
    (move .l (d@r A1 %%df_a4) (@-r SP))        ; AN (a4)
    (move .l (d@r A1 %%df_a3) (@-r SP))        ; A3 
    (move .l (d@r A1 %%df_a2) (@-r SP))        ; A2 
    (move .l (d@r A1 %%df_a1) (@-r SP))        ; A1 
    (move .l (d@r A1 %%df_a0) (@-r SP))        ; P  (a0)
    (move .l (d@r A1 %%df_pc) S1)
    (move .l S1 (@-r SP))
    (move .l nil-reg A1)                                                                   
    (cmp .l (d@r A1 slink/kernel-begin) S1)
    (j< %not-in-kernel)
    (cmp .l (d@r A1 slink/kernel-end) S1)
    (j> %not-in-kernel)
    (move .l (@r AN) (@-r SP))             ; save hack top of stack
    (clr .l (@-r SP))                      ; no pointers on top
    (jbr %t-code-done)

%not-in-kernel
    (clr .l (@-r SP))                      ; no hacked stack top

;;; find how many pointers on top of stack
    (move .l ($ -4) s1)                    ; pointer slot counter as fixnum

%find-last-template-loop
    (add .l ($ 4) s1)                      ; incr # pointer counter
    (move .l (@r+ an) s2)                  ; load next word
    (cmp .b ($ header/vframe) s2)          ; vframe?
    (j= %found-frame)                         ; .. if so, done looking

    (move .w s2 s3)                        ; copy for extend test
    (and .b ($ 3) s3)
    (cmp .b ($ tag/extend) s3)             ; extend?
    (jn=  %find-last-template-loop)        ; .. if not, keep looking
    (move .l s2 a3)                        ; copy extend pointer to fetch tem
    (move .l (d@r a3 -2) s3)               ; fetch template 
    (jpos %find-last-template-loop)        ; .. if high bit is 0, keep looking

%found-frame
    (move .l s1 (@-r sp))                  ; push number of pointers on stack
%t-code-done
    (sub .l sp s4)                         ; compute total size of frame
    (asl .l ($ 6) s4)
    (move .b ($ header/fault-frame) s4)
    (move .l s4 (@-r SP))                  ; push fault header
    (pea (label %fault-frame-template))         ; call fault handler

%fault-done                                            
    (asl .l ($ 2) S0)
    (move .l s0 a1)                             ; 1st argument is signal code
    (lea (d@r SP 6) a2)                         ; 2nd argument is frame
    (move .l (d@static p (static 'signal-handler)) p)   ; ...
    (move .l (d@r p -2) tp)                     ; ...
    (jmp (@r tp))                               ; ...

    ))                           
                    
(define (local-machine)
  (object nil                               
      ((machine-type self)          'sun)
      ((page-size self)             2048)
      ((object-file-type self)      'mo)
      ((information-file-type self) 'mi)
      ((noise-file-type self)       'mn)
      ((print-type-string self)     "Machine")))

(define (nan? x)
  (or (fx= (isnan x) 1)
      (fx= (isinf x) 1)))

(define-foreign isnan (isnan (in rep/double)) rep/integer)
(define-foreign isinf (isinf (in rep/double)) rep/integer)

(define (st_mtime stat-block)
  (+ (ash (mref-16-u stat-block 28) 16) 
     (mref-16-u stat-block 30)))

(define-integrable (st_size stat-block)
  (mref-integer stat-block 16))


(define-integrable (st_mode stat-block)
  (mref-16-u stat-block 6))
