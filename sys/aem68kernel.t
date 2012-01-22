(herald aem68kernel
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
(comment
(define-constant boot/heap1_name 7)
(define-constant boot/heap1_nameL 8)
(define-constant boot/heap1 9)
(define-constant boot/heap2_name 10)
(define-constant boot/heap2_nameL 11)
(define-constant boot/heap2 12)
(define-constant boot/heap-size 13)
(define-constant boot/interrupt-xenoid 3)
(define-constant %%boot-args-size 14)
)

;;; The procedure big_bang MUST come first in this file.     boot-arg-offset
;;; Arglist start_address (ignored)          4                   +2
;;;         datasection_address (ignored)    8
;;;         interrupt_xenoid                 12
;;;         stack_low                        16
;;;         guard1                           20
;;;         guard2                           24
;;;         heap1_name                       28
;;;         heap1_nameL                      32
;;;         heap1_address                    36
;;;         heap2_name                       40
;;;         heap2_nameL                      44
;;;         heap2_address                    48
;;;         heap_size                        52
;;;         debug?                           56


(define (big_bang) 
  (lap (*boot* *the-slink* m68-big-bang interrupt-dispatcher)

    ;; set up global-constants
    (move .l SP A1)  ; save argument pointer we have 14 boot-args
    (move .l ($ (fx+ (fixnum-ashl 14 8) header/general-vector)) (@-r SP))
    (lea (d@r SP 2) A2)                                    ; second arg to boot
    (move .l (d@static P (static '*the-slink*)) nil-reg)
    (move .l A2 (d@nil slink/boot-args))                 ; set up boot-args

    ;; make interrupt handlers accessible from assembly 
    (move .l  (d@static P (static 'interrupt-dispatcher)) 
              (d@nil slink/interrupt-handler))
    (move .l (d@static P (static 'm68-big-bang)) P)
    (move .l (d@r P -2) A2)
    (lea (label big-bang-return) TP)
;;; note that nil-reg is in AN and pointer to boot args in A1
    (jmp (@r A2))                  
big-bang-return
    ;; initialize area,area-frontier and area-limit
    (move .l (d@r A1 (fx* 9 4)) S0)         ; get address of heap boot/heap1
    (move .l S0 (d@r TASK task/area-begin))          
    (move .l S0 (d@r TASK task/area-frontier))       
    (add .l (d@r A1 (fx* 13 4)) S0)      ; add size to base boot/heap-size
    (move .l S0 (d@r TASK task/area-limit))          

    ;; Set up the procedure register P and call boot,
    ;; never to return. (note: arg 2 (*boot-args*) setup above)
    (move .l nil-reg A3)
    (tst .b (d@r A1 56))
    (j= %debug)
    (move .l ($ header/true) A3)
%debug
    (lea (d@r TASK %%task-header-offset) A1)          ; root-process
    (move .l  ($ 4) NARGS)                            ; 3 args
    (move .l  (d@static P (static '*boot*)) P)
    (move .l  (d@r P -2) TP)
    (jmp   (@r TP))))


(define (call-fault-handler) 
  (lap (fault-entry)

    (equate t_fault_$quit                   (fixnum-ashl #x120010 2))
    (equate t_time_$itimer_real             (fixnum-ashl #xD0007 2))

    (move .l ($ t_fault_$quit) A1)
    (btst ($ 1) (d@r TASK task/critical-count))                   
    (jn= %call-fault)
    (move .l ($ t_time_$itimer_real) A1)
%call-fault        
    (lea (d@r SP 6) A2)                          ; frame is 2nd arg
    (move .l (d@static P (static 'fault-entry)) P)
    (move .l (d@r P -2) TP)
    (clr .b (d@r TASK task/critical-count))
    (jmp (@r TP))))                                

(define (exit-and-dheap)
  (lap ()
    (move .l (d@nil slink/boot-args) a1)
    (lea (d@r a1 2) sp)
    (rts)))

;;;; Low-level exception handling

;;; Fault frame at time we get control:  See aegis_fault.t
;;;


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
    (lea (d@r SP (* (+ *pointer-temps* *scratch-temps* 9) 4)) A1)  
    (add .l (@r A1) A1)                        ; diag frame
    (move .l (@r+ SP) (d@r A1 %%df_pc))
    (move .l (@r+ SP) (d@r A1 (fx+ %%df_d0 32)))    ; P
    (move .l (@r+ SP) (d@r A1 (fx+ %%df_d0 36)))    ; A1
    (move .l (@r+ SP) (d@r A1 (fx+ %%df_d0 40)))    ; A2
    (move .l (@r+ SP) (d@r A1 (fx+ %%df_d0 44)))    ; A3
    (move .l (@r+ SP) (d@r A1 (fx+ %%df_d0 48)))    ; AN
    (move .l (@r+ SP) (d@r A1 (fx+ %%df_d0 52)))    ; TP

    (move .l ($ -8) S0)
%fault-restore-loop                                  ; restore temps
    (move .l (@r+ SP) (index (@r TASK) S0))
    (add .l ($ 4) S0)
    (cmp .l ($ temp-block-size) S0)          
    (j< %fault-restore-loop)

    (jbr %return-from-fault)
%fault-frame-handler
    (move .l nil-reg an)
    (rts))

(lap-template (0 0 -1 nil stack handle-foreign-return)
%foreign-return
    (add .w ($ 8) sp)                         ; pop template,header
    (move .l (@r+ SP) (d@r TASK task/foreign-call-cont))
    (jbr %return-from-fault)
handle-foreign-return
    (move .l nil-reg AN)
    (rts))
                 

(lap-template (0 0 -1 nil stack handle-enable-return)
%re-enabled
    (add .w ($ 4) sp)                         ; pop return address
    (jbr %return-from-fault)
handle-enable-return
    (move .l nil-reg AN)
    (rts))

(lap-template (0 0 -1 nil stack handle-doing-gc-return)
%doing-gc-return
    (add .w ($ 4) sp)                         ; pop return address
    (jbr %return-from-fault)
handle-doing-gc-return
    (move .l nil-reg AN)
    (rts)

    
%return-from-fault                                
    (bclr ($ 6) (d@r task task/critical-count))      ; what to do?
    (move .l (d@r sp %%old-sp-offset) sp)     ; get old SP
    (move .l (@r+ sp) tp)       ; DB is A5/TP; SB is A6/TASK
    (move .l (@r+ sp) task)
    (move .l ($ 1) .d0)         ; pfm_$return_to_faulting_code
    (rts))
                                                 


;;; Interrupts can be deferred.   
;;; the task/critical count byte has
;;; bit 7 -- interrupts deferred
;;; bit 6 -- ignore interrupts
;;; bit 1 -- quit pending
;;; bit 0 -- timer interrupt pending

(define (interrupt-dispatcher)     ; s5 is status code
  (lap (fault-entry re-enable-faults gc_interrupt)

    (equate %%old-sp-offset 4)
    (equate %%fault-sp-offset 8)
    (equate %%df_d0       #x6)
    (equate %%df_pc       #x5c)
    (equate fault_$process_interrupt      #x12001f)
    (equate fault_$quit                   #x120010)
    (equate time_$itimer_real             #xD0007)

    (move .l (d@nil slink/current-task) task)    ; restore task
    (btst ($ 6) (d@r task task/critical-count))  ; ignore interrupts?
    (jn= %ignore-interrupt)
    (cmp .l ($ time_$itimer_real) S5)             ; is this a timer interrupt?
    (j= %timer)                                   
    (cmp .l ($ fault_$quit) S5)                   ; is this a ^q?
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
    (move .l (d@static p (static 're-enable-faults)) p)    ; DON'T CONS!!!
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
    (move .l SP AN)
    (add .l (@r SP) AN)              ; diag frame in AN
    (clr .l (d@r task task/foreign-call-cont))     
    (move .l s1 (@-r sp))            ; push foreign continuation
    (sub .l sp s1)                   ; compute frame size
    (asl .l ($ 6) S1)
    (move .b ($ (fx+ header/fault-frame 128)) S1)
    (move .l s1 (@-r sp))            ; push frame size 
    (pea (label %foreign-return))
    (jbr %fault-done)
                                 
;;; registers s0=fault-sp  aN=diag-frame 
%t-code-interrupt
    (move .l (d@r sp %%fault-sp-offset) S0)        ; get fault SP in S0
    (move .l S0 A1)                        ; save fault sp
    (move .l SP AN)                                
    (add .l (@r SP) AN)                  ; save pointer to diagnostic frame AN

    (move .l ($ (fx+ temp-block-size 4)) S2)
%fault-save-loop                              ; save temps and extra p and s
    (move .l (index (d@r TASK -8) S2) (@-r SP))
    (sub .l ($ 4) S2)
    (j>= %fault-save-loop)
                                                                         
    (move .l (d@r AN (fx+ %%df_d0 52)) (@-r SP))        ; TP (a5)
    (move .l (d@r AN (fx+ %%df_d0 48)) (@-r SP))        ; AN (a4)
    (move .l (d@r AN (fx+ %%df_d0 44)) (@-r SP))        ; A3 
    (move .l (d@r AN (fx+ %%df_d0 40)) (@-r SP))        ; A2 
    (move .l (d@r AN (fx+ %%df_d0 36)) (@-r SP))        ; A1 
    (move .l (d@r AN (fx+ %%df_d0 32)) (@-r SP))        ; P  (a0)
    (move .l (d@r AN %%df_pc) S1)
    (move .l S1 (@-r SP))
    (cmp .l (d@nil slink/kernel-begin) S1)
    (j< %not-in-kernel)
    (cmp .l (d@nil slink/kernel-end) S1)
    (j> %not-in-kernel)
    (move .l (@r A1) (@-r SP))             ; save hack top of stack
    (clr .l (@-r SP))                      ; no pointers on top
    (jbr %t-code-done)

%not-in-kernel
    (clr .l (@-r SP))                      ; no hacked stack top

;;; find how many pointers on top of stack
    (move .l ($ -4) s1)                    ; pointer slot counter as fixnum

%find-last-template-loop
    (add .l ($ 4) s1)                      ; incr # pointer counter
    (move .l (@r+ a1) s2)                  ; load next word
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
    (sub .l sp s0)                         ; compute total size of frame
    (asl .l ($ 6) s0)
    (move .b ($ header/fault-frame) s0)
    (move .l s0 (@-r SP))                  ; push fault header
    (pea (label %fault-frame-template))         ; call fault handler

%fault-done                                            
    (asl .l ($ 2) S5)
    (move .l s5 a1)                             ; 1st argument is status
    (lea (d@r SP 6) a2)                         ; 2nd argument is frame
    (move .l (d@static p (static 'fault-entry)) p)   ; ...
    (move .l (d@r p -2) tp)                     ; ...
    (jmp (@r tp))                               ; ...

    ))                           

(define (local-machine)
  (object nil                               
      ((machine-type self)          'apollo)
      ((page-size self)             1024)
      ((object-file-type self)      'mo)
      ((information-file-type self) 'mi)
      ((noise-file-type self)       'mn)
      ((print-type-string self)     "Machine")))

(define (nan? x) (ignore x) '#f)