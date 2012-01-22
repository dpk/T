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

;;; The procedure big_bang MUST come first in this file.
;;; BIG_BANG is called to instantiate the root process of an external
;;; T image. It is called by a foreign stub program with arguments
;;; as follows:
;;;
;;;  (BIG_BANG memory mem-size argc argv bsd4.2?).
;;;
;;; The argument vector is saved as a T vector in *BOOT-ARGS*.  The
;;; Xenoids are created for STDIN and STDOUT and placed in the 2nd
;;; and 3rd argument registers.  The global-constant register (NIL)
;;; and the task register are initialized, and the root process
;;; block is created and initialized.  The stack is initialized.
;;; The heap-pointer and heap-limit of the root process are
;;; initialized.  Finally the address of the T procedure BOOT is
;;; placed in them P (procedure) register, and we jump through the
;;; root process block to ICALL.  Boot is called as follows:
;;;
;;;     (BOOT root-task boot-args),

;;; Unresolved issues:
;;; - Is the arg vector the right size and is the descriptor correct?
;;; - What should the initial stack size be and how can you tell?
;;; - The stack and areas should have guards - later I  guess
;;; - how to boot other systems
;;; - stdio shit?
;;; - PID as Fixnum?
;;; - *the-slink*
;;; - test stack-overflow in icall?
;;; - heap overflow code
;;; - exception code
;;; - interrupt code


;;;  When we enter Big_bang the stack looks as follows:
;;;
;;;              |      debug?   |
;;;              |_______________|
;;;              |      argv     |    Command line argv
;;;              |_______________|
;;;              |      argc     |    Command line argc
;;;              |_______________|
;;;              |  heap-size    |  
;;;              |_______________|
;;;              |     heap2     | 
;;;              |_______________|  
;;;              |     heap1     |
;;;              |_______________|
;;;       SP =>  |          | N  |    ensured by unvax_start_t.s
;;;              |__________|____|
;;;              |    header     |  <= *boot-args*
;;;              |_______________|

(define (big_bang)
  (lap (*boot* *the-slink* vax-big-bang)

    ;; set up global-constants
    (movl  (d@r P (static '*the-slink*)) nil-reg)
    (movl (d@r nil-reg 2) nil-reg)
    (ashl ($ 2) s0 s0)
    (movl  S0 (d@r nil-reg slink/interrupt-handler))    ; interrupt_xenoid
    (movl SP A1)  ; save argument pointer        
                  ; we have 6 *boot-args*
    (pushl ($ (fx+ (fixnum-ashl 6 8) header/general-vector)))
    (moval (d@r SP 2) A2)              ; 2nd arg to boot
    (movl A2 (d@r nil-reg slink/boot-args))    ; we have 6 boot-args

    (movl (d@r P (static 'vax-big-bang)) P)
    (movl (d@r p 2) p)
    (movl (d@r P -2) A2)
    (moval (label big-bang-return) TP)
;;; note that pointer to boot args in A1
    (jmp (@r A2))                  
big-bang-return

    ;; initialize area, area-frontier, and area-limit
    (movl  (d@r A1 4) S0)                       ; move addr heap
    (movl  S0 (d@r TASK task/area-begin))      
    (movl  S0 (d@r TASK task/area-frontier))         
    (addl3 (d@r A1 12) S0 (d@r TASK task/area-limit))

    ;; Set up the procedure register P and call boot,
    ;; never to return. (note: args 2 was setup above)
    (movl nil-reg A3)
    (tstl (d@r A1 24))                             ;check for debug switch
    (beql (to %debug))
    (movl ($ header/true) A3)
%debug
    (moval (d@r TASK %%task-header-offset) A1)     ; root-process
    (movl  ($ 4) NARGS)                            ; 2 args
    (movl  (d@r P (static '*boot*)) P)
    (movl (d@r p 2) p)
    (movl  (d@r P -2) TP)
    (jmp   (@r TP))))

(define (call-fault-handler)
  (lap (signal-handler)

    (equate t-interrupt                   (fixnum-ashl 2 2))
    (equate t-virtual-timer               (fixnum-ashl 26 2))

    (movl ($ t-interrupt) A1)
    (bitb ($ 2) (d@r TASK (fx+ task/critical-count 3)))
    (jn= %call-fault)
    (movl ($ t-virtual-timer) A1)
%call-fault                                
    (moval (d@r SP 6) A2)
    (movl (d@r P (static 'signal-handler)) P)
    (movl (d@r p 2) p)
    (movl (d@r P -2) TP)
    (movb ($ 0) (d@r TASK (fx+ task/critical-count 3)))
    (jmp (@r TP))))                                


;;;; Low-level exception handling

(lap-template (0 0 -1 t stack %fault-frame-handler)
%fault-frame-template
    (bisb2 ($ #b01000000) (d@r task (fx+ task/critical-count 3)))
    (ashl ($ -8) (d@r SP 4) S0)                    ; fault header
    (addl2 ($ 2) S0)                          ; 2 for header and template
    (tstl (d@r SP 12))
    (j= foobar)
    (movl (d@r SP 12) (index (@r SP) S0))   ; restore hacked top of stack
foobar
    (addl2 ($ 16) sp)        ; pop template,header,pointers on stack,hack top
    (movl (d@r SP (* (+ *pointer-temps* *scratch-temps* 10) 4))
             A1)                           ; context
    (movl (@r+ SP) (d@r A1 %%df_pc))
    (movl (@r+ SP) (d@r A1 %%df_r4))    ; P
    (movl (@r+ SP) (d@r A1 %%df_r5))    ; A1
    (movl (@r+ SP) (d@r A1 %%df_r6))    ; A2
    (movl (@r+ SP) (d@r A1 %%df_r7))    ; A3
    (movl (@r+ SP) (d@r A1 %%df_r8))    ; A4
    (movl (@r+ SP) (d@r A1 %%df_r9))    ; AN
    (movl (@r+ SP) (d@r A1 %%df_r10))    ; TP

    (movl ($ -2) S0)
%fault-restore-loop                                  ; restore temps
    (movl (@r+ SP) (index (@r TASK) S0))
    (incl S0)
    (cmpl ($ (fx/ temp-block-size 4)) S0)          
    (j> %fault-restore-loop)
    (addl2 ($ 4) SP)                           ; pop context
    (bicb2 ($ #b01000000) (d@r task (fx+ task/critical-count 3)))
    (rsb)
%fault-frame-handler
    (movl nil-reg an)
    (rsb))

(lap-template (0 0 -1 nil stack handle-foreign-return)
%foreign-return
    (bisb2 ($ #b01000000) (d@r task (fx+ task/critical-count 3)))
    (addl2 ($ 8) sp)                         ; pop template,header
    (movl (@r+ SP) (d@r TASK task/foreign-call-cont))
    (bicb2 ($ #b01000000) (d@r task (fx+ task/critical-count 3)))
    (rsb)
handle-foreign-return
    (movl nil-reg AN)
    (rsb))
                 

(lap-template (0 0 -1 nil stack handle-enable-return)
%re-enabled
    (addl2 ($ 4) sp)                         ; pop return address
    (rsb)
handle-enable-return
    (movl nil-reg AN)
    (rsb))

(lap-template (0 0 -1 nil stack handle-doing-gc-return)
%doing-gc-return
    (addl2 ($ 4) sp)                         ; pop return address
    (rsb)
handle-doing-gc-return
    (movl nil-reg AN)
    (rsb))

;;; Interrupts can be deferred.   
;;; the task/critical count byte has
;;; bit 7 -- interrupts deferred
;;; bit 6 -- interrupts ignored
;;; bit 1 -- quit pending
;;; bit 0 -- timer interrupt pending
        
(define (interrupt_dispatcher)       ; arg pointer is AN
  (lap (signal-handler enable-signals gc_interrupt)

    (equate %%fault-sp-offset 8)
    (equate %%df_r4       -36)                    ; P
    (equate %%df_r5       -32)                    : a1
    (equate %%df_r6       -112)                   ; a2
    (equate %%df_r7       -108)                   ; a3
    (equate %%df_r8       -104)                   ; a4
    (equate %%df_r9       -100)                   ; an
    (equate %%df_r10       -96)                   ; tp
    (equate %%df_pc       12)
    (equate fault-quit      3)
    (equate fault-interrupt                   2)
    (equate fault-virtual-timer               26)
                                             
    (movl (d@r AN 4) A4)                       ; get signal code
    (movl (d@r nil-reg slink/current-task) task)    ; restore task
    (bbs ($ 6) (d@r task (fx+ task/critical-count 3)) (to %ignore-interrupt))
    (movl (d@r AN 12) AN)                      ; get context
    (cmpl ($ fault-virtual-timer) A4)             ; is this a timer interrupt?
    (j= %timer)                                   
    (cmpl ($ fault-interrupt) A4)                   ; is this a ^q?
    (jn= %fault)                                  ; if so ..
    (cmpl (d@r TASK task/doing-gc?) nil-reg)    ; are we doing gc?
    (jn= %doing-gc)                               ; if not ...
    (tstl (d@r TASK task/foreign-call-cont))
    (jn= %fault)
    (bitb ($ 2) (d@r TASK (fx+ task/critical-count 3)))   ; is this the second one?                
    (j= %set-interrupt-flag)                      ; if not, defer interrupt
    (bicb2 ($ 2) (d@r TASK (fx+ task/critical-count 3)))
    (tstb (d@r TASK (fx+ task/critical-count 3)))       ; are interrupts deferred?
    (j= %fault)                                   ; if so ...
%set-interrupt-flag    
    (bisb2 ($ 2) (d@r TASK (fx+ task/critical-count 3)))  ; set quit bit 
    (jmp (label %ignore-interrupt))
%timer
    (cmpl (d@r TASK task/doing-gc?) nil-reg)    ; are we doing gc?
    (jn= %ignore-interrupt)
    (tstb (d@r TASK (fx+ task/critical-count 3)))
    (j= %fault) 
    (bisb2 ($ 1) (d@r TASK (fx+ task/critical-count 3)))  ; set timer bit 
%ignore-interrupt 
    (pushal (label %re-enabled))                     ; re-enable interrupts
    (movl (d@r p (static 'enable-signals)) p)    ; DON'T CONS!!!
    (movl (d@r p 2) p)
    (movl (d@r p -2) tp)
    (jmp (@r tp))                                                       

%doing-gc
    (pushal (label %doing-gc-return))
    (movl (d@r p (static 'gc_interrupt)) p)
    (movl (d@r p 2) p)
    (movl (d@r p -2) tp)
    (jmp (@r tp))                                                       


;;; Interrupts should be disabled here.
%fault
    (movl (d@r task task/foreign-call-cont) S1)
    (j=  %t-code-interrupt)

    ;; Interrupted out of foreign code.
    (clrl (d@r task task/foreign-call-cont))     
    (pushl s1)                       ; push foreign continuation
    (subl2 sp s1)                   ; compute frame size
    (ashl ($ 6) S1 S1)
    (movb ($ (fx+ header/fault-frame 128)) S1)
    (pushl s1)                      ; push frame size 
    (pushal (label %foreign-return))
    (jmp (label %fault-done))
                                 
;;; registers s0=fault-sp  aN=context                                   
%t-code-interrupt                    
    (pushl AN)                  ; save context
    (movl (d@r AN %%fault-sp-offset) S0)        ; get fault SP in S0
    (movl S0 A1)                        ; save fault sp

    (movl ($ (fx/ (fx+ temp-block-size 4) 4)) S2)
%fault-save-loop                              ; save temps and extra p and s
    (pushl (index (d@r TASK -8) S2))
    (decl S2)
    (j>= %fault-save-loop)
                                                                         
    (pushl (d@r AN %%df_r10)) ; TP
    (pushl (d@r AN %%df_r9)) ; AN
    (pushl (d@r AN %%df_r8)) ; A4
    (pushl (d@r AN %%df_r7)) ; A3
    (pushl (d@r AN %%df_r9)) ; A2
    (pushl (d@r AN %%df_r5)) ; A1
    (pushl (d@r AN %%df_r4)) ; P
    (movl (d@r AN %%df_pc) S1)
    (pushl S1) 
    (cmpl (d@r nil-reg slink/kernel-begin) S1)
    (j> %not-in-kernel)
    (cmpl (d@r nil-reg slink/kernel-end) S1)
    (j< %not-in-kernel)
    (pushl (@r A1))              ; save hack top of stack
    (pushl ($ 0))                      ; no pointers on top
    (jmp (label %t-code-done))

%not-in-kernel
    (pushl ($ 0))                      ; no hacked stack top

;;; find how many pointers on top of stack
    (mnegl ($ 1) s1)                    ; pointer slot counter as fixnum

%find-last-template-loop
    (incl s1)                      ; incr # pointer counter
    (movl (@r+ a1) s2)                  ; load next word
    (cmpb ($ header/vframe) s2)          ; vframe?
    (j= %found-frame)                         ; .. if so, done looking

    (bicb3 ($ #b11111100) s2 s3)                        ; copy for extend test
    (cmpb ($ tag/extend) s3)             ; extend?
    (jn=  %find-last-template-loop)        ; .. if not, keep looking
    (cmpb ($ header/template) (d@r s2 -2))               ; fetch template 
    (jn= %find-last-template-loop)        ; .. if high bit is 0, keep looking

%found-frame
    (ashl ($ 2) s1 (@-r SP))                  ; push number of pointers on stack
%t-code-done
    (subl2 sp s0)                         ; compute total size of frame
    (ashl ($ 6) s0 s0)
    (movb ($ header/fault-frame) s0)
    (pushl s0)                  ; push fault header
    (pushal (label %fault-frame-template))         ; call fault handler

%fault-done                                            
    (ashl ($ 2) A4 a1)                        ; 1st argument is signal code
    (moval (d@r SP 6) a2)                         ; 2nd argument is frame
    (movl (d@r p (static 'signal-handler)) p)   ; ...
    (movl (d@r p 2) p)
    (movl (d@r p -2) tp)                     ; ...
    (jmp (@r tp))                               ; ...

    ))                           

(define (local-machine)
  (object nil                               
      ((machine-type self)          'vax/unix)
      ((page-size self)             512)
      ((object-file-type self)      'vo)
      ((information-file-type self) 'vi)
      ((noise-file-type self)       'vn)
      ((print-type-string self)     "Machine")))

(define (nan? x) (ignore x) '#f)

(define (st_mtime stat-block)
  (+ (ash (mref-16-u stat-block 34) 16) 
     (mref-16-u stat-block 32)))

(define-integrable (st_size stat-block)
  (mref-integer stat-block 20))


(define-integrable (st_mode stat-block)
  (mref-16-u stat-block 8))
