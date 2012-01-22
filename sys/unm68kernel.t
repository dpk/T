(herald m68kernel (env tsys))

;;; note that A1 must not be destroyed and nil-reg is in AN
;;; return is in TP

(define (m68-big-bang) 
  (lap (big_bang icall-bad-proc
        icall-wrong-nargs handle-stack-base
        handle-undefined-effect
        really-gc pc-code-vector  heap-overflow-error
        call-fault-handler cont-wrong-nargs)

    (move .l  nil-reg (d@nil -3))            ; (cdr '()) = '()
    (move .l  nil-reg (d@nil 1))             ; (car '()) = '()

    (move .l  P (d@nil slink/kernel))        ; save kernel pointer
    (lea (label %undefined-effect) A3)
    (move .l A3       (d@nil  slink/undefined-effect ))
    (lea (label %make-extend) A3)
    (move .l A3       (d@nil  slink/make-extend ))
    (lea (label %make-pair) A3)
    (move .l A3       (d@nil  slink/make-pair ))
    (lea (label %nary-setup) A3)
    (move .l A3       (d@nil  slink/nary-setup ))
    (lea (label %set) A3)
    (move .l A3       (d@nil  slink/set ))
    (lea (label %icall) A3)
    (move .l A3 (d@nil  slink/icall ))
    (lea (label %cont-wrong-nargs) A3)
    (move .l A3 (d@nil  slink/cont-wrong-nargs ))

    (lea (label %kernel-begin) A3)
    (move .l A3 (d@nil slink/kernel-begin))
    (lea (label %kernel-end) A3)
    (move .l A3 (d@nil slink/kernel-end))

 
    ;; initialize root process, stored in outer space?  
    ;; zero  out extra registers

    (move .l ($ temp-block-size) S0)
initialize-loop     
    (clr .l (@-r sp))  
    (sub .l ($ 4) S0)
    (tst .l S0)
    (j> initialize-loop)
    (move .l SP TASK)                                ; load task reg
    (add  .l ($ (fx+ %%task-header-offset 4)) SP)    ; allocate task block
    (move .l ($ header/task) (@-r SP))               ; task header
    (move .l SP A3)
    (add  .l ($ 2) A3)
    (move .l A3 (d@nil slink/root-process))         ; ptr to root and
    (move .l A3 (d@nil slink/current-task))         ; current process

    ;; initialize stack
    (pea  (d@r A3 0))                                ; task block
    (move .l nil-reg (@-r SP))                       ; no parent
    (clr  .l (@-r SP))                               ; active, no current sz
    (move .l ($ (fixnum-ashl %%stack-size 2)) (@-r SP))    ; total stack size
    (move .l ($ #xBADBAD) (@-r SP))                  ; distinguished value
    (pea (label stack-base-template))                ; stack base

    ;; initialize root process
;++  (move .l SP A3)
;++  (add  .l ($ 2) A3)
;++  (move .l A3 (d@r TASK task/stack))       ; set stack in root-process
;++ what to do, task/stack is a fixnum not an extend as it should be!
    (move .l SP (d@r TASK task/stack))       ; set stack in root-process
    (clr  .l (d@r TASK task/extra-pointer))
    (clr  .l (d@r TASK task/extra-scratch))
    (move .l nil-reg (d@r TASK task/dynamic-state))
    ;; initialize area,area-frontier and area-limit
    (move .l nil-reg (d@r TASK task/doing-gc?))
    (clr .l (d@r TASK task/foreign-call-cont))
    (clr .l (d@r TASK task/critical-count))
    (move .l nil-reg (d@r TASK task/k-list))
    (move .l nil-reg (d@r TASK task/gc-weak-set-list))
    (move .l nil-reg (d@r TASK task/gc-weak-alist-list))
    (move .l nil-reg (d@r TASK task/gc-weak-table-list))
    (move .l nil-reg (d@nil slink/snapper-freelist))
    (move .l nil-reg (d@nil slink/pair-freelist))
    (move .l (d@static P (static 'big_bang)) P)
    (jmp (@r TP))
 

%make-pair
    ;; return pair in AN
    (bset ($ 7) (d@r TASK task/critical-count))
    (move .l (d@r TASK task/area-frontier) AN)
    (add .l ($ 8) AN)
    (cmp .l (d@r TASK task/area-limit) AN)
    (j> %make-pair-heap-overflow)
%make-pair-continue
    (move .l AN (d@r TASK task/area-frontier))
    (sub .l ($ (fx- 8 tag/pair)) AN)             
    (clr .l (d@r AN -3))
    (clr .l (d@r AN 1))
    (and .b ($ #x7f) (d@r TASK task/critical-count))
    (jn= %deferred-interrupts)
    (rts)
                   
%make-pair-heap-overflow
    (move .l ($ header/true) (d@r TASK task/doing-gc?))
    (jsr (label %heap-overflow))
    (move .l (d@r TASK task/area-frontier) AN)
    (add .l ($ 8) AN)
    (cmp .l (d@r TASK task/area-limit) AN)
    (j> %horrible-heap-overflow)
    (bset ($ 7) (d@r TASK task/critical-count))
    (move .l nil-reg (d@r TASK task/doing-gc?))
    (jbr %make-pair-continue)

%make-extend
    ;; receive descriptor in An, size in S1, return extend in AN
    (bset ($ 7) (d@r TASK task/critical-count))
    (move .l (d@r TASK task/area-frontier) S2) ; get area-frontier
    (add .l ($ 4) S1)
    (add .l S2 S1)                          ; one for the descriptor
    (cmp .l (d@r TASK task/area-limit) S1)
    (j> %make-extend-heap-overflow)
%make-extend-continue
    (move .l S1 (d@r TASK task/area-frontier)) ; S1 is end, S2 is begin
    (exg AN S2)                                ; AN is template
    (move .l S2 (@r+ AN))                      ; store template
    (cmp .l S1 AN)
    (j= copy-done)
    (move .l AN S2)                            ; save extend
extend-loop
    (clr .l (@r+ AN))
    (cmp .l S1 AN)
    (j< extend-loop)
    (move .l S2 AN)
copy-done
    (sub .l ($ 2) AN)
    (and .b ($ #x7f) (d@r TASK task/critical-count))
    (jn= %deferred-interrupts)
    (rts)
                     
%make-extend-heap-overflow
    (move .l ($ header/true) (d@r TASK task/doing-gc?))
    (sub .l S2 S1)
    (jsr (label %heap-overflow))
    (move .l (d@r TASK task/area-frontier) S2) ; get area-frontier
    (add .l S2 S1)                          
    (cmp .l (d@r TASK task/area-limit) S1)
    (j> %horrible-heap-overflow)
    (bset ($ 7) (d@r TASK task/critical-count))
    (move .l nil-reg (d@r TASK task/doing-gc?))
    (jbr %make-extend-continue)


%heap-overflow   
    (movem .l '(d0 d1 d2 d3 d4 d5) (@-r SP))                 ; save scratch registers
    (move .l ($ temp-block-size) S0)
save-loop                                  ; save temps
    (move .l (index (d@r TASK -4) S0) (@-r SP))
    (sub .l ($ 4) S0)
    (j>= save-loop)
    (movem .l '(a0 a1 a2 a3 a4 a5) (@-r SP))
    (move .l (d@r SP (* (+ *no-of-registers* 3) 4)) A1)   ; one for TP 2 return
    (pea (label pc-check-return))
    (move .l nil-reg P)
    (move .l (d@r P slink/kernel) P)
    (move .l (d@static P (static 'pc-code-vector)) P)
    (move .l (d@r P -2) TP)
    (jmp (@r TP))                                ; call pc-code-vector
    
%icall                     
  (move .w P S0)
  (and .b ($ 3) S0)
  (cmp .b ($ tag/extend) S0)                     ; check proc is extend
  (jn= %icall-bad-proc)
  (move .l (d@r P -2) TP)                         ; fetch template
  (move .w TP S0)
  (and .b ($ 3) S0)                 ; check proc is extend
  (cmp .b ($ tag/extend) S0)
  (jn= %icall-bad-proc)
  (move .l  (d@r TP -2) S0)       ; check template is valid (high bit set)
  (j>= %icall-bad-proc)
  (cmp .b (d@r TP template/nargs) NARGS)         ; check number of args
  (j= %icall-ok)
  (j< %icall-wrong-nargs)
  (btst ($ 30) S0)                            ; check nary bit
  (j= %icall-wrong-nargs)
%icall-ok
  (jmp (@r TP))

%icall-bad-proc
  (move .l a1 (d@r TASK task/t0))
  (move .l a2 (d@r TASK (fx+ task/t0 4)))
  (move .l a3 (d@r TASK (fx+ task/t0 8)))
  (clr .l s0)
  (jsr (label %nary-setup))
  (move .l an a2)
  (move .l p a1)
  (move .l (d@nil slink/kernel) P)
  (move .l (d@static P (static 'icall-bad-proc)) P)
  (move .l (d@r P -2) TP)
  (jmp  (@r TP))

%icall-wrong-nargs
  (move .l a1 (d@r TASK task/t0))
  (move .l a2 (d@r TASK (fx+ task/t0 4)))
  (move .l a3 (d@r TASK (fx+ task/t0 8)))
  (clr .l s0)
  (jsr (label %nary-setup))
  (move .l an a2)
  (move .l p a1)
  (move .l (d@nil slink/kernel) P)
  (move .l (d@static P (static 'icall-wrong-nargs)) P)
  (move .l (d@r P -2) TP)
  (jmp  (@r TP))


%deferred-interrupts
    (movem .l '(d0 d1 d2 d3 d4 d5) (@-r SP))
    (move .l ($ (fx+ temp-block-size 4)) S2)
%int-save-loop                              ; save temps and extra p and s
    (move .l (index (d@r TASK -8) S2) (@-r SP))
    (sub .l ($ 4) S2)
    (j>= %int-save-loop)
    (movem .l '(a0 a1 a2 a3 a4 a5) (@-r SP))
    (clr .l (@-r SP))               ; pc
    (move .l (d@r SP (* (+ *pointer-temps* *scratch-temps* 15) 4)) (@-r SP))           
    (clr .l (@-r SP))               ; no pointers on top
    (move .l ($ (+ (fixnum-ashl (+ *pointer-temps* *scratch-temps* 17) 8)
                   header/fault-frame))
             (@-r SP))
    (pea (label %int-return))
    (move .l (d@nil slink/kernel) P)
    (move .l (d@static P (static 'call-fault-handler)) P)
    (move .l (d@r P -2) TP)                      
    (jmp (@r TP))


%kernel-begin

%cont-wrong-nargs
  (neg .l nargs)
  (move .l a1 (d@r TASK task/t0))
  (move .l a2 (d@r TASK (fx+ task/t0 4)))
  (move .l a3 (d@r TASK (fx+ task/t0 8)))
  (clr .l s0)
  (jsr (label %nary-setup))
  (move .l an a2)
  (lea (d@r sp 2) a1)
  (move .l (d@nil slink/kernel) P)
  (move .l (d@static P (static 'cont-wrong-nargs)) P)
  (move .l (d@r P -2) TP)
  (jmp  (@r TP))
                
%post-gc-nary-setup
  (move .l ($ -1) S1)
  (jbr %real-nary-setup)                   
  
%nary-setup   
  (clr .l S1)       
%real-nary-setup                                ; not just after GC
  (asl .l ($ 2) S0)                                 ; required args in S0
  (sub .l ($ 2) NARGS)         
  (asl .l ($ 2) NARGS)                                 ; m68 index mode
  (move .l nil-reg AN)
  (move .l P (d@r TASK task/extra-pointer))
  (bset ($ 7) (d@r TASK task/critical-count))
  (jmp (label %nary-test))
%nary-loop
  (move .l AN P)                               ; accumulate in P
  (move .l (d@r TASK task/area-frontier) AN)
  (add .l ($ 8) AN)
  (cmp .l (d@r TASK task/area-limit) AN)
  (j> %nary-make-pair-heap-overflow)
%nary-make-pair-continue                        ; lose, lose
  (move .l AN (d@r TASK task/area-frontier))
  (sub .l ($ (fx- 8 tag/pair)) AN)             
  (clr .l (d@r AN -3))
  (clr .l (d@r AN 1))
  (move .l P (d@r AN -3))                      ; set cdr
  (move .l (index (@r TASK) NARGS) (d@r AN 1)) ; set car
  (sub .l ($ 4) NARGS)
%nary-test
  (cmp .l NARGS S0)
  (j<= %nary-loop)
  (tst .l S1)
  (jn= nary-clear-extras)
  (move .l (d@r TASK task/extra-pointer) P)                            
  (and .b ($ #x7f) (d@r TASK task/critical-count))
  (jn= %deferred-interrupts)
  (rts)     
nary-clear-extras
  (cmp .l ($ 12) S0)
  (j>= foo45)
  (move .l ($ 12) S0)
foo45
  (clr .l (index (@r TASK) S0))
  (add .l ($ 4) S0)
  (cmp .l ($ temp-block-size) S0)
  (j< foo45)
  (lea (label %nary-setup) P)
  (move .l P (d@nil slink/nary-setup))
  (move .l (d@r TASK task/extra-pointer) P)                            
  (and .b ($ #x7f) (d@r TASK task/critical-count))
  (jn= %deferred-interrupts)
  (rts)     

  
  

%nary-make-pair-heap-overflow
    (move .l ($ header/true) (d@r TASK task/doing-gc?))
    (jsr (label %heap-overflow))
    (move .l (d@r TASK task/area-frontier) AN)
    (add .l ($ 8) AN)
    (cmp .l (d@r TASK task/area-limit) AN)
    (j> %horrible-heap-overflow)
    (bset ($ 7) (d@r TASK task/critical-count))
    (move .l nil-reg (d@r TASK task/doing-gc?))
    (jbr %nary-make-pair-continue)

%set                                        ; a location is (unit  . index)
   ;;  vcell in extra-pointer
   (bset ($ 7) (d@r TASK task/critical-count))
   (movem .l '(a0 a1 a2 a3 a4) (@-r sp))
   (move .l (d@r TASK task/extra-pointer) a3)
   (move .l (d@r A3 6) A1)                  ; get locations
   (move .l (d@r A1 2) A1)                  ; get the vector in A1
   (move .l (d@r A1 -2) SCRATCH)
   (asr .l ($ 8) SCRATCH)                        ; length in S0
   (asl .l ($ 2) SCRATCH)
   (jbr %set-test)
%set-loop
   (move .l (d@nil slink/snapper-freelist) an)
   (cmp .l an nil-reg)
   (j= cons-snapper)
   (move .l (d@r an 1) p)
   (move .l (d@r an -3) (d@nil slink/snapper-freelist))
   (move .l (d@nil slink/pair-freelist) (d@r an -3))
   (move .l an (d@nil slink/pair-freelist))
%real-top
   (move .l (index (d@r A1 -6) SCRATCH) A2)      ; get unit
   (move .l (index (d@r A1 -2) SCRATCH) AN)      ; get index
   (move .l (d@r a3 2) (d@r p 2))
   (move .l a2 (d@r p 6))
   (move .l an (d@r p 10))
   (move .l p (index (d@r A2 2) AN))
   (sub .l ($ 8) SCRATCH)
%set-test
   (tst .l SCRATCH)
   (jn= %set-loop)
   (movem .l (@r+ sp) '(a0 a1 a2 a3 a4))
   (and .b ($ #x7f) (d@r TASK task/critical-count))
   (jn= %deferred-interrupts)
   (rts)
cons-snapper
   (move .l (d@r TASK task/area-frontier) AN)
   (add .l ($ 16) AN)
   (cmp .l (d@r TASK task/area-limit) AN)
   (j> %set-heap-overflow)
%set-continue                        ; lose, lose
   (move .l AN (d@r TASK task/area-frontier))
   (lea (d@r an -14) p)
   (lea (label link-snapper) a2)
   (move .l a2 (d@r p -2))
   (jbr %real-top)
%set-heap-overflow
    (move .l ($ header/true) (d@r TASK task/doing-gc?))
    (move .l ($ (+ (fixnum-ashl 5 16) (fixnum-ashl 1 8) header/vframe )) (@-r sp))
    (move .l (d@r sp 24) (@-r sp))
    (jsr (label %heap-overflow))
    (move .l (@r sp) (d@r sp 28))
    (add .w ($ 8) sp)
    (move .l (d@r TASK task/area-frontier) AN)
    (add .l ($ 16) AN)
    (cmp .l (d@r TASK task/area-limit) AN)
    (j> %horrible-heap-overflow)
    (bset ($ 7) (d@r TASK task/critical-count))
    (move .l nil-reg (d@r TASK task/doing-gc?))
    (jbr %set-continue)

%kernel-end

%horrible-heap-overflow
  (add .w ($ 4) SP)
  (bclr ($ 7) (d@r TASK task/critical-count))
  (move .l nil-reg (d@r TASK task/doing-gc?))
  (move .l (d@nil slink/kernel) P)
  (move .l (d@static P (static 'heap-overflow-error)) P)
  (move .l (d@r P -2) TP)
  (jmp (@r TP))
  
%undefined-effect    ; a1 is string
  (move .l TP A2)              ; template
  (move .l (d@nil slink/kernel) P)
  (move .l (d@static P (static 'handle-undefined-effect)) P)
  (move .l (d@r P -2) TP)
  (jmp (@r TP))
  


))                                     

(lap-template (0 0 -1 t stack %int-return-handler)
%int-return
    (bset ($ 6) (d@r task task/critical-count))
    (move .l (d@r SP 12) (d@r SP (* (+ *pointer-temps* *scratch-temps* 19) 4)))
    (add .w ($ 20) sp)        ; pop template,header,pointers on stack,hack top,pc
    (movem .l (@r+ SP) '(a0 a1 a2 a3 a4 a5))
    (move .l ($ -8) S0)
%int-return-restore-loop                                  ; restore temps
    (move .l (@r+ SP) (index (@r TASK) S0))
    (add .l ($ 4) S0)
    (cmp .l ($ temp-block-size) S0)          
    (j< %int-return-restore-loop)
    (movem .l (@r+ SP) '(d0 d1 d2 d3 d4 d5))
    (bclr ($ 6) (d@r task task/critical-count))
    (rts)
%int-return-handler
    (move .l nil-reg an)
    (rts))




(define (clear-extra-registers)
  (lap ()
    (move .l ($ -4) S0)
zero-loop                                  ; restore temps
    (clr .l (index (@r TASK) S0))
    (add .l ($ 4) S0)
    (cmp .l ($ temp-block-size) S0)
    (j< zero-loop)
    (move .l ($ -2) NARGS)
    (move .l (@r sp) tp)
    (jmp (@r tp))))
    

(lap-template (0 0 -1 t stack pc-check-return-handler) 
pc-check-return
    (add .l ($ 4) SP)                            ; pop return address
    (move .l A1 (@-r SP))                        ; code vector of pc
    (pea (d@r A1 -2))                            ; fixnumized code vector
    (pea (label gc-template))
    (move .l (d@nil slink/kernel) P)
    (move .l (d@static P (static 'really-gc)) P)
    (move .l (d@r P -2) TP)
    (jmp (@r TP))
pc-check-return-handler
  (move .l nil-reg AN)
  (rts))

                 
;;; sizes of gc template:
;;; pointer -- n registers + n temps + 1 extra + 2 code vector + tp
;;; scratch -- gc return address + 1 other + n registers + n temps

(lap-template ((+ *pointer-temps* *pointer-registers* 4) 
               (+ *scratch-temps* *scratch-registers* 2) 
               -1 t stack gc-template-handler)       ;; see gc.t
gc-template               
  (lea (label %post-gc-nary-setup) P)
  (move .l P (d@nil slink/nary-setup))
  (add .w ($ 4) SP)                                  ; pop template 
  (move .l (@r+ SP) S0)                              ; pop old code (fixnum)
  (move .l (@r+ SP) S1)                              ; pop relocated code
  (cmp .l S1 nil-reg)
  (j= gc-continue)                                   ; not relocated
  (sub .l ($ 2) S1)                                  ; fixnumize new code
  (move .l (d@r SP (* (+ *no-of-registers* 3) 4)) S2); get old pc
  (sub .l S0 S2)                                     ; offset
  (add .l S2 S1)                                     ; new pc
  (move .l S1 (d@r SP (* (+ *no-of-registers* 3) 4))); update pc
gc-continue
  (movem .l (@r+ SP) '(a0 a1 a2 a3 a4 a5))
  (move .l ($ -4) S0)
restore-loop                                  ; restore temps
  (move .l (@r+ SP) (index (@r TASK) S0))
  (add .l ($ 4) S0)
  (cmp .l ($ temp-block-size) S0)
  (j< restore-loop)
  (movem .l (@r+ SP) '(d0 d1 d2 d3 d4 d5))
  (rts)
gc-template-handler
  (move .l nil-reg AN)
  (rts))
                          
                                                            
(lap-template (0 0 0 nil stack stack-base-handler)
stack-base-template
  (jmp (*d@nil slink/undefined-effect))
stack-base-handler
  (move .l (d@nil slink/kernel) AN)
  (move .l (d@static AN (static 'handle-stack-base)) A1)
  (jmp (*d@nil slink/dispatch-label)))


(define (lap-relocate frame old-tp new-tp offset)
  (lap ()                 
    (move .l (d@r TASK 12) S0)           ; offset
    (move .l (index (d@r A1 2) S0) S1)   ; code
    (sub .l A2 S1)                       ; code-offset
    (add .l S1 A3)                       ; new code
    (move .l A3 (index (d@r A1 2) S0))
    (move .l ($ -1) NARGS)
    (move .l (@r sp) tp)
    (jmp (@r tp))))

    

(define (current-task)
 (lap ()
  (move .l TASK A1)
  (add .l ($ (fx+ %%task-header-offset 2)) A1)   ; offset is negative !
  (move .l ($ -2) nargs)
  (move .l (@r sp) tp)
  (jmp (@r tp))))

  
(define-foreign gc_interrupt (gc_interrupt) ignore)

;;; Hack for getting into the debugger.

(define (@@ address)    ; randomness
  (lap ()
    (add .l ($ 2) a1)
    (move .l ($ -2) nargs)
    (move .l (@r sp) tp)
    (jmp (@r tp))))


(define (bpt . args)
    (lap ()
        (trap (number 9))
        (move .l ($ 0) s0)
        (move .l s0 a1)
        (move .l ($ -2) NARGS)
	(move .l (@r sp) tp)
	(jmp (@r tp))))

(define (crawl-exhibit-fault-frame frame)
  (cond ((not (foreign-fault-frame? frame))       ; foreign
         (print-register frame 'p 3)
         (print-register frame 'a1 4)
         (print-register frame 'a2 5)
         (print-register frame 'a3 6)
         (print-register frame 'an 7)
         (print-register frame 'tp 8))
        (else
         (format t " In foreign code; no information available~%"))))

(define (trace-fault-frame frame)
  (cond ((alt-bit-set? frame)          
         (move-object (make-pointer frame 0)))           ; foreign cont
        (else
         (let ((tp (extend-elt frame 8)))                ; old TP
           (trace-pointers (make-pointer frame 2) 6)     ; trace registers
           (trace-pointers (make-pointer frame 9)        ; trace temps
                           (fx+ *pointer-temps* 1))
           (let ((ptrs (extend-elt frame 0))             ; trace top of stack
                 (size (fault-frame-slots frame)))
             (trace-pointers (make-pointer frame (fx- size 1)) ptrs))
           (if (eq? (extend-elt frame 1) 0)              ; hack-top-of-stack?
               (relocate-random-code frame 2 tp)         ; relocate PC
               (relocate-random-code frame 1 tp))))))    ; relocate top-of-stack

(define (relocate-random-code frame offset old-tp)
  (if (in-old-space? (extend-elt frame offset))
      (lap-relocate frame old-tp (extend-elt frame 8) offset)))

(define (make-link-snapper value unit i)
  (lap ()
    (move .l (d@nil slink/snapper-freelist) p)
    (cmp .l p nil-reg)
    (j= cons-snapper-1)
    (move .l (d@r p 1) an)
    (move .l (d@r p -3) (d@nil slink/snapper-freelist))
    (move .l (d@nil slink/pair-freelist) (d@r p -3))
    (move .l p (d@nil slink/pair-freelist))
foobarfoo
    (move .l a1 (d@r an 2))
    (move .l a2 (d@r an 6))
    (move .l a3 (d@r an 10))
    (move .l an a1)
    (move .l ($ -2) nargs)
    (move .l (@r sp) tp)
    (jmp (@r tp))
cons-snapper-1    
    (lea (label link-snapper) an)
    (move .l ($ 12) s1)
    (jsr (label %make-extend))
    (jbr foobarfoo)))

(define *link-snapper-template*
(lap-template (3 0 1 t heap handle-snapper)
link-snapper
  (move .l p an)
  (move .l (d@r p 2) p)
  (move .w P S0)
  (and .b ($ 3) S0)
  (cmp .b ($ tag/extend) S0)                     ; check proc is extend
  (jn= %icall-bad-proc)
  (move .l (d@r P -2) TP)                         ; fetch template
  (move .w TP S0)
  (and .b ($ 3) S0)                 ; check proc is extend
  (cmp .b ($ tag/extend) S0)
  (jn= %icall-bad-proc)
  (move .l  (d@r TP -2) S0)       ; check template is valid (high bit set)
  (j>= %icall-bad-proc)
  (cmp .b (d@r TP template/nargs) NARGS)         ; check number of args
  (j= snap-link)
  (j< %icall-wrong-nargs)
  (btst ($ 30) S0)                            ; check nary bit
  (j= %icall-wrong-nargs)
snap-link
  (move .l an (d@r task task/extra-pointer))
  (move .l (d@r an 10) s0)
  (move .l (d@r an 6) an)
  (move .l p (index (d@r an 2) s0))
  (move .l (d@nil slink/pair-freelist) an)
  (cmp .l an nil-reg)
  (j= cons-pair)
  (move .l (d@r an -3) (d@nil slink/pair-freelist))
consed-pair
  (move .l (d@r task task/extra-pointer) (d@r an 1))
  (move .l (d@nil slink/snapper-freelist) (d@r an -3))
  (move .l an (d@nil slink/snapper-freelist))
  (jmp (@r TP))
cons-pair
  (jsr (label %make-pair))
  (jbr consed-pair)
handle-snapper
  (move .l nil-reg AN)
  (rts)))
