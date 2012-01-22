(herald m68dispatch (env tsys))

(define (dispatch-init)
  (lap (handle-stype handle-true handle-fixnum handle-pair
        handle-char handle-nonvalue *handlers* icall-wrong-nargs
        bogus-return bogus-return-miss apply handle-template handle-immediate
	handle-magic-frame no-default-method)

    (move .l p (d@nil slink/dispatch))
    (lea (label dispatch) a1)
    (move .l a1 (d@nil slink/dispatch-label))
    (move .l ($ -1) nargs)
    (move .l (@r sp) tp)
    (jmp (@r tp))))


; vframe obj op next self

(lap-template (0 0 -1 nil stack handle-dispatch-return)
dispatch-return                  
    (cmp .l AN nil-reg)                         ; did we get a method?
    (j= default)                                ; AN contains code
    (move .l  A1 P)                             ;  environment
    (move .l (d@r P -2) TP)
    (move .l (d@r SP 16) A1)                    ; self is first arg of method
op-icall
    (cmp .b (d@r AN template/nargs) NARGS)         ; check number of args
    (j= %icall-ok)
    (j< %icall-wrong-nargs)
    (btst ($ 6) (d@r AN -2))                        ; check nary bit
    (j= %icall-wrong-nargs)
%icall-ok
    (jmp (@r AN))
%icall-wrong-nargs
  (move .l a1 (d@r TASK task/t0))
  (move .l a2 (d@r TASK (fx+ task/t0 4)))
  (move .l a3 (d@r TASK (fx+ task/t0 8)))
  (clr .l s0)
  (jsr (*d@nil slink/nary-setup))
  (move .l an a2)
  (move .l (d@r SP 8) a1)   ; operation
  (add .w ($ 20) SP)
  (move .l (d@nil slink/dispatch) P)
  (move .l (d@static P (static 'icall-wrong-nargs)) P)
  (move .l (d@r P -2) TP)
  (jmp  (@r TP))
default
    (move .l (d@r SP 16) A1)                         ; self is first arg of method
    (move .l (d@r P offset/operation-default) P)
    (cmp .l p nil-reg)
    (j= no-default)
    (add .w ($ 20) SP)
    (jmp (*d@nil slink/icall))
no-default    
  (move .l a1 (d@r TASK task/t0))
  (move .l a2 (d@r TASK (fx+ task/t0 4)))
  (move .l a3 (d@r TASK (fx+ task/t0 8)))
  (clr .l s0)
  (jsr (*d@nil slink/nary-setup))
  (move .l an a2)
  (move .l (d@r SP 8) a1)   ; operation
  (add .w ($ 20) SP)
  (move .l (d@nil slink/dispatch) P)
  (move .l (d@static P (static 'no-default-method)) P)
  (move .l (d@r P -2) TP)
  (jmp  (@r TP))
handle-dispatch-return    
    (move .l nil-reg AN)
    (rts))
                              
                           
(define *structure-template*
  (lap-template (0 0 0 nil heap structure-handler)
    (jmp (*d@nil slink/undefined-effect))
structure-handler
    (move .l (d@r A1 -2) A1)                       ; internal-template
    (move .l (d@r A1 -30) A1)                        ; stype-handler
    (jmp (label dispatch))))
    
(define *stype-template*
  (lap-template (9 0 0 nil heap stype-handler)           ; stype size is 9
    (jmp (*d@nil slink/undefined-effect))
stype-handler
  (move .l (d@nil slink/dispatch) AN)
  (move .l (d@static AN (static 'handle-stype)) A1)
  (jmp (label dispatch))))

(define *traced-op-template*
  (lap-template (0 0 0 nil stack t-op)
    (move .l A1 (@-r SP))                                       ; self
    (move .l nil-reg (@-r SP))                                  ; next
    (move .l P (@-r SP))                                        ; op
    (move .l A1 (@-r SP))                                       ; obj
    (move .l ($ (fx+ (fixnum-ashl 4 16) header/vframe)) (@-r sp))
    (pea (label traced-op-return))
    (jmp (label dispatch))
t-op))
    
(lap-template (0 0 -1 nil stack handle-traced-op-return)
traced-op-return                  
    (cmp .l AN nil-reg)                         ; did we get a method?
    (j= traced-op-default)                      ; AN contains code
    (move .l A1 P)                              ; environment
    (move .l (d@r P -2) TP)
    (move .l (d@r SP 16) A1)                         ; self is first arg of method
    (jbr op-icall)
traced-op-default
    (move .l (d@r P 6) P)                       ; rhs is operation
    (jbr default)
handle-traced-op-return    
    (move .l nil-reg AN)
    (rts))
  

;;; We have the operation in P, the object in A1 and we can use AN which is
;;; where the method id returned

(define *operation-template*
  (lap-template (3 0 1 t heap operation-handler)
    (move .l A1 (@-r SP))                                       ; self
    (move .l nil-reg (@-r SP))                                  ; next
    (move .l P (@-r SP))                                        ; op
    (move .l A1 (@-r SP))                                       ; obj
    (move .l ($ (fx+ (fixnum-ashl 4 16) header/vframe)) (@-r sp))
    (pea (label dispatch-return))
dispatch 
    (move .l A1 S0)                                 ; is object extend?
    (and .b ($ 3) S0)                              
    (cmp .b ($ tag/extend) S0)
    (jn= object-not-extend)                         ; if so
    (move .l (d@r A1 -2) S0)                        ; get object's header
    (j< template)                                   ; is high bit set?
                                                    ; watch for interrupt here!!
    (move .l (d@r A1 -2) TP)                        ; object's header again
    (and .b ($ 3) S0)                               ; is header a template?
    (cmp .b ($ tag/extend) S0)
    (jn= object-not-closure)                        ; if so
    (cmp .w ($ M68-JUMP-ABSOLUTE) (@r TP))          ; closure internal template?
    (j= cit)
    (move .w (d@r TP -8) S0)                        ; get signed handler offset
    (ext .l S0)                                     ; is it 0
    (j= no-handler)                                 ; if so, no handler
    (jmp (index (@r TP) S0))                        ; jump to handler
no-handler              
    (move .l nil-reg AN)
    (rts)
cit
    (move .l (d@r TP 2) AN)                         ; get auxilliary template
    (move .w (d@r AN -8) S0)                        ; get handler offset
    (ext .l S0)
    (j= no-handler)
    (jmp (index (@r AN) S0))                        ; jump to handler
template
    (move .l (d@nil slink/dispatch) AN)
    (move .l (d@static AN (static 'handle-template)) A1)
    (jmp (label dispatch))
object-not-extend
    (move .l (d@nil slink/dispatch) AN)
    (cmp .b ($ tag/fixnum) S0)
    (j= fixnum)
    (cmp .b ($ tag/pair) S0)
    (j= pair)
    (move .l A1 S0)
    (cmp .b ($ header/char) S0)
    (j= char)
    (cmp .b ($ header/true) S0)
    (j= true)
    (cmp .b ($ header/nonvalue) S0)
    (j= nonvalue)
    (move .l (d@static AN (static 'handle-immediate)) A1)
    (jmp (label dispatch))
true
    (move .l (d@static AN (static 'handle-true)) A1)
    (jmp (label dispatch))
fixnum   
    (move .l (d@static AN (static 'handle-fixnum)) A1)
    (jmp (label dispatch))
pair
    (move .l (d@static AN (static 'handle-pair)) A1)
    (jmp (label dispatch))
char
    (move .l (d@static AN (static 'handle-char)) A1)
    (jmp (label dispatch))            
nonvalue
    (move .l (d@static AN (static 'handle-nonvalue)) A1)
    (jmp (label dispatch))            
object-not-closure
    (move .l (d@nil slink/dispatch) AN)
    (move .l (d@static AN (static '*handlers*)) AN)
    (move .l TP S0)
    (and .l ($ #x0000007C) S0)                 ;; isolate low seven bits
    (move .l (index (d@r AN 2) S0) A1)
    (jmp (label dispatch))
operation-handler
  (move .l (d@r A1 offset/operation-handler) A1)
  (jmp (label dispatch))))

;;; At the top of the join loop the stack looks like   self                   
;;;                                                    next
;;;                                                    op
;;;                                                    obj
;;;                                                    vframe-header
;;;                                              sp -> dispatch-return-template


(define *join-template*
  (lap-template (2 0 1 t heap join-handler)
join-template
    (move .l (d@r P 2) P)                     ; joined lhs
    (jmp (*d@nil slink/icall))                       
join-handler                                            
    (move .l (d@r A1 6) (d@r SP 16))          ; next ,- rhs
    (move .l (d@r A1 2) A1)                   ; get joined lhs
    (move .l A1 (d@r SP 8))                   ; obj <- lhs
    (pea (label join-return))
    (jmp (label dispatch))))               ; try to get a handler from lhs

(lap-template (0 0 -1 t stack join-return-handler)
join-return
    (cmp .l AN nil-reg)                      ; did we get a handler?
    (j= join-miss)
    (rts)
join-miss
    (move .l (d@r SP 16) A1)
    (move .l A1 (d@r SP 8))                  ; next becomes obj
    (move .l (d@nil slink/dispatch) AN)
    (move .l nil-reg (d@r SP 16))          ; next
    (jmp (label dispatch))                 ; try rhs
join-return-handler
    (move .l nil-reg AN)
    (rts))
 
(define *bogus-entity-template*
  (lap-template (2 0 1 t heap bogus-entity-handler)
    (move .l (d@r P 2) P)
    (jmp (*d@nil slink/icall))
bogus-entity-handler
    (move .l NARGS S2)   
    (move .l A2 (d@r TASK (+ task/T0 4)))
    (move .l A3 (d@r TASK (+ task/T0 8)))
    (move .l ($ 1) S0)
    (jsr (*d@nil slink/nary-setup))
    (move .l (d@r A1 6) A2)               ; bogus-entity handler
    (move .l P A1)                        ; operation is argument to handler
    (move .l A2 P)
    (move .l S2 (@-r SP))   ; save nargs
    (move .l AN (@-r SP))   ; save arglist    
    (pea (label bogus-return))
    (move .l ($ 2) NARGS)
    (jmp (*d@nil slink/icall))))

(lap-template (2 0 -1 nil stack bogus-return-handler)
bogus-return
    (cmp .l A1 nil-reg)
    (jn= bogus-return-hit)
    (move .l (d@nil slink/dispatch) AN)
    (move .l (d@r SP 4) A3)               ; args
    (move .l A1 A2)                       ; method
    (move .l (d@static AN (static 'bogus-return-miss)) A1)
    (move .l (d@static AN (static 'apply)) P)
    (add .w ($ 12) SP)    ; pop off bogus return continuation
    (move .l ($ 4) NARGS)
    (move .l (d@r P -2) TP)
    (jmp (@r TP))
bogus-return-hit
    (move .l (d@nil slink/dispatch) AN)
    (move .l (d@r SP 4) (d@r TASK (+ task/T0 12)))               ; args
    (move .l A1 A2)                       ; method
    (move .l (d@static AN (static 'bogus-return)) A1)
    (move .l (d@static AN (static 'apply)) P)
    (move .l ($ 5) NARGS)      ; dummy obj in a3
    (move .l (d@r P -2) TP)
    (jmp (@r TP))
bogus-return-handler
    (move .l nil-reg AN)
    (rts))

(define (bogus-return-miss method  . args)
  (lap ()
    (move .l nil-reg AN)                  ; compiled handlers return register
    (lea (label join-return) A1)
    (cmp .l (@r SP) A1)
    (j= joined-bogus-return-miss)
    (move .l (d@r SP 12) P)                ; restore operation
    (rts)
joined-bogus-return-miss
    (move .l (d@r SP 16) P)                ; restore operation
    (add .w ($ 4) SP)                      ; pop return addr
    (jbr join-miss)))


(define (bogus-return method obj . args)
  (lap ()
    (move .l (d@r SP 8) NARGS)            ; restore nargs and pop continuation
    (add .l ($ 1) NARGS)                  ; add one for obj
    (add .w ($ 12) SP)
    (move .l A1 P)                        ; method in procedure register
    (lea (label join-return) A1)          ; is a join return address on top?
    (cmp .l (@r SP) A1)
    (jn= bogus-dispatch-return)
joined-bogus-return
    (add .w ($ 4) SP)                      ; pop join return addr
bogus-dispatch-return
    (move .l (d@r SP 20) A1)              ; self is first of interpreted method
    (move .l (d@r SP 8)  A2)              ; obj is second of interpreted method
    (add .w ($ 24) SP)                    ; dispatch return + vframe 
    (jmp (*d@nil slink/icall))))



(define *magic-frame-template*
 (lap-template (4 0 -1 t stack magic-frame-handler)
  (lea (d@r SP 20) SP)
  (move .l (@r sp) tp)
  (jmp (@r tp))
magic-frame-handler
  (move .l (d@nil  slink/dispatch) AN)
  (move .l (d@static AN (static 'handle-magic-frame)) A1)
  (jmp (label dispatch))))

(dispatch-init)