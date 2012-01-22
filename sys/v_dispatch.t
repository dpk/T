(herald vax_dispatch (env tsys))

(define (dispatch-init)
  (lap (handle-stype handle-true handle-fixnum handle-pair
        handle-char handle-nonvalue *handlers* icall-wrong-nargs
        bogus-return bogus-return-miss apply handle-immediate
	handle-magic-frame no-default-method)

    (movl p (d@r nil-reg slink/dispatch))
    (moval (label dispatch) (d@r nil-reg slink/dispatch-label))
    (mnegl ($ 1) nargs)
    (movl (@r sp) tp)
    (jmp (@r tp))))


(lap-template (0 0 -1 nil stack handle-dispatch-return)
dispatch-return                  
    (cmpl AN nil-reg)                             ; did we get a method?
    (j= default)
    (movl A1 P)
    (movl (d@r P -2) TP)
    (movl (d@r SP 16) A1)                    ; self is first arg of method
op-icall
    (cmpb NARGS (d@r AN template/nargs))         ; check number of args
    (j= %icall-ok)
    (j< %icall-wrong-nargs)
    (cmpb ($ (fx+ header/template 128)) (d@r AN -2))        ; check nary bit
    (jn= %icall-wrong-nargs)
%icall-ok
    (jmp (@r AN))
%icall-wrong-nargs
  (movl a1 (d@r TASK task/t0))
  (movl a2 (d@r TASK (fx+ task/t0 4)))
  (movl a3 (d@r TASK (fx+ task/t0 8)))
  (movl a4 (d@r TASK (fx+ task/t0 12)))
  (clrl s0)
  (jsb (*d@r nil-reg slink/nary-setup))
  (movl an a2)
  (movl (d@r SP 8) a1)   ; operation
  (addl2 ($ 20) SP)
  (movl (d@r nil-reg slink/dispatch) P)
  (movl (d@r P (static 'icall-wrong-nargs)) P)
  (movl (d@r p 2) p)
  (movl (d@r P -2) TP)
  (jmp  (@r TP))
default
    (movl (d@r SP 16) A1)                         ; self is first arg of method
    (movl (d@r P offset/operation-default) P)
    (cmpl p nil-reg)
    (j= no-default)
    (addl2 ($ 20) SP)
    (jmp (*d@r nil-reg slink/icall))
no-default    
  (movl a1 (d@r TASK task/t0))
  (movl a2 (d@r TASK (fx+ task/t0 4)))
  (movl a3 (d@r TASK (fx+ task/t0 8)))
  (movl a4 (d@r TASK (fx+ task/t0 12)))
  (clrl s0)
  (jsb (*d@r nil-reg slink/nary-setup))
  (movl an a2)
  (movl (d@r SP 8) a1)   ; operation
  (addl2 ($ 20) SP)
  (movl (d@r nil-reg slink/dispatch) P)
  (movl (d@r P (static 'no-default-method)) P)
  (movl (d@r p 2) p)
  (movl (d@r P -2) TP)
  (jmp  (@r TP))
handle-dispatch-return    
    (movl nil-reg AN)
    (rsb))

                              
    
(define *structure-template*
  (lap-template (0 0 0 nil heap structure-handler)
    (jmp (*d@r nil-reg slink/undefined-effect))
structure-handler
    (movl (d@r A1 -2) A1)                       ; internal-template
    (movl (d@r A1 -30) A1)                        ; stype-handler
    (jmp (label dispatch))))
    
(define *stype-template*
  (lap-template (9 0 0 nil heap stype-handler)           ; stype size is 9
    (jmp (*d@r nil-reg slink/undefined-effect))
stype-handler
    (movl (d@r nil-reg slink/dispatch) AN)
    (movl (d@r AN (static 'handle-stype)) A1)
    (movl (d@r a1 2) a1)
    (jmp (label dispatch))))
  
(define *traced-op-template*
  (lap-template (0 0 0 nil stack t-op)
    (pushl A1)                                       ; self
    (pushl nil-reg)
    (pushl P)                                        ; op
    (pushl A1)                                       ; obj
    (pushl ($ (fx+ (fixnum-ashl 4 16) header/vframe)))
    (pushal (label traced-op-return))
    (jmp (label dispatch))
t-op))
    
(lap-template (0 0 -1 nil stack handle-traced-op-return)
traced-op-return                  
    (cmpl AN nil-reg)                         ; did we get a method?
    (j= traced-op-default)                      ; AN contains code
    (movl A1 P)                              ; environment
    (movl (d@r P -2) TP)
    (movl (d@r SP 16) A1)                         ; self is first arg of method
    (jmp (label op-icall))
traced-op-default
    (movl (d@r P 6) P)                       ; rhs is operation
    (jmp (label default))
handle-traced-op-return    
    (movl nil-reg AN)
    (rsb))
  

;;; We have the operation in P, the object in A1 and we can use AN which is
;;; where the method id returned

(define *operation-template*
  (lap-template (3 0 1 t heap operation-handler)
    (pushl A1)                                   ; self
    (pushl nil-reg)
    (pushl P)                                    ; op
    (pushl A1)                                   ; obj
    (pushal ($ (fx+ (fixnum-ashl 4 16) header/vframe)))
    (pushal (label dispatch-return))
dispatch
    (bicb3 ($ #b11111100) A1 S0)                 ; get object tag 
    (cmpb S0 ($ tag/extend))                     ; is it an extend?
    (jn= object-not-extend)
    (movl (d@r A1 -2) TP)                        ; get object's header
    (bicb3 ($ #b11111100) TP S0)                 ; is it a template?
    (cmpb S0 ($ tag/extend))
    (jn= object-not-closure)
    (cmpw (@r TP) ($ VAX-JUMP-ABSOLUTE))         ; closure internal template?
    (j= cit)
    (cvtwl (d@r TP -8) S0)                       ; get handler offset
    (j= no-handler)                       ; it it's 0, no handler
    (jmp (index (@r TP) S0))                     ; call the handler         
no-handler              
    (movl nil-reg AN)
    (rsb)
cit
    (movl (d@r TP 2) AN)                         ; get auxilliary template
    (cvtwl (d@r AN -8) S0)                       ; get handler offset
    (j= no-handler)
    (jmp (index (@r AN) S0))
object-not-extend
    (movl (d@r nil-reg slink/dispatch) AN)         ; establish addressability
    (cmpb S0 ($ tag/fixnum))
    (j= fixnum)
    (cmpb S0 ($ tag/pair))
    (j= pair)
    (cmpb A1 ($ header/char))
    (j= char)
    (cmpb A1 ($ header/true))
    (j= true)
    (cmpb A1 ($ header/nonvalue))
    (j= nonvalue)
    (movl (d@r AN (static 'handle-immediate)) A1)
    (movl (d@r a1 2) a1)
    (jmp (label dispatch))
true
    (movl (d@r AN (static 'handle-true)) A1)
    (movl (d@r a1 2) a1)
    (jmp (label dispatch))
nonvalue
    (movl (d@r AN (static 'handle-nonvalue)) A1)
    (movl (d@r a1 2) a1)
    (jmp (label dispatch))
fixnum   
    (movl (d@r AN (static 'handle-fixnum)) A1)
    (movl (d@r a1 2) a1)
    (jmp (label dispatch))
pair
    (movl (d@r AN (static 'handle-pair)) A1)
    (movl (d@r a1 2) a1)
    (jmp (label dispatch))
char
    (movl (d@r AN (static 'handle-char)) A1)
    (movl (d@r a1 2) a1)
    (jmp (label dispatch))
object-not-closure
    (movl (d@r nil-reg slink/dispatch) AN)
    (movl (d@r AN (static '*handlers*)) AN)
    (movl (d@r an 2) an)
    (ashl ($ -2) TP S0)                          ; get header field
    (bicl3 ($ #xFFFFFFE0) S0 S0)                 ; isolate low five bits
    (movl (index (d@r AN 2) S0) A1)              ; index into vector of handlers
    (jmp (label dispatch))
operation-handler
  (movl (d@r A1 offset/operation-handler) A1)
    (jmp (label dispatch))))

;;; At the top of the join loop the stack looks like    self                       
;;;                                                     next
;;;                                                     op
;;;                                                     obj
;;;                                                     *state-template*
;;;                                               sp -> dispatch-return-template

(define *join-template*
  (lap-template (2 0 1 t heap join-handler)
join-template
    (movl (d@r P 2) P)                     ; joined lhs
    (jmp (*d@r nil-reg slink/icall))                       
join-handler                                            
    (movl (d@r A1 6) (d@r SP 16))          ; next <- rhs
    (movl (d@r A1 2) A1)                   ; get joined lhs
    (movl A1 (d@r SP 8))                   ; obj  <- lhs
    (pushal (label join-return))
    (jmp (label dispatch))))               ; try to get a handler from lhs

(lap-template (0 0 -1 t stack join-return-handler)
join-return
    (cmpl AN nil-reg)                      ; did we get a handler?
    (j= join-miss)
    (rsb)
join-miss
    (movl (d@r SP 16) A1)                  ; get next
    (movl A1 (d@r SP 8))                   ; obj <- next
    (movl (d@r nil-reg slink/dispatch) AN)                    
    (movl nil-reg (d@r SP 16)) ; next <- tbsh
    (jmp (label dispatch))                 ; try rhs
join-return-handler
    (movl nil-reg AN)
    (rsb))
 
(define *bogus-entity-template*
  (lap-template (2 0 1 t heap bogus-entity-handler)
    (movl (d@r P 2) P)
    (jmp (*d@r nil-reg slink/icall))
bogus-entity-handler
    (movl nargs s2)
    (movl A2 (d@r TASK 4))
    (movl A3 (d@r TASK 8))
    (movl A4 (d@r TASK 12))
    (movl ($ 1) S0)
    (jsb (*d@r nil-reg slink/nary-setup))
    (movl (d@r A1 6) A2)               ; bogus-entity handler
    (movl P A1)                        ; operation is argument to handler
    (movl A2 P)
    (pushl s2)
    (pushl AN)
    (pushal (label bogus-return))
    (movl ($ 2) NARGS)
    (jmp (*d@r nil-reg slink/icall))))

(lap-template (2 0 -1 nil stack bogus-return-handler)
bogus-return
    (cmpl A1 nil-reg)
    (jn= bogus-return-hit)
    (movl (d@r nil-reg slink/dispatch) AN)
    (movl (d@r SP 4) A3)               ; args
    (movl A1 A2)                       ; method
    (movl (d@r AN (static 'bogus-return-miss)) A1)
    (movl (d@r a1 2) a1)
    (movl (d@r AN (static 'apply)) P)
    (movl (d@r p 2) p)
    (addl2 ($ 12) SP)    ; pop off bogus return continuation
    (movl ($ 4) NARGS)
    (movl (d@r P -2) TP)
    (jmp (@r TP))
bogus-return-hit
    (movl (d@r nil-reg slink/dispatch) AN)
    (movl (d@r SP 4) a4)               ; args
    (movl A1 A2)                       ; method
    (movl (d@r AN (static 'bogus-return)) A1)
    (movl (d@r a1 2) a1)
    (movl (d@r AN (static 'apply)) P)
    (movl (d@r p 2) p)
    (movl ($ 5) NARGS)      ; dummy obj in a3
    (movl (d@r P -2) TP)
    (jmp (@r TP))
bogus-return-handler
    (movl nil-reg AN)
    (rsb))

(define (bogus-return-miss method  . args)
  (lap ()
    (movl nil-reg AN)                  ; compiled handlers return register
    (moval (label join-return) A1)
    (cmpl (@r SP) A1)
    (j= joined-bogus-return-miss)
    (movl (d@r SP 12) P)                ; restore operation
    (rsb)
joined-bogus-return-miss
    (movl (d@r SP 16) P)                ; restore operation
    (addl2 ($ 4) SP)                      ; pop return addr
    (jmp (label join-miss))))


(define (bogus-return method obj . args)
  (lap ()
    (movl (d@r SP 8) NARGS)            ; restore nargs and pop continuation
    (incl NARGS)                  ; add one for obj
    (addl2 ($ 12) SP)
    (movl A1 P)                        ; method in procedure register
    (moval (label join-return) A1)          ; is a join return address on top?
    (cmpl (@r SP) A1)
    (jn= bogus-dispatch-return)
joined-bogus-return
    (addl2 ($ 4) SP)                      ; pop join return addr
bogus-dispatch-return
    (movl (d@r SP 20) A1)              ; self is first of interpreted method
    (movl (d@r SP 8)  A2)              ; obj is second of interpreted method
    (addl2 ($ 24) SP)                    ; dispatch return + vframe 
    (jmp (*d@r nil-reg slink/icall))))


(define *magic-frame-template*
 (lap-template (4 0 -1 t stack magic-frame-handler)
  (moval (d@r SP 20) SP)
  (movl (@r sp) tp)
  (jmp (@r tp))
magic-frame-handler
  (movl (d@r nil-reg slink/kernel) AN)
  (movl (d@r AN (static 'handle-magic-frame)) A1)
  (movl (d@r a1 2) a1)
  (jmp (label dispatch))))

(dispatch-init)