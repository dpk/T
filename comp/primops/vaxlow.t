(herald vaxlow
        (env (make-empty-early-binding-locale 'nil) constants primops arith locations))

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
                 
(define-constant (return . args)
  (ignore args)
  (lap ()                           
    (mnegl NARGS NARGS)             ; !!
    (movl (@r sp) tp)
    (jmp (@r tp))))
                 
(declare simplifier return simplify-values)

(define-constant (receive-values recipient thunk)
  (ignore recipient thunk)
  (lap ()
    (pushl A1)                       ; push "recipient"
    (pushal (label receiver))
    (movl A2 P)                      ; prepare to call thunk
    (movl ($ 1) NARGS)               ; thunk takes no arguments
    (jmp (*d@r nil-reg slink/icall))))

(lap-template (1 0 -1 t stack handle-receiver)
receiver
    (movl (d@r SP 4) P)              ; prepare to call recipient
    (moval (d@r SP 8) SP)            ; restore continuation
    (mnegl NARGS NARGS)              ; !!
    (jmp (*d@r nil-reg slink/icall))
handle-receiver
  (movl nil-reg AN)
  (rsb))

(declare simplifier receive-values simplify-receive-values)
                 

(define-constant make-pointer        ; extend and number of bytes
  (primop make-pointer ()                                        
    ((primop.generate self node)
     (generate-make-pointer node))
    ((primop.type self node)
     '#[type (proc #f (proc #f top) top fixnum)])))
;     '#[type (proc #f (proc #f top) extend fixnum)])))


(define-constant task-ref
  (primop task-ref ()
    ((primop.generate self node)
     (generate-task-ref node))))

(define-constant set-task-ref
  (primop set-task-ref ()
    ((primop.side-effects? self) t)
    ((primop.generate self node)
     (generate-set-task-ref node))))

(define-constant slink-ref
  (primop slink-ref ()
    ((primop.generate self node)
     (generate-slink-ref node))))

(define-constant set-slink-ref
  (primop set-slink-ref ()
    ((primop.side-effects? self) t)
    ((primop.generate self node)
     (generate-set-slink-ref node))))

(define-constant system-global
  (object (lambda (i) (slink-ref i))
    ((setter self)
     (lambda (i val) (set-slink-ref i val)))))

(define-constant process-global
  (object (lambda (i) (task-ref i))
    ((setter self)
     (lambda (i val) (set-task-ref i val)))))


(define-constant stack-pointer
  (primop stack-pointer ()
    ((primop.generate self node)
     (generate-stack-pointer node))))
                                                   
(define-constant current-continuation
  (primop current-continuation ()
    ((primop.generate self node)
     (generate-current-continuation node))))

(define-constant disable-interrupts
  (primop disable-interrupts ()
    ((primop.side-effects? self) t)
    ((primop.generate self node)
     (emit vax/bisb2 
          (machine-num #b10000000) 
          (reg-offset TASK (fx+ task/critical-count 3))))
    ((primop.type self node)
     '#[type (proc #f (proc #f top))])))
                       
(define-constant really-enable-interrupts
  (primop really-enable-interrupts ()
    ((primop.side-effects? self) t)
    ((primop.test-code self node arg)
     (emit vax/bicb2 
           (machine-num #b10000000) 
           (reg-offset TASK (fx+ task/critical-count 3))))
    ((primop.presimplify self node)
     (presimplify-no-argument-predicate node))
    ((primop.type self node)
     '#[type (proc #f (proc #f top))])))
                       
(define-constant (enable-interrupts)
  (if (not (really-enable-interrupts))
      (handle-queued-interrupt (process-global task/critical-count))))

          

;; template junk, see template.doc
                                         
                      
(define-constant template-enclosing-object
  (primop template-enclosing-object ()
    ((primop.generate self node)
     (receive (source target rep) (one-arg-primitive node)
       (let ((reg (get-register 'scratch node '*)))
         (generate-move source target)
         (emit vax/movzwl (reg-offset target -6) reg)    ; offset field in bytes
         (emit vax/subl2 reg target)
         (mark-continuation node target))))
    ((primop.type self node)
     '#[type (proc #f (proc #f top) template)])))

(define-constant gc-extend->pair
  (primop gc-extend->pair ()
    ((primop.generate self node)
     (receive (source target rep) (one-arg-primitive node)
       (generate-move source target)
       (emit vax/incl target)
       (mark-continuation node target)))
    ((primop.type self node) 
     '#[type (proc #f (proc #f top) top)])))

(define-constant gc-pair->extend
  (primop gc-pair->extend ()
    ((primop.generate self node)
     (receive (source target rep) (one-arg-primitive node)
       (generate-move source target)
       (emit vax/decl target)
       (mark-continuation node target)))
    ((primop.type self node)
     '#[type (proc #f (proc #f top) top)])))
;     '#[type (proc #f (proc #f pair) extend)])))

                      
                      
(define-constant closure-enclosing-object
  (primop closure-enclosing-object ()
    ((primop.generate self node)
     (generate-closure-enclosing-object node))
    ((primop.type self node)
     '#[type (proc #f (proc #f top) top)])))
;     '#[type (proc #f (proc #f top) extend)])))

                              
(define-constant (bit-test operand bit)    ; true if bit is on
  (if (fixnum-equal? (fixnum-logand operand (fixnum-ashl 1 bit)) 0)
      '#f
      '#t))
                                   
(define-constant (template-internal-bit? tem)
  (let ((tem (if (fixnum-equal? (mref-16-u tem -2) vax-jump-absolute)
                 (extend-pointer-elt tem 0)
                 tem)))
    (bit-test (mref-16-u tem -12) 0)))

(define-constant (template-superior-bit? tem) ; no cit's on stack
  (bit-test (mref-16-u tem -12) 2))
                                    
(define-constant (template-nary? tem)
  (bit-test (mref-8-u tem -4) 7))

(define-constant (template-pointer-slots tem)
  (mref-8-u tem -5))

(define-constant (template-scratch-slots tem)
  (mref-8-u tem -6))

(define-constant (template-nargs tem)
  (mref-8-s tem -3))

(define-constant (template-encloser-offset template)
  (fixnum-ashr (mref-16-u template -8) 2))

(define-constant (template-handler-offset template)
  (mref-16-u template -10))

(define-constant (closure-encloser-offset closure)
  (fixnum-ashr (mref-16-u (extend-header closure) -6) 2))


(define-constant (unit-top-level-forms unit)
  (make-pointer unit 3))

(define-constant (alt-bit-set? extend)            ; if bit 7 of header is on
  (fixnum-less? (mref-8-s extend -4) 0))

(define-constant set-alt-bit!
  (primop set-alt-bit! ()
    ((primop.side-effects? self) t)
    ((primop.generate self node)                               
     (let ((reg (->register 'pointer node (leaf-value ((call-arg 2) node)) '*)))
       (emit vax/bisb2 (machine-num #b10000000) (reg-offset reg -2))))
    ((primop.type self node)
     '#[type (proc #f (proc #f top) top)])))
;     '#[type (proc #f (proc #f top) extend)])))

(define-constant clear-alt-bit!
  (primop clear-alt-bit! ()
    ((primop.side-effects? self) t)
    ((primop.generate self node)                               
     (let ((reg (->register 'pointer node (leaf-value ((call-arg 2) node)) '*)))
       (emit vax/bicb2 (machine-num #b10000000) (reg-offset reg -2))))
    ((primop.type self node)
     '#[type (proc #f (proc #f top) top)])))
;     '#[type (proc #f (proc #f top) extend)])))

(define-constant vcell-defined? alt-bit-set?)

(define-constant set-vcell-defined set-alt-bit!)

(define-constant set-vcell-undefined clear-alt-bit!)

(define-constant pure? alt-bit-set?)

(define-constant (purify! x)
  (set-alt-bit! x)
  (return))

(define (vframe-pointer-slots vframe)
  (mref-8-u vframe -2))

(define (vframe-scratch-slots vframe)
  (mref-8-u vframe -3))


