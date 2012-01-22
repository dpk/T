(herald throw (env tsys))

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

;;;; dynamic state manipulation

;;; deficiencies:
;;;  - there's no way to temporarily "bind" the dynamic state (a la pdl
;;;    pointer args to eval in maclisp).
;;;  - there's no proviso for coroutines/multitasking (stack groups).
;;; these both stem from the assumption rooted fairly deep that changes in
;;; the "dynamic state" are irreversible.  this will change.

;;; preliminaries:

(define-integrable (magic-frame? frame)
  (eq? (extend-header frame) *magic-frame-template*))

;;; state objects are actually the same as magic frames.

(define-integrable (magic-frame-state frame) frame)
                                                      
;;; binding:

(define-predicate stack?)

(define-handler stack
  (object nil
    ((stack? self) t)
    ((print-type-string self) "Stack")))
  

(define (bind-handler wind stuff unwind)
  ;; someday worry about doing things atomically.
  (wind)
  (push-magic-frame unwind stuff wind))


(define (unwind-protect-handler stuff unwind)
  (push-magic-frame unwind stuff false))

(define (bind-internal state stuff)
  (set (process-global task/dynamic-state) state)
  (receive results
           (stuff)
    (perform-unwind state)
    (set (process-global task/dynamic-state) (state-previous state))
    (apply return results)))

(define (perform-unwind state)          ; want better name!
  (let ((unwind (state-unwinder state)))
    (set (state-unwinder state)
         (if (eq? unwind throw-out-of-unwind)      ; kludge
             false
             throw-out-of-unwind))
    (unwind)
    (set (state-unwinder state) false)))

(define (throw-out-of-unwind)
  (error "attempting to throw out of an unwind or unbind action -~%~
          **~10tdoing (ret) or (reset) will abort the unwind action and~%~
          **~10tproceed with the throw anyhow."))

;;; throwing: one-stack model.

;;; the following is invariant, for now at least:
;;;  (eq? *dynamic-state* (get-dynamic-state (current-frame)))

(lset *the-current-throw-value* nil)          ; el hacko grossness
(lset *the-current-throw-frame* nil)
                                                    
(define-operation (escape-procedure-frame proc))
(define-predicate escape-procedure?)
                                
(define (*catch proc)
  (let ((cc (current-continuation)))          
    (proc (object (lambda vals (frame-throw cc vals))
            ((print-type-string self) "Escape-procedure")
            ((escape-procedure? self) t)
            ((escape-procedure-frame self) cc)))))

         
(define (call-with-current-continuation proc)
  (let* ((sp (descriptor->fixnum (current-continuation)))
         (current-state (process-global task/dynamic-state))
         (base-state (get-base-state current-state))
         (stack (copy-stack sp)))
    (proc (object (lambda vals 
                    (continuation-throw sp stack vals current-state base-state))
            ((print-type-string self) "Upward-continuation")))))
                                                                
(define (get-base-state state)
  (iterate loop ((state state))
    (let ((prev (state-previous state)))
      (cond ((null? prev) state)
            (else (loop prev))))))
           
;;; COPY-STACK The make-pointer is to pretend that the
;;; stack has a header at the top

(define (copy-stack sp)
  (let* ((size (fx+ (fx- (descriptor->fixnum (process-global task/stack)) sp) 1))
         (stack (make-vector-extend header/stack 
                                    (enforce acceptable-vector-size? size)
                                    size)))
    (disable-interrupts)
    (%copy-extend stack 
                  (make-pointer (gc-extend->pair (gc-extend->pair sp)) -2)
                  size)
    (enable-interrupts)
    stack))

(define (continuation-throw sp stack vals k-state base-state)
  (cond ((stack? stack)
         (let ((a (swap *the-current-throw-value* vals))
               (b (swap *the-current-throw-frame* stack)))
           (unwind-to-state nil)
           (set *the-current-throw-frame* b)
           (set *the-current-throw-value* a)
           (set (process-global task/dynamic-state) k-state)
           (invoke-continuation sp stack vals base-state k-state)))
        (else
         (error "throwing ~s to bad continuation ~s" vals stack))))
                                  
                             
(define (rewind-state-and-continue from to vals)
  (do ((state from (state-next state)))
      ((eq? state to) 
       ((state-winder state))
       (apply return vals))
    ((state-winder state))))


(define (frame-throw frame vals)
  (cond ((reasonable-frame? frame)
    	 (let ((a (swap *the-current-throw-value* vals))
               (b (swap *the-current-throw-frame* frame))
     	       (to-state (get-dynamic-state frame)))
          (unwind-to-state to-state)
          (set *the-current-throw-frame* b)
    	  (set *the-current-throw-value* a)
          (set (process-global task/dynamic-state) to-state)
          (invoke-stack-continuation frame vals)))
        (else
         (frame-throw (error "invalid frame - (~s ~s ~s)"
                             'frame-throw frame vals)
                      vals))))

(define (unwind-to-state to-state)
  (iterate loop ((state (process-global task/dynamic-state)))
    (cond ((eq? state to-state) 'done)
          ((null? state)
           (warning "lost big while changing dynamic context to ~s!~
                    ~%;**     attempting to do the throw anyhow...~%"
                    to-state))
          (else
           (perform-unwind state)
           (loop (state-previous state))))))

(define (get-dynamic-state frame)
  (cond ((null? frame) '())
        ((magic-frame? frame) (magic-frame-state frame))
        (else (get-dynamic-state (frame-previous frame)))))

(define (reasonable-frame? frame)
  (and (closure? frame)                   ; robust?
       (let ((frame (descriptor->fixnum frame)))
         (and (fx> frame (stack-pointer))
              (fx<= frame (process-global task/stack))))))
