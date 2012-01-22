(herald m68_bignum (env tsys))

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

(define (set-bignum-length! bignum length)
  (lap ()  
    (move .l (d@r A1 -2) S0)       ; get header
    (asr .l ($ 6) S0)              ; length in bytes
    (and .b ($ #b11111100) S0)
    (sub .l A2 S0)                 ; size of bogus bytev including header
    (sub .l ($ 4) S0)              ; bytev length
    (asl .l ($ 8) S0)
    (move .b ($ header/bytev) S0)  ; bogus bytev header
    (move .l S0 (index (d@r A1 2) A2))
    (move .l A2 S0)                ; new length
    (asl .l ($ 6) S0)
    (move .b (d@r A1 1) S0)
    (move .l S0 (d@r A1 -2))
    (move .l ($ -2) NARGS)
    (move .l (@r sp) tp)
    (jmp (@r tp))))

(define-constant (bignum-positive? bignum)   ; if bit 7 of header is on
  (fx= (mref-8-u bignum -1) 
       (fixnum-add header/bignum 128)))

(define-constant bignum-negate!
  (primop bignum-negate! ()
    ((primop.side-effects? self) t)
    ((primop.generate self node)                               
     (let ((reg (->register 'pointer node (leaf-value ((call-arg 2) node)) '*)))
       (emit m68/bchg (machine-num 7) (reg-offset reg 1))))
    ((primop.type self node)
     '#[type (proc #f (proc #f top) bignum)])))


(define (%digit-divide x1 x0 y)   ; Divide x1x0 by y with x1 < (* 2 y)
  (lap ()
    (move .l A1 S0)             ; Remainder will be in S0
    (lsr .l ($ 2) S0)
    (move .l A2 S1)
    (move .l A3 S2)
    (lsr .l ($ 2) S2)

    (move .l ($ 30) s3)
    (clr .l s4)                ; Quotient in S4
    (jmp (label integer-divide-start))

integer-divide-loop
    (lsl .l ($ 1) s4)
    (lsl .l ($ 1) s1)
    (roxl .l ($ 1) s0)

integer-divide-start
    (cmp .l s2 s0)
    (uj< integer-divide-next)
    (sub .l s2 s0)
    (or .b ($ 1) s4)
integer-divide-next
    (dbcc 'f s3 (to integer-divide-loop))

    (lsl .l ($ 2) S4)
    (move .l S4 A1)
    (lsl .l ($ 2) S0)
    (move .l S0 A2)
    (move .l ($ -3) nargs)
    (move .l (@r sp) tp)
    (jmp (@r tp))))


(comment
(define (%digit-add u v carry)    ; U + V + carry => sum and carry-out 
  (lap ()                         ; carry is 0 or 1 (T integers)
    (move .l A1 S0)
    (move .l A2 S1)
    (move .l A3 S3)

    (lsr .l ($ 3) S3)     ; Carry in => X
    (addx .l S1 S0)                            ; TAS can't do this
    (roxl .b ($ 3) S3)    ; Carry out => S3

    (move .l S0 A1)
    (move .l S3 A2)
    (move .l ($ -3) nargs)
    (jmp (d@r task task/ireturn))))

(define (%digit-subtract u v carry) ; U - V - carry => sum and carry-out 
  (lap ()                           ; carry is 0 or 1 (T integers)
    (move .l A1 S0)
    (move .l A2 S1)
    (move .l A3 S3)

    (lsr .l ($ 3) S3)     ; Carry in => X
    (subx .l S1 S0)                            ; TAS can't do this
    (roxl .b ($ 3) S3)    ; Carry out => S3

    (move .l S0 A1)
    (move .l S3 A2)
    (move .l ($ -3) nargs)
    (jmp (d@r task task/ireturn))))

(define (%digit-multiply u v)   ; Multiply U and V
  (lap ()
    (move .l a1 s1)
    (asr .l ($ 2) s1)          ;       convert 1 fixnum to machine num
    (move .l a2 s0)

    (move .w s1 s2)
    (mulu s0 s2)           ; low-u * low-v

    (swap s1)
    (move .w s1 s3)
    (mulu s0 s3)           ; high-u * low-v

    (swap s0)
    (move .w s1 s4)
    (mulu s0 s4)           ; high-u * high-v

    (swap s1)
    (mulu s0 s1)           ; low-u * high-v

    (swap s2)

    (move .w s3 s0)
    (clr .w s3)
    (swap s3)
    (add .w s0 s2)         ; low-(high-u * low-v) + high-(low-u * low-v)
    (addx .l s3 s4)        ; high-(high-u * low-v) + low(high-u * high-v)
                                               ; TAS can't do this
    (move .w s1 s0)
    (clr .w s1)
    (swap s1)
    (add .w s0 s2)         ; low-(high-v * low-u) + high-(low-u * low-v)
    (addx .l s3 s4)        ; high-(high-v * low-u) + low(high-u * high-v)
                                               ; TAS can't do this
    (swap s2)

    (move .l s2 a2)
    (move .l s4 a1)
    (move .l ($ -3) nargs)
    (jmp (d@r task task/ireturn))))
)











                                               
