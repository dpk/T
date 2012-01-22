(herald vaxi 
        (env t (assembler as_open)))

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

;;; Non branch VAX instruction field generators

(define-fg (vax$0op mnem opcode)
  (printer "~a" (? mnem))
  (f u 8 opcode))

(define-fg (vax$1op mnem opcode o1 oc1)
  (printer "~a ~g" (? mnem) (? o1))
  (f u 8 opcode)
  (fg o1 oc1))

(define-fg (vax$disp mnem opcode d dw)
  (printer "~a ~g" (? mnem) (? d))
  (local dot)
  (f u 8 opcode)
  (f s dw (fixnum-ashr (from dot d) 3))
  (mark dot))

(define-fg (vax$1op-disp mnem opcode o1 oc1 d dw)
  (printer "~a ~g,~g" (? mnem) (? o1) (? d))
  (local dot)
  (f u 8 opcode)
  (fg o1 oc1)
  (f s dw (fixnum-ashr (from dot d) 3))
  (mark dot))

(define-fg (vax$2op mnem opcode o1 oc1 o2 oc2)
  (printer "~a ~g,~g" (? mnem) (? o1) (? o2))
  (f u 8 opcode)
  (fg o1 oc1)
  (fg o2 oc2))

(define-fg (vax$2op-disp mnem opcode o1 oc1 o2 oc2 d dw)
  (printer "~a ~g,~g,~g" (? mnem) (? o1) (? o2) (? d))
  (local dot)
  (f u 8 opcode)
  (fg o1 oc1)
  (fg o2 oc2)
  (f s dw (fixnum-ashr (from dot d) 3))
  (mark dot))

(define-fg (vax$3op mnem opcode o1 oc1 o2 oc2 o3 oc3)
  (printer "~a ~g,~g,~g" (? mnem) (? o1) (? o2) (? o3))
  (f u 8 opcode)
  (fg o1 oc1)
  (fg o2 oc2)
  (fg o3 oc3))

   
(define-fg (vax$3op-disp mnem opcode o1 oc1 o2 oc2 o3 oc3 d dw)
  (printer "~a ~g,~g,~g,~g" (? mnem) (? o1) (? o2) (? o3) (? d))
  (local dot)
  (f u 8 opcode)
  (fg o1 oc1)
  (fg o2 oc2)
  (fg o3 oc3)
  (f s dw (fixnum-ashr (from dot d) 3))
  (mark dot))


(define-fg (vax$4op mnem opcode o1 oc1 o2 oc2 o3 oc3 o4 oc4)
  (printer "~a ~g,~g,~g,~g" (? mnem) (? o1) (? o2) (? o3) (? o4))
  (f u 8 opcode)
  (fg o1 oc1)
  (fg o2 oc2)
  (fg o3 oc3)
  (fg o4 oc4))


;;; BRANCH INSTRUCTION HANDLING

;;; conditional branches

(define-fg (vax/jcc cc tag)
  (printer "j~a    ~g" (jump-op-name (? cc)) (? tag))
  (local dot displ field-width)
  (depending-on (disp dot tag) 
                (choose-a-bcc (field-width 16) displ)
                (make-bcc-fg (? cc) (? field-width) (? displ)))
  (mark dot))
             
(define (make-bcc-fg cc width displ)
   (xcond ((fx= width 16) (vax/bcc-abs cc (fixnum-ashr displ 3)))
          ((fx= width 40) (list (vax/bcc-abs (reverse-jump cc) 3)
                                (vax/brw-abs (fixnum-ashr displ 3))))
          ((fx= width 64) (list (vax/bcc-abs (reverse-jump cc) 6)
                                (vax/jmp-abs (fixnum-ashr displ 3))))))

                                               
;;; Current width is the width of THIS field (by previous estimation), used in 
;;; determining the value of DISPL.  This only make a difference for backward
;;; jumps because the FGs definitions are set up to mark displacements from 
;;; after the DEPENDING-ON.

(define (choose-a-bcc current-width displ)
  (cond ((fx< displ 0)
         (let ((displ (fx+ displ current-width)))
            (cond ((8bit-in-bits? (fx- displ 16)) (return 16 (fx- displ 16)))
                  ((16bit-in-bits? (fx- displ 40)) (return 40 (fx- displ 40)))
                  (else (return 64 (fx- displ 64))))))
        (else
         (cond ((8bit-in-bits? displ) (return 16 displ))
               ((16bit-in-bits? displ) (return 40 displ))
               (else (return 64 displ))))))

;;; simple branches

(define-fg (vax/jbr tag)
  (printer "jbr     ~g"  (? tag))
  (local dot displ field-width)
  (depending-on (disp dot tag) 
                (choose-a-br (field-width 16) displ)
                (make-br-fg (? field-width) (? displ)))
  (mark dot))
                                          
(define (make-br-fg width displ)
   (xcond ((fx= width 16) (vax/brb-abs (fixnum-ashr displ 3)))
          ((fx= width 24) (vax/brw-abs (fixnum-ashr displ 3)))
          ((fx= width 48) (vax/jmp-abs (fixnum-ashr displ 3)))))

(define (choose-a-br current-width displ)
  (cond ((fx< displ 0)
         (let ((displ (fx+ displ current-width)))
            (cond ((8bit-in-bits? (fx- displ 16)) (return 16 (fx- displ 16)))
                  ((16bit-in-bits? (fx- displ 24)) (return 24 (fx- displ 24)))
                  (else (return 48 (fx- displ 48))))))
        (else
         (cond ((8bit-in-bits? displ) (return 16 displ))
               ((16bit-in-bits? displ) (return 24 displ))
               (else (return 48 displ))))))


;;; Displacement in bytes

(define-fg (vax/bcc-abs cc abs-displ)
  (printer "b~s    ~s" (jump-op-name (? cc)) (? abs-displ))
  (f u 4 1)
  (f u 4 (jump-op->vax-op (? cc)))
  (f s 8 abs-displ))

(define-fg (vax/brb-abs abs-displ)
  (printer "brb     ~s" (? abs-displ))
  (f u 8 #x11)
  (f s 8 abs-displ))

(define-fg (vax/brw-abs abs-displ)
  (printer "brw     ~s" (? abs-displ))
  (f u 8 #x31)
  (f s 16 abs-displ))

(define-fg (vax/jmp-abs abs-displ)
  (printer "jmp     ~s" (? abs-displ))
  (f u 8 #x17)
  (f u 8 #xEF)
  (f s 32 abs-displ))

(define (jump-op->vax-op cc)
  (cond ((fx< cc 0)
         ;;    '#(abs eq  le lt leu ltu) *JUMP-OPS-NEGATIVE*
         (vref '#(1   3   5  9  #xB #xF 7 #xD) (fixnum-negate cc)))
        (else 
         ;;    '#(abs neq gt ge gtu geu) *JUMP-OPS-POSITIVE*
         (vref '#(1   2   4  8  #xA #xE 6 #xC) cc))))

;;;  Labels.   This needs some work.

(define (vax/label node)
     (d@pc (data-current-label node)))

(define label vax/label)
(define template vax/label)

;;;;-------------------------------

;;; do this right (how?) sometime.  byte, word, long, etc.

(define-data-fg (vax/space x)
    (printer ".space  ~s" (? x))
    (f u x 0))

;;; Problem with signed/unsigned

(define-data-fg (vax/byte x)
    (printer ".byte   x~x" (? x))
    (f u 8 x))

(define-data-fg (vax/word x)
    (printer ".word   x~x" (? x))
    (f u 16 x))

;;; -------------------------- Template stuff.



;    |  handler offset               |  annotation offset        |H|I| :0
;    +---------------+---------------+-------------------------------+
;    |  # ptr cells  |  # scr cells  |   offset within bit vector    | :4
;    +---------------+---------------+---------------+---------+-+---+
;    |          <<---- instructions  |   # of args   |    tmplt|0|imm| <--- ptr
                                                   

;;; these fields are in the wrong order.

(define-data-fg (vax/template lambda-node handler-ib)
;    (printer ".tem    ~s,~g" (? lambda-node) (? handler-ib))
    (printer ".template")
    (local template-end)
    (f u 16 (get-template-annotation (? lambda-node)))        
    ;;handler offset
    (f s 16 (fixnum-ashr (from template-end handler-ib) 3))   
    ;;bitv offset
    (f u 16 (fx+ (fixnum-ashr (mark-address (? template-end)) 3) 2)) 
    (f u 16 (get-template-cells (? lambda-node)))
    (f u 1 (if (template-nary (? lambda-node))  1 0))
    (f u 7 (identity header/template))
    (f u 8 (get-template-nargs (? lambda-node)))
    (mark template-end)
    )

(define (emit-vax-template code-node code-ib handler-ib template-ib)
   (set (ib-align template-ib) '(24 31 0))
   (emit-to-ib template-ib (vax/template code-node handler-ib))
   (set-ib-follower template-ib code-ib)
   )

;;; A few more machine parameters

(set (machine-template-emitter *vax-machine*) emit-vax-template)
(set (machine-cond-branch      *vax-machine*) vax/jcc)
(set (machine-uncond-branch    *vax-machine*) vax/jbr)         

