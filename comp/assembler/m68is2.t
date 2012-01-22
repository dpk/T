(herald (assembler m68is2 t 0)
        (env t (assembler as_open)))

;;; Instructions

(define (m68/addx size src dst) (arith-x/d "addx" #xD (bwl size) src dst))
(define (m68/subx size src dst) (arith-x/d "subx" #x9 (bwl size) src dst))


(define (m68/addq size data dst) (aop-quick 0 (bwl size) data dst))
(define (m68/addi size data dst) (op-immediate "add" #x06 (bwl size) data dst))
(define (m68/add/ed size src dn) (op-into-dn "add" #xD (bwl size) src dn))
(define (m68/add/de size dn dst) (op-into-ea "add" #xD (bwl size) dn dst))
(define (m68/adda   size src an) (op-into-a "add" #xD (wl size) src an))

(define (m68/add size src dst)
    (or (m68/addq   size src dst)
        (m68/addi   size src dst)
        (m68/add/ed size src dst)   
        (m68/add/de size src dst)
        (m68/adda   size src dst)
        (error "no match for (add ~s ~g ~g)" size src dst)))

(define (m68/subq size data dst) (aop-quick 1 (bwl size) data dst))
(define (m68/subi size data dst) (op-immediate "sub" #x04 (bwl size) data dst))
(define (m68/sub/ed size src dn) (op-into-dn "sub" #x9 (bwl size) src dn))
(define (m68/sub/de size dn dst) (op-into-ea "sub" #x9 (bwl size) dn dst))
(define (m68/suba   size src an) (op-into-a "sub" #x9 (wl size) src an))

(define (m68/sub size src dst)
    (or (m68/subq   size src dst)
        (m68/subi   size src dst)
        (m68/sub/ed size src dst)   
        (m68/sub/de size src dst)
        (m68/suba   size src dst)
        (error "no match for (sub ~s ~g ~g)" size src dst)))

(define (m68/cmpi size data dst) (op-immediate "cmp" #x0C (bwl size) data dst))
(define (m68/cmp/ed size src dn) (op-into-dn "cmp" #xB (bwl size) src dn))
(define (m68/cmpa   size src an) (op-into-a "cmp" #xB (wl size) src an))

(define (m68/cmp size src dst)
    (or (m68/cmp/ed size src dst)   
        (m68/cmpa   size src dst)
        (m68/cmpi   size src dst)
        (error "no match for (cmp ~s ~g ~g)" size src dst)))

(define-fg (m68/cmpm-1 bwl (ar+-number? ax) (ar+-number? ay))
    (printer "cmpm.~c a~s+,a~s+" (format-bwl (? bwl)) (? ax) (? ay))
    (f u 4 #xB) (f u 3 ax) (1) (f u 2 bwl) (0 0 1) (f u 3 ay))

(define (m68/cmpm size src dst)
    (or (m68/cmpm-1 (bwl size) src dst)
        (error "no match for (cmpm ~s ~g ~g)" size src dst)))

(define (m68/andi size data dst) (op-immediate "and" #x02 (bwl size) data dst))
(define (m68/and/ed size src dn) (op-into-dn "and" #xC (bwl size) (ea-d&a? src) dn))
(define (m68/and/de size dn dst) (op-into-ea "and" #xC (bwl size) dn dst))

(define (m68/and size src dst)
    (or (m68/and/ed size src dst)   
        (m68/and/de size src dst)   
        (m68/andi   size src dst)
        (error "no match for (and ~s ~g ~g)" size src dst)))

(define (m68/ori  size data dst) (op-immediate "or"  #x00 (bwl size) data dst))
(define (m68/or/ed size src dn) (op-into-dn "or" #x8 (bwl size) (ea-d? src) dn))
(define (m68/or/de size dn dst) (op-into-ea "or" #x8 (bwl size) dn dst))

(define (m68/or size src dst)
    (or (m68/or/ed size src dst)   
        (m68/or/de size src dst)
        (m68/ori   size src dst)
        (error "no match for (or ~s ~g ~g)" size src dst)))

(define (m68/eori size data dst) (op-immediate "eor" #x0A (bwl size) data dst))
(define (m68/eor/de size dn dst) (op-into-ea-1 "eor" #xB (bwl size) dn dst))

(define (m68/eor size src dst)
    (or (m68/eor/de size src dst)   
        (m68/eori   size src dst)
        (error "no match for (eor ~s ~g ~g)" size src dst)))
        
(define (m68/asl/dd size dx   dy) (shift-op/dd #b00 1 (bwl size) dx dy))
(define (m68/asr/dd size dx   dy) (shift-op/dd #b00 0 (bwl size) dx dy))
(define (m68/asl/id size data dy) (shift-op/id #b00 1 (bwl size) data dy))
(define (m68/asr/id size data dy) (shift-op/id #b00 0 (bwl size) data dy))
(define (m68/asl/e  dst)     (shift-op/e  #b00 1  dst))
(define (m68/asr/e  dst)     (shift-op/e  #b00 0  dst))

(define (m68/asl size src dst) (shift-op #b00 1 size src dst))
(define (m68/asr size src dst) (shift-op #b00 0 size src dst))

(define (m68/lsl/dd size dx   dy) (shift-op/dd #b01 1 (bwl size) dx dy))
(define (m68/lsr/dd size dx   dy) (shift-op/dd #b01 0 (bwl size) dx dy))
(define (m68/lsl/id size data dy) (shift-op/id #b01 1 (bwl size) data dy))
(define (m68/lsr/id size data dy) (shift-op/id #b01 0 (bwl size) data dy))
(define (m68/lsl/e  dst)     (shift-op/e  #b01 1 dst))
(define (m68/lsr/e  dst)     (shift-op/e  #b01 0 dst))

(define (m68/lsl size src dst) (shift-op #b01 1 size src dst))
(define (m68/lsr size src dst) (shift-op #b01 0 size src dst))
 
(define (m68/rol/dd size dx   dy) (shift-op/dd #b11 1 (bwl size) dx dy))
(define (m68/ror/dd size dx   dy) (shift-op/dd #b11 0 (bwl size) dx dy))
(define (m68/rol/id size data dy) (shift-op/id #b11 1 (bwl size) data dy))
(define (m68/ror/id size data dy) (shift-op/id #b11 0 (bwl size) data dy))
(define (m68/rol/e  dst)     (shift-op/e  #b11 1 dst))
(define (m68/ror/e  dst)     (shift-op/e  #b11 0 dst))

(define (m68/rol size src dst) (shift-op #b11 1 size src dst))
(define (m68/ror size src dst) (shift-op #b11 0 size src dst))

(define (m68/roxl/dd size dx   dy) (shift-op/dd #b10 1 (bwl size) dx dy))
(define (m68/roxr/dd size dx   dy) (shift-op/dd #b10 0 (bwl size) dx dy))
(define (m68/roxl/id size data dy) (shift-op/id #b10 1 (bwl size) data dy))
(define (m68/roxr/id size data dy) (shift-op/id #b10 0 (bwl size) data dy))
(define (m68/roxl/e  dst)     (shift-op/e  #b10 1 dst))
(define (m68/roxr/e  dst)     (shift-op/e  #b10 0 dst))

(define (m68/roxl size src dst) (shift-op #b10 1 size src dst))
(define (m68/roxr size src dst) (shift-op #b10 0 size src dst))

(define (shift-op op dir size src dst)
    (or (shift-op/dd op dir (bwl size) src dst)   
        (shift-op/id op dir (bwl size) src dst)
        (error "no match for (~a~c ~s ~g ~g)" 
               (format-shift op)
               (format-dir dir)
               size src dst)))
 
(define (m68/bchg/de dn dst) (bit-op/de #b01 dn dst))
(define (m68/bclr/de dn dst) (bit-op/de #b10 dn dst))
(define (m68/bset/de dn dst) (bit-op/de #b11 dn dst))
(define (m68/btst/de dn dst) (bit-op/de #b00 dn dst))

(define (m68/bchg/ie data dst) (bit-op/ie #b01 data dst))
(define (m68/bclr/ie data dst) (bit-op/ie #b10 data dst))
(define (m68/bset/ie data dst) (bit-op/ie #b11 data dst))
(define (m68/btst/ie data dst) (bit-op/ie #b00 data dst))

(define (m68/bchg src dst) (bit-op #b01 src dst))
(define (m68/bclr src dst) (bit-op #b10 src dst))
(define (m68/bset src dst) (bit-op #b11 src dst))
(define (m68/btst src dst) (bit-op #b00 src dst))

(define (bit-op op src dst)
    (or (bit-op/de op src dst)
        (bit-op/ie op src dst)
        (error "no match for (~a ~g ~g)" (format-bit-op op) src dst)))

(define (m68/divs src dn) (op-reg-ea.w "divs" #x8 7 src dn))
(define (m68/divu src dn) (op-reg-ea.w "divu" #x8 3 src dn))
(define (m68/muls src dn) (op-reg-ea.w "muls" #xC 7 src dn))
(define (m68/mulu src dn) (op-reg-ea.w "mulu" #xC 3 src dn))
(define (m68/chk  src dn) (op-reg-ea.w "chk"  #x4 6 src dn))

(define (m68/clr  size dst) (op-size-ea "clr"  #x42 (bwl size) dst))
(define (m68/neg  size dst) (op-size-ea "neg"  #x44 (bwl size) dst))
(define (m68/negx size dst) (op-size-ea "negx" #x40 (bwl size) dst))
(define (m68/not  size dst) (op-size-ea "not"  #x46 (bwl size) dst))
(define (m68/tst  size dst) (op-size-ea "tst"  #x4A (bwl size) dst))

(define-fg (m68/nop)   (printer "nop")   (f u 16 #x4E71))
(define-fg (m68/rtr)   (printer "rtr")   (f u 16 #x4E77))
(define-fg (m68/rts)   (printer "rts")   (f u 16 #x4E75))
(define-fg (m68/trapv) (printer "trapv") (f u 16 #x4E76))
;(define-fg (m68/rte)   (printer "rte")   (f u 16 #x4E73))
;(define-fg (m68/reset) (printer "reset") (f u 16 #x4E70))
;(define-fg (m68/stop i)  (printer "stop #~s" (? i)) (f u 16 #x4E72) (f s 16 i))

;;; Apparently formatless instructions

(define-fg (m68/moveq (&-moveq-byte? data) (dr-number? dn))
    (printer "moveq #~s,d~s" (? data) (? dn))
    (f u 4 #x7) (f u 3 dn) (0) (f s 8 data))

(define (m68/move size src dst) 
    (cond ((and (eq? size 'l) (m68/moveq src dst))
           => identity)
          (else
           (or (m68/move-fg (blw size) src dst)
               (m68/movea-fg (lw size) src dst)
               (error "no match for (move ~s ~g ~g)" size src dst)))))

(define-fg (m68/move-fg blw (ea-all? src) (ea-d&a? dst))
    (printer "move.~c  ~g,~g" (format-blw (? blw)) (? xsrc) 
                             (if (pair? (? dst)) (car (? dst)) (? dst)))
    (local xsrc)
    (0 0) (f u 2 blw)
    (f u 6 (reverse-ea-bits (? dst)))
    (fg-named xsrc (ea-fg (? src) (context-blw (? blw))))
    (fg (if (pair? (? dst)) (cdr (? dst)) null-fg)))

(define-fg (m68/movea-fg lw (ea-all? src) (ar-number? dst))
    (printer "movea.~c ~g,a~s" (format-lw (? lw)) (? xsrc) (? dst))
    (local xsrc)
    (0 0 1) (f u 1 lw)
    (f u 3 dst) (0 0 1)
    (fg-named xsrc (ea-fg (? src) (context-lw (? lw)))))

(define (reverse-ea-bits ea)
   (let ((fg (if (pair? ea) (car ea) ea)))
     (receive (v1 w1 start2)
              (destructure-fg fg 0)
        (receive (v2 w2 #f)
                 (destructure-fg fg start2)
           (cond ((and (fx= w1 3) (fx= w2 3))
                  (fx+ (fixnum-ashl v2 3) v1))
                 (else 
                  (error "expecting an effective address, got ~s" ea)))))))

(define (m68/movem size src dst)
    (or (m68/movem/re (wl size) src dst)
        (m68/movem/er (wl size) src dst)
        (error "no match for (movem ~a ~g ~g)" size src dst)))

(define-fg (eal-fg ext context)
  (fg ext context))

(define-fg (m68/movem/re wl rl (ea-c&a-or-decr? ea))
    (printer "movem.~c ~s,~g" (format-wl (? wl)) (? rl) (? ea))
    (0 1 0 0  1 0 0 0  1) (f u 1 wl)
    (fg (if (pair? (? ea)) (car (? ea)) (? ea)))
    (f u 16 (convert-register-list (? rl) (@-a? (? ea))))
    (fg (if (pair? (? ea)) (eal-fg (cdr (? ea)) '(general 32)) null-fg)))

(define-fg (m68/movem/er wl (ea-c&-or-incr? ea) rl)
    (printer "movem.~c ~g,~s" (format-wl (? wl)) (? ea) (? rl))
    (0 1 0 0  1 1 0 0  1) (f u 1 wl) 
    (fg (if (pair? (? ea)) (car (? ea)) (? ea)))
    (f u 16 (convert-register-list (? rl) nil))
    (fg (if (pair? (? ea)) (eal-fg (cdr (? ea)) '(general 32)) null-fg)))



                            
(define-constant *forward-regs* 
                 '(d0 d1 d2 d3 d4 d5 d6 d7 a0 a1 a2 a3 a4 a5 a6 a7)) 

(define-constant *backward-regs*
                 '(a7 a6 a5 a4 a3 a2 a1 a0 d7 d6 d5 d4 d3 d2 d1 d0)) 

(define (convert-register-list rl d0-first?)
    (iterate loop ((regs (if d0-first? *forward-regs* *backward-regs*))
                   (rl rl)
                   (bits 0))
       (cond ((null? regs) bits)
             ((memq? (car regs) rl)
              (loop (cdr regs) rl (fx+ (fixnum-ashl bits 1) 1)))
             (else
              (loop (cdr regs) rl (fixnum-ashl bits 1))))))

(define-fg (m68/dbcc (convert-cc cc) (dr-number? dn) tag)
    (printer "db~a d~s,~g" (format-cc (? cc)) (? dn) (? tag))
    (local here)
    (f u 4 5) (f u 4 cc) (1 1 0 0 1) (f u 3 dn) 
    (mark here)
    (f s 16 (fixnum-ashr (from here tag) 3)))

(define-fg (m68/exgd (dr-number? dx) (dr-number? dy))
    (printer "exg d~s,d~s" (? dx) (? dy))
    (f u 4 #xC) (f u 3 dx) (1 0 1 0 0 0) (f u 3 dy))

(define-fg (m68/exga (ar-number? ax) (ar-number? ay))
    (printer "exg a~s,a~s" (? ax) (? ay))
    (f u 4 #xC) (f u 3 ax) (1 0 1 0 0 1) (f u 3 ay))

(define-fg (m68/exgad (ar-number? ax) (dr-number? dy))
    (printer "exg a~s,d~s" (? ax) (? dy))
    (f u 4 #xC) (f u 3 dy) (1 1 0 0 0 1) (f u 3 ax))

(define-fg (m68/exgda (dr-number? dx) (ar-number? ay))
    (printer "exg d~s,a~s" (? dx) (? ay))
    (f u 4 #xC) (f u 3 dx) (1 1 0 0 0 1) (f u 3 ay))
                
(define (m68/exg src dst)
    (or (m68/exga  src dst)
        (m68/exgd  src dst)
        (m68/exgad src dst)
        (m68/exgda src dst)
        (error "no match for (exg ~g ~g)" src dst)))
     
(define (m68/ext size dn)
    (or (m68/ext-1 size dn) 
        (error "no match for (ext ~s ~g)" size dn)))

(define-fg (m68/ext-1 size (dr-number? dn))
    (printer "ext.~c d~s" (if (eq? (? size) 'w) #\w #\l) (? dn))
    (0 1 0 0  1 0 0 0  1) (f u 1 (if (eq? (? size) 'w) 0 1)) (0 0 0) (f u 3 dn))

(define-fg (m68/jmp ea)
    (printer "jmp ~g" (? subfg))
    (local subfg)
    (0 1 0 0  1 1 1 0  1 1) (fg-named subfg (ea-fg (? ea) nil)))

(define-fg (m68/jsr ea)
    (printer "jsr ~g" (? subfg))
    (local subfg)
    (0 1 0 0  1 1 1 0  1 0) (fg-named subfg (ea-fg (? ea) nil)))

(define (m68/lea ea an)
    (or (m68/lea-1 ea an)
        (error "no match for (lea ~g ~g)" ea an)))

(define-fg (m68/lea-1 (ea-c? ea) (ar-number? an))
    (printer "lea ~g,a~s" (? subfg) (? an))
    (local subfg)
    (f u 4 4) (f u 3 an) (1 1 1) (fg-named subfg (ea-fg (? ea) nil)))

(define-fg (m68/link (ar-number? an) (&-word? frame-size))
    (printer "link a~s,#~s" (? an) (? frame-size))
    (0 1 0 0  1 1 1 0  0 1 0 1  0) (f u 3 an) (f s 16 frame-size))
                     
(define (m68/pea ea)
    (or (m68/pea-1 ea)
        (error "no match for (pea ~g)" ea)))

(define-fg (m68/pea-1 (ea-c? ea))
    (printer "pea ~g" (? subfg))
    (local subfg)
    (0 1 0 0  1 0 0 0  0 1) (fg-named subfg (ea-fg (? ea) nil)))

(define-fg (m68/scc (convert-cc cc) (ea-d&a? ea))
    (printer "s~a.b ~g" (format-cc (? cc)) (? subfg))
    (local subfg)
    (f u 4 5) (f u 4 cc) (1 1) (fg-named subfg (ea-fg (? ea) nil)))

(define-fg (m68/swap (dr-number? dn))
    (printer "swap d~s" (? dn))
    (0 1 0 0  1 0 0 0   0 1 0 0  0) (f u 3 dn))

(define-fg (m68/tas (ea-d&a? ea))
    (printer "tas ~g" (? subfg))
    (local subfg)
    (0 1 0 0  1 0 1 0  1 1) (fg-named subfg (ea-fg (? ea) nil)))

(define-fg (m68/trap v)
    (printer "trap #~s" (? v))
    (f u 12 #x4E4) (f u 4 v))

(define-fg (m68/unlk (ar-number? an))
    (printer "unlk a~s" (? an))
    (f u 12 #x4E5) (1) (f u 3 an))

;;; unimplemented:
;;;   abcd, nbcd, sbcd
;;;   move-to-ccr, move-to-sr, move-from-sr, move-to-usp, move-from-usp
;;;   movep

;;; Exports

(define (initialize-m68-lap-env)
  (walk (lambda (item) 
           (*define-lap-m68 (car item) (*value orbit-env (cdr item))))
     '(
       (add        . m68/add)
       (addx       . m68/addx)
       (sub        . m68/sub)
       (subx       . m68/subx)
       (cmp        . m68/cmp)
       (cmpm       . m68/cmpm)
       (and        . m68/and)
       (or         . m68/or)
       (eor        . m68/eor)
       (asl/e      . m68/asl/e)
       (asr/e      . m68/asr/e)
       (asl        . m68/asl)
       (asr        . m68/asr)
       (lsl/e      . m68/lsl/e)
       (lsr/e      . m68/lsr/e)
       (lsl        . m68/lsl)
       (lsr        . m68/lsr)
       (rol/e      . m68/rol/e)
       (ror/e      . m68/ror/e)
       (rol        . m68/rol)
       (ror        . m68/ror)
       (roxl/e     . m68/roxl/e)
       (roxr/e     . m68/roxr/e)
       (roxl       . m68/roxl)
       (roxr       . m68/roxr)
       (bchg       . m68/bchg)
       (bclr       . m68/bclr)
       (bset       . m68/bset)
       (btst       . m68/btst)
       (divs       . m68/divs)
       (divu       . m68/divu)
       (muls       . m68/muls)
       (mulu       . m68/mulu)
       (chk        . m68/chk)
       (clr        . m68/clr)
       (neg        . m68/neg)
       (negx       . m68/negx)
       (not        . m68/not)
       (tst        . m68/tst)
       (nop        . m68/nop)
       (rtr        . m68/rtr)
       (rts        . m68/rts)
       (trapv      . m68/trapv)
;       (rte        . m68/rte)
;       (reset      . m68/reset)
;       (stop       . m68/stop)
       (moveq      . m68/moveq)
       (move       . m68/move)
       (movem      . m68/movem)
       (dbcc       . m68/dbcc)
       (exg        . m68/exg)
       (ext        . m68/ext)
       (jmp        . m68/jmp)
       (jsr        . m68/jsr)
       (lea        . m68/lea)
       (link       . m68/link)
       (pea        . m68/pea)
       (scc        . m68/scc)
       (swap       . m68/swap)
       (tas        . m68/tas)
       (trap       . m68/trap)
       (unlk       . m68/unlk)
       (jbcc       . m68/jbcc)
       (jbra       . m68/jbra)
       (jbsr       . m68/jbsr)
       )))

(initialize-m68-lap-env)

