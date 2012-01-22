(herald vaxis)

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

(define-local-syntax (define-vax-instructions . ispecs)
  (labels
    (((process-context opspecs)
      (map (lambda (os)
             (cond ((symbol? (cadr os))
                    (cond ((eq? (cadr os) 'bb) 8)
                          ((eq? (cadr os) 'bw) 16)
                          (else (concatenate-symbol '*vax$ (cadr os) '*))))
                   (else 
                    (cadr os))))
           opspecs))

    ((vaxi-procedure-name vars)
      (cond ((memq? 'displ vars)
             (let ((len (length vars)))
               (cond ((fx= len 1) 'vax$disp)
                     (else (concatenate-symbol 'vax$ (fx- len 1) 'op-disp)))))
            (else
             (concatenate-symbol 'vax$ (length vars) 'op))))

    ((process-vaxi opcode mnemonic opspecs)
      (if (fx> opcode 255) (error "define-vaxi - opcode ~s too big" opcode))
      (let ((vars (map car opspecs))
            (contexts (process-context opspecs)))
        (let ((parms (apply append! (map list vars contexts)))
              (proc-name (vaxi-procedure-name vars)))
          `(let ((proc (lambda ,vars (,proc-name 
                                       ,(vax-mnemonic mnemonic)
                                       ,opcode 
                                       ,@parms))))     
              (define ,(concatenate-symbol 'vax/ mnemonic) proc)
              (*define-lap *vax-machine* ',mnemonic proc))
          )))

    ((vax-mnemonic m)
        (let ((dest (copy-string "       "))
              (source (symbol->string m)))
          (string-downcase! 
           (string-replace dest source (string-length source))))))


    `(block
      ,@(iterate loop ((ispecs ispecs) (is '()))
          (cond ((null? ispecs) is)
                (else
                 (let ((ispec (car ispecs)))
                   (loop (cdr ispecs)
                         (cons (process-vaxi (cadr ispec) (car ispec) (cddr ispec))
                               is)))))))

    ))
           
(define-vax-instructions            
  (ADDD2 #x60 (add rd) (sum md))
  (ADDD3 #x61 (add1 rd) (add2 rd) (sum wd))
  (ADDB2 #x80 (add rb) (sum mb))
  (ADDB3 #x81 (add1 rb) (add2 rb) (sum wb))
  (ADDL2 #xC0 (add rl) (sum ml))
  (ADDL3 #xC1 (add1 rl) (add2 rl) (sum wl))
  (ADDW2 #xA0 (add rw) (sum mw))
  (ADDW3 #xA1 (add1 rw) (add2 rw) (sum ww))
  (AOBLEQ #xF3 (limit rl) (index ml) (displ bb))
  (AOBLSS #xF2 (limit rl) (index ml) (displ bb))
  (ASHL #x78 (cnt rb) (src rl) (dst wl))
  (ASHQ #x79 (cnt rb) (src rq) (dst wq))
  (BBC #xE1 (pos rl) (base vb) (displ bb))
  (BBS #xE0 (pos rl) (base vb) (displ bb))
  (BCC #x1E (displ bb))
  (BCS #x1F (displ bb))
  (BEQL #x13 (displ bb))
  (BEQLU #x13 (displ bb))
  (BGEQ #x18 (displ bb))
  (BGEQU #x1E (displ bb))
  (BGTR #x14 (displ bb))
  (BGTRU #x1A (displ bb))
  (BICB2 #x8A (mask rb) (dst mb))
  (BICB3 #x8B (mask rb) (src rb) (dst wb))
  (BICL2 #xCA (mask rl) (dst ml))
  (BICL3 #xCB (mask rl) (src rl) (dst wl))
  (BICW2 #xAA (mask rw) (dst mw))
  (BICW3 #xAB (mask rw) (src rw) (dst ww))
  (BISB2 #x88 (mask rb) (dst mb))
  (BISB3 #x89 (mask rb) (src rb) (dst wb))
  (BISL2 #xC8 (mask rl) (dst ml))
  (BISL3 #xC9 (mask rl) (src rl) (dst wl))
  (BISW2 #xA8 (mask rw) (dst mw))
  (BISW3 #xA9 (mask rw) (src rw) (dst ww))
  (BITL #xD3 (mask rl) (src rl))
  (BITB #x93 (mask rb) (src rb))
  (BLEQ #x15 (displ bb))
  (BLEQU #x1B (displ bb))
  (BLSS #x19 (displ bb))
  (BLSSU #x1F (displ bb))
  (BNEQ #x12 (displ bb))
  (BNEQU #x12 (displ bb))
  (BPT #x03)
  (BRB #x11 (displ bb))
  (BRW #x31 (displ bw))
  (BVC #x1C (displ bb))
  (BVS #x1D (displ bb))
  (CALLG #xFA (arglist ab) (dst ab))
  (CALLS #xFB (numarg rl) (dst ab))
  (CLRL #xD4 (dst wl))
  (CLRQ #x7C (dst wq))
  (CMPB #x91 (src1 rb) (src2 rb))
  (CMPD #x71 (src1 rd) (src2 rd))
  (CMPL #xD1 (src1 rl) (src2 rl))
  (CMPW #xB1 (src1 rw) (src2 rw))
  (CMPZV #xED (pos rl) (size rb) (base vb) (src rl))
  (CVTBL #x98 (src rb) (dst wl))
  (CVTBW #x99 (src rb) (dst ww))
  (CVTWL #x32 (src rw) (dst wl))
  (DECL #xD7 (dif ml))
  (DIVB2 #x86 (divr rb) (quo mb))
  (DIVB3 #x87 (divr rb) (divd rb) (quo wb))
  (DIVD2 #x66 (divr rd) (quo md))
  (DIVD3 #x67 (divr rd) (divd rd) (quo wd))
  (DIVL2 #xC6 (divr rl) (quo ml))
  (DIVL3 #xC7 (divr rl) (divd rl) (quo wl))
  (DIVW2 #xA6 (divr rw) (quo mw))
  (DIVW3 #xA7 (divr rw) (divd rw) (quo ww))
  (EDIV #x7B (divr rl) (divd rq) (quo wl) (rem wl))
  (EMODF #x54 (mulr rf) (mulrx rb) (muld rf) (int wl) (fract wf))
  (INCL #xD6 (sum ml))
  (JMP #x17 (dst ab))
  (JSB #x16 (dst ab))
  (MCOML #xD2 (src rl) (dst wl))
  (MNEGD #x72 (src rd) (dst wd))
  (MNEGL #xCE (src rl) (dst wl))
  (MOVAB #x9E (src ab) (dst wl))
  (MOVAD #x7E (src ad) (dst wd))
  (MOVAL #xDE (src al) (dst wl))
  (MOVB #x90 (src rb) (dst wb))
  (MOVD #x70 (src rd) (dst wd))
  (MOVL #xD0 (src rl) (dst wl))
  (MOVW #xB0 (src rw) (dst ww))
  (MOVZBL #x9A (src rb) (dst wl))
  (MOVZBW #x9B (src rb) (dst ww))
  (MOVZWL #x3C (src rw) (dst wl))
  (MULB2 #x84 (mulr rb) (prod mb))
  (MULB3 #x85 (mulr rb) (muld rb) (prod wb))
  (MULD2 #x64 (mulr rd) (prod md))
  (MULD3 #x65 (mulr rd) (muld rd) (prod wd))
  (MULL2 #xC4 (mulr rl) (prod ml))
  (MULL3 #xC5 (mulr rl) (muld rl) (prod wl))
  (MULW2 #xA4 (mulr rw) (prod mw))
  (MULW3 #xA5 (mulr rw) (muld rw) (prod ww))
  (POPR #xBA (mask rw))
  (PUSHAD #x7F (src ad))
  (PUSHAL #xDF (src al))
  (PUSHL #xDD (src rl))
  (PUSHR #xBB (mask rw))
  (RET #x04)
  (ROTL #x9C (cnt rb) (src rl) (dst wl))
  (RSB #x05)
  (SOBGEQ #xF4 (index ml) (displ bb))
  (SOBGTR #xF5 (index ml) (displ bb))
  (SUBB2 #x82 (sub rb) (dif mb))
  (SUBB3 #x83 (sub rb) (min rb) (dif wb))
  (SUBD2 #x62 (sub rd) (dif md))
  (SUBD3 #x63 (sub rd) (min rd) (dif wd))
  (SUBL2 #xC2 (sub rl) (dif ml))
  (SUBL3 #xC3 (sub rl) (min rl) (dif wl))
  (SUBW2 #xA2 (sub rw) (dif mw))
  (SUBW3 #xA3 (sub rw) (min rw) (dif ww))
  (TSTD #x73 (src rd))
  (TSTL #xD5 (src rl))
  (TSTB #x95 (src rb))
  (XORB2 #x8C (mask rb) (dst mb))
  (XORB3 #x8D (mask rb) (src rb) (dst wb))
  (XORL2 #xCC (mask rl) (dst ml))
  (XORL3 #xCD (mask rl) (src rl) (dst wl))
  (XORW2 #xAC (mask rw) (dst mw))
  (XORW3 #xAD (mask rw) (src rw) (dst ww))
  )

