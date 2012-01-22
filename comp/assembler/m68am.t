(herald (assembler m68am t 0)
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

(define (ea-fg ea context)
    (cond ((pair? ea) (ea-fg-1 (car ea) (cdr ea) context))
          (else ea)))

(define-fg (ea-fg-1 mode ext context)
    (printer "~g~g" (? mode) (? ext))
    (fg mode context)
    (fg ext context))

(define (ea-imm-fg ea imm context)
  (cond ((pair? ea) (ea-imm-fg-2 (car ea) (cdr imm) (cdr ea) context))
        (else (ea-imm-fg-1 ea (cdr imm) context))))

(define-fg (ea-imm-fg-1 mode imm context)
    (printer "~g,~g" (? imm) (? mode))
    (fg mode context)
    (fg imm context))

(define-fg (ea-imm-fg-2 mode imm ext context)
    (printer "~g,~g~g" (? imm) (? mode) (? ext))
    (fg mode context)
    (fg imm context)
    (fg ext context))

;;; Addressing modes.

(define (error-if-not-an an id)
    (cond ((or (fx< an 8) (fx> an 15))
           (error "(~s ~s) -- ~s is not an address register" id an an))))

(define (r regnum)
  (vref *register-fgs* regnum))

(define d r)
(define (a n) 
    (r (fx+ n 8)))

(lset *register-fgs* (make-vector 16))

(define (%register regnum)
    (if (fx> regnum 7) (%aregister (fx- regnum 8)) (%dregister regnum)))

(define-fg (%aregister regnum)
    (printer "a~s" (? regnum))
    (0 0 1) (f u 3 regnum))

 (define %aregister? (fg-predicator %aregister))  

(define-fg (%dregister regnum)
    (printer "d~s" (? regnum))
    (0 0 0) (f u 3 regnum))

 (define %dregister? (fg-predicator %dregister))  

(do ((i 0 (fx+ i 1)))
    ((fx> i 15)
     'done)
  (set (vref *register-fgs* i) (%register i)))

(define-fg (@a regnum)
    (printer "(a~s)" (? regnum))
    (0 1 0) (f u 3 regnum))

 (define @a? (fg-predicator @a))   ; used by index

 (define (@r n) 
     (error-if-not-an n '@r) 
     (@a (fx- n 8)))

(define-fg (@a+ regnum)
    (printer "(a~s)+" (? regnum))
    (0 1 1) (f u 3 regnum))

 (define @a+? (fg-predicator @a+))   ; used by cmpm.

 (define (@r+ n) 
     (error-if-not-an n '@r+) 
     (@a+ (fx- n 8)))

(define-fg (@-a regnum)
    (printer "-(a~s)" (? regnum))
    (1 0 0) (f u 3 regnum))

 (define @-a? (fg-predicator @-a))   ; used by movem.

 (define (@-r n) 
     (error-if-not-an n '@-r) 
     (@-a (fx- n 8)))
                  
(define (d@a regnum displ)
  (cond ((fx= displ 0) (@a regnum))
        (else (%d@a regnum displ))))

  (define (d@r n displ) 
      (error-if-not-an n 'd@r) 
      (d@a (fx- n 8) displ))

(define (%d@a regnum displ)
    (cons (d@a-bits regnum displ)
          (d@a-ext displ)))

  (define-fg (d@a-bits regnum displ)
      (printer "~s(a~s)" (? displ) (? regnum))
      (1 0 1) (f u 3 regnum))

  (define d@a-bits? (fg-predicator d@a-bits))   ; used by index

  (define-fg (d@a-ext displ)
      (printer "")
      (f s 16 displ))

               
;;; Address register indirect, with diplacement
    
(define (d@ax.w ar xr displ)
    (d@ax ar xr 0 displ))

(define (d@ax.l ar xr displ)
    (d@ax ar xr 1 displ))

  (define (d@rx.w n x displ) 
      (error-if-not-an n 'd@rx.w) 
      (d@ax.w (fx- n 8) x displ))

  (define (d@rx.l n x displ) 
      (error-if-not-an n 'd@rx.l) 
      (d@ax.l (fx- n 8) x displ))

(define (d@ax ar xr is-long displ)
    (cons (d@ax-bits ar)
          (d@ax-ext ar xr is-long displ)))

  (define-fg (d@ax-bits ar)
      (printer "")
      (1 1 0) (f u 3 ar))

  (define-fg (d@ax-ext ar xr is-long displ)
      (printer "~s(a~s,~c~s.~c)" 
               (? displ) 
               (? ar) 
               (if (fx> (? xr) 7) #\a #\d)
               (if (fx> (? xr) 7) (fx- (? xr) 8) xr)
               (if (fx= (? is-long) 1) #\L #\W))
      (f u 4 xr) (f u 1 is-long) (0 0 0) (f s 8 displ))

(define (index.w fg xr) (index-1 fg xr 0))
(define (index.l fg xr) (index-1 fg xr 1))
(define index index.l)

(define (index-1 fg xr is-long?)
    (cond ((fg? fg)
           (if (not (@a? fg))
               (error "can't index ~s" fg))
           (receive (v w ns) (destructure-fg fg 0)
              (receive (ar w ns) (destructure-fg fg ns)
                 (d@ax ar xr is-long? 0))))
          ((and (pair? fg) (fg? (car fg)) (fg? (cdr fg)))
           (receive (v w ns) (destructure-fg (car fg) 0)
              (receive (ar w ns) (destructure-fg (car fg) ns)
                 (receive (displ w ns) (destructure-fg (cdr fg) 0)
                    (d@ax ar xr is-long? displ)))))
          (else
           (error "can't index ~s" fg))))

;;; Absolute short, missing
;;; Absolute long, missing
     
;;; PC relative

(define (d@pc tag)
    (cons d@pc-bits
          (d@pc-ext tag)))
 
  (define-fg (d@pc-bits-fg)
       (printer "")
       (1 1 1) (0 1 0))                        
  (define-constant d@pc-bits (d@pc-bits-fg))

  (define-fg (d@pc-ext tag)
    (printer "~g" (? tag))
    (local here)
    (mark here)
    (f s 16 (fixnum-ashr (from here tag) 3)))

;;; Indexed PC relative, missing

;;; Immediate

(define ($ value)
  (cons &-bits
        (&-ext value)))

  (define (&? x) 
    (cond ((and (pair? x) (eq? (car x) &-bits) x)
           x)
          ((fixnum? x)  ; should we be making this for the luser?
           ($ x))
          (else nil)))

  (define (&-quick? x)
    (cond ((&? x) 
           (let ((v (fg-argref (cdr x) 0)))
             (quick? v)))   
          ((quick? x))
          (else nil)))

  (define-integrable (16bit x) 
    (if (16bit? x) x nil))
                         
  (define (&-word? x)
    (cond ((&? x) 
           (let ((v (fg-argref (cdr x) 0)))
             (16bit v)))
          ((fixnum? x) (16bit x))
          (else nil)))
                            
  (define-integrable (8bit x) 
    (if (8bit? x) x nil))

  (define (&-moveq-byte? x)
    (cond ((&? x) 
           (let ((v (fg-argref (cdr x) 0)))
             (8bit v)))
          ((fixnum? x) (8bit x))
          (else nil)))

  (define-fg (&-bits-fg)
       (printer "")
       (1 1 1) (1 0 0))
  (define-constant &-bits (&-bits-fg))

  (define-fg (&-ext value)
    (context (general size))
    (printer "~g" (? subfg))
    (local subfg)
    (fg-named subfg (choose-imm-fg (? size) (? value)) #f))

  (define (choose-imm-fg size value)
    (xcond ((fx= size 8) (&-8 value))
           ((or (fx= size 16) (fx= size 32)) (&-16-or-32 size value))))

  (define-fg (&-8 value)
     (printer "#~s.B" (? value))
     (0 0 0 0 0 0 0 0) (f s 8 value))
  
  (define-fg (&-16-or-32 size value)
     (printer "#~s.~c" (? value) (if (fx= (? size) 16) #\W #\L))
     (f s size value))

;;; This is here just so recompiling is quicker, ought to be in ...is

;;; -------------------------- Template stuff.

;       3                   2                   1
;     1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;    |                              ...                              |
;    +-------------------------------+-------------------------------+
;    |       annotation offset       |  handler's jump displacement  |
;    +---------------+---------------+-------------------------------+
;    |  # ptr cells  |  # scr cells  |    offset within bit vector   |
;    +-+-+-----------+---------------+-------------------------------+
;    |1|N|unused     |   # of args   |  instructions ---->>          | <--- ptr
;    +-+-+-----------+---------------+                               |
;    |                      instruction stream                       |
;    |                              ...                              |

;;; these fields are in the wrong order.

(define-data-fg (m68/template lambda-node handler-ib)
;    (printer ".tem    ~s,~g" (? lambda-node) (? handler-ib))
    (printer ".template")
    (local template-end)
    (f u 16 (get-template-annotation (? lambda-node)))        
    ;;handler offset
    (f s 16 (fixnum-ashr (from template-end handler-ib) 3))   
    (f u 16 (get-template-cells (? lambda-node)))
    ;;bitv offset
    (f u 16 (fx+ (fixnum-ashr (mark-address (? template-end)) 3) 2)) 
    (1)
    (f u 1 (if (template-nary (? lambda-node))  1 0))
    (f u 6 0)
    (f u 8 (get-template-nargs (? lambda-node)))
    (mark template-end)
    )

(define (emit-m68-template code-node code-ib handler-ib template-ib)
   (set (ib-align template-ib) '(24 31 0))
   (emit-to-ib template-ib (m68/template code-node handler-ib))
   (set-ib-follower template-ib code-ib)
   )

;;; Floating point bit fields.

;;; <n,s> means bit field of length s beginning at bit n of the first
;;; WORD (not longword)
;;;                    sign      exponent   MSB       fraction
;;; Apollo IEEE flonum <15,1>    <4,11>     hidden    <0,4>+next 3 words
;;; VAX11 flonum (D)   <15,1>    <7,8>      hidden    <0,7>+next 3 words
;;; Apollo IEEE flonum - binary point follows  hidden MSB, 53 bits of
;;;     precision, if hidden bit is included
;;; VAX11 flonum (D)   - binary point precedes hidden MSB, 56 bits of
;;;     precision, if hidden bit is included 

;;; On the vax
;;;   (integer-decode-float 1.0 list) =>
;;;     (36028797018963968 -55)
;;;     actual stored exponent is 129

;;; On the Apollo
;;;   (integer-decode-float 1.0 list)
;;;     (4503599627370496 -52)
;;;     actual stored exponent is 1023

(define-constant %%apollo-d-ieee-size 53)
(define-constant %%apollo-d-ieee-excess 1023)

(define (apollo-d-ieee-floating-bits flonum)
   (receive (s nm ne)
            (normalized-float-parts flonum 
                                    %%apollo-d-ieee-size 
                                    %%apollo-d-ieee-excess 
                                    t)
      (apollo-d-ieee-floating-fg flonum s nm ne)))

(define-data-fg (apollo-d-ieee-floating-fg flonum s m e)
    (printer ".dfloat ~s" (? flonum))
    (f u 1 s) (f u 11 e) (f u 4  (bignum-bit-field (? m) 48 4))
    (f u 16 (bignum-bit-field (? m) 32 16))
    (f u 16 (bignum-bit-field (? m) 16 16))
    (f u 16 (bignum-bit-field (? m) 0  16)))

