(herald vaxam 
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

(define *vax-register-names* 
                 '#( "s0" "s1" "s2" "s3"  "p"    "a1" "a2" "a3" 
                     "a4" "an" "tp" "nil" "task" "fp" "sp" "pc" ))

(define-integrable (vax-register-name rn)
    (vref *vax-register-names* rn))

(define (r regnum)
  (vref *register-fgs* regnum))

(define-fg (%r regnum)
  (printer "~a" (vax-register-name (? regnum)))
  (f u 4 5)
  (f u 4 regnum))

(lset *register-fgs* (make-vector 16))

(do ((i 0 (fx+ i 1)))
    ((fx> i 15)
     'done)
  (set (vref *register-fgs* i) (%r i)))

(define-fg (@r regnum)
  (printer "(~a)" (vax-register-name (? regnum)))
  (f u 4 6)
  (f u 4 regnum))

(define-fg (@-r regnum)
  (printer "-(~a)" (vax-register-name (? regnum)))
  (f u 4 7)
  (f u 4 regnum))

(define-fg (@r+ regnum)
  (printer "(~a)+" (vax-register-name (? regnum)))
  (f u 4 8)
  (f u 4 regnum))

(define-fg (*@r+ regnum)
  (printer "*(~a)+" (vax-register-name (? regnum)))
  (f u 4 9)
  (f u 4 regnum))

(define-fg (s^& u6bit)
  (printer "s^$~s" (? u6bit))
  (f u 2 0)
  (f u 6 u6bit))

(define (d@r regnum displ)
  (cond ((fx= displ 0) (@r regnum))
        (else (%d@r regnum displ))))

(define-fg (%d@r regnum displ)
  (printer "~s(~a)" (? displ) (vax-register-name (? regnum)))
  (f u 4 (d@r-mode (? displ)))
  (f u 4 regnum)
  (v s (8 16 32) displ))

(define-fg (*d@r regnum displ)
  (printer "*~s(~a)" (? displ) (vax-register-name (? regnum)))
  (f u 4 (*d@r-mode (? displ)))
  (f u 4 regnum)
  (v s (8 16 32) displ))

(define-fg (index indexable regnum)
  (printer "~g[~a]" (? indexable) (vax-register-name (? regnum)))
  (f u 4 4)
  (f u 4 regnum)
  (fg indexable))

(define-fg (absolute location) 
  (printer "*$~s" (? location))
  (f u 8 #x9F)
  (f u 32 location))

(define (d@r-mode displ)
  (cond ((8bit? displ) #xA)
        ((16bit? displ) #xC)
        (else #xE)))

(define (d@r-mode-given-width width)
  (cond ((fx<= width 8) #xA)
        ((fx<= width 16) #xC)
        (else #xE)))

(define (*d@r-mode displ)
  (cond ((8bit? displ) #xB)
        ((16bit? displ) #xD)
        (else #xF)))

(define (*d@r-mode-given-width width)
  (cond ((fx<= width 8) #xB)
        ((fx<= width 16) #xD)
        (else #xF)))

(define-fg (d@pc tag)
  (printer "~g" (? tag))
  (local dot width displ)
  (f u 4 (d@r-mode-given-width (? width)))
  (f u 4 #xF)
  (depending-on (disp dot tag)
                (choose-a-pcrel (width 8) displ) 
                (displacement-fg (? width) (? displ)))
  (mark dot))

(define (choose-a-pcrel current-width displ)
  (cond ((fx< displ 0)
         (let ((displ (fx+ displ current-width)))
            (cond ((8bit-in-bits? (fx- displ 8))  (return 8 (fx- displ 8)))
                  ((16bit-in-bits? (fx- displ 16)) (return 16 (fx- displ 16)))
                  (else (return 32 (fx- displ 32))))))
        (else
         (cond ((8bit-in-bits? displ) (return 8 displ))
               ((16bit-in-bits? displ) (return 16 displ))
               (else (return 32 displ))))))

(define-fg (displacement-fg width displ)
    (f s width (fixnum-ashr (? displ) 3)))

;;; Immediates

(define ($ x)
  (cond ((and (fixnum? x) (fx>= x 0) (fx< x 64))
         (s^& x))
        (else
         (& x))))
            
(define-fg (& value) 
  (context (general size type access))
  (printer "$~s" (? value))
  (fg (vax-immediate (? value) (? size) (? type))))

(define (vax-immediate value size type)
  (case type
    ((byte word long quad) 
     (vax-immediate-integer value size))
    ((f-float d-float)
     (cond ((vax-short-floating-operand value)
            => identity)
           ((eq? type 'd-float)
            (vax-immediate-fg (vax-d-floating-bits value)))
           (else
            (vax-immediate-fg (vax-f-floating-bits value)))))
    (else
     (bug "cannot emit a type ~s operand" type))))
                              
(define-fg (vax-immediate-integer value size)
    (f u 8 #x8F)
    (f s size value))
                              
(define-fg (vax-immediate-fg bits)
    (f u 8 #x8F)
    (fg bits))

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
       

(define-constant %%vax-d-size 56)
(define-constant %%vax-d-excess 128)
(define-constant %%vax-f-size 24)
(define-constant %%vax-f-excess 128)

(import t-implementation-env %ash)

(define (vax-short-floating-operand flonum)
   (receive (sign mantissa exponent)
            (normalized-float-parts flonum %%vax-f-size %%vax-f-excess nil)
      (cond ((and (fx= sign 0)
                  (fx>= exponent 128)
                  (fx<  exponent 136))
             (let* ((shift (fx- %%vax-f-size 4))
                    (short-m (%ash mantissa (fixnum-negate shift)))
                    (short-e (fx- exponent 128)))
               (cond ((= mantissa (%ash short-m shift))
                      (short-float-immediate-fg short-m short-e))
                     (else nil))))
            (else nil))))

(define-fg (short-float-immediate-fg m e)
    (f u 2 0) (f u 3 e) (f u 3 m))

(define (vax-d-floating-bits flonum)
   (receive (s nm ne)
            (normalized-float-parts flonum %%vax-d-size %%vax-d-excess nil)
      (vax-d-floating-fg flonum s nm ne)))

(define (vax-f-floating-bits flonum)
   (receive (s nm ne)
            (normalized-float-parts flonum %%vax-f-size %%vax-f-excess nil)
      (vax-f-floating-fg flonum s nm ne)))

(define-data-fg (vax-d-floating-fg flonum s m e)
    (printer ".d_float ~s" (? flonum))
    (f u 1 s) (f u 8 e) (f u 7 (bignum-bit-field (? m) 48 7))
    (f u 16 (bignum-bit-field (? m) 32 16))
    (f u 16 (bignum-bit-field (? m) 16 16))
    (f u 16 (bignum-bit-field (? m) 0  16)))

(define-data-fg (vax-f-floating-fg flonum s m e)
    (printer ".f_float ~s" (? flonum))
    (f u 1 s) (f u 8 e) (f u 7 (hacked-bit-field (? m) 16 7))
    (f u 16 (hacked-bit-field (? m) 0 16)))


