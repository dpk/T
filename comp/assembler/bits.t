(herald (assembler bits t 0)
        (env t (assembler as_open) 
               (assembler fg) 
               (assembler ib) 
               (assembler mark)))

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

;;; Output bits; also checks to make sure address line up.
;;; Keeping track of the address is vestigial, it should be flushed

(define (bits ibv size machine)
  (let ((ibv-length (vector-length ibv)))
    (let ((bits (cons-bits size machine)))
      (do ((i 0 (fx+ i 1))
           (addr 0 (bits-ib bits addr (vref ibv i))))
          ((fx>= i ibv-length)
           (return bits (bytev-length (bits-bv bits))))))))

(define (bits-ib bits start-addr ib)
  (let ((a (ib-align ib)))
    (let ((start-addr 
           (cond ((and a (fx> a 0))
                  (write-bits bits a 0)
                  (fx+ start-addr a))
                 (else 
                  start-addr))))
       (if (fxn= start-addr (ib-address ib))
           (error "misaligned in bits"))
       (iterate loop ((i's (ib-instructions ib))
                      (addr start-addr))
          (cond ((null? i's) addr)
                (else
                 (let ((new-addr (bits-fg bits addr (car i's))))
                    ;; really just for debugging & listings
                    ;; (set (fg-size (car i's)) (fx- new-addr addr))
                    (loop (cdr i's) new-addr))))))))

;;; First because maybe defined integrable.
(define (bits-field bits sign width vop voc1 vars vals)
    (let ((value (get-value vop voc1 vars vals)))
      ;; could check that width is enough for sign&value.
      (write-bits bits width value)))
                                                          
(define (bits-fg bits start-addr fg)
  (let* ((fgt (fg-type fg))
         (vars (fg-vars fg))
         (vals (fg-type-vals fgt)))
    (iterate loop ((ops (fg-type-ops fgt))
                   (addr start-addr))
      (cond ((null? ops) addr)
            (else
             (xselect (car ops)
               ((wop/fix)
                (destructure (((#f sign width vop voc1 . ops) ops))
                  (bits-field bits sign width vop voc1 vars vals)
                  (loop ops (fx+ addr width))))
               ((wop/@fix)
                (destructure (((#f sign width-i vop voc1 . ops) ops))
                  (let ((width (vref vars width-i)))
                     (bits-field bits sign width vop voc1 vars vals)
                     (loop ops (fx+ addr width)))))
               ((wop/proc)
                (destructure (((#f sign cw-i proc-i vop voc1 . ops) ops))
                  (let ((width (vref vars cw-i)))
                     (bits-field bits sign width vop voc1 vars vals)
                     (loop ops (fx+ addr width)))))
               
               ((wop/var)
                (destructure (((#f sign cw-i opt-i vop voc1 . ops) ops))
                  (let ((width (vref vars cw-i)))
                     (bits-field bits sign width vop voc1 vars vals)
                     (loop ops (fx+ addr width)))))

               ((wop/depending-on)
                (destructure (((#f sdf#-i sdf-i mark-i fge-i . ops) ops))
                  (let* ((sdf (vref vars sdf-i))
                         (width (sdf-width sdf)))
                    (let ((fgs ((vref vals fge-i) vars)))
                      (if (list? fgs) 
                          (walk (lambda (fg) (bits-fg bits 0 fg)) fgs)
                          (bits-fg bits 0 fgs)))
                    (loop ops (fx+ addr width)))))
               
               ((wop/subfield-ic)
                (destructure (((#f sf-i vop voc1 . ops) ops))
                  (let ((new-addr (bits-fg bits addr (vref vars sf-i))))
                    (loop ops new-addr))))
               
               ((wop/mark)
                (destructure (((#f mark-i . ops) ops))
                  (loop ops addr)))
               ))))))

;;;;  The real grubby bits stuff

(define-structure-type bits
  clump-size clump-writer clumps clumps-i clump-remaining bv bvpos)

(let ((b (stype-master bits-stype)))
  (set (bits-clumps-i b) 0)
  (set (bits-bvpos b) 0)
  )

(define (cons-bits bit-size machine)
  (let ((b (make-bits))
        (size (fx/ (fixnum-ceiling bit-size 32) 8)))

    ;; these two cached from machine guy for convenience
    (set (bits-clump-size b)      (machine-clump-size machine))
    (set (bits-clump-writer b)    (machine-clump-writer machine))

    (set (bits-clump-remaining b) (bits-clump-size b))
    (set (bits-bv b)              (make-bytev size))
    (set (bits-clumps b)          (make-vector (machine-maximum-clumps machine)))
    b))

(define-integrable (hacked-bf value start count)
  (cond ((fixnum? value)
         (fixnum-logand (fixnum-lognot (fixnum-ashl -1 count))
                        (fixnum-ashr value start)))
        (else
         (bignum-bit-field-fixnum value start count))))

;;; Should be just BIT-FIELD, when it is finally in T.  This is 
;;; used by various FGs

(define hacked-bit-field hacked-bf)

;;; Hacking bits is gross in T.

(define (bignum-bit-field-fixnum v s c)
  (let ((result (bignum-bit-field v s c)))
    (if (fixnum? result) 
        result
        (error "tas expects a fixnum~%  (bignum-bit-field ~s ~s ~s)" v s c))))
                                                                       
;;; Fill clumps until on an even clump boundry, then dump clumps.
;;; First clump is high bits.

(define (write-bits bits width value)
  (cond ((and (fx> width 32) (fx= value 0))  ; ???? this is completely wrong
         (write-bits-space bits width))
        (else
         (write-bits-1 bits width value))))

;;; (put here because of define-integrable)

(define-integrable (write-clumps bits clumps-i)
  (if (fx>= clumps-i (vector-length (bits-clumps bits)))
      (error "(while writing bits) too many buffered clumps: ~s" clumps-i))
  (let ((clumps (bits-clumps bits))
        (bv (bits-bv bits))
        (bvpos (bits-bvpos bits)))
    (set (bits-bvpos bits) ((bits-clump-writer bits) clumps clumps-i bv bvpos))
    (set (bits-clumps-i bits) 0)
    (set (bits-clump-remaining bits) (bits-clump-size bits))
    ))

(define (write-bits-1 bits width value)
  (let ((clumps (bits-clumps bits))
        (clumps-i (bits-clumps-i bits))
        (c-rem (bits-clump-remaining bits))
        (csize (bits-clump-size bits)))
      (iterate make-clumps ((clumps-i clumps-i) (v-width width) (c-rem c-rem))
           (cond ((fx< v-width c-rem)
                  ;(format t "fits: ~s ~s ~%" v-width c-rem)
                  (modify (vref clumps clumps-i)
                          (lambda (c) (fixnum-logior (fixnum-ashl c v-width) 
                                                     (hacked-bf value 0 v-width))))
                  (set (bits-clumps-i bits) clumps-i)
                  (set (bits-clump-remaining bits) (fx- c-rem v-width)))
                 (else
                  ;(format t "no fit: ~s ~s ~%" v-width c-rem)
                  (let ((start-bit (fx- v-width c-rem)))
                    (modify (vref clumps clumps-i)
                            (lambda (c) 
                               (fixnum-logior (fixnum-ashl c c-rem)
                                              (hacked-bf value start-bit c-rem))))
                    (cond ((fx> start-bit 0)
                           ;(format t "looping - start-bit is ~s~%" start-bit)
                           (make-clumps (fx+ clumps-i 1) start-bit csize))
                          (else 
                           (write-clumps bits clumps-i)))
                    ))))))

;;; put some amount of "space" out, clumps must be clear?

(define (write-bits-space bits count)
  (let ((csize (bits-clump-size bits)))
    (cond ((not (fx= (fixnum-remainder count csize) 0))
           (error "(while writing bits) odd amount of bit space ~S" count))
          ((fxn= (bits-clump-remaining bits) csize)
           (error "(while writing bits) misaligned bit space - 1" count))
          ((fxn= (bits-clumps-i bits) 0)
           (error "(while writing bits) misaligned bit space - 2" count))
          (else
           (modify (bits-bvpos bits)
                   (lambda (p) (fx+ p (fx/ count csize))))))))


;;; Flonum dismemberment.

;;; Returns sign, and normalized mantissa and exponent  
;;; PRECISION is number of bits desired in the mantissa 
;;; EXCESS is the exponent excess
;;; HIDDEN-BIT-IS-1.? is true if the hidden bit preceeds the
;;;  binary point (it does in Apollo IEEE, does not on the VAX).

(define (normalized-float-parts flonum precision excess hidden-bit-is-1.?)
    (cond ((fl= flonum 0.0)
           (return 0 (%ash 1 (fx+ precision 1)) 0))
          (else
           (integer-decode-float
            (proclaim float? flonum)
            (lambda (m e)
              (let* ((have (integer-length m))
                     (need (fx- precision have))
                     (normalized-m (%ash m need))
                     (normalized-e (- (+ e 
                                         precision 
                                         excess
                                         (if hidden-bit-is-1.? -1 0))
                                       need)))
                 (return (if (fl< flonum 0.0) 1 0) normalized-m normalized-e)
                 ))))))


;;; Machine specfic clump writers.

;;; These routine could be made into a single machine independent
;;; one that is parameterized with bits/byte, bytes/clump, clump order,
;;; bit order, and clump size.  This way seems simpler.

;;; Write the bits in the clumps [0..clumps-i] into the byte vector 
;;; BV starting at BVPOS.  CLUMPS is a vector of fixnums, each fixnum 
;;; a clump, the number of bits in the clump depends on the machine.
;;; The choices are which way to look over the clumps (the most
;;; significant clump is index 0), which way to write the bits of
;;; a single clump, and how many bits of each clump to put into a byte 
;;; (this is usually 8), and whether low bits

;;; Return the next unused position in BV which will be
;;; something like (+ BVPOS (* BYTES/CLUMP (+ CLUMPS-I 1)))

;;; 1 byte/clump, 8 bits/byte, low clumps first, low bits first
(define (vax/write-clumps clumps clumps-i bv bvpos)
  (do ((i clumps-i (fx- i 1))
       (bvpos bvpos (fx+ bvpos 1)))
      ((fx< i 0) 0)
    (set (bref bv bvpos) (vref clumps i))
    (set (vref clumps i) 0))
  (fx+ bvpos (fx+ 1 clumps-i)))

;;; 2 bytes/clump, 8 bits/byte, high clumps first, high bits first
(define (m68/write-clumps clumps clumps-i bv bvpos)
  (do ((i 0 (fx+ i 1))
       (bvpos bvpos (fx+ bvpos 2)))
      ((fx> i clumps-i) 0)
    (let ((c (vref clumps i)))
       (set (bref bv bvpos) (fixnum-ashr c 8))
       (set (bref bv (fx+ bvpos 1)) c))
    (set (vref clumps i) 0))
  (fx+ bvpos (fixnum-ashl (fx+ 1 clumps-i) 1)))


