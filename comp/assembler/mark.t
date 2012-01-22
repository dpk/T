(herald (assembler mark t 0)
        (env t (assembler as_open) (assembler fg) (assembler ib)))

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

;;; Compute initial address (minimum spans); make table of SDF's and
;;; of marks.  Set mark addresses, mark sdf pos, ib sdf pos, ib-addresses

;;;; MARKER

(define-structure-type sdf
  span crossers selector first-width width next-dirty vars indices backwards?)
                                               
;;; multiplex 2 fields of the structure

(define-integrable sdf-mark sdf-span)
(define-integrable sdf-label sdf-width)

(define (cons-sdf l ws fw i)
  (let ((sdf (make-sdf)))
    (set (sdf-span          sdf) *empty*) ; initial span will be the mark
    (set (sdf-crossers      sdf) '())
    (set (sdf-selector      sdf) ws)
    (set (sdf-first-width   sdf) fw)
    (set (sdf-width         sdf) l)       ; initial width is actually the label
    (set (sdf-next-dirty    sdf) nil)
    (set (sdf-indices       sdf) i)       ; index for width var and displ var 
                                          ; in VARS
    sdf
    ))

;;; Statistics hack.
                                        
(define (count-align-sdfs sdfs)
  (let ((sdfs-length (vector-length sdfs)))
    (do ((i 0 (fx+ i 1))
         (count 0 (if (empty? (sdf-span (vref sdfs i))) (fx+ count 1) count)) )
        ((fx>= i sdfs-length)
         count))))

(lset *mark-addresses* nil)
(lset *mark-sdf-positions* nil)
(lset *sdfs* nil)

;;; Number of sdf's precedeing some spot.  This number is remembered for
;;; each mark or label, as encountered.  This is used in later processing
;;; to fixup labels and marks after the width of each sdf has been determined.

(lset *current-sdf-number* -1)

;;; Returns last address = size in bits (minimum possible)

(define (marker ibv mark-count span-count)
    ;; these should be a bind, debug for now.
    (set *mark-addresses* (make-vector mark-count))
    (set *mark-sdf-positions* (make-vector mark-count))
    (set *sdfs* (make-vector span-count))
    (bind ((*current-sdf-number* 0))
      (let ((ibv-length (vector-length ibv)))
        (do ((i 0 (fx+ i 1))
             (addr 0 (marker-ib addr (vref ibv i))))
            ((fx>= i ibv-length)
             (return addr *sdfs* *mark-addresses* *mark-sdf-positions*))))))
                                                     
(define (marker-ib start-addr ib)
  (let* ((a (ib-align ib))
         (maximum-alignment-filler (if a (car a) 0))
         (start-addr (fx+ start-addr maximum-alignment-filler)))

    ;; if alignment is specified, make an alignment sdf
    (set (ib-address ib) start-addr)
    (if a
      (let ((sdf-pos (ib-sdf-number ib)))
        (set *current-sdf-number* (fx+ sdf-pos 1))
        (set (vref *sdfs* sdf-pos) (cons-sdf nil nil nil nil))))

    (set (ib-sdf-number ib) *current-sdf-number*)
    (iterate loop ((i's (ib-instructions ib))
                   (addr start-addr))
      (cond ((null? i's) addr)
            (else
             (let ((new-addr (marker-fg addr (car i's) )))
               ;; really just for debugging
               ;; (set (fg-size (car i's)) (fx- new-addr addr))
               (loop (cdr i's) new-addr)))))))

(define (marker-fg start-addr fg)
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
                  (loop ops (fx+ addr width))))
               ((wop/@fix)
                (destructure (((#f sign width-i vop voc1 . ops) ops))
                  (loop ops (fx+ addr (vref vars width-i)))))
               ((wop/proc)
                (destructure (((#f sign cw-i proc-i vop voc1 . ops) ops))
                  (loop ops (fx+ addr (vref vars cw-i)))))
               ((wop/var)
                (destructure (((#f sign cw-i opt-i vop voc1 . ops) ops))
                  (loop ops (fx+ addr (vref vars cw-i)))))

               ((wop/depending-on)
                (destructure (((#f sdf#-i sdf-i mark-i fge-i . ops) ops))
                  (let ((sdf (vref vars sdf-i))
                        (sdf# (vref vars sdf#-i)))
                    (set (sdf-mark sdf) (vref vars mark-i))  
                    (set (sdf-vars sdf) vars)
                    (set *current-sdf-number* (fx+ sdf# 1))
                    (set (vref *sdfs* sdf#) sdf)
                    (loop ops (fx+ addr (sdf-first-width sdf))) )))

               ((wop/subfield-ic)
                (destructure (((#f sf-i vop voc1 . ops) ops))
                  (loop ops (marker-fg addr (vref vars sf-i)))))
               
               ((wop/mark)
                (destructure (((#f marker-i . ops) ops))
                  (set (vref *mark-sdf-positions* (vref vars marker-i))
                       *current-sdf-number*)
                  (set (vref *mark-addresses* (vref vars marker-i))
                       addr)
                  (loop ops addr)))
               ))))))
