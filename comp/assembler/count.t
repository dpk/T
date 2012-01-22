(herald (assembler count t 37)
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

;;; Count number of marks, and number of span-dependent fg's

(define (count-spans ibv)
  (let ((ibv-length (vector-length ibv)))
    (iterate loop ((i 0) (marks 0) (sdfs 0))
       (cond ((fx>= i ibv-length) (return marks sdfs))
             (else 
              (receive (m' s') (count-ib (vref ibv i) marks sdfs)
                 (loop (fx+ i 1) m' s')))))))

(define (count-ib ib m first-s)
  (let ((new-s (cond ((pair? (ib-align ib))
                      (set (ib-sdf-number ib) first-s)
                      (fx+ first-s 1))
                     (else first-s))))
    (iterate loop ((i's (ib-instructions ib)) (m m) (s new-s))
      (cond ((null? i's)
             (return m s))
            (else
             (receive (m' s') (count-fg (car i's) m s)
                (loop (cdr i's) m' s')))))))

(define (count-fg fg m s)
  (let* ((fgt (fg-type fg))
         (vars (fg-vars fg))
         (vals (fg-type-vals fgt)))
    (iterate loop ((ops (fg-type-ops fgt))
                   (m m)
                   (s s))
      (cond ((null? ops) (return m s))
            (else
             (xselect (car ops)
               ((wop/fix)
                (destructure (((#f sign width vop voc1 . ops) ops))
                  (loop ops m s)))
               ((wop/@fix)
                (destructure (((#f sign width-i vop voc1 . ops) ops))
                  (loop ops m s)))
               ((wop/proc)
                (destructure (((#f sign cw-i proc-i vop voc1 . ops) ops))
                  (loop ops m s)))
               ((wop/var)
                (destructure (((#f sign cw-i opt-i vop voc1 . ops) ops))
                  (loop ops m s)))
               ((wop/depending-on)
                (destructure (((#f sdf#-i sdf-i mark-i fge-i . ops) ops))
                  (set (vref vars sdf#-i) s)
                  (loop ops m (fx+ s 1))))
               ((wop/subfield-ic)
                (destructure (((#f sf-i vop voc1 . ops) ops))
                  (receive (m' s') (count-fg (vref vars sf-i) m s)
                    (loop ops m' s'))))
               ((wop/mark)
                (destructure (((#f count-i . ops) ops))
                  (set (vref vars count-i) m)
                  (loop ops (fx+ m 1) s)))
               ))))))


