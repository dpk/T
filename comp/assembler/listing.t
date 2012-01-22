(herald (assembler listing t 1)
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

;;; maybe for listings

(define (size-fg fg)
  (let ((fgt (fg-type fg))
        (vars (fg-vars fg)))
    (iterate loop ((ops (fg-type-ops fgt))
                   (size 0))
      (cond ((null? ops) size)
            (else
             (xselect (car ops)
               ((wop/fix)
                (destructure (((#f sign width vop voc1 . ops) ops))
                  (loop ops (fx+ size width))))
               ((wop/@fix)
                (destructure (((#f sign width-i vop voc1 . ops) ops))
                  (loop ops (fx+ size (vref vars width-i)))))
               ((wop/proc)
                (destructure (((#f sign cw-i proc-i vop voc1 . ops) ops))
                  (loop ops (fx+ size (vref vars cw-i)))))
               ((wop/var)
                (destructure (((#f sign cw-i opt-i vop voc1 . ops) ops))
                  (loop ops (fx+ size (vref vars cw-i)))))
               ((wop/depending-on)
                (destructure (((#f sdf#-i sdf-i mark-i fge-i . ops) ops))
                  (let ((sdf (vref vars sdf-i)))
                    (loop ops (fx+ size (sdf-width sdf))) )))
               ((wop/subfield-ic)
                (destructure (((#f sf-i vop voc1 . ops) ops))
                  (loop ops (fx+ size (size-fg (vref vars sf-i))))))
               ((wop/mark)
                (destructure (((#f marker-i . ops) ops))
                  (loop ops size)))
               ))))))
                          

;;; Generate an assembly listing, given an ib vector, and optional bits.

(define (quicklist . args)
  (destructure (((ibv bits) args))
    (print-listing (terminal-output) 
                   (if ibv ibv *current-ib-vector*)
                   0 
                   (and bits (bits-bv bits)))))

(define (listing . args)
  (destructure (((ibv bits) args))
    (print-listing (terminal-output) 
                   (if ibv ibv *current-ib-vector*)
                   0 
                   (if bits (bits-bv bits) (bits-bv *current-bits*)))))


(define (write-listing-to-file filespec)
    (with-open-streams ((listing-stream (open (->filename filespec) '(out))))
        (print-listing listing-stream 
                       *current-ib-vector* 
                       0 
                       (bits-bv *current-bits*))))
                                           
;;; Start address in bits.

(define (print-listing port ibv start-addr bytev)
  (let ((offset start-addr)
        (len (vector-length ibv)))
    (iterate loop ((i 0) (names->hashes '()))
        (cond ((fx>= i len)
               (format port "~&~% label -> hash: ~s~%" names->hashes)
               *repl-wont-print*)
              (else 
               (let ((ib (vref ibv i)))
                 (list-ib port ib offset bytev)
                 (loop (fx+ i 1)
                       (cond ((empty? (ib-name ib)) names->hashes)
                             (else `((,(ib-name ib) 
                                      (,(if (ib-data-label? ib) 'd 'l)
                                       ,(object-hash ib)))
                                      ,@names->hashes))))))))
    ))

;;; Basically, just loop through instructions, and display each fg.
;;; For each element of instruction list, check to see if there is a comment
;;; to be printed after the fg.  If BYTEV is not null, then print the
;;; actual instruction bytes in the listing.

(lset *list-instruction-bytes* 6)

(define (list-ib port ib offset bytev) 
  (let ((is (ib-instructions ib)))
    (receive (label-tab instruction-tab min-cc)
             (cond (bytev (return "~21t" "~28t" 48))
                   (else  (return "~7t"  "~14t" 34)))
      (cond ((null? is)
             (format port "~&~k~g:~%" label-tab ib))
            (else
             (iterate loop ((addr  (fixnum-ashr (ib-address ib) 3))
                            (label ib)
                            (is    is)
                            (cs    (list-cs port (ib-comments ib) '() min-cc)))
               (cond ((null? is) *repl-wont-print*)
                     (else
                       (let* ((fg (car is))
                              (size (fixnum-ashr (size-fg fg) 3)))
                         (format port "~&~-5x  " (fx+ addr offset))
                         (if bytev
                             (display-bytev-slice port bytev addr (min 6 size)))
                         (if label (format port "~k~g:" label-tab label))
                         (format port "~k~g " instruction-tab fg)
                         (let ((new-cs (list-cs port cs is min-cc)))
                           (if (and bytev 
                                    (fx> *list-instruction-bytes* 6)
                                    (fx> size 6))
                               (list-extra-bytes port bytev addr size 6))
                           (loop (fx+ size addr) nil (cdr is) new-cs)))))
                ))))))

;;; List comments.

;;; Hack-o, returns a possibly cdr'd list of comments -- this is an
;;; efficiency hack.

(define (list-cs port cs is min-cc)
   (cond (cs
          (cond ((assq is cs)
                 => (lambda (spec) 
                      (list-comments port (cdr spec) min-cc)
                      (cond ((null? (caar cs)) (cdr cs))
                            ((eq? (car cs) spec) (cdr cs))
                            (else cs))))
                (else cs)))
         (else nil)))

(lset *as-list-comments?* t)
                
(define (list-comments port comments minimum-comment-column)
   (cond (*as-list-comments?*
          (let ((c-pos (fixnum-maximum (fixnum-ceiling (hpos port) 8)
                                       minimum-comment-column)))
            (walk-backwards (lambda (c)
                              (set (hpos port) c-pos)
                              (cond ((string? c)
                                     (format port "~a" c))
                                    ((procedure? c)
                                     (c port))
                                    (else
                                     (format port "bad comment: ~s" c))))
                           comments)))))

;;; Display the given number of bytev from a bytev.
 
(define (display-bytev-slice port bytev start run)
  (let* ((blen (bytev-length bytev))
         (given-end (fx+ start run))
         (end (if (fx> given-end blen) blen given-end)))
    (do ((i start (+ i 1)))
        ((fx>= i end) 'done)
      (let ((byte (bref bytev i)))
        (writec port (digit->char (fixnum-ashr byte 4.) 16.))
        (writec port (digit->char (fixnum-logand byte 15.) 16.))
        ))))

;;; 

(define (list-extra-bytes port bytev start length runsize)
  (let ((length (fx- (min length *list-instruction-bytes*) runsize))
        (start (fx+ start runsize)))
    (iterate loop ((start start) (length length))
        (cond ((fx< length 1) 'done)
              (else
               (format port "~&       ")
               (display-bytev-slice port bytev start (min length runsize))
               (loop (fx+ start runsize) (fx- length runsize)))))))
