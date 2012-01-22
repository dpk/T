(herald (assembler mini t 0)
        (env t (assembler as_open) (assembler mark) (assembler ib)))

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

;;; Implements Szymanski's algorithm for span dependent instruction 
;;; computation.  April 1978, CACM, pp300-308.

(define (minimize-displacements sdfs)
    (receive (first last) (initialize-sdfs sdfs)
      (compute-sdf-widths sdfs first last)))

(define (fixup-labels ibv sdfs m-addrs m-sdf-numbers)
    (let ((adjs (compute-adjustments sdfs)))
      (fixup-ibs ibv adjs sdfs)
      (adjust-adjustments-for-alignments adjs sdfs)
      (fixup-marks m-addrs m-sdf-numbers adjs) 
                                                   
      ;; womp final width and displacement into the fg
      (walk-vector (lambda (sdf) 
                     (cond ((not (empty? (sdf-span sdf)))
                            (set (vref (sdf-vars sdf) (car (sdf-indices sdf)))
                                 (sdf-width sdf))
                            (set (vref (sdf-vars sdf) (cdr (sdf-indices sdf)))
                                 (sdf-span sdf)))))
                   sdfs)

      ;; return maximum adjustment (amount of increase in output size)
      (let ((last-adj-i (fx- (vector-length adjs) 1)))
         (if (fx< last-adj-i 0) 
             0 
             (vref adjs last-adj-i)))
      ))

;;; The data structures have been set up by 'count' and 'mark'

;;; Initialize sdf structures:
;;;  - set 'next-dirty' slot of prev sdf to this sdf
;;;  - compute initial spans
;;;  - return the first and last dirty sdfs.

(define (initialize-sdfs sdfs)
  (let ((sdfs-length (vector-length sdfs))
        (first nil))
    (iterate loop ((i 0) (prev nil))
        (cond ((fx>= i sdfs-length) (return first prev))
              (else
               (let ((sdf (vref sdfs i)))
                 (cond ((empty? (sdf-span sdf))   ; an alignment sdf
                        (loop (fx+ i 1)  prev))
                       (else                                     
                        (initialize-span sdfs i sdf)
                        (cond (prev (set (sdf-next-dirty prev) sdf))
                              (else (set first sdf)))
                        (loop (fx+ i 1) sdf)))))))))
       
;;; Initialize a single sdf.  For a given sdf, add it to the crossers
;;; list of all sdfs that it crosses.

(define (initialize-span sdfs index sdf)
   (set (sdf-span sdf)
        (fx- (ib-address (sdf-label sdf)) 
             (vref *mark-addresses* (sdf-mark sdf))))
   (let ((dest (ib-sdf-number (sdf-label sdf))))
      (receive (start end) 
               (cond ((fx> dest index) (return (fx+ index 1) dest)) ;forward
                     (else (return dest index)))
         (do ((i start (fx+ i 1)))
             ((fx>= i end) sdf)
           (push (sdf-crossers (vref sdfs i)) sdf))))
   (set (sdf-width sdf) (sdf-first-width sdf)))

;;; Main loop for computing widths.
;;; Take next sdf off list of (possibly) dirty sdfs.  If the width
;;; of the sdf is big enough to hold its current value, mark it clean;
;;; otherwise, change the width, mark sdfs that cross this one dirty,
;;; and then mark this one clean.

(define (compute-sdf-widths sdfs next last)
   (iterate loop ((next next) (last last) (clean-i 0) (dirty-i 0))
      (cond ((null? next) (cons clean-i dirty-i))   ; only informational
            (else
             (let* ((sdf next)
                    (cur-w (sdf-width sdf)))
               (receive (new-w maybe-new-span)
                        ((sdf-selector sdf) cur-w (sdf-span sdf))
                 ;; width changed?
                 (cond ((fx> new-w cur-w) 
                        (set (sdf-width sdf) new-w)
                        ;; if width didn't change, span shouldn't change.
                        (set (sdf-span sdf) maybe-new-span)
                        (let ((new-last (dirty-crossers sdf (fx- new-w cur-w) last)))
                           (loop (swap (sdf-next-dirty next) 0) 
                                 new-last
                                 (fx+ dirty-i 1)
                                 clean-i)))
                       (else
                        (loop (swap (sdf-next-dirty next) 0) 
                              last 
                              dirty-i
                              (fx+ clean-i 1))))
                 )))))) 
                 
;;; Utility for adjusting spans in all sdfs that span an sdf that has changed.

(define (dirty-crossers sdf delta current-last)
  ;; list affected sdf's as dirty
  (iterate set-dirty ((dirts (sdf-crossers sdf))
                      (new-last current-last))
     (cond ((null? dirts) new-last)
           (else
            (modify (sdf-span (car dirts))
                    (lambda (s) (cond ((fx< s 0) (fx- s delta))
                                      ((fx> s 0) (fx+ s delta))
                                      (else (error "zero span 1")))))
            (cond ;; was clean?
                  ((eq? 0 (sdf-next-dirty (car dirts))) 
                   (set (sdf-next-dirty new-last) (car dirts))
                   (set (sdf-next-dirty (car dirts)) nil)
                   (set-dirty (cdr dirts) (car dirts)))
                  ;; already dirty
                 (else  
                  (set-dirty (cdr dirts) new-last)))))))
                                  
;;; Alignment adjustments happen at the end, and are not subjected
;;; to minimization.

(define (adjust-align-crossers sdf delta)
  (let ((xers (sdf-crossers sdf)))
    (if (not (empty? (sdf-span sdf))) (error "non-alignment sdf"))
    (do ((xers xers (cdr xers)))
        ((null? xers) nil)
      (modify (sdf-span (car xers))
         (lambda (s) 
             (cond ((fx< s 0) (fx- s delta))
                   ((fx> s 0) (fx+ s delta))
                   (else (error "zero span 2"))))))))

;;; After the sdf withs have been computed, we have to go back and adjust all 
;;; the label (and mark) address. Some addresses will have to be adjusted for 
;;; alignment. 'mark' inserted the maximum possible fill for alignment,
;;; and now we remove whatever is necessary.  Spans of sdf's that cross the
;;; alignments must be adjusted.
                                                                 
;;; Compute adjustments table: eg, a label is preceded by 6 sdfs, so the 6th 
;;; element of this table will give the amount to adjust the label by.
;;; This leaves the 0th slot as a dummy (this is a feature).

(define (compute-adjustments sdfs)
  (let* ((sdfs-length (vector-length sdfs))
         (adj-length (fx+ sdfs-length 1))
         (adjustments (make-vector adj-length)))
    (set (vref adjustments 0) 0)
    (iterate loop ((i 0) (accum-adjustment 0))
      (cond ((fx>= i sdfs-length) adjustments)
            (else
              (let ((sdf (vref sdfs i)))             
                (cond ;; align sdfs don't count
                      ((empty? (sdf-span sdf))
                       (set (vref adjustments (fx+ i 1)) accum-adjustment)
                       (loop (fx+ i 1) accum-adjustment))
                      (else
                       (let ((adj (fx+ accum-adjustment
                                       (fx- (sdf-width sdf)
                                            (sdf-first-width sdf)))))
                         (set (vref adjustments (fx+ i 1)) adj)
                         (loop (fx+ i 1) adj))))))))))

;;; Apply the adjustments to the labels (IBs).  Align each
;;; after adjustment, and accumulate the adjustments made for alignment.

;; Hacko alignment stuff.

  ;;; M is one less than multiple being align to.  The multiple must be
  ;;; a power of 2.  So, to do quadword alignment, M is 7

  ;;; Except that we do everything in terms of bits, not bytes, so M is 63

  (define-integrable (as-align lc m)
    (fixnum-logand (fx+ lc m) (fixnum-lognot m)))

  ;;; OFFSET is number of units past a boundry (as determined my M)

  (define-integrable (offset-align lc m offset)
    (cond (offset 
	       (fx+ offset (as-align (fx- lc offset) m)))
          (else
           (as-align lc m))))
                                                       
;;; The goal here is to set the address of each IB to the corrected
;;; value.  Ignoring alignments, this is straigtforward - just add the
;;; amount from the appropriate slot in the adjustments table.  If
;;; we do have to deal with alignment,  we compute that amount
;;; of shrinkage, record it in the alignment sdf, and add the shrinkage
;;; to the accumulating 'align-error'

;;; [side note: some sdfs are in the vector of sdfs only to indicate that an
;;;  alignment happens at that point.  When we adjust an ib by reducing the 
;;;  number of alignment bytes preceeding it, we record the number of
;;;  bytes eliminated in the alignment sdf width field. ]

;;; The only reason for recording the alignment shrinkage in the alignment
;;; sdfs, is that we must fix up the mark addresses also.  It is not
;;; possible to fixup the adjustments table as you go along, because
;;; you are iterating across the ibs, not the adjustments.
;;; So instead, we save the shrinks, and them apply them all at once
;;; to the adjustments vector by calling 'adjust-adjustments-for-alignments.'
;;; The new adjustments are applied to the marks by calling 'fixup-marks.'

(define (fixup-ibs ibv adj's sdfs)
  (let ((ibv-length (vector-length ibv)))
    (iterate loop ((i 0) (align-error 0))  ; accumulated alignment adj's
       (cond ((fx>= i ibv-length) 'done)
             (else
              (let* ((ib (vref ibv i))
                     (sdf# (ib-sdf-number ib))
                     (ib-fix (fx+ align-error (vref adj's sdf#)))
                     (new-ib-addr (fx+ (ib-address ib) ib-fix))
                     (a (ib-align ib)))
;                (format t "~&ib,sdf#,ib-fix,new-ib-add,error ~s~%" 
;                    (list ib sdf# ib-fix new-ib-addr align-error))
                (cond ((not a)
                       (set (ib-address ib) new-ib-addr)
                       (loop (fx+ i 1) align-error))
                      (else
                       (let* ((re-aligned (offset-align (fx- new-ib-addr (car a))
                                                        (cadr a)
                                                        (caddr a)))
                              (shrink (fx- re-aligned new-ib-addr))
                              (align-sdf (vref sdfs (fx- sdf# 1)))
                              )
                         (if (fx> shrink 0) (error "alignment caused increase"))
                         (set (ib-align ib) (fx+ (car a) shrink))
                         (set (ib-address ib) re-aligned)
                         (set (sdf-width align-sdf) shrink)
                         (adjust-align-crossers align-sdf shrink)
                         (loop (fx+ i 1) (fx+ align-error shrink))
                         )))))))))        


(define (adjust-adjustments-for-alignments adjs sdfs)
  (let ((sdfs-length (vector-length sdfs)))
    (iterate loop ((i 0) (align-error 0))
        ;; because indices info adjs are offset by 1, we can do the
        ;; set here, but it is easier to think about if at the bottom
        (modify (vref adjs i) (lambda (a) (fx+ align-error a)))
        (cond ((fx>= i sdfs-length) 'done)     
              (else
               (let* ((sdf (vref sdfs i))
                      (a (if (empty? (sdf-span sdf)) (sdf-width sdf) 0)))
                 (loop (fx+ i 1) (fx+ align-error a))))))))



(define (fixup-marks m-addrs m-sdf-numbers adjs)
  (if (fxn= (vref adjs 0) 0)
      (error "dummy adjustment slot changed"))
  (let ((len (vector-length m-addrs)))
    (do ((i 0 (fx+ i 1)))             
        ((fx>= i len) 'done)
      (modify (vref m-addrs i) 
              (lambda (ma) 
                (fx+ ma (vref adjs (vref m-sdf-numbers i))))))))


