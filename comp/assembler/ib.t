(herald (assembler ib t 6))

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

(define-structure-type ib
  pos           ; position of this IB in vector of all IBs
  sdf-number    ; records the sdf number of the align sdf, if any;
                ;  later, it records the number of sdfs that precede 
                ;  this ib. (used only by IB-FOLLOW, IB-ORDER stuff)

  address       ; address of block
                ; *** MULTIPLEXED w/ IB-PENDING-IBS

  align         ; <n> for specific number of bytes, or (<max> <n> <m>)
                ;  where <n> is alignment interval less 1, <m> is offset,
                ;  and <max> is the largest possible amount of space that 
                ;  this alignment will result in.  False means no alignment.

  instructions  ; list of fgs

  jump-op       ; number indicating EQ, GT, etc
  1tag          ; IB to jump to if successful
  0tag          ; IB to jump to if not successful

  next          ; the IB that may or must follow this one, if any.  This slot is 
                ;  for partial ordering IBs (like forcing the IB containing 
                ;  the instructions for a template to follow the IB with the
                ;  template in it; or to get ordering of blocks in a loop right)
                ;  The target of this field must have its POS slot
                ;  set to point to this IB.  If the link is provisional
                ;  then the pos slot should be set to (cons 'maybe <this-ib>)

  comments      ; alist, keyed by pairs taken from the ib-instructions list.
                ;  Comments will be printed after the fg in the car of the pair
                ;  is printed (in a listing).  Comments keyed by the null list
                ;  are printed before anything else.

  jumped-to-by  ; list of ibs that have a jump or fall through to this ib

  data-label?   ; true if this ib is the subject of a "data" reference
                ;   (and therefore can not be eliminated)

  name          ; for listings

  (((pretty-print self stream)
     (pretty-print-ib self stream))))

(define-integrable ib-pending-ibs ib-address)

(let ((ib (stype-master ib-stype)))
  (set (ib-pos ib) *empty*)

  (set (ib-pending-ibs ib) '())

  (set (ib-align ib) nil)
  (set (ib-instructions ib) '())

  (set (ib-jump-op ib) *empty*)
  (set (ib-1tag ib) *empty*)
  (set (ib-0tag ib) *empty*)

  (set (ib-next ib) *empty*)
  (set (ib-comments ib) '())

  (set (ib-jumped-to-by ib) '())
  (set (ib-data-label?  ib) nil)

  (set (ib-name  ib) *empty*)
  )


(lset *pretty-print-tag* (undefined-value '*pretty-print-tag*))

(define (pretty-print-ib ib stream)
  (*pretty-print-tag* ib stream))

;;; One likely candidate for *pretty-print-tag*

(define (pp-ib-as-hash tag str)
  (format str "~c~s" (if (ib-data-label? tag) #\D #\L) (object-hash tag)))

;;; Another likely candidate for *pretty-print-tag*

(define (pp-ib-as-name-or-hash tag str)
  (cond ((empty? (ib-name tag))
         (format str "~c~s" (if (ib-data-label? tag) #\D #\L) (object-hash tag)))
        (else
         (format str "~a" (ib-name tag)))))
     
(define (set-ib-follower first-ib next-ib)
  (let ((next-pos (ib-pos next-ib))
        (first-next (ib-next first-ib)))
    (cond ((not (empty? first-next))
           ;; first has follower, but is it only provisional? 
           (let ((old-back-link (ib-pos first-next)))
             (cond ((and (pair? old-back-link) (eq? (car old-back-link) 'maybe))
                    (set (ib-pos first-next) *empty*))
                   (else
                    (error "~s already has a follower~% (SET-IB-FOLLOWER ~s ~s)"
                           first-ib
                           first-ib
                           next-ib)))))
          ((not (empty? next-pos))
           (cond ((and (pair? next-pos) (eq? (car next-pos) 'maybe))
                  (set (ib-next (cdr next-pos)) *empty*))
                 (else
                  (error "~s already ordered~% (SET-IB-FOLLOWER ~s ~s)"
                         next-ib
                         first-ib
                         next-ib)))))
      (set (ib-next first-ib) next-ib)
      (set (ib-pos next-ib) first-ib)))

(define (maybe-set-ib-follower first-ib next-ib)
  (cond ((and (empty? (ib-pos next-ib))
              (empty? (ib-next first-ib))
        	  (not (eq? first-ib next-ib)))
         (set (ib-next first-ib) next-ib)
         (set (ib-pos next-ib) (cons 'maybe first-ib)))))


;;; CHOOSE BRANCH INSTRUCTIONS

;;; This pass also reverses the instructions

(define (branchify ibv machine)
  (let ((ibv-length (vector-length ibv))
        (cond-branch (machine-cond-branch machine))
        (uncond-branch (machine-uncond-branch machine)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i ibv-length)
         ibv)
      (let ((ib (vref ibv i))
            (fall (cond ((fx< (fx+ i 1) ibv-length) (vref ibv (fx+ i 1)))
                        (else nil))))
        (cond ((empty? (ib-jump-op ib)))
              ((empty? (ib-0tag ib))  ; abs jump
               (cond ((eq? (ib-1tag ib) fall)
                      (set (ib-jump-op ib) 'fall)
                      (set (ib-1tag  ib) *empty*))
                     (else 
                      (set-branch ib (uncond-branch (ib-1tag ib)))
                      )))
              ((eq? fall (ib-0tag ib))
               (set-branch ib (cond-branch (ib-jump-op ib) (ib-1tag ib)))
               (set (ib-0tag ib) 'fall))
              ((eq? fall (ib-1tag ib))
               (set (ib-1tag ib) (ib-0tag ib))
               (modify (ib-jump-op ib) reverse-jump)
               (set-branch ib (cond-branch (ib-jump-op ib) (ib-1tag ib)))
               (set (ib-0tag ib) 'fall))
              ;; neither can fall through, both must jump
              ;; if we knew sizes, we could jump to the closer one
              (else 
               (set-2-branches ib cond-branch uncond-branch)))
        (modify (ib-comments ib) reverse!)
        (modify (ib-instructions ib) reverse!)))))

(define (set-branch ib branch-fg)
  (push (ib-instructions ib) (compress-fg branch-fg)))

(define (set-2-branches ib cond-branch uncond-branch)
  (let ((1pos (ib-pos (ib-1tag ib)))
        (0pos (ib-pos (ib-0tag ib)))
        (pos (ib-pos ib)))
    (receive (near far)
             (cond ((fx< (fixnum-abs (fx- pos 1pos))
                         (fixnum-abs (fx- pos 0pos)))
                    (return 1pos 0pos))
                   (else
                    (return 0pos 1pos)))
      (cond ((eq? far (ib-1tag ib))
             (exchange (ib-1tag ib) (ib-0tag ib))
             (modify (ib-jump-op ib) reverse-jump)))
      (set-branch ib (cond-branch (ib-jump-op ib) (ib-1tag ib)))
      (set-branch ib (uncond-branch (ib-0tag ib))))))

;;;; ORDER INSTRUCTION BLOCKS (IB'S)

;;; Given a list of ibs in the order generated, put them
;;; into a vector and set the POS field

(define-integrable (ib-free? ib)
  (empty? (ib-pos ib)))

(define-integrable (ib-seen? ib)
  (null? (ib-pos ib)))

(define-integrable (ib-ordered? ib)
  (fixnum? (ib-pos ib)))

(define-integrable (ib-done? ib)
  (and (fixnum? (ib-pos ib)) (fx>= (ib-pos ib) 0)))

(define (first-unordered-ib l)
    (iterate loop ((l l))
        (cond ((null? l) nil)
              ((not (ib-ordered? (car l))) l)
              (else
               (loop (cdr l))))))

(lset *queued-ibs* nil)
(lset *unqueued-ibs* nil)
             
(define (ib-order ibs)
  ;; convert pos slots to a canonical mark for easy checking.
  (walk (lambda (ib) (if (not (ib-free? ib)) (set (ib-pos ib) -1)))
        ibs)
  (bind ((*queued-ibs* 0) (*unqueued-ibs* 0))
    (let ((ibv (make-vector (length ibs)))
          (ibs (first-unordered-ib ibs)))
      (iterate loop ((fall-to (car ibs)) (ibs (cdr ibs)) (pos 0))
        (set (ib-pos fall-to) pos)
        (set (vref ibv pos) fall-to)
        (receive (winner loser)
                 ;; check to see if a next is specified, if not choose one
                 (cond ((empty? (ib-next fall-to))
                        (ib-order-choose fall-to))
                       (else
                        (return (ib-next fall-to) nil)))
          (let ((ibs (cond ((ib-pending-ibs fall-to) 
                            => (lambda (x) (append! x ibs)))
                           (else ibs))))
            (cond (winner
                   (if loser (set (ib-pos loser) nil))  ; mark as seen
                   (loop winner ibs (fx+ pos 1)))
                  (else
                   (let ((ibs (next-free-ib ibs)))
                      (cond ((null? ibs) 
                             (noise "~s IBs queued, ~s IBs unqueued~%" 
                                    *queued-ibs*
                                    *unqueued-ibs*)
                             ibv)
                            (else 
                             (loop (car ibs) (cdr ibs) (fx+ pos 1)))))))))))))

(define (next-free-ib ibs)
  (iterate next-free ((ibs ibs))
    (cond ((null? ibs) ibs)
          (else
           (let ((next (car ibs)))
             (cond ((ib-ordered? next)
                    (next-free (cdr ibs)))
                   ((not (empty? (ib-jump-op next)))
                    ibs)
                   ;; dead end - so try to queue on pending-ibs of some ib
                   (else
                    (let ((froms (ib-jumped-to-by next)))
                      (iterate queue ((froms froms) (q 0) (uq 0))
                         (cond ((null? froms)
                                (set *queued-ibs* (fx+ *queued-ibs* q))
                                (set *unqueued-ibs* (fx+ *unqueued-ibs* uq))
                                (if (fx> q 0) (next-free (cdr ibs)) ibs))
                               ((and (not (ib-done? (car froms)))
                                     (empty? (ib-next (car froms))))
                                ;(format t "queue ~g on ~g~%~%" next (car froms))
                                (push (ib-pending-ibs (car froms)) next)
                                (queue (cdr froms) (fx+ q 1) uq))
                               (else
                                ;(format t "didn't queue on ~g~% pos - ~s~% next - ~s~%~%" (car froms) (ib-pos (car froms)) (ib-next (car froms)))
                                (queue (cdr froms) q (fx+ uq 1)))))))
                   ))))))



;;; Returns 2 return, winner and loser.  If loser is null, then there is
;;; one alternative, if winner is null, there is no alternative.
(define (ib-order-choose ib)
  (let ((0tag (ib-0tag ib))
        (1tag (ib-1tag ib)))
    (cond ((empty? (ib-jump-op ib))
           (return nil nil))
          ((empty? 0tag)
           (return (if (ib-ordered? 1tag) nil 1tag)
                   nil))
          ((and (ib-ordered? 0tag) (not (ib-ordered? 1tag)))
           (return 1tag nil))
          ((and (ib-ordered? 1tag) (not (ib-ordered? 0tag)))
           (return 0tag nil))
          ;; they are both ordered, or both not
          ((ib-ordered? 0tag)
           (return nil nil))
          ;; both unordered
          ((fx> (ib-situation 0tag) (ib-situation 1tag))
           (return 0tag 1tag))
          (else
           (return 1tag 0tag)))))

;;; This should take into account whether or not the IB-NEXT slot is filled 
(define (ib-situation ib)
  (cond ((empty? (ib-jump-op ib)) 0)
        ((empty? (ib-0tag ib))  ; means block ends in unconditional jump
         (cond ((ib-free? (ib-1tag ib))
                6)    ;1 exit, free future
               ((ib-seen? (ib-1tag ib))
                7)    ;1 exit, already queued
               (else
                1)))
        (else
         (let ((1ordered? (ib-ordered? (ib-1tag ib)))
               (0ordered? (ib-ordered? (ib-0tag ib))))
           (cond ((and (not 1ordered?) (not 0ordered?))
                  3)  
                 ((and 1ordered? 0ordered?)
                  2)   
                 ((or (ib-seen? (ib-0tag ib)) (ib-seen? (ib-1tag ib)))
                  5)
                 (else
                  4))))))
