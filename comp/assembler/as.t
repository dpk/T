(herald (assembler as t 37)
        (env t (assembler ib)))

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

;;; Random parameters which must be set by the machine description.
                                                               
(define-structure-type machine
  template-emitter ; For EMIT-TEMPLATE - where does this belong? - routine
  cond-branch      ; For the branchifier (change branches to jumps and fall throughs)
  uncond-branch    ;   FG generators for [un]conditional branches
  clump-size       ; 8 for Vax, 16 for 68000
  maximum-clumps   ; 4 for vax  5 or 7 for 68000, max number of pending clumps.
  clump-writer     ; routine to write a clump, see BITS

  lap-env          ; For lap processor, table mapping mnemonics to FG routines
  pseudo-ops       ; ... alist of procedures called while processing lap
  pseudo-operands  ; ... ...
  )             
                             

;;; used by emit-template and who else?
(lset *current-machine* (undefined-value '*current-machine*))

;;; Maximally crufty interface.
                                       
(lset *current-ib* nil)
(lset *current-assembly-labels* nil)
                                
;;; Optionally retain as data structures for post-mortem

(lset *assembler-retains-pointers?* nil)
(lset *current-ib-vector* nil)  
(lset *current-bits* nil)       

;;; The interface maintains a list of IBs generated, a table relating
;;; code nodes to IBs, and a "current IB."  Emitters side effect the 
;;; current IB.  Delayed comments are implemented at this level.

(define (assemble-init c)
  (bind ((*current-ib* nil)
         (*delayed-comments* '())
         (*current-assembly-labels* 
          (make-labels-table '*current-assembly-labels*)))
    (cond (*assembler-retains-pointers?*
           (set *sdfs* nil)
           (set *current-ib-vector* nil)
           (set *current-bits* nil)
           (c))
          (else
           (bind ((*sdfs* nil)
                  (*current-ib-vector* nil)
                  (*current-bits* nil))
             (c))))))

(define (as-debug)
  (set *assembly-comments?* t)
  (set *assembler-retains-pointers?* t))

(define (as-undebug)
  (set *sdfs* nil)
  (set *current-ib-vector* nil)
  (set *current-bits* nil)
  (set *assembly-comments?* nil)
  (set *assembler-retains-pointers?* nil))

(define (assemble) 
   (let ((bv (bits-bv (as (reverse (table-entry *current-assembly-labels* '&&all&&))
                          *current-machine*))))
      bv))
                                                         
;;; Comments.

;;; Delayed comments are tacked on the next thing emitted.

(lset *delayed-comments* '())

(define (delayed-comment the-comment)
   (push *delayed-comments* the-comment))

(define (emit-delayed-comments ib)
    (emit-comments-to-ib *current-ib* *delayed-comments*)
    (set *delayed-comments* '()))

(define-integrable (flush-delayed-comments)
    (if (not (null? *delayed-comments*)) 
        (emit-delayed-comments *current-ib*)))

;;; Comment is tacked on to previously emitted instruction.

(define (comment-now the-comment)
   (emit-comment-to-ib *current-ib* the-comment))

(define emit-comment delayed-comment)

;;; 

(define (emit-jump symbolic-jop 1tag 0tag)
  (let ((jop (xcond ((fixnum? symbolic-jop) symbolic-jop)
                    ((eq? symbolic-jop 'jneq) jump-op/jn=)
                    ((eq? symbolic-jop 'jgeq) jump-op/j>=)
                    ((eq? symbolic-jop 'jgtr) jump-op/j>)
                    ((eq? symbolic-jop 'jeql) jump-op/j=)
                    ((eq? symbolic-jop 'jmp)  jump-op/jabs)
                    )))
     (emit-jump-to-ib *current-ib* jop 1tag 0tag)
     (flush-delayed-comments)))

;;; Try to force the 1tag block to follow the current block

(define (emit-avoid-jump symbolic-jop 1tag 0tag)
  (emit-jump symbolic-jop 1tag 0tag)
  (maybe-set-ib-follower *current-ib* (xcurrent-label 1tag)) ; xcu... is wrong
  )

(define (emit-template code-node handler-node)
  (let ((cib (data-current-label code-node))
        (hib (data-current-label handler-node)))
    (let ((tib (data-current-label cib)))
      ((machine-template-emitter *current-machine*) code-node cib hib tib)
      (set *current-ib* cib)
      (flush-delayed-comments)
      )))

(define (emit-tag code-node)
  (let ((ib (xcurrent-label code-node)))
    (set *current-ib* ib)
    (cond ((and *as-list-comments?* (node? code-node))
           (set (ib-name ib)
                (xcond ((lambda-node? code-node)
                        (lambda-name code-node))
                       ((leaf-node? code-node)
                        (variable-unique-name code-node))))))
    ))

(define (code-vector-offset label)
    (fixnum-ashr (code-offset *current-assembly-labels* label) 3))

;;; ---- The clean assembler interface routines.
                                                              
(define (emit-to-ib ib fg)
  (context-fg fg)
  (compress-fg fg)
  (push (ib-instructions ib) fg))

(define (emit-jump-to-ib ib jop 1tag 0tag)
    (set (ib-jump-op ib) jop)
    (if 1tag (set (ib-1tag ib) (jump-current-label 1tag ib)))
    (if 0tag (set (ib-0tag ib) (jump-current-label 0tag ib)))
    )

;;; Any assembly has an associated table of labels, maintained by the following.
;;; LABELZ ought to be spelled LABELS. 

;;; A labels table remebers a list of every ib entered.  This
;;; ought to be a structure.
                                  
(define (make-labels-table id)
  (let ((tab (make-table id)))
    (set (table-entry tab '&&all&&) '())
    tab))
                 
;;; Return ib associated with the label, make (& return) a new ib if no
;;;   such label exists.

(define (jump-current-label label jumper-ib)
  (let ((target-ib (xcurrent-label label)))
    (push (ib-jumped-to-by target-ib) jumper-ib)
    target-ib))

(define (data-current-label label)
  (let ((target-ib (xcurrent-label label)))
    (set (ib-data-label? target-ib) t)
    target-ib))

(define (xcurrent-label label)
    (as-label *current-assembly-labels* label))

(define (as-label labelz label)
  (cond ((table-entry labelz label)
         => identity)
        (else
         (let ((ib (make-ib)))
           (modify (table-entry labelz '&&all&&) (lambda (x) (cons ib x)))
           (set (table-entry labelz label) ib)
           ib))))

;;; Returns tag that can be used to access emitted data.
;;; e.g. 
;;;      (emit-pure-data (vax-d-floating-bits 1.2))
;;;      (emit-pure-data (apollo-d-ieee-floating-bits 1.2))

(define (emit-pure-data data-fg)
    (let ((ib (data-current-label data-fg)))
        (emit-to-ib ib data-fg)
        ib))
        
;;; Comments are keyed by pairs in the ib-instructions list, so the comment
;;; is tacked on to the last thing emitted.

(define (emit-comment-to-ib ib the-comment)
   (let ((i's (ib-instructions ib)))
     (let ((key (if (null? i's) '() i's)))
       (let ((c's (ib-comments ib)))
          (cond ((and (pair? c's) (pair? (car c's)) (eq? (caar c's) key))
                 (modify (cdr (car c's)) (lambda (l) (cons the-comment l))))
                (else
                 (set (ib-comments ib) `((,i's ,the-comment) ,@c's))))))))

;;; As above but for a list of comments.  Blah.

(define (emit-comments-to-ib ib the-comments)
   (let ((i's (ib-instructions ib)))
     (let ((key (if (null? i's) '() i's)))
       (let ((c's (ib-comments ib)))
          (cond ((and (pair? c's) (pair? (car c's)) (eq? (caar c's) key))
                 (modify (cdr (car c's)) (lambda (l) (append the-comments l))))
                (else
                 (set (ib-comments ib) `((,i's ,@the-comments) ,@c's))))))))

;;; Given label, return its offset in the code

(define (code-offset labelz label)
  (cond ((table-entry labelz label)
         => (lambda (n) 
               (cond ((not (ib? n))
                      (bug "no IB for label ~s in ~s" label labelz))
                     (else              
                      (ib-address n)))))
        (else
         (bug "no label ~s in ~s" label labelz))))
                                          
;;; Fixup IO to luser.  Assemble a list of IBs into a bytev vector.
;;; returns a BITS structure.

(define (as ibs machine)
  (orbit-debug ";;; (@ ~s) = *CURRENT-ASSEMBLY-LABELS*~%" 
               (object-hash *current-assembly-labels*))
  (let ((ibv (ib-order ibs)))

    (set *current-ib-vector* ibv)   ; debugging
    ;; consistency check
    (let ((ibv-length (vector-length ibv)))
       (do ((i 0 (fx+ i 1)))
           ((fx>= i ibv-length) '*)
         (if (or (not (ib? (vref ibv i)))
                 (fxn= i (ib-pos (vref ibv i))))
             (bug "ibs not ordered correctly"))))

    (branchify ibv machine)
    (receive (mark-count span-count) 
             (count-spans ibv)
      (receive (min-size sdfs mark-addresses mark-sdf-positions)
               (marker ibv mark-count span-count)
        (let* ((mini-iterations (minimize-displacements sdfs))  ;; nia loses
               (max-adj (fixup-labels ibv sdfs mark-addresses mark-sdf-positions)))
          (receive (b bits-length)
                   (bits ibv (fx+ min-size max-adj) machine)
            (format *noise+terminal* "~g~%"
                   `(as
                     (ib      ,(vector-length ibv))
                     (sdf     ,(vector-length sdfs))
                     (align   ,(count-align-sdfs sdfs))
                     (mark    ,(vector-length mark-addresses))
                     (clean   ,(car mini-iterations))
                     (dirty   ,(cdr mini-iterations))
                     (bytes   ,bits-length)
                     ))
             (set *current-bits* b)
             b))))))

