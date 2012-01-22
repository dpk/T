(herald (assembler fg t 20))

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

;;; What's an FG

(define-structure-type fg
  type vars size
  (((pretty-print self stream)
     (pretty-print-fg self stream))))


(define-structure-type fg-type
  printer 
  ops 
  vals 
  context 
  subfield-indices 
  first-parameter-index
  data?)

(define (pretty-print-fg fg stream)
  ((fg-type-printer (fg-type fg)) (fg-vars fg) stream))

(define (cons-fg t v)
  (let ((fg (make-fg)))
    (set (fg-type fg) t)
    (set (fg-vars fg) v)
    fg
    ))

(define (cons-fg-type p o v c s fpi data?)
  (let ((fgt (make-fg-type)))
    (set (fg-type-printer fgt) p)
    (set (fg-type-ops fgt) o)
    (set (fg-type-vals fgt) v)
    (set (fg-type-context fgt) c)
    (set (fg-type-subfield-indices fgt) s)
    ;; so that given an FG we can get the args passed to it.
    (set (fg-type-first-parameter-index fgt) fpi)        
    (set (fg-type-data? fgt) data?)
    fgt
    ))

(define-integrable (fg-argref fg n)
   (vref (fg-vars fg) (fx+ n (fg-type-first-parameter-index (fg-type fg)))))
                                                        
;;; These are for defined fg predicates.

(define-operation (get-fg-type fg-maker)
   (error "not an fg maker: (GET-FG-TYPE ~s)" fg-maker))

(define (fg-predicator fg-maker)
  (let ((ft (get-fg-type fg-maker)))
    (lambda (obj) 
       (and (fg? obj) 
            (eq? (fg-type obj) ft)))))

(define (data-fg? fg)
    (and (fg? fg) (fg-type-data? (fg-type fg))))

;;; COMPRESS-FG

(define (compress-fg fg)
  (let* ((fgt (fg-type fg))
         (vars (fg-vars fg))
         (vals (fg-type-vals fgt)))
    (iterate loop ((ops (fg-type-ops fgt)))
      (cond ((null? ops) fg)
            (else
             (xselect (car ops)
               ((wop/fix)
                (destructure (((#f sign width vop voc1 . ops) ops))
                  (loop ops)))
               ((wop/@fix)
                (destructure (((#f sign width-i vop voc1 . ops) ops))
                  (loop ops)))
               ((wop/proc)
                (destructure (((#f sign cw-i proc-i vop voc1 . ops) ops))
                  (format t "~&computed width in ~s~%" fg)
                  (let ((width ((vref vals proc-i) vars)))
                    (set (vref vars cw-i) width)
                    (loop ops))))

               ((wop/var)
                (destructure (((#f sign cw-i opt-i vop voc1 . ops) ops))
                  (let ((opts (vref vals opt-i))
                        (val (get-fixed-value vop voc1 vars vals)))
                    (set (vref vars cw-i)
                         (cond (val (choose-width opts (fxn= sign 0) val))
                               (else 
                                (no-op (error "non fixed value in v field"))
                                ))))
                  (loop ops)))
               ((wop/depending-on)
                (destructure (((#f sdf#-i sdf-i mark-i fge-i . ops) ops))
                  (loop ops)))

               ((wop/subfield-ic)
                (destructure (((#f sf-i vop voc1 . ops) ops))
                  (let ((sf (vref vars sf-i)))
                    (let ((sf (cond ((fixnum? sf)
                                     ;(format t "got sf=~s~%" sf)
                                     (let ((real-sf ((vref vals sf) vars)))
                                       (set (vref vars sf-i) real-sf)
                                       ;(format t "got real sf=~s~%" real-sf)
                                       real-sf))
                                    (else 
                                     sf))))
                      (if (not (fg? sf))
                          (error "non-fg ~s in fg ~s" sf fg))
                      (context-fg sf)
                      (compress-fg sf)
                      (loop ops)))))

               ((wop/mark)
                (destructure (((#f marker-i . ops) ops))
                  (loop ops)))
               ))))))

;;; CHOOSE-WIDTH & friends.  

(define (choose-width opts signed? val)
  (let ((want (integer-field-size val signed?)))
    (iterate loop ((l opts))
        (cond ((null? l)
               (error "no width fits~%  (choose-width ~s ~s ~s)"
                      opts signed? val))
              ((fx>= (car l) want) (car l))
              (else (loop (cdr l)))))))

(define-integrable (integer-field-size n signed?)
  (if (not (fixnum? n)) (error "integer-field-size on non fixnum"))
  (if signed?
      (signed-field-size n)
      (unsigned-field-size n)))

(define-integrable (signed-field-size n)
  (fx+ (if (fx>= n 0)
           (fixnum-howlong n)
           (fixnum-howlong (fx- -1 n)))
       1))

(define-integrable (unsigned-field-size n)
  (if (fx>= n 0)
      (fixnum-howlong n)
      (error "(unsigned-field-size ~s)" n))) ; (fixnum-howlong (fx- -1 n))



;;;; CONTEXT-FG - called after a fg has been instantiated (so it knows 
;;; all its subfields).  Each subfield that needs to know its context 
;;; is womped with it.
 
(define (context-fg fg)
  (if (not (fg? fg))
      (error "trying to set context for non-fg ~s" fg))
  (let* ((fgt (fg-type fg))
         (sfi's (fg-type-subfield-indices fgt)))
    (let ((vars (fg-vars fg))
          (vals (fg-type-vals fgt)))
      (iterate loop ((ops (fg-type-ops fgt))
                     (sfi's sfi's))
        (cond ((null? sfi's) fg)
              (else
               (let ((ops (nthcdr ops (car sfi's))))
                 (destructure (((#f subfg-i vop voc1) ops))
                   (let ((subfg (vref vars subfg-i)))
                     ;; can't contextify <expr> subfgs
                     (cond ((not (fixnum? subfg))
                            (if (fg-type-context (fg-type subfg))
                               (let ((context (get-value vop voc1 vars vals)))
                                 (set-context-in-subfg subfg context))))))
                   (loop ops (cdr sfi's))))))))))

(define (set-context-in-subfg subfg context)
  (cond ((neq? (fg-type-context (fg-type subfg)) (car context))
         (error "sub-field ~s not valid in context ~s"
                subfg context))
        (else
         (let ((vars (fg-vars subfg)))
           (do ((i 0 (fx+ i 1))
                (l (cdr context) (cdr l)))
               ((null? l) subfg)
             (set (vref vars i) (car l)))))))
                                 
;;; DESTRUCTURE-FG.  This is kind of a hack for pulling an FG apart, 
;;; after it has been made.  Returns field value, field width, 
;;; and undated start.

(define (destructure-fg fg start)
  (let* ((fgt (fg-type fg))
         (vars (fg-vars fg))
         (vals (fg-type-vals fgt))
         (ops  (fg-type-ops fgt))
         (ops (nthcdr ops start)))
     (cond ((null? ops)
            (return nil 0 start))
           (else
            (select (car ops)
              ((wop/fix)
               (destructure (((#f sign width vop voc1 . ops) ops))
                 (return (get-value vop voc1 vars vals) 
                         width 
                         (fx+ start 5))))
              ((wop/@fix)
               (destructure (((#f sign width-i vop voc1 . ops) ops))
                 (return (get-value vop voc1 vars vals) 
                         (vref vars width-i)
                         (fx+ start 5))))
              ((wop/proc)
               (destructure (((#f sign cw-i proc-i vop voc1 . ops) ops))
                 (return (get-value vop voc1 vars vals) 
                         (vref vars cw-i)
                         (fx+ start 6))))
              ((wop/var)
               (destructure (((#f sign cw-i opt-i vop voc1 . ops) ops))
                 (return (get-value vop voc1 vars vals) 
                         (vref vars cw-i)
                         (fx+ start 6))))

              ((wop/depending-on)
               (destructure (((#f sdf#-i sdf-i mark-i fge-i . ops) ops))
                 (error "can't do depending-on in DESTRUCTURE-FG")))
              ((wop/subfield-ic)
               (destructure (((#f sf-i vop voc1 . ops) ops))
                 (error "can't do subfield in DESTRUCTURE-FG")))
              ((wop/mark)
               (destructure (((#f marker-i . ops) ops))
                 (return 0 0 (fx+ start 2))))
              (else
               (error "bad start~%  (DESTRUCTURE-FG ~s ~s)" fg start))
              )))))



