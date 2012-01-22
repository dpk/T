(herald locations
  (env (make-empty-early-binding-locale 'nil) primops))

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

(define-constant setter
  (%operation '#f 'setter handle-operation))

(declare simplifier setter simplify-setter)

(define-constant make-location 
  (primop make-location (name offset rep quasi-type nargs
                         type contents-type set-type)

    (((primop.simplify self node)
      (simplify-parameterized-primop self node)))

    ((primop.variant-id self) name)
    ((primop.location? self) t)
    ((primop.location-specs self) offset)
    ((primop.rep-wants self) rep)
    ((primop.make-closed self)
     (case offset 
       ((vector)                                 
           `(*define-vector-accessor ',name ,quasi-type
                 (lambda (x i) (,primop/contents-location ,self x i))
                 (lambda (x i v) (,primop/set-location ,self v x i))))
        ((1 -3)
         `(object (lambda (x)          
                    (let ((x (if (list? x) x (*enforce list? x))))
                      (,primop/contents-location ,self x)))
             ((setter self)
              (lambda (x v)
                (let ((x (if (pair? x) x (*enforce pair? x))))
                  (,primop/set-location ,self v x))))
	     ((identification self) ',name)))
        (else
         `(*define-accessor ',name ,quasi-type ,(fx/ (fx- offset 2) 4)))))
    ((primop.simplify self node)
     (simplify-location node))
    ((primop.settable? self) t)
    ((primop.simplify-setter self call)
     (simplify-location-set self call nargs))
    ((primop.type self node) type)
    ((primop.contents-type self) contents-type)
    ((primop.set-type self) set-type)))

(define-constant make-structure-accessor
  (primop make-structure-accessor (offset)

    (((primop.simplify self node)
      (simplify-parameterized-structure-accessor self node))
     ((primop.make-closed self)
      '(lambda (stype #f id) (stype-selector stype id))))

    ((primop.location? self) t)
    ((primop.location-specs self) offset)
    ((primop.rep-wants self) 'rep/pointer)
     
    ((primop.simplify self node)
     (simplify-location node))
    ((primop.settable? self) t)
    ((primop.simplify-setter self call)
     (simplify-location-set self call 1))
    ((primop.type self node)
     '#[type (object (proc #f (proc #f top) top)
                     (setter #f (proc (#f (proc #f top top)))))])))

(define-local-syntax (define-accessor name offset arg-type qtype . contents)
  (let* ((s-type (if (eq? arg-type 'list) 'pair arg-type))
         (c-type (if (null? contents) 'top (car contents)))
         (type (->type `(object (proc #f (proc #f ,c-type) ,arg-type)
                          (setter #f (proc #f (proc #f ,s-type ,c-type))))))
         (contents-type (->type `(proc #f (proc #f ,c-type) top ,arg-type)))
         (set-type (->type `(proc #f (proc #f) top ,c-type ,s-type))))
    `(define-constant ,name
                      (make-location ',name
                                     ,(if (eq? arg-type 'list)
                                          (fx- (fx* offset 4) 3)
                                          (fx+ (fx* offset 4) 2))
                                     'rep/pointer
                                     ',qtype
                                     1
                                     ',type
                                     ',contents-type
                                     ',set-type))))
                                   
(define-accessor cdr                    0 list    list?)
(define-accessor car                    1 list    list?)
(define-accessor string-text            0 string  string? text)
(define-accessor cell-value             0 cell    true?)
(define-accessor foreign-name           0 foreign foreign?)
(define-accessor symbol-hash            0 symbol  symbol?)
(define-accessor vcell-contents         0 vcell   vcell?)
(define-accessor vcell-locations        1 vcell   vcell?)
(define-accessor vcell-id               2 vcell   vcell?)
(define-accessor vcell-vcell-locations  3 vcell   vcell?)
(define-accessor state-unwinder         0 top     true?)
(define-accessor state-previous         1 top     true?)
(define-accessor state-winder           2 top     true?)
(define-accessor state-next             3 top     true?)
(define-accessor extend-header         -1 top     extend?)
(define-accessor %operation-default     0 top     true?)
(define-accessor %operation-id          1 top     true?)
(define-accessor %operation-handler     2 top     true?)
(define-accessor stype-handler          0 top     stype?)
(define-accessor stype-predicator       1 top     stype?)
(define-accessor stype-constructor      2 top     stype?)
(define-accessor stype-selectors        3 top     stype?)
(define-accessor stype-master           4 top     stype?)
(define-accessor stype-id               5 top     stype?)
(define-accessor unit-source-filename   0 unit    unit?)
(define-accessor unit-herald            1 unit    unit?)
(define-accessor unit-env               2 unit    unit?)
(define-accessor joined-lhs             0 top     joined?)
(define-accessor joined-rhs             1 top     joined?)
(define-accessor bogus-entity-procedure 0 top     bogus-entity?)
(define-accessor bogus-entity-handler   1 top     bogus-entity?)
(define-accessor weak-set-elements   0 top weak-set?)
(define-accessor weak-alist-elements 0 top weak-alist?)
(define-accessor weak-table-table    0 top weak-table?)
(define-accessor weak-table-vector   1 top weak-table?)
(define-accessor weak-cell-contents  0 top weak-cell?)

(define-constant vector-type-length
  (primop vector-type-length (name type)

    (((primop.simplify self node)
      (simplify-parameterized-primop self node)))
                                   
    ((primop.variant-id self) name)
    ((primop.type self node) type)
    ((primop.generate self node)
     (generate-vector-type-length node))))

(define-local-syntax (define-vector-type-length name)
  (let ((full-name (concatenate-symbol name '-length))
        (type (->type `(proc #f (proc #f fixnum) ,name))))
    `(define-constant ,full-name
                      (vector-type-length ',full-name ',type))))
                                   

(define-vector-type-length vector )
(define-vector-type-length bytev )
(define-vector-type-length text )
(define-vector-type-length unit )
(define-vector-type-length symbol )
(define-vector-type-length bignum )
(define-vector-type-length stack )

;;; This must come before STRING-LENGTH
(define-constant set-string-length
  (primop set-string-length () 
    ((primop.side-effects? self) t)
    ((primop.generate self node)
     (generate-set-vector-type-length node))
    ((primop.type self node) '#[type (proc #f (proc #f) string fixnum)])))

(define-constant string-length
  (primop string-length ()
    ((primop.generate self node)
     (generate-vector-type-length node))
    ((primop.settable? self) t)
    ((primop.simplify-setter self call)
     (replace (call-proc call) 
              (create-primop-node (table-entry primop-table
                                               'set-string-length))))
    ((primop.type self node)
     '#[type (object (proc #f (proc #f fixnum) string)
                     (setter #f (proc #f (proc #f string fixnum))))])
    ((primop.make-closed self)
     '(object (lambda (s)
                (string-length (enforce string? s)))
        ((setter self)
         (lambda (s i)
           (if (and (string? s)
                    (fx<= i (text-length (string-text s))))
               (set-string-length s i)
               (error "error in string-length ~s" 
                      (list 'set (list 'string-length s) i)))))))))
                
;;; vector accessors take two specifiers 
;;; type of object
;;; representation it yields by default and expects

(define-local-syntax (define-vector-accessor name quasi-type type rep . c-type)
  (let* ((c-type (if (null? c-type) 'fixnum (car c-type)))
         (r-type (->type
                  `(object (proc #f (proc #f ,c-type) ,type fixnum)
                           (setter #f (proc #f (proc #f ,type fixnum ,c-type))))))
         (contents-type (->type `(proc #f (proc #f ,c-type) top ,type fixnum)))
         (set-type (->type `(proc #f (proc #f) top ,c-type ,type fixnum))))
    `(define-constant ,name
       (make-location ',name 'vector ',rep ',quasi-type 2
                      ',r-type ',contents-type ',set-type))))

(define-vector-accessor vector-elt         vector? vector rep/pointer top)
(define-vector-accessor bignum-digit       bignum? bignum rep/pointer)
(define-vector-accessor extend-pointer-elt extend? top    rep/pointer top)
(define-vector-accessor text-elt           text?   text   rep/char    char)
(define-vector-accessor symbol-elt         symbol? symbol rep/char    char)
(define-vector-accessor mref-8-u           extend? top    rep/integer-8-u)
(define-vector-accessor mref-8-s           extend? top    rep/integer-8-s)
(define-vector-accessor mref-16-u          extend? top    rep/integer-16-u)
(define-vector-accessor mref-16-s          extend? top    rep/integer-16-s)
(define-vector-accessor mref-integer       extend? top    rep/integer)
;(define-vector-accessor mref-32            bytev?  bytev  rep/integer-32)
