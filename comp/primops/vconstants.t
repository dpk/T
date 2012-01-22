(herald vaxconstants)

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

(define-constant VAX-JUMP-ABSOLUTE #x9f17)
(define-constant JUMP-ABSOLUTE VAX-JUMP-ABSOLUTE)
(define-constant *pointer-registers* 6)

;++ in t3 this will be #x1FFFFFFF fix later
;(define-constant %%maximum-fixnum             #x3ffffff)
;(define-constant *max-fixnum*                 #x3ffffff)
;(define-constant %%maximum-fixnum-as-string   "2147483648")
;++ in t3 this will be #x20000000 fix later
;(define-constant %%minimum-fixnum             #x-3ffffff)
;(define-constant *min-fixnum*                 #x-3ffffff)
;(define-constant %%minimum-fixnum-as-string  "-2147483647")

(define-constant %%slice-size 2)
(define-constant %%vcell-size 4)
(define-constant %%foreign-size 1)
(define-constant %%operation-size 3)

(define-constant CELL 4)

(define-constant tag/fixnum    0)
(define-constant tag/immediate 1)
(define-constant tag/extend    2)
(define-constant tag/pair      3)


                                        
;;;                                           N
(define-constant header/char           #b0000001)  ;; so we can shift char 
(define-constant header/nonvalue      #b10000001)  
(define-constant header/unit           #b0001001) 
(define-constant header/text           #b0010001)
(define-constant header/general-vector #b0011001)
(define-constant header/slice          #b0100001)
(define-constant header/symbol         #b0101001)
(define-constant header/bytev          #b0110001)
(define-constant header/foreign        #b0111001) 
(define-constant header/template       #b1000001)
(define-constant header/cell           #b1001001)  ;; ???
(define-constant header/task           #b1011001)  ;; ???
(define-constant header/true           #b1100001)  ;; ???
(define-constant header/vcell          #b1101001)  ;; ???
(define-constant header/vframe         #b1110001)  ;; stack allocated obj
(define-constant header/stack          #b1111001)

(define-constant header/bignum         #b0000101) 
(define-constant header/short-float    #b0001101) 
(define-constant header/double-float   #b0010101)
(define-constant header/single-float   #b0011101)
(define-constant header/ratio          #b0100101)
(define-constant header/complex        #b0101101)
(define-constant header/fault-frame    #b0110101)

(define-constant header/weak-set   #b1000101)   
(define-constant header/weak-alist #b1001101)
(define-constant header/weak-table #b1010101)
(define-constant header/weak-cell  #b1011001)

(define-constant %%number-of-immediate-types 32)

                                          
(define-local-syntax (define-slink-slots . slots)
  (do ((slots slots (cdr slots))
       (i -3 (if (fixnum? (car slots))
		 (+ i (* (car slots) 4))
		 (+ i 4)))
       (defines '() (if (fixnum? (car slots))
	                defines
			`((define-constant ,(concatenate-symbol 'slink/
								(car slots))
			                   ,i)
			  ,@defines))))
      ((null? slots) `(block (define-constant %%slink-size ,(+ i 3))
			     ,@defines))))

(define-slink-slots
  nil-cdr         
  nil-car         
  nary-setup      
  make-pair       
  make-extend       
  set               
  icall
  kernel            
  dispatch
  current-task      
  critical-count
  undefined-effect  
  root-process      
  interrupt-handler 
  initial-pure-memory-begin   
  initial-pure-memory-end     
  initial-impure-memory-begin 
  initial-impure-memory-end   
  gc-stamp                    
  symbol-generator-count      
  hash-generator-count        
  initial-impure-base         
  boot-args                   
  boot-area-base              
  kernel-begin                
  kernel-end
  cont-wrong-nargs
  dispatch-label
  pair-freelist
  snapper-freelist)

(define-constant task/extra-pointer      -4)
(define-constant task/extra-scratch      -8)
(define-constant task/dynamic-state      -12)
(define-constant task/stack              -16)
(define-constant task/area-frontier      -20)     ;++ area-frontier
(define-constant task/area-limit         -24)
(define-constant task/area               -28)
(define-constant task/doing-gc?          -32)
(define-constant task/foreign-call-cont  -36)
(define-constant task/re-enter-on-fault  -40)
(define-constant task/critical-count     -44)
(define-constant task/area-begin         -48)
(define-constant task/k-list             -52)
(define-constant task/gc-weak-set-list   -56)
(define-constant task/gc-weak-alist-list -60)
(define-constant task/gc-weak-table-list -64)

(define-constant %%task-header-offset   -88)

(define-constant task/t0 0)
(define-constant process/pointer-start         14)
(define-constant *maximum-number-of-arguments* 64)
(define-constant *pointer-temps*               64)
(define-constant *scratch-temps*               5)
(define-constant temp-block-size (fx* (fx+ *pointer-temps* *scratch-temps*) 4))

(define-constant %%stack-size      10000)
(define-constant template/nargs    -1)


(define-constant offset/string-text 2)
(define-constant offset/string-base 6)
(define-constant offset/operation-default 2)
(define-constant offset/operation-handler 10)

(define-constant version-number 31)

(define-local-syntax (define-opcodes . opcodes)
  (do ((i 1 (fx+ i 1))
       (l opcodes (cdr l))
       (z '() (cons `(define-constant ,(concatenate-symbol 'op/ (car l))
                       ,i)
                    z)))
      ((null? l)
       `(block ,@(reverse! z)
               (define *number-of-opcodes* ,i)))))

(define-opcodes
  ;; Structural nonsense
  literal               ;Literal s-expression
  foreign               ;Name of a foreign procedure (a string, e.g. "_getc")
  closure               ;Object is a code vector offset - put a cl-int-cl here
  template1             ;Similarly, but put a cl-int-template here
  template2             ; (cl-int-templates take up 3 cells)
  template3             ; (info is copied from the aux. template)
  ;; Variable references
  vcell-stored-definition     ;Pair (name . cl-offset) for a top-level define
  vcell-defined               ;Name of a var otherwise defined
  vcell-lset                  ;Name of a free variable which is lset
  vcell                ;Name of some other free variable
  variable-value
  ;; Other stuff
  special-literal       ;Descriptor for something peculiar, e.g. a primop
  nonlocal              ;Pair (env-name . var-name) for (*value x 'y)
  )
