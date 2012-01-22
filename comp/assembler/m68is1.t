(herald (assembler m68is1 t 0)
        (env t (assembler as_open)))


;;; Procedures for the following instructions do error
;;; checking but return false instead of erring out when the checks
;;; fail.  
;;;
;;; link, dbcc, scc, swap, tas, trap, unlk, jsr, jmp

;;; The whole way of converting arguments and selecting alternatives 
;;; is bogus.  A real parser is needed to do the job right.  Blah.

;;; 68000 instruction format
;;; op-word
;;; [ imm-word-1 [ imm-word-2 ] ]
;;; [ src-ext-1  [ src-ext-2  ] ]
;;; [ dest-ext-1 [ dest-ext-2 ] ]

;;; Size hacking utilities.

(define-constant bwl-data  '((b . 0) (w . 1) (l . 2)))
(define (bwl size) (cdr (assq size bwl-data)))
(define (format-bwl bits) (symbol->character (car (rassq bits bwl-data))))

(define-constant blw-data  '((b . 1) (w . 3) (l . 2)))
(define (blw size) (cdr (assq size blw-data)))
(define (format-blw bits) (symbol->character (car (rassq bits blw-data))))

(define-constant wl-data '((w . 0) (l . 1)))
(define (wl size) (cdr (assq size wl-data)))
(define (format-wl bits) (symbol->character (car (rassq bits wl-data))))

(define-constant lw-data '((w . 1) (l . 0)))
(define (lw size) (cdr (assq size lw-data)))
(define (format-lw bits) (symbol->character (car (rassq bits lw-data))))

(define (symbol->character symbol)
    (char-downcase (string-head (symbol->string symbol))))

(define (format-quick d) (if (fx= d 0) 8 d))   

(define (context-bwl bits) (bits->context bits bwl-data))
(define (context-blw bits) (bits->context bits blw-data))
(define (context-wl bits)  (bits->context bits wl-data))
(define (context-lw bits)  (bits->context bits lw-data))

(define (bits->context bits data)
    (xcase (car (rassq bits data))
       ((b) '(general  8))
       ((w) '(general 16))
       ((l) '(general 32))))

;;; Random converters
(define (quick? d)
   (cond ((and (fixnum? d) (fx<= d 8) (fx> d 0))
          (if (fx= d 8) 0 d))
         (else nil)))

(define (dr-number? frob)
    (cond ((and (fixnum? frob) (fx< frob 8) (fx>= frob 0)) frob)
          ((%dregister? frob) (fg-argref frob 0))
          (else nil)))

(define (ar-number? frob)
    (cond ((and (fixnum? frob) (fx< frob 16) (fx>= frob 8)) (fx- frob 8))
          ((%aregister? frob) (fg-argref frob 0))
          (else nil)))

(define (ar+-number? frob)
    (cond ((and (fixnum? frob) (fx< frob 16) (fx>= frob 8)) (fx- frob 8))
          ((@a+? frob) (fg-argref frob 0))
          (else nil)))

;;; Category predicates (must return argument if true)

;;; The bits are: data? memory? control? alterable?

(define-constant *ea-categories*
    '#(#b1001 #b0001 #b1111 #b1101 #b1101 #b1111 #b1111))  

;;; Return a predicate that checks to see if the given ea
;;; falls into the categories specified by mask.

(define (ea-category-predicate mask)
  (lambda (ea)
    (and (fg? (if (pair? ea) (car ea) ea))
         (let ((fg (if (pair? ea) (car ea) ea)))
           (receive (v1 w1 start2)
                    (destructure-fg fg 0)
              (cond ((fxn= w1 3) (error "expecting an ea, got ~s" ea))
                    ((fx< v1 7)
                     (fx= mask (fixnum-logand (vref *ea-categories* v1) mask)))
                    (else
                     (receive (v2 w2 #f)
                              (destructure-fg fg start2)
                        (cond ((fx= w2 3)
                               (cond ((fx< v2 2) 
                                      t)
                                     ((fx= v2 4) 
                                      (fx= mask (fixnum-logand #b1100 mask)))
                                     (else 
                                      (not (fixnum-odd? mask)))))
                              (else 
                               (error "expecting an ea, got ~s" ea))))))))
         ea)))

(define (ea-all? ea)
    (if (fg? (if (pair? ea) (car ea) ea))
        ea 
        nil))

(define ea-m&a?  (ea-category-predicate #b0101))
(define ea-d&a?  (ea-category-predicate #b1001))
(define ea-a?    (ea-category-predicate #b0001))
(define ea-d?    (ea-category-predicate #b1000))
(define ea-c?    (ea-category-predicate #b0010))
(define ea-c&a?  (ea-category-predicate #b0011))

(define (ea-c&a-or-decr? ea)
    (if (or (ea-c&a? ea) (@-a? ea)) ea nil))

(define (ea-c&-or-incr? ea)
    (if (or (ea-c? ea) (@a+? ea)) ea nil))

;;; Some semi-general formats
                    
;;; Arithmetic op formats
                                      
(define-fg (aop-quick sub? bwl (&-quick? data) (ea-a? dst))
   (local subfg)
   (printer "~aq.~c #~s,~g" 
            (if (fx= 1 (? sub?)) "sub" "add")
            (format-bwl (? bwl)) 
            (format-quick (? data)) 
            (? subfg))
   (0 1 0 1) (f u 3 data) (f u 1 sub?) (f u 2 bwl)
   (fg-named subfg (ea-fg (? dst) (context-bwl (? bwl)))))

(define-fg (op-immediate opname opcode bwl (&? data) (ea-d&a? dst))
   (local the-rest)
   (printer "~ai.~c ~g" (? opname) (format-bwl (? bwl)) (? the-rest))
   (f u 8 opcode) (f u 2 bwl) 
   (fg-named the-rest (ea-imm-fg (? dst) (? data) (context-bwl (? bwl)))))

(define-fg (op-into-ea opname opcode bwl (dr-number? dn) (ea-m&a? dst))
   (local subfg)
   (printer "~a.~c d~s,~g" (? opname) (format-bwl (? bwl)) (? dn) (? subfg))
   (f u 4 opcode) (f u 3 dn) (1) (f u 2 bwl) 
   (fg-named subfg (ea-fg (? dst) (context-bwl (? bwl)))))

;;; '(&!%"# EOR is almost like OP-INTO-EA

(define-fg (op-into-ea-1 opname opcode bwl (dr-number? dn) (ea-d&a? dst))
   (local subfg)
   (printer "~a.~c d~s,~g" (? opname) (format-bwl (? bwl)) (? dn) (? subfg))
   (f u 4 opcode) (f u 3 dn) (1) (f u 2 bwl) 
   (fg-named subfg (ea-fg (? dst) (context-bwl (? bwl)))))

(define-fg (op-into-dn opname opcode bwl (ea-all? src) (dr-number? dn))
   (local subfg)
   (printer "~a.~c ~g,d~s" (? opname) (format-bwl (? bwl)) (? subfg) (? dn))
   (f u 4 opcode) (f u 3 dn) (0) (f u 2 bwl) 
   (fg-named subfg (ea-fg (? src) (context-bwl (? bwl)))))

(define-fg (op-into-a opname opcode wl (ea-all? src) (ar-number? an))
   (local subfg)
   (printer "~aa.~c ~g,a~s" (? opname) (format-wl (? wl)) (? subfg) (? an))
   (f u 4 opcode) (f u 3 an) (f u 1 wl) (1 1)
   (fg-named subfg (ea-fg (? src) (context-wl (? wl)))))
                                                             
;;; addx and subx added by RK

(define-fg (arith-x/d name op bwl (dr-number? dx) (dr-number? dy))
  (printer "~a.~c d~s,d~s" (? name)
                           (format-bwl (? bwl))
                           (? dx) (? dy))
  (f u 4 op) (f u 3 dx) (1) (f u 2 bwl) (f u 3 0) (f u 3 dy))



;;; Shift op formats

(define (format-dir d)
    (if (fx= d 0) #\r #\l))

(define (format-shift op)
    (cond ((fx= op 0) "as")
          ((fx= op 1) "ls")
          ((fx= op 3) "ro")
          ((fx= op 2) "rox")))
                  
(define-fg (shift-op/dd op dir bwl (dr-number? dx) (dr-number? dy))
    (printer "~a~c.~c d~s,d~s" (format-shift (? op)) 
                               (format-dir (? dir)) 
                               (format-bwl (? bwl))
                               (? dx) (? dy))
    (f u 4 #xE) (f u 3 dx) (f u 1 dir) (f u 2 bwl) (1) (f u 2 op) (f u 3 dy))

(define-fg (shift-op/id op dir bwl (&-quick? data) (dr-number? dy))
    (printer "~a~c.~c #~s,d~s" (format-shift (? op)) 
                               (format-dir (? dir)) 
                               (format-bwl (? bwl))
                               (format-quick (? data)) (? dy))
    (f u 4 #xE) (f u 3 data) (f u 1 dir) (f u 2 bwl) (0) (f u 2 op) (f u 3 dy))

(define-fg (shift-op/e op dir dst)
    (printer "~a~c.w ~g" (format-shift (? op)) 
                         (format-dir (? dir))
                         (? subfg))
    (local subfg)
    (f u 4 #xE) (0) (f u 2 op) (f u 1 dir) (1 1)
    (fg-named subfg (ea-fg (? dst) nil)))

;;; BIT op formats      

(define (format-bit-op op)
    (cond ((fx= op 1) "bchg")
          ((fx= op 2) "bclr")
          ((fx= op 3) "bset")
          ((fx= op 0) "btst")))

(define-fg (bit-op/de op (dr-number? dn) dst)
    (printer "~a d~s,~g" (format-bit-op (? op)) (? dn) (? subfg))
    (local subfg)
    (0 0 0 0) (f u 3 dn) (1) 
    (f u 2 op)
    (fg-named subfg (ea-fg (? dst) nil)))

(define-fg (bit-op/ie op (&-moveq-byte? data) dst)
    (printer "~a #~s,~g" (format-bit-op (? op)) (? data) (? subfg))
    (local subfg)
    (0 0 0 0   1 0 0 0)
    (f u 2 op)
    (fg-named subfg (if (fg? (? dst)) (? dst) (car (? dst))))
    (f u 8 0) (f s 8 data)
    (fg (if (fg? (? dst)) null-fg (cdr (? dst)))))

(define-fg (%null-fg) (printer ""))
(define null-fg (%null-fg))

;;; Random format 1 - DIVS DIVU MULS MULU CHK                
(define (op-reg-ea.w opname op1 op2 src dn)
    (or (op-reg-ea.w-1 opname op1 op2 src dn)
        (error "no match for (~a.w ~g ~g)" opname src dn)))

(define-fg (op-reg-ea.w-1 opname op1 op2 (ea-d? src) (dr-number? dn))
    (printer "~a.w ~g,d~s" (? opname) (? subfg) (? dn))
    (local subfg)
    (f u 4 op1) (f u 3 dn) (f u 3 op2) 
    (fg-named subfg (ea-fg (? src) '(general 16))))

;;; Random format 2 - NEG NEGX NOT TST CLR
(define (op-size-ea opname op size dst)
    (or (op-size-ea-1 opname op size dst)
        (error "no match for (~a ~s ~g)" opname size dst)))

(define-fg (op-size-ea-1 opname op bwl (ea-d&a? dst))
    (printer "~a.~c ~g" (? opname) (format-bwl (? bwl)) (? subfg))
    (local subfg)
    (f u 8 op) (f u 2 bwl) (fg-named subfg (ea-fg (? dst) nil)))



;;; Branches: BRA, BSR, Bcc

(define (m68/jbcc jump-op tag) (branch-op (jump-op->m68-cc jump-op) tag))
(define (m68/jbra tag)         (branch-op 0 tag))
(define (m68/jbsr tag)         (branch-op 1 tag))

(define-fg (branch-op cc tag)
  (printer "jb~a    ~g" (format-br (? cc)) (? tag))
  (local dot displ width)
  (mark dot)
  (depending-on (disp dot tag) 
		(choose-a-br (width 0) displ)
		(make-bxx-fg (? cc) (? width) (? displ)))
  )
             
(define (make-bxx-fg cc width displ)
  (let ((displ (fx- displ 16)))
    (xcond ((fx= width  0) null-fg)
	   ((fx= width 16) (m68/bxx.s-abs cc (fixnum-ashr displ 3)))
	   ((fx= width 32) (m68/bxx.l-abs cc (fixnum-ashr displ 3))))))

(define-fg (m68/bxx.s-abs cc displ)
    (printer "b~a ~s" (format-cc (? cc)) (? displ))
    (f u 4 #x6) (f u 4 cc) (f u 8 displ))

(define-fg (m68/bxx.l-abs cc displ)
    (printer "b~a ~s" (format-cc (? cc)) (? displ))
    (f u 4 #x6) (f u 4 cc) (f u 8 0) (f u 16 displ))

(define (choose-a-br current-width displ)
  (let ((actual-backwards-displ (fx- displ 16)))
    (cond ((fx< actual-backwards-displ -16)
	   (cond ((8bit-in-bits? actual-backwards-displ) (return 16 displ))
		 ((16bit-in-bits? actual-backwards-displ) (return 32 displ))
		 (else (error "32 bit conditional branch"))))
	  (else
	   (let ((min-forward-displ (fx- displ current-width)))
	     (cond ((fx= min-forward-displ 0) 
		    (return 0 0))
		   ((8bit-in-bits? min-forward-displ) 
		    (return 16 (fx+ min-forward-displ 16)))
		   ((16bit-in-bits? (fx+ min-forward-displ 16))
		    (return 32 (fx+ min-forward-displ 32)))
		   (else (error "32 bit conditional branch"))))))))


;;; CC formating

(let ((data '(("cc" . 4) ("cs" . 5) ("eq" . 7) ("f" . 1) 
              ("ge" . 12) ("gt" . 14) ("hi" . 2) ("le" . 15) 
              ("ls" . 3) ("lt" . 13) ("mi" . 11) ("ne" . 6) 
              ("pl" . 10) ("t" . 0) ("vc" . 8) ("vs" . 9))))

   (define (format-br bits)
      (cond ((fx= bits 0) "ra")
            ((fx= bits 1) "sr")
            (else (car (rassq bits data)))))

   (define (format-cc bits)
      (car (rassq bits data)))

   )

(let ((data '((cc . 4) (cs . 5) (eq . 7) (f . 1) 
              (ge . 12) (gt . 14) (hi . 2) (le . 15) 
              (ls . 3) (lt . 13) (mi . 11) (ne . 6) 
              (pl . 10) (t . 0) (vc . 8) (vs . 9))))

   (define (m68-cc? cc-symbol)
      (cond ((assq cc-symbol data)
             => cdr)
            (else nil)))

   (define (convert-cc cc-symbol)
      (cond ((m68-cc? cc-symbol) => identity)
            (else 
             (error "expecting a 68000 condition code, got ~s" cc-symbol))))

   )

(define (jump-op->m68-cc cc)
  (cond ((fx< cc 0)
         ;;    '#(abs eq  le  lt   leu ltu neg vs) *JUMP-OPS-NEGATIVE*
         (vref '#(0   7  #xF  #xD  3   5   #xB  9) (fixnum-negate cc)))
        (else 
         ;;    '#(abs neq gt  ge   gtu geu pos vc) *JUMP-OPS-POSITIVE*
         (vref '#(0   6  #xE  #xC  2   4   #xA  8) cc))))

;;;  Labels.   This needs some work.

(define (m68/label node)
     (d@pc (data-current-label node)))

(define label m68/label)
(define template m68/label)

;;;;-------------------------------

;;; do this right (how?) sometime.  byte, word, long, etc.

(define-data-fg (m68/space x)
    (printer ".space  ~s" (? x))
    (f u x 0))

;;; Problem with signed/unsigned

(define-data-fg (m68/byte x)
    (printer ".byte   x~x" (? x))
    (f u 8 x))

(define-data-fg (m68/word x)
    (printer ".word   x~x" (? x))
    (f u 16 x))

;;; Set machine parameters.

(set (machine-template-emitter *m68-machine*) emit-m68-template)
(set (machine-cond-branch      *m68-machine*) m68/jbcc)
(set (machine-uncond-branch    *m68-machine*) m68/jbra)         

