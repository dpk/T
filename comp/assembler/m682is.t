(herald (assembler m682is t 0)
        (env t (assembler as_open)))

; m68am
(define (d@nil displ)
    (cons (d@ax-bits 0)
          (d@nil-ext 7 displ)))

  (define-fg (d@nil-ext  reg displ)
      (printer "~s(d~d)" (? displ) (? reg))
      (0) (f u 3 reg) (1) (0 0) (1) (1) (0) (1 0) (0) (0 0 0)
      (f s 16 displ))

(define (*d@nil displ)
    (cons (d@ax-bits 0)
          (*d@nil-ext 7 displ)))

  (define-fg (*d@nil-ext  reg displ)
      (printer "*~s(d~d)" (? displ) (? reg))
      (0) (f u 3 reg) (1) (0 0) (1) (1) (0) (1 0) (0) (0 0 1)
      (f s 16 displ))

(define (d@static reg displ)
    (cons (d@ax-bits (fx- reg 8))
          (d@static-ext (fx- reg 8) displ)))

  (define-fg (d@static-ext  reg displ)
      (printer "-2(*~s(a~d))" (? displ) (? reg))
      (0) (0 0 0) (1) (0 0) (1) (0) (1) (1 0) (1) (0 1 0)
      (f s 16 displ) (f s 16 2))



(define-fg (m68/hack-jbcc cc displ)
  (printer "jb~a   *~d" (format-br (? cc)) (? displ))
  (0 1 1 0) (f u 4 cc) (f s 8 displ))


(define (m68/divsl src dr dq)
    (or (divsl src dr dq)
        (error "no match for (divs.l ~g ~g ~g)" src dr dq)))

(define-fg (divsl (ea-d? src) (dr-number? dr) (dr-number? dq))
    (printer "divs.l ~g,d~s:d~s" (if (pair? (? src)) (car (? src)) (? src)) 
                                 (? dr) (? dq))
    (0 1 0 0 1 1 0 0 0 1) (fg (if (pair? (? src)) (car (? src)) (? src)))
    (0) (f u 3 dq) (1 0 0 0 0 0 0 0 0) (f u 3 dr)
    (fg (if (pair? (? src)) (eal-fg (cdr (? src)) '(general 32)) null-fg)))

(define (m68/mulsl src dn)
    (or (mulsl src dn)
        (error "no match for (muls.l ~g ~g)" src dn)))

(define-fg (mulsl (ea-d? src) (dr-number? dn))
    (printer "muls.l ~g,d~s" (if (pair? (? src)) (car (? src)) (? src))
                             (? dn))
    (0 1 0 0 1 1 0 0 0 0) (fg (if (pair? (? src)) (car (? src)) (? src)))
    (0) (f u 3 dn) (1 0 0 0 0 0 0 0 0 0 0 0)
    (fg (if (pair? (? src)) (eal-fg (cdr (? src)) '(general 32)) null-fg)))

