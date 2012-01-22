(herald compile_as)

(define tas-files
    '((assembler ib)
      (assembler as_open)      ;; for assembler & machine descriptions
      (assembler fg)
      (assembler lap)
      (assembler as_utils)     ;; utilities for the assembler
      (assembler as)           ;; client compiler interface
      (assembler count)
      (assembler mark)
      (assembler mini)
      (assembler bits)
      (assembler listing)))

(define tas-vax-files
    '((assembler as_vax)
      (assembler vmodes)
      (assembler vaxis)
      (assembler vaxi)
      (assembler vaxam)
      ))

(define tas-m68-files
    '((assembler as_m68)
      (assembler m68am)
      (assembler m68is1)
      (assembler m68is2)
      ))

(define (compile-tas-syntax)
  (walk comfile '((assembler expand)
                  (assembler fg_spec)  
                  (assembler fg_expr)  
                  (assembler as_syntax))))

(define (compile-tas)
  (walk comfile tas-files))
  
(define (compile-m68-tas)
  (compile-machine tas-m68-files))

(define (compile-vax-tas)
  (compile-machine tas-vax-files))
      
(define tas-locale (make-locale standard-env 'tas))
(define tas-syntax-table (env-syntax-table tas-locale))

(load '(assembler as_utils) tas-locale)
(load '(assembler fg_spec) tas-locale)
(load '(assembler fg_expr) tas-locale)
(load '(assembler expand) tas-locale)
(load '(assembler as_syntax) tas-locale)

