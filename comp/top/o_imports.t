(herald orbit-imports)

(block
(lset *t-exports* (*value t-implementation-env '*tsys-exports*))
t)

(define no-value return)
(define no-values return)
(define setdiff set-difference)  ;++ flush when backend changed

(import t-implementation-env
;  *set-value            ;++ done
  self-evaluating?
  atom-expander
  check-special-form-syntax
  t-syntax
;  *print-env-warnings?* 
;  print-env-warnings?
  make-infinite-vector
;  check-arg             ;++ done
  expand-object-form
;  make-simple-switch
  set-local-syntax
;  env-for-syntax-definition
  port-truename
   
;  *read-keyword-table*
  read-to-right-bracket

  print-one-line
;  write-text            ;++ obsolete
;  new-line              ;++ done
  set-encoder
  set-decoder
  source-file-extension
  parse-herald
  herald-read-table
  herald-syntax-table
  herald-environment
;  augment-context

  cons-from-freelist
  return-to-freelist
  return-list-to-freelist
  free-map
  free-delq!
  recycle

;  walk-table
  clean-table
  create-%table
  set-table-entry
  make-tree-table

  descriptor-hash
  descriptor->fixnum

  integer-decode-float
  normalize-float-parts
  bignum-bit-field
  %ash
  fixnum-howlong
  *bits-per-fixnum*

  comex?
  comex-code
  comex-objects
  comex-module-name
  comex-opcodes
  comex-annotation
  comex-stype
  make-comex
  install-comex
  instantiate-comex
  write-comex-to-file

  )










