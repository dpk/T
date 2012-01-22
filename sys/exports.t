(herald exports
  (env tsys))

(define (declare-tsys-exports . args)
  (set *tsys-exports* (append args *tsys-exports*))
  (no-value))

(define (declare-tsys-syntax-exports . args)
  (set *tsys-syntax-exports* (append args *tsys-syntax-exports*))
  (no-value))

(define (export-tsys to-env)
  (walk (lambda (sym)
          (let ((old (if (pair? sym) (car sym) sym))
                (new (if (pair? sym) (cdr sym) sym)))
            (*define to-env old (*value t-implementation-env new))))
        *tsys-exports*)
  (let ((from (env-syntax-table t-implementation-env))
        (to (env-syntax-table to-env)))
    (walk (lambda (sym)
            (let ((desc (syntax-table-entry from sym)))
              (cond ((null? desc)
                     (error "no syntax for ~S" sym))
                    (else
                     (set-syntax-table-entry to sym desc)))))
          *tsys-syntax-exports*)
    (walk (lambda (current obsolete)
            (set (syntax-table-entry to obsolete) (syntax-table-entry from current)))
          '(with-open-ports with-output-width-port)
          '(with-open-streams with-output-width-stream))))

;;; Standard T syntax and value export lists.

;;; Syntax to be exported from system syntax table to base syntax table.
(block
(lset *tsys-syntax-exports*
 '(
   ;; Primitive special forms.  Everything not in this group is implemented
   ;; as a macro.
   quote
   if
   block
   lambda
   lset
   let-syntax
   define-local-syntax
   locale
   labels
   declare

   ;; MACROS
   define
   define-integrable
   define-constant
   set
   block0
   define-structure-type
   quasiquote
   do                   
   iterate
   locative
   delay
   unwind-protect
   with-output-to-string
   with-input-from-string
   with-output-to-list
   with-output-width-port
   with-open-ports
   import
   require
   bound?                               ; Unreleased but handy
   catch
   ignore
   ignorable
   comment
   safe-macro-expander
   pattern-predicate
   macro-expander
   define-safe-syntax
   define-syntax
   herald                               ; signals a syntax error

   ;; LET
   let
   bind
   destructure
   let*
   destructure*                         ; Candidate for future release
   bind*                                ; Candidate
   receive

   ;; COND
   cond
   xcond
   or
   and
   case
   xcase
   select
   xselect

   ;; OBJECT
   object
   operation
   define-operation
   define-settable-operation
   define-predicate
   fix-up-default
   join-methods

   ;; MODIFY
   modify-location
   modify
   swap
   exchange
   increment
   decrement
   push
   pop

;;; Pragmatics

   trace
   untrace
   pp
   time

;;; Internal unreleased special forms.

   named-lambda
   the-environment
   var-locative
   variable-value
   set-variable-value
   define-variable-value
   lset-variable-value
   ))

;;; Values exported to base environment from T system environment.

(lset *tsys-exports*
 '(
   apply
   receive-values

   ;; KERNEL
   true
   false
   true?
   eof
   repl-wont-print?
   repl-wont-print
   undefined-value
   no-op
   make-simple-switch

   ;; ERROR
   error
   warning
   losing-xcond
   losing-xselect
   losing-xcase
   undefined-if-value
   unbound-label
   let-missing-initializer
   no-more-cond-clauses
   case-fell-off-end
   select-fell-off-end

   ;; standard-support
   unquote
   unquote-splicing
   undefined-effect
    eq?
    fixnum?
    list?
    char?
    char=
    char<
    char->ascii
    ascii->char
    return
    setter
    make-structure-accessor
    print-structure
    exhibit-structure
    car
    cdr
    string-text
    stype-predicator
    stype-constructor
    stype-selectors
    stype-master
    stype-id
    stype-handler
    vector-length
    bytev-length
    text-length
    string-length
    ;set-string-length
    vector-elt
    text-elt
    nil
    t
    else
    number-of-char-codes
    not
    false?
    boolean?
    always
    proj0
    proj1
    proj2
    proj3
    projn
    identity
    cond-=>-aux
    or-aux
    ;proclaim
    enforce
    *enforce
    check-arg
    *check-arg
    null?
    atom?
    pair?
    symbol?
    vector?
    bytev?
    text?
    string?
    null-list?
    nonnegative-fixnum?
    eof?
    newline?
    mem?
    memq?
    neq?
    vset
    non-empty-string?
    string-empty?
    string-tail
    string-tail!
    string-nthtail
    string-nthtail!
    string-elt
    string-head
    char>
    charn=
    char>=
    char<=
    cons
    char
    chdr
    chdr!
    nthchdr
    nthchdr!
    nthchar
    vref
    bref
    bref-8
    bref-16
    bref-32
    bref-8-u
    bref-8-s
    bref-16-u
    bref-16-s
    caar
    cadr
    cdar
    cddr
    caaar
    caadr
    cadar
    caddr
    cdaar
    cdadr
    cddar
    cdddr
    caaaar
    caaadr
    caadar
    caaddr
    cadaar
    cadadr
    caddar
    cadddr
    cdaaar
    cdaadr
    cdadar
    cdaddr
    cddaar
    cddadr
    cdddar
    cddddr

   ;; EQUALITY
   alike?
   alikeq?
   alikev?
   equiv?

   ;; VECTOR
   make-bytev
   make-vector
   make-text
   vector-fill
   vector-replace
   copy-vector
   copy-bytev
   copy-text
   list->vector
   vector->list
   vector-pos
   vector-posq
   walk-vector
   vector

   ;; SYMBOL
   string->symbol
   symbol->string
   concatenate-symbol
   generate-symbol
   walk-symbols

   ;; LIST
   proper-list?
   list
   %list   ;++ yowsir
   cons*
   length
   mem
   memq
   nth
   nthcdr
   last
   lastcdr
   ass 
   ass?
   assq
   assq?
   rass
   rassq
   circular?
   del
   delq
   del!
   delq!
   sublist
   copy-list
   append
   append!
   reverse
   reverse! 
   pos
      
   ;; MAP
   map!
   map
   mapcdr
   walk
   walkcdr
   any?
   anycdr?
   every?
   everycdr?
   any
   anycdr
   every
   everycdr
   *and
   *or
   *if
   
   ;; THROW
   bind-handler
   unwind-protect-handler
   *catch
   call-with-current-continuation
   
   ;; OPERATION
   operation?
   %operation
   %massage-default
   join
   *object
   %predicate
   handle-operation
   %settable-operation
   procedure?
   argspectrum
   identification

   ;; STRUCT
   selector-id
   structure-type
   stype?
   structure?
   copy-structure
   copy-structure!
   make-stype
   stype-selector
   stype-compatible?

   ;; STRING
   chopy
   chopy!
   make-string
   string-replace
   copy-string
   string-equal?
   list->string
   string->list
   string-append
   string-slice
   substring
   walk-string
   map-string
   map-string!
   string-upcase
   string-downcase
   string-upcase!
   string-downcase!
   string-fill
   char->string
   string-posq

   ;; CHARACTER
   char=ic                              ; Ought to be documented.
   char<ic
   char>ic
   charn=ic
   char>=ic
   char<=ic
   lowercase?
   uppercase?
   alphabetic?
   char-upcase
   char-downcase
   graphic?
   digit
   digit?
   char->digit
   digit->char
   control?                           
   controlify
   uncontrolify

   ;; CONDITION

   syntax-error
   read-error

   ;; POPULATE
   make-population
   population?
   add-to-population
   remove-from-population
   population->list
   walk-population                    
                                      
   ;; Weak

    make-weak-set
    weak-set?
    weak-set-member?
    weak-set-empty?
    add-to-weak-set!
    remove-from-weak-set!
    weak-set->list
    walk-weak-set

   ;; POOL
   make-pool
   obtain-from-pool
   return-to-pool

   ;; TABLE
   make-hash-table
   hash-table?
   table-entry
   find-table-entry
   table-walk
   walk-table            

   make-table
   table?
   make-string-table
   string-table?
   make-symbol-table
   symbol-table?

   ;; HASH
   object-hash
   object-unhash

   ;; GC_TOP
   gc
   gc-noisily?
   
   ;; TREE
   subst
   substq
   substv
   copy-tree
   tree-hash

   ;; LOAD
   load-out-of-date-action
   load
   load-noisily
   load-quietly
   load-quietly-if-present
   load-noisily?
   print-load-message?
   *require

   ;; LOCATIVE
   contents
   set-contents
   locative?
   make-locative
   make-delay
   delay?
   force

   ;; SYNTAX
   make-syntax-table                    
   make-empty-syntax-table              
   syntax-descriptor?                   
   macro-expander?                      
   expand-macro-form                    
   env-syntax-table
   syntax-table-entry
   macro-expand
   make-macro-descriptor
   env-for-syntax-definition

   ;; ENV
   print-env-warnings?
   locale?                              ; Document
   make-locale                          ; Document
   make-empty-locale
   env-locale
   env-superior
   *lset
   *define                              ; release?
   *value                               ; release?
   *set-value
   t-implementation-env
   *define-syntax

   ;; READTABLE
   read-table?
   read-table-entry
   whitespace?
   make-read-table
   standard-read-table                ; Document.
   vanilla-read-table                 ; Document.

   ;; READ
   nothing-read
   read-object
   read-refusing-eof
   make-dispatch-read-macro
   dispatch-syntax
   delimiting-read-macro?
   make-list-reader                     ; New
   list-terminator                      ; New
             
   ;; PRINT
   print
   display
   print-type-string
   write-spaces

   ;; FORMAT
   format
   prompt

   ;; PP
   pretty-print
   *pp
   *pp-symbol

   ;; STROPS
   port?
   close
   maybe-open
   open
   line-length
   input-port?
   interactive-port?
   read-char
   readc
   maybe-read-char
   char-ready?
   unread-char
   unreadc
   peek-char
   peekc
   read
   port-read-table
   port-name
   read-line
   clear-input
   output-port?
   write-char
   writec
   write-string
   writes
   write-line
   write
   space
   newline
   new-line
   freshline
   fresh-line
   force-output
   hpos
   vpos
   print-width
   display-width

   ;; STREAMS
   null-port
   string->input-port
   with-open-ports-handler
   read-objects-from-string

   ;; STANDARD
   terminal-input
   terminal-output
   standard-input
   standard-output
   error-output
   debug-output

   ;; RATIO
   ratio?
   rational?
   numerator
   denominator

   ;; arithmetic
   integer?
   number?
   float?
   short-float?
   real?
   add
   subtract
   multiply
   divide
   quotient
   remainder
   %%add
   %%subtract
   %%multiply
   %%divide
   %%remainder
   negate
   quotient&remainder
   +
   -
   *
   /
   add1
   subtract1
   1+
   -1+
   %%less?
   %%equal?
   less?
   not-less?
   number-equal?
   equal?
   number-not-equal?
   not-equal?
   greater?
   not-greater?
   <
   <=
   =
   n=
   >
   >=
   negative?
   zero?
   positive?
   not-negative?
   not-zero?
   not-positive?
   <0?
   =0?
   >0?
   >=0?
   n=0?
   <=0?
   odd?
   even?
   logand                               ; Release ...
   logior
   logxor
   lognot
   ash
   %%logand                               ; Release ...
   %%logior
   %%logxor
   integer-length
   bit-field
   set-bit-field                        ; ...
   max
   min
   abs
   expt
   gcd
   mod
   ->integer
   ->float
   most-negative-fixnum
   most-positive-fixnum
   fixnum-add
   fixnum-subtract
   fixnum-logior
   fixnum-logand
   fixnum-logxor
   fixnum-multiply
   fixnum-divide
   fixnum-remainder
   fixnum-abs
   fixnum-min
   fixnum-max
   fixnum-negate
   fixnum-lognot
   fixnum-odd?
   fixnum-bit?
   fixnum-ashl
   fixnum-ashr
   fixnum-ash
   fixnum-length
   fixnum-expt
   fixnum-bit-field
   set-fixnum-bit-field
   fixnum-positive?
   fixnum-negative?
   fixnum-zero?
   fixnum-not-positive?
   fixnum-not-negative?
   fixnum-not-zero?
   fixnum-even?
   fixnum-equal?
   fixnum-less?
   fixnum-greater?
   fixnum-not-equal?
   fixnum-not-less?
   fixnum-not-greater?
   fx+
   fx-
   fx-and
   fx-ior
   fx-xor
   fx-not
   fx-abs
   fx-negate
   fx-odd?  
   fx-even? 
   fx-bit?  
   fx-ashl  
   fx-ashr  
   fx-ash   
   fx-length
   fx-expt  
   fx-zero? 
   fx*
   fx/ 
   fx=
   fx<
   fx>
   fxn=
   fx>=
   fx<=
   fx-rem
   flonum-add
   flonum-subtract
   flonum-multiply
   flonum-divide
   flonum-less?
   flonum-equal?
   flonum-greater?
   flonum-not-equal?
   flonum-not-less?
   flonum-not-greater?
   fixnum->flonum
   flonum->fixnum
   fl+
   fl-
   fl*
   fl/ 
   fl=
   fl<
   fl>
   fln=
   fl>=
   fl<=
   fl+!
   fl-!
   fl*!
   fl/!
   sin
   cos
   tan
   atan 
   exp
   log
   sqrt
   asin
   acos

   ;; EVAL
   eval
   standard-compiler
   run-compiled-code

   ;; REPL
   breakpoint
   ret
   reset
   repl-results                         ; New in 2.9
   repl-prompt
   repl-read
   repl-eval
   repl-print
   repl-output                          ; release?
   repl-input                           ; ...
   repl-env
   standard-prompt
   alternate-prompt                     ; document?
   backtrace
   crawl
   crawl-exhibit
   debug

   ;; DEBUG
   where-defined
   get-environment                      ; Document
   disclose

   ;; TRACE
   set-traced                           ; Internal
   set-untraced                         ; ""
   display-traced-objects
   untrace-traced-objects

   ;; STREAM_MISC
   make-output-width-port
   make-broadcast-port
   make-echo-port
  
   ;; SORT
   sort-list!
   sort-list

   ;; FS
   ->filename
   make-filename
   filename-with-type
   filename->string
   filename-fs
   filename-dir
   filename-name
   filename-type
   filename?
   local-fs
   fs-name
   aegis-fs?
   unix-fs?
   vms-fs?

   ;; Port
   file-exists?
   file-probe
   file-write-date
   file-delete
   file-truncate
   file-newer?
   file-length
   file-directory?
   working-directory
   naming-directory
   home-directory

   ;; Combinators
   conjoin
   disjoin
   complement
   compose

   ;; Sets
   union
   intersection
   intersection?
   set-difference
   add-to-set
   remove-from-set
   set-member?
   empty-set
   map-set
   walk-set

   ;; Random numbers
   make-random

   ;; miscellaneous  
   call-foreign
   load-foreign
   augment-context
   command-line
   t-system
   version
   exit
   transcript-on
   transcript-off
;; these should be de-released when syntax works better
   get-buffer
   buffer->string
   release-buffer

   ))
 nil)
