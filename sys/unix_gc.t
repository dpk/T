(herald unix_gc (env tsys))

;;; Initialize the available areas and set up the relevant
;;; system and process globals.

(define (initialize-areas)
  (let* ((boot-args (system-global  slink/boot-args))
         (heap-size (vref boot-args 3)))
    (set *old-space* 
         (create-area 'area1
                      (vref boot-args 2)
                      heap-size
                      nil))
    (set *new-space*                            ; current area
         (create-area 'area0
                      (vref boot-args 1)
                      heap-size
                      nil))
    (set (area-base *new-space*)
         (system-global slink/boot-area-base))
    (set (process-global task/area) *new-space*)))

(define (zero-out-area area) (return))

;;; In the following strings that are passed to printf we make sure that there
;;; are not a multiple of 4 bytes so the strings are actually asciz!!!  Yuk

(define gc-message
  (let ((string (copy-string "; %d objects copied  ")))
    (set (string-elt string 20) #\newline)
    (lambda (count)
      (printf-number string count))))

(define (gc-error-message string address)
  (unix-write-string 1 "GC error: " 10)
  (unix-write-string 1 string (string-length string))
  (printf-number " %x\n" address))

(define-foreign printf-number (printf (in rep/string)
                                      (in rep/integer))
       rep/integer)
