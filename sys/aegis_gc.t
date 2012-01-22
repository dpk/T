(herald aegis_gc (env tsys))

;++ flush this later
(comment
 (define-foreign boot-name_$resolve
    (name_$resolve (in     rep/value        xpname)
                   (in     rep/integer-16-u xpnamlen)
                   (ignore rep/extend       rtn_uid)
                   (out    rep/integer      status))
    ignore)

;++ fix this later -flush boot_name_resolve
 (define (get-uid name len)
  (let ((uid (make-bytev 8)))
    (let ((status (boot-name_$resolve name len uid nil)))
      (if (fxn= status 0)
          (error "boot-name_$resolve failed with status #x~x" status)
          uid))))
)

;++ Temporary definition - flush later
(define unmap-areas true)

(define-foreign ms_$truncate
  ("MS_$TRUNCATE" (in  rep/integer address)
                (in  rep/integer length)
                (out rep/integer status))
      ignore)

(define (zero-out-area area)
  (check-status (ms_$truncate (fx-ashl (area-begin area) 2) 0 nil))
  (no-value))
                                          
(define (initialize-areas)
  (let* ((boot-args (system-global  slink/boot-args))
         (heap-size (vref boot-args 13)))  ; boot/heap-size
    (set *old-space* 
         (create-area 'area2 
                      (vref boot-args 12) ; boot/heap2
                      heap-size
                      nil))             ;++ fix this
    (set *new-space*                            ; current area
         (create-area 'area1 
                      (vref boot-args 9)  ; boot/heap1
                      heap-size
                      nil))             ;++ fix this
    (set (area-base *new-space*)
         (system-global slink/boot-area-base))
    (set (process-global task/area) *new-space*)))

(define (gc-message count)
  (vfmt-number "; %D objects copied%." count 0))

(define (gc-error-message string address)
  (vfmt "GC error: %A%." (string-text string) (string-length string))
  (vfmt-number "Pointer location #x%H%." (fixnum-ashl address 2) 0)
  (vfmt-number "On into the unknown...%." 0 0))

(define-foreign vfmt-number ("VFMT_$WRITE2" (in rep/string)
                                          (in rep/integer)
                                          (in rep/integer))
                            ignore)


(define-enumerated ms_$advice_opt_t
                    ms_$normal
                    ms_$random
                    ms_$sequential)

(define-foreign ms_$advice
  ("MS_$ADVICE" (in rep/integer address)
              (in rep/integer length)
              (in rep/integer access)
              (in rep/integer options)
              (in rep/integer record-length)
              (out rep/integer status))
      ignore)

(define (advise-area-access area type)
  (let ((type (if (eq? type 'gc) ms_$sequential ms_$random)))
    (check-status (ms_$advice (fx-ashl (area-begin area) 2)
                              (area-size area)
                              type
                              0
                              0
                              nil))))

(define (advise-impure-area-access type)
  (let ((begin (fx-ashl (system-global slink/initial-impure-memory-begin) 2))
        (end   (fx-ashl (system-global slink/initial-impure-memory-end) 2))
        (type  (if (eq? type 'gc) ms_$sequential ms_$random)))
    (check-status (ms_$advice begin
                              (fx- end begin)
                              type
                              0
                              0
                              nil))))
