(herald fs_parse (env tsys))

;;; Parser for system dependent strings

(define (string->filename path path-delimiter type-delimiter)
  (let* ((len  (string-length path))
         (i    (substring-reverse-find-char path path-delimiter 0 len))
         (dir  (if i (substring path 0 i) nil))
         (j    (substring-reverse-find-char path
                                            type-delimiter
                                            (if i i 0)
                                            len))
         (type (if j (substring path (fx+ j 1) (fx- len (fx+ j 1))) nil))
         (i    (if i (fx+ i 1) 0))
         (cnt  (fx- (if j j len) i))
         (name (substring path i cnt)))
      (make-filename nil dir name type)))


(define (substring-find-char string ch start end)
  (let* ((string (enforce string? string))
         (ch (enforce char? ch)))
      (iterate loop ((i start))
        (cond ((fx>= i end) nil)
              ((char= (nthchar string i) ch) i)
              (else (loop (fx+ i 1)))))))

(define (substring-reverse-find-char string ch start end)
  (let ((string (enforce string? string))
        (ch (enforce char? ch)))
    (iterate loop ((i end))
      (let ((i (fx- i 1)))
        (cond ((fx< i start) nil)
              ((char= (nthchar string i) ch) i)
              (else (loop i)))))))
