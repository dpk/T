(herald oload (env tsys))

(let ((oenv (make-locale standard-env 'orbit-env)))
  (*define standard-env 'orbit-env oenv)

  (*define t-implementation-env 'orbit-env oenv)

  (*define oenv 'load-orbit
    (lambda system
      (let ((os (if (null? system) 
                    ((*value t-implementation-env 'os-type)
                     ((*value t-implementation-env 'local-os)))
                    (car system)))
            (processor (if (null? system) 
                           ((*value t-implementation-env 'processor-type)
                            ((*value t-implementation-env 'local-processor)))
                           (cadr system))))
        (load '(build orbit_files) oenv)
        (walk (lambda (f) (load f oenv)) (*value oenv '*orbit-files*))
        (walk (lambda (f) (load f oenv)) (*value oenv '*top-files*))
        (walk (lambda (f) (load f oenv)) (*value oenv '*front-files*))
        (walk (lambda (f) (load f oenv)) (*value oenv '*back-end-files*))
        (xcase processor
          ((mc68000)
           (walk (lambda (f) (load f oenv)) (*value oenv '*orbit-m68-files*)))
          ((vax11)
           (walk (lambda (f) (load f oenv)) (*value oenv '*orbit-vax-files*))))
        (walk (lambda (f) (load f oenv)) (*value oenv '*tas-files*))
        (xcase processor
          ((mc68000)
           (walk (lambda (f) (load f oenv)) (*value oenv '*tas-m68-files*)))
          ((vax11)
           (walk (lambda (f) (load f oenv)) (*value oenv '*tas-vax-files*))))
        (load (xcase os
                ((aegis) '(back_end aem68gen))
                ((unix)
                 (xcase processor
                   ((mc68000) '(back_end unm68gen))
                   ((vax11) '(back_end unvaxgen)))))
              orbit-env)
        (xcase processor
            ((mc68000)
             (load-quietly            '(t3_primops mconstants) oenv)
             ((*value oenv 'orbit-m68-init) 't3_primops))
            ((vax11)
             (load-quietly            '(t3_primops vconstants) oenv)
             ((*value oenv 'orbit-vax-init) 't3_primops)))
        (*define t-implementation-env 'comfile (*value oenv 'comfile))
	(*define standard-env 'compile (*value oenv 'orbit))
        (walk (lambda (sym)
                (*define standard-env sym (*value oenv sym)))
              '(cl 
                listing
                orbit 
                comfile 
                compile-file 
                comfile2 
                tc-syntax-table
                make-empty-early-binding-locale
                )))))

    oenv)
