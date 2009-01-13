(use srfi-1)
(use srfi-13)
(use file.util)
(use simply)

(require "./src/common")
(require "./src/words")
(require "./src/scanner")
(require "./src/parser")

;(define *debug* #t)
(define *debug* #f)
(define (debug . str) (if *debug* (apply print str)))

; =init
; -----------------------
(define (namakemono-initialize)
  ; default words
  (for-each
    (lambda (x)
      (set-variable (car x) (cadr x))
      )
    *words*)

  ; library
  (if (not *debug*) (for-each load-source (collect-library *library-path*)))
  )

; =del-tab
; --------------
(define (del-tab str)
  (regexp-replace-all* str #/[\t]/ "")
  )

; =run-tokens
; ------------------
(define (run-tokens all-tokens)
  (debug "* all tokens\n  " all-tokens)
  (fold
    (lambda (each-line-token last-result)
      (let1 each-pipe-token (list-split each-line-token (lambda (x) (eq? (car x) :pipe)))
        (cond
          [(! null? each-pipe-token)
           (debug "* each pipe token\n  " each-pipe-token)
           (let1 res (parser each-pipe-token)
             (debug "")
             res
             )
           ]
          [else last-result]
          )
        )
      )
    '()
    (list-split all-tokens (lambda (x) (eq? (car x) :end)))
    )
  )

; =collect-library
; --------------------
(define (collect-library path)
  (fold
    (lambda (file res)
      (cond
        [(file-is-directory? file)
         (append res (collect-library file))
         ]
        [else
          (if (#/.+\.nmk$/ file)
            (append res (list file))
            res
            )
          ]
        )
      )
    '()
    (directory-list path :children? #t :add-path? #t))
  )

; =run-source
; --------------------
(define (run-source source-code)
  (let* ((code (del-tab (trim source-code)))
         (all-tokens (time (scanner code)))
         )
    (time
    (run-tokens all-tokens)
    )
    )
  )

; =load-source
; ----------------
(define (load-source filename)
  (run-source (file->string filename))
  )

; =command-line-environment
(define (command-line-environment)
  (define (get-user-input)
    (display "nmk> ")
    (flush)
    (read-line)
    )

  (let loop((input (get-user-input)))
    (cond
      [(string=? input "exit")
       (print "bye")
       ]
      [(! string=? input "")
       (let1 res (run-source input)
          (print res)
          (loop (get-user-input))
          )
       ]
      [else
        (loop (get-user-input))
        ]
      )
    )
  )

; =main
; ---------------
(define (main args)
  (case (length args)
    [(1)
     (print "namakemono " *nmk-version*)
     (display "* initializing..") (flush)
     ; initialize
     (namakemono-initialize)
     (print "ok")
     ; launch command line environment
     (command-line-environment)
     ]
    [(2)
     ; initialize
     (namakemono-initialize)
     ; load and run source
     (load-source (cadr args))
     ]
    [else
      (error "execute parameter")
      ]
    )
  )
