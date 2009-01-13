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
         (all-tokens (scanner code))
         )
    (run-tokens all-tokens)
    )
  )

; =load-source
; ----------------
(define (load-source filename)
  (run-source (file->string filename))
  )


