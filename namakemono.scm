(use srfi-1)
(use srfi-13)
(use file.util)
(use simply)

(require "./src/words")
(require "./src/common")
(require "./src/scanner")
(require "./src/parser")

;(define *debug* #t)
(define *debug* #f)
(define (debug . str) (if *debug* (apply print str)))

; =init
; -----------------------
(define (init)
  (for-each
    (lambda (x)
      (set-variable (car x) (cadr x))
      )
    *words*)
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

; =load-source
; ----------------
(define (load-source filename)
  (let* ((code (del-tab (trim (file->string filename))))
         (all-tokens (scanner code))
         )
    (run-tokens all-tokens)
    )
  )

; =main
; ---------------
(define (main args)
  (when (> (length args) 1)
    ; initialize
    (init)
    ; load librarys
    (if (not *debug*) (for-each load-source (collect-library *library-path*)))
    ; load and run source
    (load-source (cadr args))
    )
  )
