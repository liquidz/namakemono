(use srfi-1)
(use srfi-13)
(use file.util)
(use gauche.parseopt)
(use simply)

(require "./src/common")
(require "./src/words")
(require "./src/scanner")
(require "./src/parser")

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
  (for-each load-source (collect-library *library-path*))
  )

; =del-tab
; --------------
(define (del-tab str)
  (regexp-replace-all* str #/[\t]/ "")
  )

; =apply-macro
; ------------------
#|
(define (apply-macro tokens)
  (let* ((token-len (length tokens))
         (mns (*macro-namespace* (value (car tokens))))
         )
    (let1 target-macro (if (mns '() 'exists? token-len)
                         (mns token-len)
                         (if (mns '() 'exists? -1) (mns -1) #f)
                         )
      (cond
        [target-macro
          (list-receive
            (param body) target-macro

            (let1 cls (let loop((n 0) (res '()))
                        (cond
                          [(= n token-len)
                           (r res)
                           ]
                          [else
                            (if (char=? #\* (string-ref (keyword->string (value (list-ref param n))) 0))
                              (r cons (drop tokens n) res)
                              (loop (++ n) (cons (list-ref tokens n) res))
                              )
                            ]
                          )
                        )
              (debug 8 "cls = " cls)
              (let1 final-res (r fold
                                 (lambda (x res)
                                   (let1 index (list-index (lambda (t) (eq? (value x) (value t))) param)
                                     (if index
                                       ; 対応するパラメータがある場合
                                       (if (char=? #\* (string-ref (keyword->string (value x)) 0))
                                         ; 可変長のパラメータの場合、ちゃんと実行できるようにリストから取り出す
                                         (fold (lambda (y z) (cons y z)) res (list-ref cls index))
                                         ; 固定長の場合はそのまま結果にポンっ
                                         (cons (list-ref cls index) res)
                                         )
                                       ; 対応するパラメータがない場合はそのまま
                                       (cons x res)
                                       )
                                     )
                                   )
                                 '()
                                 body)
                (debug 8 "final-res = " final-res)
                final-res
                )
              )
            )
          ]
        [else
          tokens
          ;each-pipe-token
          ]
        )
      )
    )
  )
|#

; =run-tokens
; ------------------
(define (run-tokens all-tokens)
  (debug 4 "* all tokens\n  " all-tokens)
  (fold
    (lambda (each-line-token last-result)
      (let1 each-pipe-token (list-split each-line-token (lambda (x) (eq? (car x) :pipe)))
        (cond
          [(! null? each-pipe-token)
           (debug 4 "* each pipe token\n  " each-pipe-token)
           (let1 res (parser each-pipe-token)
             (debug 4 "")
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

; =command-line-environment
(define (command-line-environment)
  (define (get-user-input)
    (display "nmk> ")
    (flush)
    (read-line)
    )

  (let loop((input (get-user-input)))
    (cond
      [(or (! string? input) (string=? input "exit"))
       (print "bye")
       ]
      [(! string=? input "")
       (let1 res (guard (e (else e))
                   (run-source input)
                   )
         (print (if (eq? (class-of res) <error>) (slot-ref res 'message) res))
         (loop (get-user-input))
         )
       ]
      [else
        (loop (get-user-input))
        ]
      )
    )
  )

; =version-print
; -----------------
(define (version-print)
  (print "namakemono " *nmk-version*)
  )

; =main
; ---------------
(define (main args)
  (let-args (cdr args)
    ((debug "d|debug=i" 0)
     (version "v|version")
     . rest-args
     )

    ; initialize
    (namakemono-initialize)

    (set! *debug* debug)

    (let1 stdin (standard-input-port)
      (cond
        [(char-ready? stdin)
         ; 標準入力でソースが読まれた場合
         (run-source (port->string stdin))
         ]
        [else
          ; ファイルの実行など
          (case (length rest-args)
            ; command line mode
            [(0)
             (when version
               (version-print)
               (exit)
               )
             (command-line-environment)
             ]

            ; file execute mode
            [(1)
             ; load and run source
             (load-source (car rest-args))
             ]
            [else
              (error "too many parameter error: " rest-args)
              ]
            )
          ]
        )
      )
    )
  )

