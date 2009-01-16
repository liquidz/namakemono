(define *cr* #/\\r/)
(define *lf* #/\\n/)

; =mymin
; ---------------------------
(define (mymin . args)
  (if (> (length args) 0)
    (fold
      (lambda (x res)
        (cond
          [(number? res)
            (if (number? x)
              (if (< x res) x res)
              res
              )
            ]
          [else x]
          )
        )
      (car args) (cdr args))
    #f)
  )

; =correct-escape-sequence
; ---------------------------
(define (correct-escape-sequence str)
  (let loop((ls (string->list str)) (flag #f) (res '()))
    (cond
      [(null? ls) (list->string (reverse res))]
      [else
        (let1 fc (car ls)
          (cond
            [(char=? fc #\\)
             (if flag
               (loop (cdr ls) #f (cons #\\ res))
               (loop (cdr ls) #t res)
               )
             ]
            [(char=? fc #\r)
             (loop (cdr ls) #f (cons (if flag #\return fc) res))
             ]
            [(char=? fc #\n)
             (loop (cdr ls) #f (cons (if flag #\newline fc) res))
             ]
            [else
              (loop (cdr ls) #f (cons fc res))
              ]
            )
          )
        ]
      )
    )
  )

; =del-head-words
; ------------------------
(define (del-head-words target-str . chars)
  (define (del-words-body str)
    (cond
      [(string=? str "") str]
      [else
        (let1 fc (string-ref str 0)
          (if (block _break
                 (fold (lambda (c res)
                         (if (char=? fc c) (_break #t) #f)
                         ) #f chars)
                 )
            (del-words-body (string-drop str 1))
            str
            )
          )
        ]
      )
    )

  (del-words-body target-str)
  )

; =del-head-space
; ----------------------
(define (del-head-space target-str)
  (del-head-words target-str #\space #\tab)
  )
; =del-head-crlf
; ----------------------
(define (del-head-crlf target-str)
  (del-head-words target-str #\return #\newline)
  )
; =pickup
; --------------
(define (pickup ls start end)
  (drop (take ls (++ end)) start)
  )
; =kiritori
; baseの先頭からdelを検索し、del以前の文字列・以後の文字列を多値で返す
; なおバックスラッシュでエスケープされているdelは無視する
; ----------------
(define (kiritori base del)
  (let ((len (string-length base))
        (del-len (string-length del))
        )
    (let1 res (block _break
                     (dotimes (n len)
                       (when (char=? (string-ref base n) (string-ref del 0))
                         (cond
                           [(> n 0)
                            (when (! char=? (string-ref base (-- n)) #\\)
                              (if (string=? (substring base n (+ n del-len)) del) (_break n))
                              )
                            ]
                           [else
                             (if (string=? (substring base n (+ n del-len)) del) (_break n))
                             ]
                           )
                         )
                       )
                     0
                     )
      (values (string-drop base (+ res del-len))
              (string-take base res)
              )
      )
    )
  )

; =this?
; -------------------
(define (this? code)
  (let1 n (string-scan code "this")
    (if n (= n 0) #f)
    )
  )

; =has-flexible-length-argument?
; --------------------------------
(define (has-flexible-length-argument? arg-ls)
  (char=? (string-ref (keyword->string (cadr (last arg-ls))) 0) #\*)
  )

; =get-correct-end
; ----------------------
(define (get-correct-end original-code original-start-str original-end-str)
  (let ((start-find-regexp (string->regexp #`".*?,|original-start-str|"))
        (end-find-regexp (string->regexp #`"(.*?),|original-end-str|"))
        (start-str (string-join (string-split original-start-str #\\) ""))
        (end-str (string-join (string-split original-end-str #\\) ""))
        )
    (let loop((code original-code) (level 0) (result ""))
      (let ((start (string-scan code start-str))
            (end (string-scan code end-str)))
        (cond
          [(and start end)
           (cond
             [(< (string-scan code start-str) (string-scan code end-str))
              (let1 m (start-find-regexp code)
                (loop (m 'after) (++ level) (string-append result (m)))
                )
              ]
             [else
               (let1 m (end-find-regexp code)
                 (cond
                   [(= level 0)
                    (string-append result (m 1))
                    ]
                   [else
                     (loop (m 'after) (-- level) (string-append result (m)))
                     ]
                   )
                 )
               ]
             )
           ]
          [(and (not start) end)
           (let1 m (end-find-regexp code)
             (cond
               [(= level 0)
                (string-append result (m 1))
                ]
               [else
                 (loop (m 'after) (-- level) (string-append result (m)))
                 ]
               )
             )
           ]
          [else
            (error "lambda parameter" "original-code = " original-ode)
            ]
;            'lambda-parameter-error]
          )

        )
      )
    )
  )

; =make-lambda
; ----------------------
(define (make-lambda original-param original-body)
  (let* ((param (scanner (if original-param original-param *implicit-variable*)))
         (body (scanner (trim original-body)))
         (flexible-arg (has-flexible-length-argument? param))
         )
    (debug "* lambda: param = " param ", body = " body)
    (list :lambda
          (lambda p
            (let ((uid (make-uid))
                  (last-uid *current-uid*))
              (change-current-uid uid)

              (make-local-namespace uid)
              (take-over-variables last-uid uid)

              (cond
                ; 可変長の引数の場合
                [flexible-arg
                  ; register temporary variable
                  (block
                    _break
                    (for-each
                      (lambda (n)
                        (let* ((var-name (value (list-ref param n)))
                               (s-var-name (keyword->string var-name))
                               )
                          (cond
                            [(char=? (string-ref s-var-name 0) #\*)
                             (set-variable (make-keyword (substring s-var-name 1 (string-length s-var-name)))
                                           (drop p n))
                             ; 残りのパラメータは全て１つの引数に登録されたので
                             ;  for-each を抜ける
                             (_break)
                             ]
                            [else
                              (set-variable var-name (list-ref p n))
                              ]
                            )
                          )
                        )
                      (iota (length p) 0))
                    )

                  (let1 result (run-tokens body)
                    ; delete temporary variables
                    (delete-local-namespace uid)
                    (change-current-uid last-uid)

                    result
                    )
                  ]
                ; 固定長の引数の場合
                [else
                  (cond
                    [(= (length p) (length param))
                     ; register temporary variable
                     (for-each
                       (lambda (n)
                         (set-variable (value (list-ref param n)) (list-ref p n))
                         )
                       (iota (length p) 0))

                     (let1 result (run-tokens body)
                       ; delete temporary variables
                       (delete-local-namespace uid)
                       (change-current-uid last-uid)

                       result
                       )
                     ]
                    [else
                      (change-current-uid last-uid)
                      (error "lambda parameter" "correct = " param ", get = " p)
                      ]
                    )
                  ]
                )
              )

            )
          )
    )
  )

; =scanner
; -------------------
(define (scanner original-code)
  (let loop((code original-code) (res '()))
    ;(debug "* scanning code\n=====start=====\n" code "\n===== end =====")
    (if (string=? code "") (r res)
      (let1 first-char (string-ref code 0)
        (cond
          [(char=? first-char #\#)
           (let1 second-char (string-ref code 1)
             (cond
               ; multiple lines comment
               [(char=? second-char #\|)
                (loop (del-head-space (kiritori code "|#")) res)
                ]
               ; regexp
               [(char=? second-char #\/)
                (receive (after before) (kiritori (string-drop code 2) "/")
                  (loop (del-head-space after)
                        (cons (list :regexp (string->regexp before)) res))
                  )
                ]
               )
             )
           ]
          ; end
          [(or (char=? first-char #\newline) (char=? first-char #\return))
           (loop (del-head-space (del-head-crlf code)) (cons (list :end '()) res))
           ]
          ; one line comment
          [(char=? first-char #\;)
           (let1 next-newline (string-scan code #\newline)
             (if next-newline
               (loop (del-head-space (string-drop code (++ next-newline))) res)
               (loop "" res) ; 末尾の場合
               )
             )
           ]
          ; string
          [(char=? first-char #\")
           (receive (after before) (kiritori (string-drop code 1) "\"")
             (loop (del-head-space after) (cons (list :string (correct-escape-sequence before)) res))
             )
           ]
          ; string
          [(char=? first-char #\')
           (receive (after before) (kiritori (string-drop code 1) "\'")
             (loop (del-head-space after) (cons (list :string (correct-escape-sequence before)) res))
             )
           ]
          ; keyword
          [(char=? first-char #\:)
           (let1 m (#/\:([A-Za-z_\+\-\*\/\%\=\<\>\!\?][A-Za-z0-9_\+\-\*\/\%\=\<\>\!\?]*)/ code)
             (loop (del-head-space (m 'after)) (cons (list :keyword (make-keyword (m 1))) res))
             )
           ]
          ; lambda
          [(char=? first-char #\[)
           (let* ((m (#/^\[[\ \t]*(\((.+?)\))?/ code))
                  (param (m 2))
                  (body (get-correct-end (m 'after) "\\[" "\\]"))
                  )
             ; "+ 1" is length for "]"
             (loop (del-head-space (string-drop (m 'after) (+ (string-length body) 1)))
                   (cons (make-lambda param body) res))
             )
           ]
          ; simple lambda
          [(char=? first-char #\,)
           (let* ((code2 (string-drop code 1))
                  (next-comma (string-scan code2 #\,))
                  (next-period (string-scan code2 #\.))
                  (next-newline (string-scan code2 #\newline))
                  )
             (receive (n clc?) (let1 t (mymin next-comma next-period next-newline)
                                 (cond
                                   [t (values t (if (and (number? next-comma) (= t next-comma)) #t #f))]
                                   [else (error "simple lambda")]
                                   )
                                 )
               (let* ((before (string-take code2 n))
                      ; "+ 1" is length for ","
                      (after (string-drop code2 (if clc? (+ n 1) n)))
                      (m (#/^[\ \t]*(\((.+?)\))?/ before))
                      )
                 (loop (del-head-space after) (cons (make-lambda (m 2) (m 'after)) res))
                 )
               )
             )
           ]
          ; pipe
          [(char=? first-char #\.)
           (let1 m (#/^\.(([0-9]+)\.)?[\ \t]*/ code)
             (if (m 2)
               (loop (m 'after)
                     (cons (list :pipe-pos (string->number (m 2))) (cons (list :pipe '()) res)))
               (loop (m 'after) (cons (list :pipe '()) res))
               )
             )
           ]
          ; this syntax
          [(this? code)
           ; 4 is length of 'this'
           (loop (del-head-space (string-drop code 4)) (cons (list :this '()) res))
           ]
          ; number
          ;[(#/^[\ \t]*(\-?[0-9]+([\.\/]([0-9]+))?)[\ \t]*/ code)
          [(#/^(\-?[0-9]+([\.\/]([0-9]+))?)[\ \t]*/ code)
           => (lambda (m)
                (loop (m 'after) (cons (list :number (string->number (m 1))) res))
                )
           ]
          ; word
          ;[(#/^[\ \t]*([A-Za-z_\+\-\*\/\%\=\<\>\!\?][A-Za-z0-9_\+\-\*\/\%\=\<\>\!\?]*)[\ \t]*/ code)
          [(#/^([A-Za-z_\+\-\*\/\%\=\<\>\!\?][A-Za-z0-9_\+\-\*\/\%\=\<\>\!\?]*)[\ \t]*/ code)
           => (lambda (m)
                (loop (m 'after) (cons (list :word (make-keyword (m 1))) res))
                )
           ]
          [else
            (error "scanning" "code = " code)
            ]
          )
        )
      )
    )
  )
