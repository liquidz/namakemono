(define *cr* #/\\r/)
(define *lf* #/\\n/)

; =my-string-drop
; ほんのちょっとだけstring-dropより速い
; ---------------------------------
(define (my-string-drop str n)
  (substring str n (string-length str))
  )

; =string-partial-delete
; -----------------------------
(define (string-partial-delete str start end)
  (string-append (string-take str start) (my-string-drop str (+ end 1)))
  )

; =delete-comments
; -------------------------
(define (delete-comments str)
  (cond
    [(string-scan str #\;)
     => (lambda (start)
          (let* ((len (string-length str))
                 (end (string-scan (my-string-drop str (+ start 1)) #\newline))
                 )
            (if end
              (delete-comments (string-partial-delete str start (+ start end 1)))
              (delete-comments (string-take str start))
              )
            )
          )
     ]
    [(string-scan str "#|")
     => (lambda (start)
          (let* ((len (string-length str))
                 (end (string-scan (my-string-drop str (+ start 1)) "|#"))
                 )
            (if end
              (delete-comments (string-partial-delete str start (+ start end 2)))
              (error "comment is not closed near:" str)
              )
            )
          )
     ]
    [else str ]
    )
  )

; =mymin
;  数字以外があっても最小値を算出
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
;   \n などの文字列を適切なエスケープシーケンスに変換する
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

; =delete-head-words
; ------------------------
(define (delete-head-words target-str . chars)
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
            (del-words-body (my-string-drop str 1))
            str
            )
          )
        ]
      )
    )

  (del-words-body target-str)
  )

; =delete-head-space
; ----------------------
(define (delete-head-space target-str)
  (delete-head-words target-str #\space #\tab)
  )
; =delete-head-crlf
; ----------------------
(define (delete-head-crlf target-str)
  (delete-head-words target-str #\return #\newline)
  )
(define (delete-head-space-crlf target-str)
  (delete-head-words target-str #\space #\tab #\return #\newline)
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
      (values (my-string-drop base (+ res del-len))
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

; =defmac?
; -------------------------
#|
(define (defmac? code)
  (let1 n (string-scan code "defmac")
    (if n (= n 0) #f)
    )
  )
|#

; =has-flexible-length-argument?
; --------------------------------
(define (has-flexible-length-argument? arg-ls)
  (char=? (string-ref (keyword->string (cadr (last arg-ls))) 0) #\*)
  )

; =get-correct-end
;  対応する文字列（括弧など)を検索し、そこまでの文字列を返す
; ---------------------------------------------------------
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
            ;(let1 tmp-code (regexp-replace-all* original-code #/[\r\n]/ "")
            (let1 len (string-length original-code)
              (error "lambda scanning error near: " (substring original-code 0 (if (> len 20) 20 len)))
              )
            ]
          )

        )
      )
    )
  )

; =count-make-lambda-params
; -------------------------
(define (count-make-lambda-params param)
  (if (char=? #\* (string-ref (keyword->string (value (last param))) 0))
    -1
    (length param)
    )
  )

; =make-lambda
; ----------------------
(define (make-lambda original-param original-body)
  (let* ((param (scanner (if original-param original-param *implicit-variable*)))
         (body (scanner (trim original-body)))
         (flexible-arg (has-flexible-length-argument? param))
         )
    (debug 2 "* lambda: param = " param ", body = " body)
    (list :lambda
          (lambda p
            (cond
              [(and (! null? p) (eq? (car p) *get-lambda-params-length*))
               ; この場合だけは特別にパラメータ数を返す
               ; （オーバーロード用）
               (count-make-lambda-params param)
               ;(length param)
               ]
              [else
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
                        (dotimes (n (length p))
                          (let* ((var-name (value (list-ref param n)))
                                 (s-var-name (keyword->string var-name)))
                            (cond
                              [(char=? (string-ref s-var-name 0) #\*)
                               (set-variable
                                 (make-keyword
                                   ; 先頭の*は抜かす
                                   (my-string-drop s-var-name 1))
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
                        )

                      ; execute
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
                          (error "lambda parameter error:"  'required (length p)  'got (length param))
                          ]
                        )
                      ]
                    )
                  )
                ])

            )
          )
    )
  )

; =scanner
; -------------------
(define (scanner original-code)
  ;(let loop((code original-code) (res '()))
  (let loop((code (delete-comments original-code)) (res '()))
    ;(print "\n******************\n" code)
    (if (string=? code "") (r res)
      (let1 first-char (string-ref code 0)
        (cond
          ; regexp
          [(and (char=? first-char #\#) (char=? (string-ref code 1) #\/))
           (receive (after before) (kiritori (my-string-drop code 2) "/")
             (loop (delete-head-space after)
                   (cons (list :regexp (string->regexp before)) res))
             )
           ]
          ; end
          [(or (char=? first-char #\newline) (char=? first-char #\return))
           (loop (delete-head-space (delete-head-crlf code)) (cons (list :end '()) res))
           ]
          ; string
          [(char=? first-char #\")
           (receive (after before) (kiritori (my-string-drop code 1) "\"")
             (loop (delete-head-space after) (cons (list :string (correct-escape-sequence before)) res))
             )
           ]
          ; string
          [(char=? first-char #\')
           (receive (after before) (kiritori (my-string-drop code 1) "\'")
             (loop (delete-head-space after) (cons (list :string (correct-escape-sequence before)) res))
             )
           ]
          ; keyword
          [(char=? first-char #\:)
           (let1 m (#/\:([A-Za-z_\+\-\*\/\%\=\<\>\!\?][A-Za-z0-9_\+\-\*\/\%\=\<\>\!\?]*)/ code)
             (loop (delete-head-space (m 'after)) (cons (list :keyword (make-keyword (m 1))) res))
             )
           ]
          ; lambda
          [(char=? first-char #\[)
           (let* ((m (#/^\[[\ \t]*(\((.+?)\))?/ code))
                  (param (m 2))
                  (body (get-correct-end (m 'after) "\\[" "\\]"))
                  )
             ; "+ 1" is length for "]"
             (loop (delete-head-space (my-string-drop (m 'after) (++ (string-length body))))
                   (cons (make-lambda param body) res))
             )
           ]
          ; simple lambda
          [(char=? first-char #\,)
           (let* ((code2 (my-string-drop code 1))
                  (next-comma (string-scan code2 #\,))
                  (next-period (string-scan code2 #\.))
                  (next-newline (string-scan code2 #\newline))
                  )
             (receive (n clc?) (let1 t (mymin next-comma next-period next-newline)
                                 (cond
                                   [t (values t (if (and (number? next-comma) (= t next-comma)) #t #f))]
                                   [else
                                     (error "not found simple lambda end: near" code)
                                     ]
                                   )
                                 )
               (let* ((before (string-take code2 n))
                      ; "+ 1" is length for ","
                      (after (my-string-drop code2 (if clc? (+ n 1) n)))
                      (m (#/^[\ \t]*(\((.+?)\))?/ before))
                      )
                 (loop (delete-head-space after) (cons (make-lambda (m 2) (m 'after)) res))
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
           (loop (delete-head-space (my-string-drop code 4)) (cons (list :this '()) res))
           ]
          ; def-mac syntax
          #|
          [(defmac? code)
           (let* ((m (#/^defmac[\ \t]*[\"\'](.+?)[\"\'][\ \t]*\[/ code))
                  (title (m 1))
                  (original-body (get-correct-end (m 'after) "\\[" "\\]"))
                  )
             (let dm-loop((tmp (delete-head-crlf original-body)) (dm-res '()))
               (cond
                 [(string=? tmp "")
                  ; macroに登録
                  (let1 title-key (make-keyword title)
                    (*macro-namespace* title-key (make-hash-table-wrap))
                    (let1 mns (*macro-namespace* title-key)
                      (for-each
                        (lambda (x)
                          (mns (count-make-lambda-params (list-ref x 0))
                               (list (list-ref x 0) (list-ref x 1)))
                          )
                        dm-res)
                      )
                    (loop (delete-head-space (my-string-drop (m 'after) (+ (string-length original-body) 1))) res)
                    )
                  ]
                 [else
                   (let* ((macro-body (get-correct-end (my-string-drop tmp 1) "\\[" "\\]"))
                          (m (#/[\ \t]*\((.+?)\)/ macro-body))
                          (param (m 1))
                          (body (trim (m 'after)));(delete-head-space-crlf (m 'after)))
                          )
                     ; "+ 2" は先頭の"["と末尾の"]"の分
                     (dm-loop (delete-head-space-crlf (my-string-drop tmp (+ (string-length macro-body) 2)))
                              (cons (list (scanner param) (scanner body)) dm-res)
                              )
                     )
                   ]
                 )
               )
             )
           ]
          |#
          ; number
          [(#/^(\-?[0-9]+([\.\/]([0-9]+))?)[\ \t]*/ code)
           => (lambda (m)
                (loop (m 'after) (cons (list :number (string->number (m 1))) res))
                )
           ]
          ; word
          [(#/^([A-Za-z_\+\-\*\/\%\=\<\>\!\?][A-Za-z0-9_\+\-\*\/\%\=\<\>\!\?]*)[\ \t]*/ code)
           => (lambda (m)
                (loop (m 'after) (cons (list :word (make-keyword (m 1))) res))
                )
           ]
          [else
            (error "scanning error: " code)
            ;(error "scanning" "code = " code)
            ]
          )
        )
      )
    )
  )
