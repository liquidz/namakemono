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
; =delete-head-space-crlf
; -----------------------
(define (delete-head-space-crlf target-str)
  (delete-head-words target-str #\space #\tab #\return #\newline)
  )
; =pickup
; ----------------------
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

; =scanner
; -------------------------
(define (scanner original-code)
  (let loop((code (delete-comments original-code)) (res '()))
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
           (loop (delete-head-space-crlf code) (cons (list :end '()) res))
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
          ; lambda-start
          [(char=? first-char #\[)
           (loop (delete-head-space (my-string-drop code 1)) (cons (list :lambda-start '()) res))
           ]
          ; lambda-end
          [(char=? first-char #\])
           (loop (delete-head-space (my-string-drop code 1)) (cons (list :lambda-end '()) res))
           ]
          ; parameter-start
          [(char=? first-char #\()
           (loop (delete-head-space (my-string-drop code 1)) (cons (list :parameter-start '()) res))
           ]
          ; parameter-end
          [(char=? first-char #\))
           (loop (delete-head-space (my-string-drop code 1)) (cons (list :parameter-end '()) res))
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
            ]
          )
        )
      )
    )
  )
