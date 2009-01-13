(define *cr* #/\\r/)
(define *lf* #/\\n/)

(define (correct-escape-sequence str)
  (regexp-replace-all* str *cr* "\r" *lf* "\n")
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
              (set! *current-uid* uid)

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
                    (set! *current-uid* last-uid)

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
                       (set! *current-uid* last-uid)

                       result
                       )
                     ]
                    [else
                      (set! *current-uid* last-uid)
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
    (cond
      [(string=? code "")
       (r res)
       ]
      [(#/^\#\|.*?\|\#/ code)
       => (lambda (m)
            ; comment
            (loop (m 'after) res)
            )
       ]
      [(#/^[\ \t]*\;[^\r\n]*/ code)
       => (lambda (m)
            ; comment
            (loop (m 'after) res)
            )
       ]
      [(#/^\s*\\\s*/ code)
       => (lambda (m)
            (loop (m 'after) res)
            )
       ]
      [(#/^[\ \t]*\"(.*?)\"/ code)
       => (lambda (m)
            (loop (m 'after) (cons (list :string (correct-escape-sequence (m 1))) res))
            )
       ]
      [(#/^[\ \t]*\'(.*?)\'/ code)
       => (lambda (m)
            (loop (m 'after) (cons (list :string (correct-escape-sequence (m 1))) res))
            )
       ]
      [(#/^[\ \t]*(\-?[0-9]+([\.\/]([0-9]+))?)/ code)
       => (lambda (m)
            (loop (m 'after) (cons (list :number (string->number (m 1))) res))
            )
       ]
      [(#/^[\ \t]*(true|false)/ code)
       => (lambda (m)
            (loop (m 'after) (cons (list :true (if (string=? (m 1) "true") #t #f)) res))
            )
       ]
      [(#/^[\ \t]*this/ code)
       => (lambda (m)
            (loop (m 'after) (cons (list :this '()) res))
            )
       ]
      [(#/^[\ \t]*#\/(.+?)\// code)
       => (lambda (m)
            (loop (m 'after) (cons (list :regexp (string->regexp (m 1))) res))
            )
       ]
      [(#/^[\ \t]*fn[\ \t]*(\((.+?)\))?/ code)
       => (lambda (m)
            (let ((param (m 2))
                  (body (get-correct-end (m 'after) "fn" "end")))
              ; "+ 3" is length for "end"
              (loop (string-drop (m 'after) (+ (string-length body) 3)) (cons (make-lambda param body) res))
              )
            )
       ]
      [(#/^[\ \t]*\[[\ \t]*(\((.+?)\))?/ code)
       => (lambda (m)
            (let ((param (m 2))
                  (body (get-correct-end (m 'after) "\\[" "\\]")))
              ; "+ 1" is length for "]"
              (loop (string-drop (m 'after) (+ (string-length body) 1)) (cons (make-lambda param body) res))
              )
            )
       ]
      [(#/^[\ \t]*\{(.+?)\}/ code)
       => (lambda (m)
            (loop (m 'after) (cons (list :group (scanner (m 1))) res))
            )
       ]
      [(#/^[\ \t]*\.(([0-9]+)\.)?/ code)
       => (lambda (m)
            (if (m 2)
              (loop (m 'after) (cons (list :pipe-pos (string->number (m 2))) (cons (list :pipe '()) res)))
              (loop (m 'after) (cons (list :pipe '()) res))
              )
;            (loop (m 'after) (cons (list :pipe '()) res))
            )
       ]
      [(#/^[\ \t]*[\r\n]+/ code)
       => (lambda (m)
            (loop (m 'after) (cons (list :end '()) res))
            )
       ]
      [(#/^[\ \t]*([A-Za-z_\+\-\*\/\%\=\<\>\!\?][A-Za-z0-9_\+\-\*\/\%\=\<\>\!\?]*)/ code)
       => (lambda (m)
            (loop (m 'after) (cons (list :word (make-keyword (m 1))) res))
            )
       ]
      [else
        (error "scanning" "code = " code)
;        (debug "scanning error = " code)
        ]
      )
    )
  )
