(define *nil-returned-value* 'nothing)

; =my-string-drop
; ほんのちょっとだけstring-dropより速い
; ---------------------------------
(define (my-string-drop str n)
  (substring str n (string-length str))
  )

; =pickup
; ----------------------
(define (pickup ls start end)
  (drop (take ls (++ end)) start)
  )

; =append-with-null
; ----------------------------
(define (append-with-null ls1 ls2)
  (cond
    [(and (null? ls1) (null? ls2))
     (list '() '())
     ]
    [(and (null? ls1) (! null? ls2))
     (cons '() ls2)
     ]
    [(and (! null? ls1) (null? ls2))
     (reverse (cons '() (reverse ls1)))
     ]
    [else
      (append ls1 ls2)
      ]
    )
  )

; =list-insert
; ----------------------
(define (list-insert original-ls target num)
  (let loop((ls original-ls) (i 0) (res '()))
    (cond
      [(null? ls) (r res)]
      [(= i num)
       (loop (cdr ls) (++ i) (cons (car ls) (cons target res)))
       ]
      [else
        (loop (cdr ls) (++ i) (cons (car ls) res))
        ]
      )
    )
  )

; =has-flexible-length-argument?
; --------------------------------
(define (has-flexible-length-argument? arg-ls)
  (char=? (string-ref (keyword->string (cadr (last arg-ls))) 0) #\*)
  )
; =count-make-lambda-params
; ---------------------------
(define (count-make-lambda-params param)
  (if (char=? #\* (string-ref (keyword->string (value (last param))) 0))
    -1
    (length param)
    )
  )
; =make-lambda
; ----------------------
(define (make-lambda param body)
  (let1 flexible-arg (has-flexible-length-argument? param)
    (debug 2 "* lambda: param = " param ", body = " body)
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

; =parse-lambda
(define (parse-lambda ls)
  (cond
    [(null? ls) (error "parsing lambda error: body is empty") ]
    [else
      (receive (param body) (if (eq? (caar ls) :parameter-start)
                              (let1 index (list-index (lambda (x) (eq? (car x) :parameter-end)) ls)
                                (if index
                                  (values (pickup ls 1 (-- index)) (drop ls (++ index)))
                                  (error "parsing lambda error: parameter is not closed" ls)
                                  )
                                )
                              (values (scanner *implicit-variable*) ls)
                              )
        ;(print "    param = " param)
        ;(print "    body = " body)
        (make-lambda param (collect-lambda body))
        )
      ]
    )
  )

; =get-correct-end
; -----------------------
(define (get-correct-end scanned-ls start-key end-key)
  (let loop((ls scanned-ls) (num 0) (level 0))
    (if (null? ls) (error "dameda!")
      (let1 first-token (caar ls)
        (cond
          [(eq? first-token start-key)
           (loop (cdr ls) (++ num) (++ level))
           ]
          [(eq? first-token end-key)
           (if (= (-- level) 0) num
             (loop (cdr ls) (++ num) (-- level))
             )
           ]
          [else
            (loop (cdr ls) (++ num) level)
            ]
          )
        )
      )
    )
  )

; =collect-lambda
; --------------------------
(define (collect-lambda scanned-ls)
  (let1 len (length scanned-ls)
    (let loop((index 0) (res '()))
      (if (= len index) (r res)
        (let1 first-token (list-ref scanned-ls index)
          (cond
            [(eq? (car first-token) :lambda-start)
             (let1 end (get-correct-end (drop scanned-ls index) :lambda-start :lambda-end)
               ; 先頭のlambda-startと終わりのlambda-endは抜かしてtarget-lsとする
               (let1 target-ls (pickup scanned-ls (++ index) (+ index end -1))
                 ; 終わりのlambda-endの次から始めたいので + 1
                 (loop (+ index end 1) (cons (list :lambda (parse-lambda target-ls)) res))
                 )
               )
             ]
            [else
              (loop (++ index) (cons first-token res))
              ]
            )
          )
        )
      )
    )
  )

; =make-params
;   @position:
;     null => 他パラメータの末尾にreturned-valueを追加
;     else => 他パラーメータの指定された箇所にreturned-valueを挿入
; ---------------------
(define (make-params expr returned-value position)
  (let1 tmp-param (if (= 1 (length expr))
                    'no-param
                    (r fold
                       (lambda (x res)
                         (cond
                           [(_word? x)
                            (cons (get-variable (value x)) res)
                            ]
                           [else
                             (cons (value x) res)
                             ]
                           )
                         )
                       '() (cdr expr))
                    )
    (cond
      [(! eq? returned-value *nil-returned-value*)
       (if (eq? tmp-param 'no-param)
         (list returned-value)
         (if (null? position)
           (append-with-null tmp-param (list returned-value))
           (list-insert tmp-param returned-value position)
           )
         )
       ]
      [else
        ; 暗黙変数"_"のために何も引数がない場合には nil を入れておく
        (if (eq? tmp-param 'no-param) (list *nil*) tmp-param)
        ]
      )
    )
  )

; =execute
; ---------------------------
(define (execute expr returned-value pos)
  (let1 first-token (car expr)
    (cond
      ; トークン列の最初が関数や変数の場合
      [(_word? first-token)
       (let1 first-val (get-variable (value first-token) (if (eq? returned-value *nil-returned-value*)
                                                           (cdr expr)
                                                           (cons returned-value (cdr expr))))
         (cond
           ; 関数の場合は無条件で実行
           ; 正規表現の結果セットの場合は引数(または前の関数の戻り値)があれば実行
           [(or (procedure? first-val)
              (and (regmatch? first-val) (or (> (length expr) 1)
                                           (! eq? returned-value *nil-returned-value*)
                                           ))
              )
            (apply first-val (make-params expr returned-value pos))
            ]
           [else first-val]
           )
         )
       ]
      ; トークン列の最初が無名関数、または正規表現オブジェクトの場合
      ;   引数があれば実行。引数がなければ else パートに飛びオブジェクトと見なす
      [(or (and (_lambda? first-token) (or (> (length expr) 1) (! eq? returned-value *nil-returned-value*)))
         (and (_regexp? first-token) (or (> (length expr) 1) (! eq? returned-value *nil-returned-value*))))
       (let1 first-val (value first-token)
         (apply first-val (make-params expr returned-value pos))
         )
       ]
      ; 直前の処理の戻り値をそのまま処理対象ワードをして扱う場合 (this構文)
      [(_this? first-token)
       (when (! eq? *nil-returned-value* returned-value)
         (apply returned-value (make-params expr *nil-returned-value* pos))
         )
       ]
      ; 戻り値のパラメータへの挿入場所指定がある場合
      [(_pipe-pos? first-token)
       (when (! eq? *nil-returned-value* returned-value)
         (execute (cdr expr) returned-value (value first-token))
         )
       ]
      ; トークン列の最初がそれ以外（数字・文字列など）の場合
      [else 
        (value first-token)
        ]
      )
    )
  )

; =parser
; --------------------
(define (parser each-pipe-token . default-returned-value)
  (define returned-value (if (null? default-returned-value)
                           *nil-returned-value*
                           (car default-returned-value)))

  (for-each
    (lambda (expr)
      (debug 3 "* parsing token\n  " expr)
      (let1 _return (execute expr returned-value '())
        (set! returned-value _return)
        (debug 3 "* set returned value = " _return " -- " each-pipe-token)
        )
      )
    each-pipe-token)

  returned-value
  )
