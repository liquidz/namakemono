(define *nil-returned-value* 'nothing)

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
  (cond
    ; トークン列の最初が関数や変数の場合
    [(_word? (car expr))
     (let1 first-val (get-variable (value (car expr)))
       (cond
         [(procedure? first-val)
          (apply first-val (make-params expr returned-value pos))
          ]
         [else first-val]
         )
       )
     ]
    ; トークン列の最初が無名関数、または正規表現オブジェクトの場合
    ;   無名関数の場合は引数があれば実行。引数がなければ else パートに飛びオブジェクトと見なす
    [(or (and (_lambda? (car expr)) (> (length expr) 1))
       (_regexp? (car expr)))
     (let1 first-val (value (car expr))
       (apply first-val (make-params expr returned-value pos))
       )
     ]
    ; 直前の処理の戻り値をそのまま処理対象ワードをして扱う場合 (this構文)
    [(_this? (car expr))
     (when (! eq? *nil-returned-value* returned-value)
       (apply returned-value (make-params expr *nil-returned-value* pos))
       )
     ]
    ; 戻り値のパラメータへの挿入場所指定がある場合
    [(_pipe-pos? (car expr))
     (when (! eq? *nil-returned-value* returned-value)
       (execute (cdr expr) returned-value (value (car expr)))
       )
     ]
    ; トークン列の最初がそれ以外（数字・文字列など）の場合
    [else 
      (value (car expr))
      ]
    )
  )

; =parser
; --------------------
(define (parser each-pipe-token)
  (define returned-value *nil-returned-value*)
  (for-each
    (lambda (expr)
      (debug "* parsing token\n  " expr)
      (let1 _return (execute expr returned-value '())
        (set! returned-value _return)
        (debug "* set returned value = " _return " -- " each-pipe-token)
        )
      )
    each-pipe-token)

  returned-value
  )
