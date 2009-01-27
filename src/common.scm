(define *nmk-version* "0.15")

(define *debug* 0)

(define *global-namespace* (make-hash-table-wrap))
(define *local-namespace* (make-hash-table-wrap))
;(define *macro-namespace* (make-hash-table-wrap))

(define *id-count* 1)
(define *current-uid* '())
(define *last-uid* '())
(define *implicit-variable* "_")
(define *implicit-variable-word* (list :word (make-keyword *implicit-variable*)))
(define *nil* 'nmk-nil)
(define *get-lambda-params-length* :get-lambda-params)

; =overloaded-function
(define-class <overloaded-function> ()
  ((table :init-value '()))
  )
(define-method initialize ((self <overloaded-function>) init-args)
  (next-method)
  (set! (slot-ref self 'table) (make-hash-table-wrap))
  )

(define (debug level . str)
  (when (<= level *debug*) (apply print str))
  ;(apply print str)
  )

; =overloaded-function?
; ----------------------
(define (overloaded-function? obj)
  (eq? <overloaded-function> (class-of obj)))

; =nmk-lambda
; ----------------------
(define-syntax nmk-lambda
  (syntax-rules (<>)
    [(_ params expr ...)
     (lambda <>
       (cond
         [(and (not (null? <>)) (eq? (car <>) *get-lambda-params-length*))
          (let1 params-ls '(params)
            (if (list? (car params-ls)) (length (car params-ls)) -1)
            )
          ]
         [else
           (receive params (apply values <>)
             expr ...
             )
           ]
         )
       )
     ]
    )
  )

; =word?
; --------------
(define (_word? x) (eq? (car x) :word))
; =regexp?
; -------------
(define (_regexp? x) (eq? (car x) :regexp))
; =regmatch?
(define (_regmatch? x) (regmatch? (cadr x)))
; =lambda?
; -----------------
(define (_lambda? x) (eq? (car x) :lambda))
; =this?
; -----------------------
(define (_this? x) (eq? (car x) :this))
; =pipe-pos?
; ------------------
(define (_pipe-pos? x) (eq? (car x) :pipe-pos))
; =value
; ---------------
(define (value x) (cadr x))

; =make-uid
; -------------
(define (make-uid)
  (let1 id *id-count*
    (set! *id-count* (++ id))
    (string->symbol #`"u,|id|")
    )
  )

; =take-over-variables
;   uid毎に作られるローカル名前空間上の変数を
;   fromからtoへ引き継ぐ
; --------------------
(define (take-over-variables from-uid to-uid)
  (when (and
          (*local-namespace* '() 'exists? from-uid)
          (*local-namespace* '() 'exists? to-uid)
          )
    (hash-table-for-each
      ((*local-namespace* from-uid))
      (lambda (key value)
        (let1 hs (*local-namespace* to-uid)
          (hs key value)
          )
        )
      )
    )
  )

; =make-local-namespace
; -----------------------
(define (make-local-namespace id)
  (*local-namespace* id (make-hash-table-wrap))
  )

; =delete-local-namespace
; -------------------------
(define (delete-local-namespace id)
  (*local-namespace* '() 'delete id)
  )

; =change-current-uid
; ---------------------
(define (change-current-uid to-uid)
  (set! *last-uid* *current-uid*)
  (set! *current-uid* to-uid)
  )

; =_get-local-variable
; --------------------
(define (_get-local-variable key param-len)
  (let1 ns (*local-namespace* *current-uid*)
    (cond
      [(ns '() 'exists? key)
       (let1 tmp (ns key)
         (if (overloaded-function? tmp)
           ; オーバーロードされてる場合は引数の数から関数を探す
           (if ((slot-ref tmp 'table) '() 'exists? param-len)
             ((slot-ref tmp 'table) param-len)
             ; 可変長のパラメータを持つ関数がないか確認
             (if ((slot-ref tmp 'table) '() 'exists? -1)
               ((slot-ref tmp 'table) -1)
               (error "unknown local overloaded function: " key *current-uid*)
               )
             )
           tmp
           )
         )
       ]
      [else
        (_get-global-variable key param-len)
        ]
      )
    )
  )

; =_get-global-variable
; ------------------------
(define (_get-global-variable key param-len)
  (cond
    [(*global-namespace* '() 'exists? key)
     (let1 tmp (*global-namespace* key)
       (cond
         [(overloaded-function? tmp)
          ; オーバーロードされてる場合は引数の数から関数を探す
          (if ((slot-ref tmp 'table) '() 'exists? param-len)
            ((slot-ref tmp 'table) param-len)
            ; 可変長のパラメータを持つ関数がないか確認
            (if ((slot-ref tmp 'table) '() 'exists? -1)
              ((slot-ref tmp 'table) -1)
              (error "unknown global overloaded function: " key *current-uid*)
              )
            )
          ]
         [else
           tmp
           ]
         )
       )
     ]
    [else
      (error "unknown variable: " key *current-uid*)
      ]
    )
  )

; =get-variable
; ----------------
(define (get-variable key . param-len)
  (cond
    [(*local-namespace* '() 'exists? *current-uid*)
     (_get-local-variable key (if (null? param-len) -1 (car param-len)))
     ]
    [else
      (_get-global-variable key (if (null? param-len) -1 (car param-len)))
      ]
    )
  )

; =get-lambda-params-length
; -----------------------------
(define (get-lambda-params-length fn)
  (guard (e (else 1))
    (let1 res (fn *get-lambda-params-length*)
      res
      )
    )
  )

; =set-variable
; ----------------------
(define (set-variable key value . options)
  (let-keywords options ((target-uid :uid *current-uid*)
                         (overload :overload #f)
                         )
    (let1 target-namespace (if (*local-namespace* '() 'exists? target-uid)
                             (*local-namespace* target-uid)
                             *global-namespace*)
      (if (not overload)
        (target-namespace key value)
        (cond
          [(target-namespace '() 'exists? key)
           (let1 tmp (target-namespace key)
             (cond
               [(overloaded-function? tmp)
                ; 既にオーバーロードされている場合
                ((slot-ref tmp 'table) (get-lambda-params-length value) value)
                ]
               [(procedure? tmp)
                ; 新たにオーバーロードする場合
                (debug 3 "* make overloaded function: " key)
                (let1 olf (make <overloaded-function>)
                  ((slot-ref olf 'table) (get-lambda-params-length tmp) tmp)
                  (debug 3 "  * add " key " (len = " (get-lambda-params-length tmp) ")")
                  ((slot-ref olf 'table) (get-lambda-params-length value) value)
                  (debug 3 "  * add " key " (len = " (get-lambda-params-length value) ")")
                  (target-namespace key olf)
                  )
                ]
               [else
                 ; 関数じゃないなら上書き
                 (target-namespace key value)
                 ]
               )
             )
           ]
          [else
            ; 未定義の場合は普通に定義
            (target-namespace key value)
            ]
          )
        )
      )
    )
  )

