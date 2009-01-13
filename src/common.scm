(define *global-namespace* (make-hash-table-wrap))
(define *local-namespace* (make-hash-table-wrap))

(define *id-count* 1)
(define *current-uid* '())
(define *implicit-variable* "_")
(define *library-path* "./lib")
(define *nil* 'nmk-nil)

; =word?
; --------------
(define (_word? x) (eq? (car x) :word))

; =regexp?
; -------------
(define (_regexp? x) (eq? (car x) :regexp))

; =lambda?
; -----------------
(define (_lambda? x) (eq? (car x) :lambda))

;(define (_redirect? x) (eq? (car x) :redirect))
(define (_this? x) (eq? (car x) :this))

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

; =_get-local-variable
; --------------------
(define (_get-local-variable key)
  (let1 ns (*local-namespace* *current-uid*)
    (cond
      [(ns '() 'exists? key) (ns key)]
      [else (_get-global-variable key)]
      )
    )
  )

; =_get-global-variable
; ------------------------
(define (_get-global-variable key)
  (if (*global-namespace* '() 'exists? key)
    (*global-namespace* key)
    (string->symbol #`"do not found variable  key = ,|key| / uid = ,|*current-uid*|")
;    'do-not-found-variable
    )
  )

; =get-variable
; ----------------
(define (get-variable key)
  (cond
    [(*local-namespace* '() 'exists? *current-uid*)
     (_get-local-variable key)
     ]
    [else
      (_get-global-variable key)
      ]
    )
  )

; =set-variable
; ----------------------
(define (set-variable key value)
  (cond
    [(*local-namespace* '() 'exists? *current-uid*)
     ((*local-namespace* *current-uid*) key value)
     ]
    [else
      (*global-namespace* key value)
      ]
    )
  )

(define (error-occur title . msg)
  (print "* " title " error")
  (apply print msg)
  (exit)
  )

