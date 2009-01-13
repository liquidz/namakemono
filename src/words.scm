(use gauche.interactive)

(define *empty-list* 'empty)

(define *words*
  (list
    (list :cons (lambda (x y) (cons x y)))
    (list :car (lambda (ls) (car ls)))
    (list :cdr (lambda (ls) (cdr ls)))
    ;(list :cdr (lambda (ls) (if (null? (cdr ls)) *empty-list* (cdr ls))))
    ;(list :null? (lambda (x) (d x) (eq? x *nil*)))
    (list :null? (lambda (x) (null? x)))
    (list :fun? (lambda (x) (procedure? x)))
    (list :load (lambda (filename) (load-source filename)))
;    (list :! (lambda (bool) (not bool)))
    (list :+ (lambda ints (apply + ints)))
    (list :- (lambda ints (apply - ints)))
    (list :* (lambda ints (apply * ints)))
    (list :/ (lambda ints (apply / ints)))
    (list :% (lambda (x y) (modulo x y)))
    (list := (lambda (x y) (==? x y)))
    (list :> (lambda (x y) (> x y)))
    (list :>= (lambda (x y) (>= x y)))
    (list :< (lambda (x y) (< x y)))
    (list :<= (lambda (x y) (<= x y)))
    (list :file->list (lambda (filename) (file->list filename)))
;    (list :each (lambda (fn ls) (for-each fn ls)))
    (list :pr (lambda x (apply print x)))
;    (list :split (lambda (str del) (string-split str del)))
    ;(list :arr (lambda elems (apply list elems)))
    (list :arr (lambda elems (if (and (= 1 (length elems)) (eq? *nil* (car elems))) '() (apply list elems))))
;    (list :append (lambda ls (apply append ls)))
;    (list :reverse (lambda (ls) (reverse ls)))
    (list :match (lambda (regexp-str target-str) ((string->regexp regexp-str) target-str)))
    (list :string->regexp (lambda (str) (string->regexp str)))
    (list :if (lambda (ok ng pred)
                (let1 target (if pred ok ng)
                  (cond
                    [(procedure? target)
                     (target 'nullxxx)
                     ]
                    [else target]
                    )
                  )
                ))
    (list :def (lambda (key value)
                 (cond
                   [(or (string? key) (symbol? key)) (set-variable (make-keyword key) value)]
                   [(keyword? key) (set-variable key value)]
                   )
                 ))
    )
  )
