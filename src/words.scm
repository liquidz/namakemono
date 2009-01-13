(use gauche.interactive)

(define *empty-list* 'empty)

(define *words*
  (list
    (list :ver *nmk-version*)
    (list :cons (lambda (x y) (cons x y)))
    (list :car (lambda (ls) (car ls)))
    (list :cdr (lambda (ls) (cdr ls)))
    (list :null? (lambda (x) (null? x)))
    (list :fun? (lambda (x) (procedure? x)))
    (list :load (lambda (filename) (load-source filename)))
    (list :apply (lambda (fn params) (apply fn params)))
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
    (list :pr (lambda x (apply print x)))
    (list :di (lambda x (for-each display x)))
    (list :arr (lambda elems (if (and (= 1 (length elems)) (eq? *nil* (car elems))) '() (apply list elems))))
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
    (list :eval (lambda (src)
                  (run-source src)
                  ))
    )
  )
