(use file.util)

(define *from-file* "src/namakemono.scm.base")
(define *to-file* "src/namakemono.scm")
(define *label* "@NMK_PATH@")

(define (main args)
  (let1 cd (current-directory)
    (with-output-to-file
      *to-file*
      (lambda ()
        (for-each
          (lambda (line)
            (display (regexp-replace-all* line *label* cd))
            (display "\n")
            )
          (file->list read-line *from-file*))
        )
      )
    )
  0
  )
