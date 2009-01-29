; --------------------------------------------------------------
; http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3Ascm2exe
; --------------------------------------------------------------

(use gauche.parseopt)
(use gauche.config)
(use file.util)

(define p print)

(define (usage)
  (p "Usage: scm2exe [-o outfile] <script>")
  (exit 0))

(define (main args)
  (let-args (cdr args) ((outfile "o=s")
                        (help "h|help" => (cut usage))
                        . args)
    (when (null? args) (usage))
    (let* ((ifile (car args))
           (ofile (or outfile
                    (if (#/\.scm$/ ifile)
                      (regexp-replace #/\.scm$/ ifile ".out")
                      #`",|ifile|.out")))
           (cfile (generate-cprog ifile)))
      (compile cfile ofile)
      (sys-unlink cfile)
      ))
  0
  )

(define (generate-cprog ifile)
  (receive (out tmpfile) (sys-mkstemp "tmp")
    (with-output-to-port out
                         (lambda ()
                           (p "#include <gauche.h>")
                           (p "static const char *script = ")
                           (dolist (line (file->string-list ifile))
                             (format #t "~s ~s\n" line "\n"))
                           (p ";")
                           (p "int main(int argc, char **argv)")
                           (p "{")
                           (p "  int i; ScmObj h = SCM_NIL, t = SCM_NIL;")
                           (p "  ScmObj progstr, progport;")
                           (p "  ScmObj mainproc;")
                           (p "  ScmModule *usermod;")
                           (p)
                           (p "  Scm_Init(GAUCHE_SIGNATURE);")
                           (p "  Scm_Load(\"gauche-init.scm\", 0);")
                           (p "  for (i=0; i<argc; i++) {")
                           (p "    SCM_APPEND1(h, t, SCM_MAKE_STR(argv[i]));")
                           (p "  }")
                           (p "  usermod = Scm_UserModule();")
                           (p "  SCM_DEFINE(usermod, \"*argv*\", SCM_CDR(h));")
                           (p "  SCM_DEFINE(usermod, \"*program-name*\", SCM_CAR(h));")
                           (p "  progstr = SCM_MAKE_STR(script);")
                           (p "  progport = Scm_MakeInputStringPort(SCM_STRING(progstr), TRUE);")
                           (p "  Scm_LoadFromPort(SCM_PORT(progport), 0);")
                           (p "  mainproc = Scm_SymbolValue(usermod, SCM_SYMBOL(SCM_INTERN(\"main\")));")
                           (p "  if (SCM_PROCEDUREP(mainproc)) {")
                           (p "    ScmObj r = Scm_Apply(mainproc, SCM_LIST1(h));")
                           (p "    if (SCM_INTP(r)) Scm_Exit(SCM_INT_VALUE(r));")
                           (p "    else Scm_Exit(70);")
                           (p "  }")
                           (p "  Scm_Exit(0);")
                           (p "}")
                           ))
    (close-output-port out)
    (let1 cfile #`",|tmpfile|.c"
      (sys-rename tmpfile cfile)
      cfile)))

(define (compile cfile ofile)
  (let* ((cc      (gauche-config "--cc"))
         (cflags  (gauche-config "-I"))
         (ldflags (gauche-config "-L"))
         (libs    (gauche-config "-l"))
         (cmd #`",cc ,cflags -o ,ofile ,ldflags ,cfile ,libs")
         )
    (print cmd)
    (sys-system cmd)
    ))

