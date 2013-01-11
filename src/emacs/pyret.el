(require 'generic-x) ;; we need this

(define-generic-mode 
  'pyret-mode
  '("#")                                  ;; comments start with #
  '("fun" "def" "cond" "import" "provide"
    "data" "end" "true" "false" "do"
    "as" "with" "sharing")    ;; keywords
  '(("=" .  'font-lock-operator)
    (":" .  'font-lock-builtin)
    ("::" . 'font-lock-bultin)
    ("=>" . 'font-lock-builtin)
    ("," .  'font-lock-builtin)
    ("{" .  'font-lock-builtin)
    ("}" .  'font-lock-builtin)
    ("." .  'font-lock-builtin)
    ("\\" . 'font-lock-builtin)
    (";" .  'font-lock-builtin)
    ("|" .  'font-lock-builtin))
  '("\\.arr$")                            ;; files for which to activate this mode 
   (list (lambda () (set (make-local-variable 'comment-start) "#"))
	 (lambda () (set (make-local-variable 'tab-width) 2))
	 (lambda () (set (make-local-variable indent-tabs-mode) nil)))
  "A mode for Pyret files"                ;; doc string for this mode
)
