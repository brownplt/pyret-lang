(require 'generic-x) ;; we need this

(define-generic-mode 
  'pyret-mode
  '("#")                                  ;; comments start with #
  '("fun" "def" "cond" "import" "provide"
    "data" "end" "true" "false" "do" )    ;; keywords
  '(("=" .  'font-lock-operator)
    (":" .  'font-lock-builtin)
    ("::" . 'font-lock-bultin)
    ("=>" . 'font-lock-builtin)
    ("," .  'font-lock-builtin)
    ("{" .  'font-lock-builtin)
    ("}" .  'font-lock-builtin)
    ("." .  'font-lock-builtin)
    ("\\" . 'font-lock-builtin)
    (";" .  'font-lock-builtin))
  '("\\.arr$")                            ;; files for which to activate this mode 
   nil                                    ;; other functions to call
  "A mode for Pyret files"                ;; doc string for this mode
)
