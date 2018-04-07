(require 'generic-x)

(define-generic-mode 'sugar-mode
  ;; Comments
  '("#")
  ;; Keywords
  '("sugar" "end")
  ;; Operators & builtins
  (append
   (mapcar (lambda (regex) (cons regex 'font-lock-builtin-face))
           '("\|" "=>" ":" "@" "\(" "\)" "\{" "\}" "\\[" "\\]" "<" ">" "\\.\\.\\." "_"))
   (mapcar (lambda (regex) (cons regex 'font-lock-function-name-face))
           '("meta" "biject" "fresh" "capture" "true" "false" "some" "none")))
  ;; files for which to activate this mode
  '("\\.sugar$")
  ;; other functions to call
  nil
  ;; doc string for this mode
  "A mode for pyret sugar files")
