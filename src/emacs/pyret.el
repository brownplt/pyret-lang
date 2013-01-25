(defvar pyret-mode-hook nil)
(defvar pyret-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "RET" 'newline-and-indent)
    map)
  "Keymap for Pyret major mode")


(defconst pyret-font-lock-keywords-1
  (list
   `(,(regexp-opt '(":" "::" "=>" "," "^" "(" ")" "{" "}" "." "->" "\\" ";" "|" "=")) . font-lock-builtin-face)
   `(,(concat 
       "\\<"
       (regexp-opt
        '("fun" "var" "cond" "import" "provide"
          "data" "end" "do"
          "as" "with" "sharing") t)
       "\\>") . font-lock-keyword-face)
   `(,(concat "\\<" (regexp-opt '("true" "false") t) "\\>") . font-lock-constant-face)
   )
  "Minimal highlighting expressions for Pyret mode")

(defconst pyret-font-lock-keywords-2
  (append
   pyret-font-lock-keywords-1
   (let* ((ident "[a-zA-Z_][a-zA-Z0-9$_\\-]*"))
     (list
      '("\\([|]\\)[ \t]+\\(else\\)" (1 font-lock-builtin-face) (2 font-lock-builtin-face))
      `(,(concat "\\(\\<data\\>\\)[ \t]+\\(" ident "\\)") 
        (1 font-lock-keyword-face) (2 font-lock-type-face))
      `(,(concat "\\([|]\\)[ \t]+\\(" ident "\\)[ \t]*\\(?:::\\|with\\)") 
        (1 font-lock-builtin-face) (2 font-lock-type-face))
      `(,(concat "\\(" ident "\\)[ \t]*::[ \t]*\\(" ident "\\)") 
        (1 font-lock-variable-name-face) (2 font-lock-type-face))
      `(,(concat "\\(" ident "\\)[ \t]*\\((\\|:\\)")  (1 font-lock-function-name-face))
      `(,ident . font-lock-variable-name-face)
     )))
  "Additional highlighting for Pyret mode")

(defconst pyret-font-lock-keywords pyret-font-lock-keywords-2
  "Default highlighting expressions for Pyret mode")


(defconst pyret-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?$ "w" st)
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?^ "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?, "." st)
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?| "." st)
    st)
  "Syntax table for pyret-mode")


;; Experiments in getting indentation right...but this is HARD

;; (defun pyret-get-depth-of-unmatched-line (str n)
;;   (cond
;;    ((< n 1)
;;     (error "Infinite loop on %s" str))
;;    ((string-match "^[ \t]*\\(\\(?:provide\\|data\\|cond\\).*\\(?:end\\)\\)[ \t]*$" str)
;;     (message "Found matched pair; continuing on >>%s<<" (match-string 1 str))
;;     (pyret-get-depth-of-unmatched-line (match-string 1 str) (- n 1)))
;;    ((string-match ".*?\\(?:provide\\|data\\|cond\\)\\(.*\\)" str)
;;     (message "Found unmatched open; +1; continuing on >>%s<<" (match-string 1 str))
;;     (+ (pyret-get-depth-of-unmatched-line (match-string 1 str) (- n 1)) 1))
;;    ((string-match "\\(.*\\)\\(?:end\\).*" str)
;;     (message "Found unmatched close; -1; continuing on >>%s<<" (match-string 1 str))
;;     (- (pyret-get-depth-of-unmatched-line (match-string 1 str) (- n 1)) 1))
;;    (t
;;     (message "Found no opens or closes; 0; remainder is >>%s<<" str)
;;     0)))

;; (defun pyret-get-depth-of-unmatched-open ()
;;   (interactive)
;;   (save-excursion
;;     (let ((depth 0))
;;       (forward-line -1)
;;       (while (and (not (bobp)) (> (current-indentation) 0))
;;         (cond
;;          ((looking-at "^[ \t]*#")
;;           ; ignore comment lines
;;           )
;;          (t
;;           (message "Testing unmatched size for >>%s<<" (thing-at-point 'line))
;;           (setq depth (+ depth (pyret-get-depth-of-unmatched-line (thing-at-point 'line) 5)))))
;;         (forward-line -1))
;;       (message "depth is %d" depth)
;;       (max 0 depth))
;;     ))

;; (defun pyret-get-pipe-depth ()
;;   (save-excursion
;;     (let ((depth 0))
;;       (forward-line -1)
;;       (while (and (not (bobp)) (> (current-indentation) 0))
;;         (cond
;;          ((looking-at "^[ \t]*end")
;;           (setq depth (- depth 1)))
;;          ((looking-at "^[ \t]*provide[ \t]+{[^}]*}[ \t]+end$")
;;           ; ignore a complete provide/end line
;;           )
;;          ((looking-at "^[ \t]*\\(data\\|cond\\|provide\\)")
;;           (setq depth (+ depth 1))))
;;         (forward-line -1))
;;       (message "depth is %d" depth)
;;       (max 0 depth))
;;     ))


(defun pyret-indent-line ()
  "Indent current line as Pyret code"
  (interactive)
  (beginning-of-line)
  (indent-line-to (* (pyret-count-pipe-depth) tab-width)))

(defun pyret-indent-line ()
  "Indent current line as Pyret code"
  (interactive)
  (let ((prev-indent (current-indentation)))
    (save-excursion
      (beginning-of-line)
      (if (bobp)  ; If at the beginning of the file, indent to column 0
          (indent-line-to 0)
        (let ((not-indented t) cur-indent)
          (cond
           ((looking-at "^[ \t]*provide[ \t]+{[^}]*}[ \t]+end$")
            ; ignore a complete provide/end line
            )
           ((looking-at "^[ \t]*\\(end\\|}\\)") ; If we're at an "end" line, de-indent
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
           ((looking-at "^[ \t]*sharing")
            (save-excursion
              (forward-line -1)
              (while (and (not (bobp)) (looking-at "^[ \t]*#"))
                (forward-line -1))
              (cond
               ((or (looking-at "^[ \t]*\\(end\\|}\\)") (looking-at ".*,$"))
                (setq cur-indent (- (current-indentation) (* 2 tab-width))))
               (t
                (setq cur-indent (- (current-indentation) tab-width))))))
           ((looking-at "^[ \t]*[|]")
            (save-excursion
              (forward-line -1)
              (cond
               ((or (looking-at "^[ \t]*end") (looking-at ".*,$"))
                (setq cur-indent (- (current-indentation) tab-width))) ; (pyret-get-pipe-depth) tab-width))
               ((looking-at "^[ \t]*\\(cond\\|data\\|provide\\|{\\)")
                (setq cur-indent (+ (current-indentation) tab-width)))
               (t
                (setq cur-indent (current-indentation))))
              (setq not-indented nil)))
           (t
            (save-excursion 
              (while (and (not (bobp)) not-indented)
                (forward-line -1)
                (cond
                 ((or (looking-at "^[ \t]*#") ; ignore comment lines
                      (looking-at "^[ \t]*provide[ \t]+{[^}]*}[ \t]+end[ \t]*$")) ; ignore a complete provide/end line
                  )
                 ; If we see an "end" line before the current line, indent to that line
                 ((looking-at "^[ \t]*\\(end\\|}\\)") 
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
                 ; If we see a "start" line, increase indentation relative to that line
                 ((or (looking-at "^[ \t]*\\(data\\|provide\\|sharing\\|{\\)")
                      (looking-at ".*\\<with[ \t]*$")
                      (and (looking-at ".*:[ \t]*$") (not (looking-at ".*::[ \t]*$"))))
                  (setq cur-indent (+ (current-indentation) tab-width))
                  (setq not-indented nil))
                 ((looking-at "^[ \t]*[|]")
                  (save-excursion
                    (forward-line -1)
                    (cond
                     ((looking-at "^[ \t]*\\(end\\|}\\)")
                      (setq cur-indent (- (current-indentation) tab-width))) ; (pyret-get-pipe-depth) tab-width))
                     ((looking-at ".*,[ \t]*$")
                      (setq cur-indent (- (current-indentation) (* 2 tab-width))))
                     ((looking-at "^[ \t]*\\(cond\\|data\\|provide\\|{\\)")
                      (setq cur-indent (+ (current-indentation) tab-width)))
                     (t
                      (setq cur-indent (current-indentation))))
                    (setq not-indented nil)))
                 (t
                  (if (bobp) ; If we've reached the beginning of the file, bail out
                      (setq not-indented nil))))))))
          (if cur-indent
              (indent-line-to cur-indent)
                                        ; If we didn't see an indentation hint, then allow no indentation
            (indent-line-to 0)))))
    (if (< (current-column) (current-indentation))
        (forward-char (- (current-indentation) (current-column))))
    ))


(defun pyret-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "#") (comment-end ""))
    (comment-dwim arg)))

(defun pyret-mode ()
  "Major mode for editing Pyret files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table pyret-mode-syntax-table)
  (use-local-map pyret-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(pyret-font-lock-keywords))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-line-function) 'pyret-indent-line)  
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (setq major-mode 'pyret-mode)
  (setq mode-name "Pyret")
  (run-hooks 'pyret-mode-hook))

(provide 'pyret-mode)

(add-to-list 'auto-mode-alist '("\\.arr$" . pyret-mode))
