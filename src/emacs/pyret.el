(defvar pyret-mode-hook nil)
(defvar pyret-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "RET" 'newline-and-indent)
    map)
  "Keymap for Pyret major mode")

(defconst pyret-ident-regex "[a-zA-Z_][a-zA-Z0-9$_\\-]*")
(defconst pyret-keywords-regex 
  (regexp-opt
   '("fun" "var" "cond" "import" "provide"
     "data" "end" "do"
     "as" "with" "sharing")))
(defconst pyret-punctuation-regex
  (regexp-opt '(":" "::" "=>" "," "^" "(" ")" "{" "}" "." "\\" ";" "|" "=")))
(defconst pyret-font-lock-keywords-1
  (list
   '(,(regexp-opt '("->" "<" ">")) . font-lock-builtin-face)
   `(,(concat 
       "\\(^\\|[ \t]\\|" pyret-punctuation-regex "\\)\\("
       pyret-keywords-regex
       "\\)\\($\\|[ \t]\\|" pyret-punctuation-regex "\\)") 
     (1 font-lock-builtin-face) (2 font-lock-keyword-face) (3 font-lock-builtin-face))
   `(,pyret-punctuation-regex . font-lock-builtin-face)
   `(,(concat "\\<" (regexp-opt '("true" "false") t) "\\>") . font-lock-constant-face)
   )
  "Minimal highlighting expressions for Pyret mode")

(defconst pyret-font-lock-keywords-2
  (append
   (cdr pyret-font-lock-keywords-1)
   (list
    '("\\([|]\\)[ \t]+\\(else\\)" (1 font-lock-builtin-face) (2 font-lock-builtin-face))
    `(,(concat "\\(\\<data\\>\\)[ \t]+\\(" pyret-ident-regex "\\)") 
      (1 font-lock-keyword-face) (2 font-lock-type-face))
    `(,(concat "\\([|]\\)[ \t]+\\(" pyret-ident-regex "\\)[ \t]*\\(?:::\\|with\\)") 
      (1 font-lock-builtin-face) (2 font-lock-type-face))
    `(,(concat "\\(" pyret-ident-regex "\\)[ \t]*::[ \t]*\\(" pyret-ident-regex "\\)") 
      (1 font-lock-variable-name-face) (2 font-lock-type-face))
    `(,(concat "\\(->\\)[ \t]*\\(" pyret-ident-regex "\\)")
      (1 font-lock-builtin-face) (2 font-lock-type-face))
    `(,(regexp-opt '("<" ">")) . font-lock-builtin-face)
    `(,(concat "\\(" pyret-ident-regex "\\)[ \t]*\\((\\|:\\)")  (1 font-lock-function-name-face))
    `(,pyret-ident-regex . font-lock-variable-name-face)
    ))
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



;; Eight (!) kinds of indentation:
;; bodies of functions
;; bodies of conditions (these indent twice, but lines beginning with a pipe indent once)
;; bodies of data declarations (these also indent twice excepting the lines beginning with a pipe)
;; bodies of sharing declarations
;; additional lines inside unclosed parentheses
;; bodies of objects (and list literals)
;; unterminated variable declarations
;; lines beginning with a period

(defvar nestings (vector))
(defvar nestings-dirty t)

(defun indent (funs conds datas shareds parens objects vars period)
  (vector funs conds datas shareds parens objects vars period))

(defun compute-nestings ()
  (let ((nlen (if nestings (length nestings) 0))
        (doclen (count-lines (point-min) (point-max))))
    (cond 
     ((>= (+ doclen 1) nlen)
      (setq nestings (vconcat nestings (make-vector (+ 1 (- doclen nlen)) (indent 0 0 0 0 0 0 0 0)))))
     (t nil)))
  (let ((n 0)
        (open-fun 0) (cur-opened-fun 0) (cur-closed-fun 0)
        (open-cond 0) (cur-opened-cond 0) (cur-closed-cond 0)
        (open-data 0) (cur-opened-data 0) (cur-closed-data 0)
        (open-shared 0) (cur-opened-shared 0) (cur-closed-shared 0)
        (open-parens 0) (cur-opened-parens 0) (cur-closed-parens 0)
        (open-object 0) (cur-opened-object 0) (cur-closed-object 0)
        (open-vars 0) (cur-opened-vars 0) (cur-closed-vars 0)
        (initial-period 0)
        (opens nil))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (aset nestings n 
              (indent open-fun open-cond open-data open-shared open-parens open-object open-vars initial-period))
        (setq cur-opened-fun 0) (setq cur-opened-cond 0) (setq cur-opened-data 0)
        (setq cur-opened-shared 0) (setq cur-opened-parens 0) (setq cur-opened-object 0)
        (setq cur-closed-fun 0) (setq cur-closed-cond 0) (setq cur-closed-data 0)
        (setq cur-closed-shared 0) (setq cur-closed-parens 0) (setq cur-closed-object 0)
        (setq cur-opened-vars 0) (setq cur-closed-vars 0) (setq initial-period 0)
        (while (not (eolp))
          (cond
           ((and (looking-at "[^ \t]") (equal (car-safe opens) 'needsomething))
            (pop opens))
           ((looking-at "^[ \t]*\\.") 
            (setq initial-period 1)
            (goto-char (match-end 0)))
           ((looking-at "^[ \t]*.*?(.*)[ \t]*\\(->[^:]+\\)?:")
            (incf open-fun) (incf cur-opened-fun)
            (push 'fun opens)
            (goto-char (match-end 0)))
           ((looking-at ":")
            (if (equal (car-safe opens) 'wantcolon)
                (pop opens))
            (forward-char))
           ((looking-at "^[ \t]*var[ \t]+")
            (incf open-vars) (incf cur-opened-vars)
            (push 'var opens)
            (push 'needsomething opens)
            (push 'wantcolon opens)
            (goto-char (match-end 0)))
           ((looking-at "\\bfun\\b")
            (incf open-fun) (incf cur-opened-fun)
            (push 'fun opens)
            (goto-char (match-end 0)))
           ((looking-at "[ \t]+") (goto-char (match-end 0)))
           ((looking-at "\\bcond:")
            (incf open-cond) (incf cur-opened-cond)
            (push 'cond opens)
            (goto-char (match-end 0)))
           ((looking-at "\\bdata\\b")
            (incf open-data) (incf cur-opened-data)
            (push 'data opens)
            (goto-char (match-end 0)))
           ((looking-at "\\bprovide\\b")
            (push 'provide opens)
            (goto-char (match-end 0)))
           ((looking-at "\\bsharing\\b")
            (decf open-data) (incf cur-closed-data)
            (incf open-shared) (incf cur-opened-shared)
            (cond 
             ((equal (car opens) 'data)
              (pop opens)
              (push 'shared opens)))
            (goto-char (match-end 0)))           
           ((looking-at "{\\|\\[")
            (incf open-object) (incf cur-opened-object)
            (push 'object opens)
            (forward-char))
           ((looking-at "\\]\\|}")
            (decf open-object) (incf cur-closed-object)
            (if (equal (car-safe opens) 'object)
                (pop opens))
            (cond 
             ((equal (car-safe opens) 'var)
              (pop opens)
              (incf cur-closed-vars)))
            (forward-char))
           ((looking-at "(")
            (incf open-parens) (incf cur-opened-parens)
            (push 'parens opens)
            (forward-char))
           ((looking-at ")")
            (decf open-parens) (incf cur-closed-parens)
            (if (equal (car-safe opens) 'parens)
                (pop opens))
            (cond 
             ((equal (car-safe opens) 'var)
              (pop opens)
              (incf cur-closed-vars)))
            (forward-char))
           ((looking-at "\\bend\\b")
            (let ((h (car-safe opens)))
              (cond
               ((equal h 'provide)
                (pop opens))
               ((equal h 'fun) 
                (decf open-fun) (incf cur-closed-fun) 
                (pop opens))
               ((equal h 'cond)
                (decf open-cond) (incf cur-closed-cond)
                (pop opens))
               ((equal h 'data)
                (decf open-data) (incf cur-closed-data)
                (pop opens))
               ((equal h 'shared)
                (decf open-shared) (incf-cur-closed-shared)
                (pop opens))
               (t nil)))
            (let ((h (car-safe opens)))
              (while (equal h 'var)
                (pop opens)
                (incf cur-closed-vars)
                (setq h (car-safe opens))))
            (goto-char (match-end 0)))
           (t (if (not (eobp)) (forward-char)))))
        (aset nestings n (indent (- open-fun (max 0 (- cur-opened-fun cur-closed-fun)))
                                 (- open-cond cur-opened-cond)
                                 (- open-data cur-opened-data)
                                 (- open-shared cur-opened-shared)
                                 (+ open-parens (- cur-closed-parens cur-opened-parens))
                                 (- open-object (max 0 (- cur-opened-object cur-closed-object)))
                                 (- open-vars cur-opened-vars)
                                 initial-period))
        (let ((h (car-safe opens)))
          (while (equal h 'var)
            (pop opens)
            (incf cur-closed-vars)
            (setq h (car-safe opens))))
        (setq open-vars (- open-vars cur-closed-vars))
        (incf n)
        (if (not (eobp)) (forward-char))))
    (aset nestings n 
          (indent open-fun open-cond open-data open-shared open-parens open-object open-vars initial-period)))
  (setq nestings-dirty nil))


(defun pyret-indent-line ()
  "Indent current line as Pyret code"
  (interactive)
  (cond
   (nestings-dirty
    (compute-nestings)))
  (let* ((indents (aref nestings (min (- (line-number-at-pos) 1) (length nestings))))
         (open-fun    (elt indents 0))
         (open-cond   (elt indents 1))
         (open-data   (elt indents 2))
         (open-shared (elt indents 3))
         (open-parens (elt indents 4))
         (open-object (elt indents 5))
         (open-vars   (elt indents 6))
         (initial-period (elt indents 7))
         (total-indent (+ open-fun (* 2 open-cond) (* 2 open-data) open-shared open-object open-parens open-vars initial-period)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[ \t]*[|]")
          (if (> total-indent 0)
              (indent-line-to (* tab-width (- total-indent 1)))
            (indent-line-to 0))
        (indent-line-to (max 0 (* tab-width total-indent)))))
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
  (set (make-local-variable 'nestings) nil)
  (set (make-local-variable 'nestings-dirty) t)
  (add-hook 'before-change-functions
               (function (lambda (beg end) 
                           (setq nestings-dirty t)))
               nil t)
  (run-hooks 'pyret-mode-hook))



(provide 'pyret-mode)

(add-to-list 'auto-mode-alist '("\\.arr$" . pyret-mode))
