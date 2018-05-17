;;; pyret-debug-mode.el --- Minor mode for debugging Pyret

;; Author: Philip Blair <philip@pblair.org>
;; Created 17 December 2017

;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file defines a debugging minor mode to be used with pyret-mode.
;; It is recommended to bind this to something like "C-c C-d" if one would
;; like to use it.  It is not very useful for anyone who is not a
;; pyret-mode contributor.

;;; Code:

(require 'cl-lib)
(require 'pyret)

(make-variable-buffer-local
 (defvar pyret-debug-buffer nil
   "Buffer containing pyret-mode debug information"))

(make-variable-buffer-local
 (defvar pyret-debug-line-starts nil
   "List of line start points"))

(defun pyret-debug-get-buffer ()
  "Fetch the debug buffer associated with this buffer."
  (or pyret-debug-buffer
      (progn
        (setq pyret-debug-buffer (generate-new-buffer "*Pyret-Mode Debug*"))
        pyret-debug-buffer)))

(defun pyret-debug-get-window ()
  "Fetch the window associated with this buffer's debug window."
  (get-buffer-window (pyret-debug-get-buffer)))

(defun pyret-debug-kill-buffer ()
  "Kill the debug buffer associated with this buffer."
  (when pyret-debug-buffer
    (kill-buffer pyret-debug-buffer)
    (setq pyret-debug-buffer nil)))

(defun pyret-debug-calc-line-starts ()
  "Calculate the start offsets for each line in this buffer."
  (save-excursion
    (setq pyret-debug-line-starts (make-hash-table))
    (goto-char (point-min))
    (let ((line-no (line-number-at-pos)))
      (while (not (eobp))
        (beginning-of-line)
        (puthash (point) line-no pyret-debug-line-starts)
        (incf line-no)
        (forward-line)))))

(defun pyret-debug-get-line-number ()
  "Fetch line number at point."
  (save-excursion
    (beginning-of-line)
    (or (gethash (point) pyret-debug-line-starts)
        (progn (previous-line)
               (beginning-of-line)
               (gethash (point) pyret-debug-line-starts)))))

(defun pyret-debug-update-nestings ()
  "Update the debug buffer with the nestings at the current point."
  (interactive)
  (let* ((line-no (pyret-debug-get-line-number))
         (line-starts pyret-nestings-at-line-start)
         (start-indent (aref line-starts (- line-no 1)))
         (line-ends pyret-nestings-at-line-end)
         (end-indent (condition-case nil
                         (aref line-ends line-no)
                       ((args-out-of-range-error)
                        ;; last line. show something
                        (aref line-ends (1- line-no)))))
         (start-token-stack (aref pyret-tokens-stack (1- line-no)))
         (end-token-stack (condition-case nil
                              (aref pyret-tokens-stack line-no)
                            ((args-out-of-range-error)
                             ;; last line. show something
                             (aref pyret-tokens-stack (1- line-no))))))
    (cl-flet ((print-line (name start end)
                          (insert (format "  %s:\t%4d\t%s\t%4d\n" name start (if (= start end) "  " "->") end))))
      (with-current-buffer (pyret-debug-get-buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "Indentation for line %d:\n" line-no))
        (insert "         \t Start\t  \t End\n")
        (print-line "Fun   " (pyret-indent-fun start-indent) (pyret-indent-fun end-indent))
        (print-line "Cases " (pyret-indent-cases start-indent) (pyret-indent-cases end-indent))
        (print-line "Data  " (pyret-indent-data start-indent) (pyret-indent-data end-indent))
        (print-line "Shared" (pyret-indent-shared start-indent) (pyret-indent-shared end-indent))
        (print-line "Try   " (pyret-indent-try start-indent) (pyret-indent-try end-indent))
        (print-line "Except" (pyret-indent-except start-indent) (pyret-indent-except end-indent))
        (print-line "Graph " (pyret-indent-graph start-indent) (pyret-indent-graph end-indent))
        (print-line "Parens" (pyret-indent-parens start-indent) (pyret-indent-parens end-indent))
        (print-line "Object" (pyret-indent-object start-indent) (pyret-indent-object end-indent))
        (print-line "Vars  " (pyret-indent-vars start-indent) (pyret-indent-vars end-indent))
        (print-line "Fields" (pyret-indent-fields start-indent) (pyret-indent-fields end-indent))
        (print-line "Period" (pyret-indent-initial-period start-indent) (pyret-indent-initial-period end-indent))
        (print-line "#|..|#" (pyret-indent-block-comment-depth start-indent) (pyret-indent-block-comment-depth end-indent))
        (insert "\n")
        (insert "Line Token stack:\n")
        (if (equal start-token-stack end-token-stack)
            (insert (format "  %s" start-token-stack))
          (insert (format "  %s\t->\t%s" start-token-stack end-token-stack)))
        (setq buffer-read-only t)))))

(defun pyret-debug-mode-open ()
  "Start the pyret-debug-mode window."
  (message "Loading pyret-debug-mode...")
  (add-hook 'post-command-hook #'pyret-debug-update-nestings nil t)
  (display-buffer-pop-up-window (pyret-debug-get-buffer)
                                '((window-width . 1.0) ; <- these seem to not work correctly, but oh well.
                                  (window-height . 0.15)))
  (message "[Pyret-Debug] Computing nestings for the whole file...")
  (pyret-compute-nestings (point-min) (point-max))
  (message "[Pyret-Debug] Finished computing nestings.")
  (pyret-debug-calc-line-starts)
  (setq buffer-read-only t)
  (with-current-buffer (pyret-debug-get-buffer)
    (setq buffer-read-only t))
  (message "Finished loading.")
  (message "Pyret mode debugger is active. Press 'q' to quit."))

(defun pyret-debug-mode-close ()
  "Close the pyret-debug-mode window."
  (interactive)
  (remove-hook 'post-command-hook #'pyret-debug-update-nestings t)
  (setq buffer-read-only nil)
  (setq pyret-debug-line-starts nil)
  (setq pyret-debug-mode nil)
  (let ((window-to-delete (pyret-debug-get-window)))
    (when window-to-delete
      (delete-window window-to-delete))
    (pyret-debug-kill-buffer)))

(defvar pyret-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'pyret-debug-mode-close)
    map))

(define-minor-mode pyret-debug-mode
  "Debugger for pyret-mode"
  nil
  :lighter " [Pyret-Debug]"
  :keymap pyret-debug-mode-map
  (if pyret-debug-mode
      (pyret-debug-mode-open)
    (pyret-debug-mode-close)))

(provide 'pyret-debug-mode)

;;; pyret-debug-mode.el ends here
