;; -*- lexical-binding: t; -*-

;; evil is required because evil-define-text-object is a macro
;; and this file will be byte compiled.
(require 'evil)

;;;###autoload
(defun kill-buffer-and-window-quiet ()
  "Kill current buffer, and kill window if there is more than one."
  (interactive)
  (when (kill-buffer (current-buffer))
    (unless (one-window-p)
      (evil-window-delete))))

;;;###autoload
(defun save-then-kill-buffer-and-window-quiet ()
  "Save and kill current buffer, and kill window if there is more than one."
  (interactive)
  (when (buffer-file-name)
    (save-buffer))
  (kill-buffer-and-window-quiet))

;; Create a text object between |'s. This is especially useful for org tables.
(evil-define-text-object evil-org-table-cell-inner (count &optional beg end type)
  "Select the contents of an org-mode table cell, excluding the | delimiters."
  (evil-select-paren "|" "|" beg end type count))

(provide 'my-evil)
