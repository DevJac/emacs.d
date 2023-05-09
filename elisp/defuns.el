(defun save-then-kill-buffer-and-window ()
  (interactive)
  (if (buffer-file-name)
      (save_buffer))
  (kill-buffer-and-window))

(defun trim-trailing-whitespace ()
  (interactive)
  (delete-trailing-whitespace)
  (message "Trimmed trailing whitespace"))

(defun insert-timestamp ()
  (interactive)
  (org-time-stamp '(16)))

(defun org-insert-heading-promote ()
  (interactive)
  (org-insert-heading-after-current)
  (org-promote)
  (evil-insert 1))

(defun org-insert-heading-down ()
  (interactive)
  (if (org-at-item-p)
      (progn
        (evil-append-line 1)
        (org-insert-item (org-at-item-checkbox-p)))
    (progn
      (org-insert-heading-after-current)
      (evil-insert 1))))

(defun org-insert-heading-up ()
  (interactive)
  (if (org-at-item-p)
      (progn
        (evil-beginning-of-line)
        (org-insert-item (org-at-item-checkbox-p)))
    (progn
      (org-back-to-heading)
      (org-insert-heading)
      (end-of-line 1)
      (evil-insert 1))))

(defun org-insert-heading-demote ()
  (interactive)
  (org-insert-heading-after-current)
  (org-demote)
  (evil-insert 1))

;; org-mode and whitespace-mode both modify Emacs' "display tables".
;; When leaving whitespace-mode, my custom org-ellipsis were being replaced with
;; the standard "...". To fix this, I reapply the display table modifications
;; made by org-mode when leaving whitespace-mode.
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Tables.html
(defun fix-org-ellipsis-after-whitespace-mode ()
  (when (and (eq major-mode 'org-mode) (eq whitespace-mode nil))
    ;; The remainder of this function was copied from the org-mode function.
    (unless org-display-table
      (setq org-display-table (make-display-table)))
    (set-display-table-slot
     org-display-table 4
     (vconcat (mapcar (lambda (c) (make-glyph-code c 'org-ellipsis))
                      org-ellipsis)))
    (setq buffer-display-table org-display-table)))

;; When we scale text, we want our rendered latex fragments to scale as well.
(defun scale-latex-fragments ()
  (interactive)
  (org-toggle-latex-fragment '(16))
  (let ((scale
         (if (boundp 'text-scale-mode-step)
             (* 2.0 (expt text-scale-mode-step text-scale-mode-amount))
           2.0)))
    (plist-put org-format-latex-options :scale scale))
  (org-toggle-latex-fragment '(16)))

(defun evil-fix-eval-print-last-sexp (&optional arg)
  (interactive "P")
  (cl-case evil-state
    ('normal (progn
               (evil-append 1)
               (eval-print-last-sexp arg)
               (evil-normal-state)))
    ('visual (progn
               (evil-append 1)
               (eval-print-last-sexp arg)
               (evil-normal-state)
               (evil-visual-restore)))
    (otherwise (eval-print-last-sexp arg))))

(defun evil-fix-eval-last-sexp (&optional arg)
  (interactive "P")
  (cl-case evil-state
    ('normal (progn
               (evil-append 1)
               (eval-last-sexp arg)
               (evil-normal-state)))
    ('visual (progn
               (evil-append 1)
               (eval-last-sexp arg)
               (evil-visual-restore)))
    (otherwise (eval-last-sexp arg))))
