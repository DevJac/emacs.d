(defun my/complete-in-minibuffer ()
  "Override the current `completion-in-region-function' and use `consult-completion-in-region'.

This is helpful if you want to move a completion in the main buffer into the minibuffer."
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (completion-at-point)))

(defun my/enable-babel-evaluate ()
  "By default babel code blocks will ask for confirmation before evaluating. Disable that."
  (interactive)
  (setq org-confirm-babel-evaluate nil))

(defun my/org-table-change-field ()
  "Blank the org table field then begin inserting text."
  (interactive)
  (org-table-blank-field)
  (purr-exit))

(define-key purr-mode-map (kbd "SPC o t c") #'purr-org-table-change-field)
