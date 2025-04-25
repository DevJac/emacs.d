(defun my/complete-in-minibuffer ()
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (completion-at-point)))
