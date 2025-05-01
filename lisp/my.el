(defun my/complete-in-minibuffer ()
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (completion-at-point)))

(defun my/enable-babel-evaluate ()
  (interactive)
  (setq org-confirm-babel-evaluate nil))
