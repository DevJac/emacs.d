;; -*- lexical-binding: t; -*-

(declare-function consult-completion-in-region "consult")

;;;###autoload
(defun enable-babel-evaluate ()
  "Disable the Org Babel evaluation confirmation in the current buffer."
  (interactive)
  (setq-local org-confirm-babel-evaluate nil))

;;;###autoload
(defun toggle-trailing-whitespace ()
  "Show trailing whitespace by setting the `show-trailing-whitespace' variable."
  (interactive)
  (setq-local show-trailing-whitespace (not show-trailing-whitespace))
  (message "Trailing whitespace %s"
           (if show-trailing-whitespace "shown" "hidden")))

;;;###autoload
(defun complete-in-minibuffer ()
  "Complete using `consult-completion-in-region'.

This is helpful if you want to move a completion from the main buffer
into the minibuffer."
  (interactive)
  (require 'consult)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (completion-at-point)))
