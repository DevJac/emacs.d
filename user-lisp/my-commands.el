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
