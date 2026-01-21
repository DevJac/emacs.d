(defun enable-babel-evaluate ()
  "By default babel code blocks will ask for confirmation before evaluating. Disable that."
  (interactive)
  (setq org-confirm-babel-evaluate nil))

(defun toggle-trailing-whitespace ()
  "Show trailing whitespace by setting the `show-trailing-whitespace' variable."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Trailing whitespace %s"
           (if show-trailing-whitespace "shown" "hidden")))
