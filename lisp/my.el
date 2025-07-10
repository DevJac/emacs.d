(defun complete-in-minibuffer ()
  "Override the current `completion-in-region-function' and use `consult-completion-in-region'.

This is helpful if you want to move a completion in the main buffer into the minibuffer."
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (completion-at-point)))

(defun enable-babel-evaluate ()
  "By default babel code blocks will ask for confirmation before evaluating. Disable that."
  (interactive)
  (setq org-confirm-babel-evaluate nil))

(defun show-trailing-whitespace ()
  "Show trailing whitespace by setting the `show-trailing-whitespace' variable."
  (interactive)
  (setq show-trailing-whitespace t))

(defun find-file-rec (&rest paths)
  "Fuzzy find on ALL files in the given directories."
  (find-file
   (completing-read "Find file: "
                    ;; Using #'process-lines-ignore-status here
                    ;; might help with debugging.
                    (apply #'process-lines "fd" "-u" "." paths))))

(defun find-file-home ()
  "Fuzzy find on ALL files in the home directory."
  (interactive)
  (find-file-rec (expand-file-name "~")))

(defun find-file-code ()
  "Fuzzy find on ALL files in the Code directory."
  (interactive)
  (find-file-rec (expand-file-name "~/Code")))

(defun find-file-emacs ()
  "Fuzzy find on ALL files in the .emacs.d directory."
  (interactive)
  (find-file-rec (expand-file-name "~/.emacs.d")))
