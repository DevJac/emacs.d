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
  "Fuzzy find on ALL files in the home directory."
  (find-file
   (completing-read "Find file: "
                    ;; Using #'process-lines-ignore-status here
                    ;; might help with debugging.
                    (apply #'process-lines "fd" "-u" "." paths))))

(defun find-file-home () (interactive) (find-file-rec "/home/devjac"))
(defun find-file-code () (interactive) (find-file-rec "/home/devjac/Code"))
(defun find-file-emacs () (interactive) (find-file-rec "/home/devjac/.emacs.d"))
