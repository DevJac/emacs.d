;; -*- lexical-binding: t; -*-

(declare-function consult-completion-in-region "consult")
(declare-function consult-fd "consult")
(declare-function consult-ripgrep "consult")
(declare-function org-time-stamp "org")
(declare-function project-prompt-project-dir "project")

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

;;;###autoload
(defun insert-file-path ()
  "Prompt for a filesystem path and insert it at point."
  (interactive)
  (insert (read-file-name "Path: ")))

;;;###autoload
(defun insert-timestamp ()
  (interactive)
  (org-time-stamp '(16)))

;;;###autoload
(defun insert-literal-tab ()
  "Insert a literal TAB character."
  (interactive)
  (insert ?\t)
  (message "C-q TAB will also insert a TAB"))

;;; Search functions

;;;###autoload
(defun my/search-home ()
  "Search the home directory using ripgrep."
  (interactive)
  (consult-ripgrep "~"))

;;;###autoload
(defun my/search-code ()
  "Search the ~/Code directory using ripgrep."
  (interactive)
  (consult-ripgrep "~/Code"))

;;;###autoload
(defun my/search-notes ()
  "Search the ~/Notes directory using ripgrep."
  (interactive)
  (consult-ripgrep "~/Notes"))

;;;###autoload
(defun my/search-emacs-config ()
  "Search the Emacs config directory using ripgrep."
  (interactive)
  (consult-ripgrep user-emacs-directory))

;;;###autoload
(defun my/search-recent-project ()
  "Select and search a recent project."
  (interactive)
  (consult-ripgrep (project-prompt-project-dir "Search project" nil t)))

(defun find-file-rec (&rest paths)
  "Select and open a file recursively from PATHS."
  (find-file
   (completing-read
    "Find file: "
    (apply #'process-lines
           "fd"
           "--hidden"
           "--exclude" ".git"
           "--exclude" ".venv"
           "."
           (mapcar #'expand-file-name paths))
    nil t)))

;;;###autoload
(defun my/find-file-in-home ()
  "Find and open a file in the home directory using fd."
  (interactive)
  (consult-fd "~"))

;;;###autoload
(defun my/find-file-in-code ()
  "Find and open a file in the ~/Code directory."
  (interactive)
  (find-file-rec "~/Code"))

;;;###autoload
(defun my/find-file-in-notes ()
  "Find and open a file in the ~/Notes directory."
  (interactive)
  (find-file-rec "~/Notes"))

;;;###autoload
(defun my/find-file-in-emacs-config ()
  "Find and open a file in the Emacs config directory."
  (interactive)
  (find-file-rec user-emacs-directory))

;;;###autoload
(defun my/find-file-in-recent-project ()
  "Find and open a file in a recent project."
  (interactive)
  (let ((default-directory (project-prompt-project-dir "Search project" nil t)))
    (project-find-file)))

;;;###autoload
(defun my/dired-recent-project ()
  "Open the root of a recent project in Dired."
  (interactive)
  (dired (project-prompt-project-dir "Dired project" nil t)))
