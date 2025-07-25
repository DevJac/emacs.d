;;; evil config

(use-package goto-chg
  :ensure t)
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))
  :config
  (global-undo-tree-mode 1))
(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1))
(use-package evil-anzu
  :ensure t)
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

;;; evil keybinds

(defun kill-buffer-and-window-quiet ()
  "Kill current buffer, and kill window if there is more than one."
  (interactive)
  (kill-buffer (current-buffer))
  (unless (one-window-p)
    (evil-window-delete)))

(defun save-then-kill-buffer-and-window-quiet ()
  "Save and kill current buffer, and kill window if there is more than one."
  (interactive)
  (if (buffer-file-name)
      (save-buffer))
  (kill-buffer-and-window-quiet))

;; Don't allow evil commands to quit Emacs.
(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window-quiet)
(evil-ex-define-cmd "wq" #'save-then-kill-buffer-and-window-quiet)
(evil-define-key '(motion normal) 'global
  (kbd "ZQ") #'kill-buffer-and-window-quiet
  (kbd "ZZ") #'save-then-kill-buffer-and-window-quiet)

;; Create a text object between |'s. This is especially useful for org tables.
(evil-define-text-object evil-org-table-cell-inner
  (count &optional beg end type)
  "Select the contents of an org-mode table cell, excluding the | delimiters."
  (evil-select-paren "\\|" "\\|" beg end type count))

(define-key evil-inner-text-objects-map "|" #'evil-org-table-cell-inner)

;;; leader keybinds

(evil-set-leader '(motion normal) (kbd "SPC"))

(evil-define-key '(motion normal) 'global
  (kbd "<leader> v") #'expreg-expand
  (kbd "<leader> V") #'expreg-contract
  (kbd "<leader> P") #'consult-yank-from-kill-ring
  (kbd "<leader> t t") #'show-trailing-whitespace
  (kbd "<leader> p f") #'project-find-file
  (kbd "<leader> p g") #'consult-ripgrep
  (kbd "<leader> p a h") #'find-file-home
  (kbd "<leader> p a c") #'find-file-code
  (kbd "<leader> p a n") #'find-file-notes
  (kbd "<leader> p a e") #'find-file-emacs
  (kbd "<leader> o s") #'insert-timestamp
  (kbd "<leader> o i h") #'org-insert-heading-promote
  (kbd "<leader> o i j") #'org-insert-heading-down
  (kbd "<leader> o i k") #'org-insert-heading-up
  (kbd "<leader> o i l") #'org-insert-heading-demote
  (kbd "<leader> o c c") #'org-capture
  (kbd "<leader> o c g") #'org-capture-goto-last-stored)
