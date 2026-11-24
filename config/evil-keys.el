;; -*- lexical-binding: t; -*-

(evil-set-leader '(motion normal) (kbd "SPC"))

(evil-define-key '(motion normal) 'global
  ;; C-o moves backward, M-o moves forward.
  ;; This is needed because C-i and TAB are intertwined.
  (kbd "M-o") #'evil-jump-forward
  (kbd "#") #'my/evil-highlight-word
  (kbd "*") #'my/evil-highlight-symbol
  (kbd "<leader> u") #'universal-argument
  (kbd "<leader> v") #'expreg-expand
  (kbd "<leader> V") #'expreg-contract
  (kbd "<leader> P") #'consult-yank-from-kill-ring
  ;;; project keybinds
  (kbd "<leader> p b") #'consult-project-buffer
  (kbd "<leader> p r") #'my/dired-recent-project
  ;;; project keybinds - find-file
  (kbd "<leader> p f f") #'project-find-file
  (kbd "<leader> p f h") #'my/find-file-in-home
  (kbd "<leader> p f c") #'my/find-file-in-code
  (kbd "<leader> p f n") #'my/find-file-in-notes
  (kbd "<leader> p f e") #'my/find-file-in-emacs-config
  (kbd "<leader> p f r") #'my/find-file-in-recent-project
  ;;; project keybinds - search
  (kbd "<leader> p g g") #'consult-ripgrep
  (kbd "<leader> p g h") #'my/search-home
  (kbd "<leader> p g c") #'my/search-code
  (kbd "<leader> p g n") #'my/search-notes
  (kbd "<leader> p g e") #'my/search-emacs
  (kbd "<leader> p g r") #'my/search-recent
  ;;; project keybinds - end
  (kbd "<leader> t t") #'whitespace-mode
  (kbd "<leader> t w 1") #'my/line-wrap-truncate
  (kbd "<leader> t w 2") #'my/line-wrap-character
  (kbd "<leader> t w 3") #'my/line-wrap-word
  (kbd "<leader> t w 4") #'my/line-wrap-narrow
  (kbd "<leader> t w 5") #'my/line-wrap-center
  (kbd "<leader> o s") #'insert-timestamp
  (kbd "<leader> o i h") #'org-insert-heading-promote
  (kbd "<leader> o i j") #'org-insert-heading-down
  (kbd "<leader> o i k") #'org-insert-heading-up
  (kbd "<leader> o i l") #'org-insert-heading-demote
  (kbd "<leader> o c c") #'org-capture
  (kbd "<leader> o c g") #'org-capture-goto-last-stored)

;; Make q quit in *Messages* buffer.
;; See: special-mode
(evil-define-key '(motion normal) special-mode-map
  (kbd "q") #'quit-window)

;; The *Messages* buffer loads very early during Emacs startup.
;; To make sure our keybinds work in this buffer we do the following:
(with-current-buffer "*Messages*" (evil-normalize-keymaps))
