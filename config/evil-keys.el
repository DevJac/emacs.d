;; -*- lexical-binding: t; -*-

(evil-set-leader '(motion normal) (kbd "SPC"))

(evil-define-key '(motion normal) 'global
  ;; C-o moves backward, M-o moves forward.
  ;; This is needed because C-i and TAB are intertwined.
  (kbd "M-o") #'evil-jump-forward
  (kbd "<leader> u") #'universal-argument
  (kbd "<leader> v") #'expreg-expand
  (kbd "<leader> V") #'expreg-contract
  (kbd "<leader> P") #'consult-yank-from-kill-ring
  (kbd "<leader> p b") #'consult-project-buffer
  (kbd "<leader> p f") #'project-find-file
  (kbd "<leader> p g") #'consult-ripgrep
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
