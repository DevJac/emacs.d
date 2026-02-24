;;; leader keybinds

(evil-set-leader '(motion normal) (kbd "SPC"))

(evil-define-key '(motion normal) 'global
  (kbd "M-o") #'evil-jump-forward
  (kbd "<leader> v") #'expreg-expand
  (kbd "<leader> V") #'expreg-contract
  (kbd "<leader> P") #'consult-yank-from-kill-ring
  (kbd "<leader> t t") #'whitespace-mode
  (kbd "<leader> p b") #'consult-project-buffer
  (kbd "<leader> p f") #'project-find-file
  (kbd "<leader> p g") #'consult-ripgrep
  (kbd "<leader> o s") #'insert-timestamp
  (kbd "<leader> o i h") #'org-insert-heading-promote
  (kbd "<leader> o i j") #'org-insert-heading-down
  (kbd "<leader> o i k") #'org-insert-heading-up
  (kbd "<leader> o i l") #'org-insert-heading-demote
  (kbd "<leader> o c c") #'org-capture
  (kbd "<leader> o c g") #'org-capture-goto-last-stored)

;; Make q quit in *Messages* buffer.
(evil-define-key '(motion normal) special-mode-map
  (kbd "q") #'quit-window)

;; The *Messages* buffer loads very early during Emacs startup.
;; To make sure our keybinds work in this buffer we do the following:
(with-current-buffer "*Messages*" (evil-normalize-keymaps))
