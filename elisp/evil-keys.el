(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window)
(evil-define-key 'normal 'global
  (kbd "Z Z") #'save-then-kill-buffer-and-window)
(evil-define-key 'motion 'global
  ;; (C-x C-f), (C-x C-b), and (C-x k), are basic Emacs keys and should be used.
  ;; This will ensure I remain somewhat comfortable in vanilla Emacs.
  (kbd "SPC") nil
  (kbd "Z Z") #'save-then-kill-buffer-and-window
  (kbd "SPC TAB") #'ivy-switch-buffer
  (kbd "SPC v") #'er/expand-region
  ;; projectile
  (kbd "SPC f") #'projectile-find-file
  (kbd "SPC p k") #'projectile-kill-buffers
  ;; windows
  (kbd "SPC w h") #'evil-window-left
  (kbd "SPC w j") #'evil-window-down
  (kbd "SPC w k") #'evil-window-up
  (kbd "SPC w l") #'evil-window-right
  (kbd "SPC w w") #'evil-window-next
  (kbd "SPC w r") #'evil-window-rotate-downwards
  (kbd "SPC w o") #'delete-other-windows
  (kbd "SPC w d") #'evil-window-delete
  (kbd "SPC w s") #'evil-window-split
  (kbd "SPC w v") #'evil-window-vsplit
  ;; parens
  (kbd "SPC p p") #'rainbow-delimiters-mode
  (kbd "SPC p h") #'show-paren-mode
  ;; whitespace
  (kbd "SPC t l") #'visual-line-mode
  (kbd "SPC t t") #'trim-trailing-whitespace
  (kbd "SPC t w") #'whitespace-mode
  ;; spelling
  (kbd "SPC t s") #'flyspell-mode
  (kbd "SPC s s") #'flyspell-buffer
  ;; org
  (kbd "SPC o i") #'org-indent-mode
  (kbd "SPC o h") #'org-insert-heading-promote
  (kbd "SPC o j") #'org-insert-heading-down
  (kbd "SPC o k") #'org-insert-heading-up
  (kbd "SPC o l") #'org-insert-heading-demote
  (kbd "SPC o c c") #'org-clock-in
  (kbd "SPC o c o") #'org-clock-out
  (kbd "SPC o c g") #'org-clock-goto
  (kbd "SPC o c l") #'org-clock-last
  (kbd "SPC o c d") #'org-clock-display
  (kbd "SPC o c r") #'org-clock-report
  ;; neotree
  (kbd "SPC q q") #'neotree-toggle
  (kbd "SPC q d") #'neotree-dir)
(evil-define-key 'normal neotree-mode-map  ; We have to use 'normal map here?
  (kbd "a") #'neotree-hidden-file-toggle
  (kbd "d") #'neotree-change-root
  (kbd "TAB") #'neotree-quick-look
  (kbd "RET") #'neotree-enter)
(evil-define-key 'motion Info-mode-map
  (kbd "SPC") nil   ; Why is this needed here, but not in other modes?
  (kbd "SPC SPC") (lookup-key Info-mode-map (kbd "SPC")))
(evil-define-key 'motion markdown-mode-map
  (kbd "TAB") #'markdown-cycle)
(evil-define-key 'motion messages-buffer-mode-map
  (kbd "SPC SPC") (lookup-key messages-buffer-mode-map (kbd "SPC")))
