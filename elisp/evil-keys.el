(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window)

;; normal state has precedence over motion state,
;; so we have to clear the normal state key
;; for our motion state key to be seen.
(evil-define-key 'normal 'global
  (kbd "Z Z") nil)
(evil-define-key 'motion 'global
  (kbd "Z Z") #'save-then-kill-buffer-and-window)

(evil-define-key 'motion 'global
  ;; (C-x C-f), (C-x C-b), and (C-x k), are basic Emacs keys and should be used.
  ;; This will ensure I remain somewhat comfortable in vanilla Emacs.
  (kbd "SPC") nil
  (kbd "Z Z") #'save-then-kill-buffer-and-window
  (kbd "SPC TAB") #'ivy-switch-buffer
  (kbd "SPC v") #'er/expand-region
  (kbd "SPC /") #'swiper
  (kbd "SPC f") #'evil-avy-goto-char-timer
  ;; projectile
  (kbd "SPC p f") #'projectile-find-file
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
  (kbd "SPC o a") #'org-agenda
  (kbd "SPC o [") #'org-agenda-file-to-front
  (kbd "SPC o u") #'org-columns
  (kbd "SPC o s") (lambda () (interactive) (org-time-stamp '(16)))
  (kbd "SPC o d") #'org-todo
  (kbd "SPC o t") #'org-insert-structure-template
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
  ;; eval
  (kbd "SPC e e") #'evil-fix-eval-last-sexp
  (kbd "SPC e j") #'evil-fix-eval-print-last-sexp
  ;; motion fixes
  (kbd "g j") (lambda () (interactive) (line-move-visual 1))
  (kbd "g k") (lambda () (interactive) (line-move-visual -1))
  ;; narrow
  (kbd "SPC n w") #'widen
  (kbd "SPC n n") #'narrow-to-region
  (kbd "SPC n d") #'narrow-to-defun
  (kbd "SPC n p") #'narrow-to-page)

;;; org keys
(evil-define-key 'motion org-mode-map
  (kbd "TAB") #'org-cycle
  (kbd "<C-return>") #'org-insert-heading-down)
(evil-define-key 'insert org-mode-map
  (kbd "<C-return>") #'org-insert-heading-down)

;;; SPC rebinds
(evil-define-key 'motion Info-mode-map
  (kbd "SPC") nil
  (kbd "SPC SPC") (lookup-key Info-mode-map (kbd "SPC")))
(evil-define-key 'motion messages-buffer-mode-map
  (kbd "SPC") nil
  (kbd "SPC SPC") (lookup-key messages-buffer-mode-map (kbd "SPC")))
