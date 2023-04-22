;; Don't entirely close Emacs on :q, just kill buffer and window
(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window)

(evil-set-leader 'motion (kbd "SPC"))

(evil-define-key 'motion 'global
  (kbd "<leader> TAB") #'consult-buffer
  (kbd "<leader> w") evil-window-map
  ;; which-key C-h needs to be free
  (kbd "<leader> w C-h") nil)

(evil-define-key 'motion org-mode-map
  (kbd "TAB") #'org-cycle)
