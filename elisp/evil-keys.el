;; Don't entirely close Emacs on :q, just kill buffer and window
(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window)

(evil-set-leader 'motion (kbd "SPC"))

(evil-define-key 'motion 'global
  (kbd "<leader> TAB") #'consult-buffer)

(evil-define-key 'motion org-mode-map
  (kbd "TAB") #'org-cycle)
