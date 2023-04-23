;; Don't entirely close Emacs on :q, just kill buffer and window
(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window)

(evil-set-leader 'motion (kbd "SPC"))

(evil-define-key 'motion 'global
  (kbd "<leader> TAB") #'consult-buffer
  (kbd "<leader> v") #'er/expand-region
  (kbd "<leader> t l") #'visual-line-mode
  (kbd "<leader> t t") #'trim-trailing-whitespace
  (kbd "<leader> t w") #'whitespace-mode
  (kbd "<leader> t p") #'rainbow-delimiters-mode
  ;;; window
  (kbd "<leader> w") evil-window-map
  ;; which-key C-h needs to be free
  (kbd "<leader> w C-h") nil
  ;;; org
  (kbd "<leader> o a") #'org-agenda
  (kbd "<leader> o [") #'org-agenda-file-to-front
  (kbd "<leader> o ]") #'org-remove-file
  (kbd "<leader> o u") #'org-columns
  (kbd "<leader> o s") #'insert-timestamp
  (kbd "<leader> o t") #'org-todo
  (kbd "<leader> o p") #'org-priority
  (kbd "<leader> o x") #'org-latex-preview
  (kbd "<leader> o b") #'org-insert-structure-template
  (kbd "<leader> o i") #'org-indent-mode
  (kbd "<leader> o h") #'org-insert-heading-promote
  (kbd "<leader> o j") #'org-insert-heading-down
  (kbd "<leader> o k") #'org-insert-heading-up
  (kbd "<leader> o l") #'org-insert-heading-demote
  (kbd "<leader> o c c") #'org-clock-in
  (kbd "<leader> o c o") #'org-clock-out
  (kbd "<leader> o c g") #'org-clock-goto
  (kbd "<leader> o c l") #'org-clock-last
  (kbd "<leader> o c d") #'org-clock-display
  (kbd "<leader> o c r") #'org-clock-report
  ;;; narrow
  (kbd "<leader> n w") #'widen
  (kbd "<leader> n n") #'narrow-to-region
  (kbd "<leader> n d") #'narrow-to-defun
  (kbd "<leader> n p") #'narrow-to-page
  ;;; roam
  (kbd "<leader> r f") #'org-roam-node-find
  (kbd "<leader> r i") #'org-roam-node-insert
  (kbd "<leader> r c") #'org-roam-capture
  ;;; eval
  (kbd "<leader> e e") #'eval-expression
  (kbd "<leader> e r") #'eval-region
  (kbd "<leader> e s") #'evil-fix-eval-last-sexp
  (kbd "<leader> e j") #'evil-fix-eval-print-last-sexp)

(evil-define-key 'motion org-mode-map
  (kbd "TAB") #'org-cycle)
