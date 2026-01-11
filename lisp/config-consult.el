;; consult and friends

(defun complete-in-minibuffer ()
  "Override the current `completion-in-region-function' and use `consult-completion-in-region'.
This is helpful if you want to move a completion in the main buffer into the minibuffer."
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (completion-at-point)))

(use-package consult
  :ensure t
  :bind
  ("C-x b" . #'consult-buffer)
  ;; :bind* overrides all minor mode bindings.
  :bind*
  ("C-M-i" . #'complete-in-minibuffer)
  :config
  (consult-customize
   ;; These consult commands will be debounced.
   consult-buffer consult-project-buffer
   consult-grep consult-git-grep consult-ripgrep
   consult-find consult-fd
   consult-theme
   :preview-key '(:debounce 0.4 any)))
(use-package corfu
  :ensure t
  :config
  (corfu-history-mode 1)
  (global-corfu-mode 1))
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))
(use-package orderless
  :ensure t
  :custom
  ;; These customizations are suggested by the orderless docs.
  ;; Apparently file completion in TRAMP must use basic; see docs for more info.
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))
(use-package embark
  :ensure t
  :bind
  ("C-c c" . #'embark-act))
(use-package embark-consult
  :ensure t)
