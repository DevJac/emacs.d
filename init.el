; The following line is a workaround for Emacs 26.2.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(setq package-selected-packages
      '(amx
	company-lsp
	counsel
	evil
	flycheck
	gruvbox-theme
	keyfreq
	lsp-mode
	lsp-ui
	projectile
	restart-emacs
	rust-mode
	whitespace
	yasnippet))

(evil-mode 1)

; ivy config
(ivy-mode 1)
(setq ivy-use-virtual-buffers 1)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") #'swiper)
(global-set-key (kbd "M-x") #'counsel-M-x)
(global-set-key (kbd "C-x C-f") #'counsel-find-file)
(global-set-key (kbd "C-h f") #'counsel-describe-function)
(global-set-key (kbd "C-h v") #'counsel-describe-variable)
(setq spc-map (make-sparse-keymap))
(define-key evil-normal-state-map (kbd "SPC") spc-map)
(define-key spc-map (kbd "TAB") #'ivy-switch-buffer)

; keyfreq config
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-file "~/.emacs.d/keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/keyfreq-lock")

(setq scroll-margin 2)
(setq scroll-conservatively 10)

(load-theme 'gruvbox-dark-hard 1)
(set-face-attribute 'default nil :height 160)

(custom-set-variables
 '(ivy-initial-inputs-alist
   (quote
    ((counsel-minor . "^+")
     (counsel-package . "^+")
     (counsel-org-capture . "^")
     (counsel-M-x . "")
     (counsel-describe-function . "")
     (counsel-describe-variable . "")
     (org-refile . "^")
     (org-agenda-refile . "^")
     (org-capture-refile . "^")
     (Man-completion-table . "^")
     (woman . "^")))))
(custom-set-faces
 )
