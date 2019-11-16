; The following line is a workaround for Emacs 26.2.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(setq package-selected-packages
      '(amx                 ; history provider for counsel
	company-lsp
	counsel
	evil
	evil-collection     ; evil bindings for more modes
	flycheck
	gruvbox-theme
	keyfreq
	lsp-mode
	lsp-ui
	projectile
	rainbow-delimiters
	restart-emacs
	rust-mode
	which-key
	whitespace
	yasnippet))         ; suggested requirement of lsp

; theme config
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-face-attribute 'default nil :height 160)
(setq scroll-margin 2)
(setq scroll-conservatively 100)
(setq next-screen-context-lines 4)
(load-theme 'gruvbox-dark-hard t)

; evil config
(setq evil-search-module 'evil-search)
(setq evil-want-keybinding nil)
(evil-mode 1)
(evil-collection-init)
(evil-define-key '(motion normal) 'global
  (kbd "SPC") (make-sparse-keymap)
  (kbd "SPC TAB") #'ivy-switch-buffer)
(evil-define-key '(motion normal) Info-mode-map
  (kbd "SPC") (make-sparse-keymap)
  (kbd "SPC SPC") (lookup-key Info-mode-map (kbd "SPC")))

; ivy & counsel config
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") #'swiper)
(setq ivy-initial-inputs-alist
      '((counsel-M-x . "")
	(counsel-describe-function . "")
	(counsel-describe-variable . "")))

; keyfreq config
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-file "~/.emacs.d/keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/keyfreq-lock")

; which-key config
(which-key-mode)
