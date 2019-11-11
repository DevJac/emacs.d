(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/undo-tree")
(require 'undo-tree)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(setq package-selected-packages
      '(counsel
	gruvbox-theme
	projectile))

(load-theme 'gruvbox-dark-hard 1)

(ivy-mode 1)
