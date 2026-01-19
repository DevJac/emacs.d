;;; Theme collections

(use-package doom-themes
  :ensure t)
(use-package ef-themes
  :ensure t)
(use-package gruvbox-theme
  :ensure t)
(use-package modus-themes
  :ensure t)

;;; Color packages

(use-package rainbow-mode
  :ensure t)
(use-package ct
  :ensure t)

;;; My theme config

(load-theme 'gruvbox-dark-hard 'noconfirm)

(when (member 'gruvbox-dark-hard custom-enabled-themes)
  (set-face-attribute 'highlight nil
                      :background
                      (color-darken-name
                       (face-attribute 'highlight :background)
                       40)))
