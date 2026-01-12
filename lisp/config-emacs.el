;; Emacs customizations

;; Store customizations in another file to keep init.el clean
;; The custom-file variable is not a variable I created
;; See `C-h v custom-file`
(setq custom-file (locate-user-emacs-file "custom-sets.el"))
(load custom-file 'noerror 'nomessage)
;; GUI cleanup
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-face-attribute 'default nil
                    :family "Fira Mono"
                    :height 130)
;; Better built-in completions
;; See: https://robbmann.io/posts/emacs-29-completions/
(setq completion-auto-help 'visible)
(setq completion-auto-select 'second-tab)
;; TAB first tries to indent, then tries to complete
(setq tab-always-indent 'complete)
;; No weird files
(auto-save-visited-mode 1)
(setq make-backup-files nil)
;; Remember recently opened files
(recentf-mode 1)
(setq recentf-max-saved-items 200)
;; Remember recent minibuffer entries, such as M-x commands
(savehist-mode 1)
;; Remember cursor location in previously opened files
(save-place-mode 1)
;; Automatically update buffers if the underlying file changes
(global-auto-revert-mode 1)
;; Show match count in isearch
(setq isearch-lazy-count t)
;; Display keybindings
(which-key-mode 1)
;; Scroll customizations: scroll margin and avoid recentering point
(setq scroll-margin 2)
(setq scroll-conservatively 101)
;; Don't indent with TABs; this is buffer local so we change the default
(setq-default indent-tabs-mode nil)
;; Enable transient repeat-maps; `spc v spc v` becomes `spc v v`
(repeat-mode 1)
