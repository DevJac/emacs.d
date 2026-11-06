;; -*- lexical-binding: t; -*-

;; Emacs customizations

;; GUI cleanup
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Better built-in completions
;; Built-in minibuffer completion
(setq completions-format 'one-column)
(setq completions-detailed t)
(setq completions-sort 'historical)
(setq completions-max-height 12)
(setq completion-show-help nil)
;; Emacs 31: show and update completions as we type
(setq completion-eager-display t)
(setq completion-eager-update t)
(setq completion-auto-help 'always)
;; Navigate completions without leaving the minibuffer
(setq completion-auto-select nil)
(setq minibuffer-visible-completions 'up-down)

;; TAB first tries to indent, then tries to complete
(setq tab-always-indent 'complete)

;; Keep backups and auto-saves outside working directories.
(let ((backup-dir (locate-user-emacs-file "backups/"))
      (auto-save-dir (locate-user-emacs-file "auto-saves/")))
  (make-directory backup-dir t)
  (make-directory auto-save-dir t)
  (setq backup-directory-alist
        `(("." . ,backup-dir)))
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir sha1))))

;; Remember recently opened files
(setq recentf-max-saved-items 200)
(recentf-mode 1)

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

;; Show column number of point
(column-number-mode 1)

;; Start at Notes/inbox.org
(setq initial-buffer-choice "~/Notes/inbox.org")
