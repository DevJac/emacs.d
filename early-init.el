;; -*- lexical-binding: t; -*-

;; evil-collection insists this be set before evil loads.
;; evil was being loaded by some of my byte compiled
;; code in user-lisp/ ; we need to set this really early
;; so that it runs before the user-lisp/ code is byte compiled.
(setq evil-want-keybinding nil)
