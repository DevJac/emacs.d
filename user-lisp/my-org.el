;; -*- lexical-binding: t; -*-

(declare-function org-insert-heading "org")
(declare-function org-insert-heading-after-current "org")
(declare-function org-back-to-heading "org")
(declare-function org-promote "org")
(declare-function org-demote "org")
(declare-function org-insert-item "org-list")
(declare-function org-at-item-p "org-list")
(declare-function org-at-item-checkbox-p "org-list")
(declare-function evil-append-line "evil-commands")
(declare-function evil-insert-line "evil-commands")
(declare-function evil-insert-state "evil-states")

;;;###autoload
(defun org-insert-heading-promote ()
  (interactive)
  (org-insert-heading-after-current)
  (org-promote)
  (evil-insert-state 1))

;;;###autoload
(defun org-insert-heading-down ()
  (interactive)
  (if (org-at-item-p)
      (progn
        (evil-append-line 1)
        (org-insert-item (org-at-item-checkbox-p)))
    (progn
      (org-insert-heading-after-current)
      (evil-insert-state 1))))

;;;###autoload
(defun org-insert-heading-up ()
  (interactive)
  (if (org-at-item-p)
      (progn
        (evil-insert-line 1)
        (org-insert-item (org-at-item-checkbox-p)))
    (progn
      (org-back-to-heading)
      (org-insert-heading)
      (end-of-line 1)
      (evil-insert-state 1))))

;;;###autoload
(defun org-insert-heading-demote ()
  (interactive)
  (org-insert-heading-after-current)
  (org-demote)
  (evil-insert-state 1))
