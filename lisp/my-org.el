;;; org config

(use-package org
  :ensure t
  :init
  (setq org-directory "~/Notes/")
  (setq org-ellipsis " ▼")
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-log-into-drawer "LOGS")
  (setq org-log-done 'time)
  (setq org-default-notes-file (file-name-concat org-directory "inbox.org"))
  (setq org-capture-templates
        '(("c" "Capture"
           entry (file "inbox.org")
           "* %?")
          ("j" "Journal"
           plain (file+olp+datetree "Journal.org")
           "%T\n\n%?"
           :empty-lines 1))))

;;; org related functions

(defun org-insert-heading-promote ()
  (interactive)
  (org-insert-heading-after-current)
  (org-promote)
  (evil-insert-state 1))

(defun org-insert-heading-down ()
  (interactive)
  (if (org-at-item-p)
      (progn
        (evil-append-line 1)
        (org-insert-item (org-at-item-checkbox-p)))
    (progn
      (org-insert-heading-after-current)
      (evil-insert-state 1))))

(defun org-insert-heading-up ()
  (interactive)
  (if (org-at-item-p)
      (progn
        (evil-beginning-of-line)
        (org-insert-item (org-at-item-checkbox-p)))
    (progn
      (org-back-to-heading)
      (org-insert-heading)
      (end-of-line 1)
      (evil-insert-state 1))))

(defun org-insert-heading-demote ()
  (interactive)
  (org-insert-heading-after-current)
  (org-demote)
  (evil-insert-state 1))

(defun insert-timestamp ()
  (interactive)
  (org-time-stamp '(16)))
