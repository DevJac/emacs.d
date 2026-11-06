;; -*- lexical-binding: t; -*-

(use-package org
  :ensure t
  :init
  (setq org-directory "~/Notes")
  (setq org-default-notes-file (file-name-concat org-directory "inbox.org"))
  (setq org-ellipsis " ▼")
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-log-into-drawer "LOGS")
  (setq org-log-done 'time)
  (setq org-capture-templates
        '(("c" "Capture"
           entry (file "inbox.org")
           "* %?")
          ("j" "Journal"
           plain (file+olp+datetree "Journal.org")
           "%T\n\n%?"
           :empty-lines 1))))
