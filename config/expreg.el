;; -*- lexical-binding: t; -*-

(use-package expreg
  :ensure t)

(require 'my-expreg)

(add-to-list 'expreg-functions #'my/expreg--org-table-field)
(add-to-list 'expreg-functions #'my/expreg--org-block)
(setq-default expreg-functions expreg-functions)

;;; transient maps

(put 'expreg-expand 'repeat-map my/expreg--repeat-map)
(put 'expreg-contract 'repeat-map my/expreg--repeat-map)
