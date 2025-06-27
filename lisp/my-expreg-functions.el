;;; Add org table fields to expreg regions.

(defun my/expreg--org-table-field ()
  "Identify org table fields for expreg."
  (when (org-at-table-p)
    (save-excursion
      (let ((beg (progn (org-table-beginning-of-field 0) (point)))
            (end (progn (org-table-end-of-field 0) (point))))
        (list `(org-table-field . ,(cons beg end)))))))

(defun my/expreg--org-block ()
  "Identify src blocks for expreg."
  (save-excursion
    (let ((case-fold-search t)
          (orig-point (point))
          block-beg block-end)
      (search-backward "#+begin_" nil 'NOERROR)
      (next-line)
      (setq block-beg (point))
      (search-forward "#+end_" nil 'NOERROR)
      (beginning-of-line)
      (setq block-end (point))
      (when (and (<= block-beg orig-point) (<= orig-point block-end))
        (list `(org-block . ,(cons block-beg block-end)))))))

(add-to-list 'expreg-functions #'my/expreg--org-table-field)
(add-to-list 'expreg-functions #'my/expreg--org-block)
(setq-default expreg-functions expreg-functions)
