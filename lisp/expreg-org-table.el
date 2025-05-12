;; Add org table fields to expreg regions.

(defun my/expreg--org-table-field ()
  "Identify org table fields for expreg."
  (when (org-at-table-p)
    (save-excursion
      (let ((beg (progn (org-table-beginning-of-field 0) (point)))
            (end (progn (org-table-end-of-field 0) (point))))
        (list `(org-table-field . ,(cons beg end)))))))

(defun my/expreg--add-org-table-field ()
  (add-to-list 'expreg-functions #'my/expreg--org-table-field))

(add-hook 'org-mode-hook #'my/expreg--add-org-table-field)
