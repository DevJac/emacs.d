(defun purr-org-table-change-field ()
  "Blank the org table field then begin inserting text."
  (interactive)
  (org-table-blank-field)
  (purr-exit))

(define-key purr-mode-map (kbd "SPC o t c") #'purr-org-table-change-field)
