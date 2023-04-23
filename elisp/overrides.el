;; org-drill version was stripping end characters from org-time-stamp-format
(defun org-drill-time-to-inactive-org-timestamp (time)
  "Convert TIME into org-mode timestamp."
  (format-time-string
   (concat "[" (cdr org-time-stamp-formats) "]")
   time))

;; added line to set property: ROAM_EXCLUDE t
(defun org-drill-id-get-create-with-warning (session)
  (when (and (not (oref session warned-about-id-creation))
             (null (org-id-get)))
    (message (concat "Creating unique IDs for items "
                     "(slow, but only happens once)"))
    (sit-for 0.5)
    (setf (oref session warned-about-id-creation) t))
  (org-id-get-create)
  (org-set-property "ROAM_EXCLUDE" "t"))
