;;; total-review.el --- Track line review coverage against committed or saved text  -*- lexical-binding: t; -*-

;; Version: 0.3.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience

;;; Commentary:
;;
;; For Git-committed files, Total Review tracks review status per committed
;; file version and maps those bits onto unchanged lines in the live buffer.
;; Editing, saving and undoing cannot change the committed baseline.  Separate
;; states for multiple committed versions allow branch-switch restoration.
;; Files not present in HEAD use the earlier snapshot-based approach.
;; Source files and the Git repository are never modified by Total Review.
;;
;; The database is central, per profile and project.  Git and GNU diff are
;; required for Git-backed tracking.  This is an experimental package; back
;; up review data while testing it.  Version 1 databases are not migrated.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'diff)
(require 'project)

(declare-function dired-get-marked-files "dired")

(defgroup total-review nil
  "Track coverage of code you have reviewed."
  :group 'tools)

(defcustom total-review-directory
  (locate-user-emacs-file "total-review/")
  "Root directory for Total Review databases."
  :type 'directory
  :group 'total-review)

(defcustom total-review-profile "default"
  "Name of the active review profile.
Each profile contains one database per project and a global fallback.
Use the default profile name or any other valid string name."
  :type 'string
  :group 'total-review)

(defun total-review--pending-face ()
  "Return the face for lines awaiting review.
Use `total-review-pending-face' if it exists.  Otherwise, draw
a subtle underline derived from the theme background,
without changing syntax highlighting or the text background."
  (if (facep 'total-review-pending-face)
      'total-review-pending-face
    (let* ((background (face-background 'default nil t))
           (rgb (and (stringp background)
                     (color-name-to-rgb background)))
           (line-color
            (when rgb
              (if (color-dark-p rgb)
                  (color-lighten-name background 20)
                (color-darken-name background 20)))))
      (if line-color
          `(:underline ,line-color)
        ;; Colorless terminals (or an unspecified background).
        '(:underline t)))))

(defvar total-review-mode nil)
(defvar global-total-review-mode nil)

(defvar-local total-review--status nil
  "Flags for working-buffer lines: 1 reviewed, 0 pending, - changed.")

(defvar-local total-review--base-flags nil
  "Review bits for the committed baseline, or snapshot for non-Git files.")

(defvar-local total-review--mapping nil
  "Vector mapping working-line indices to committed-line indices, or nil.")

(defvar-local total-review--wanted nil
  "Non-nil if highlighting was requested, even if a profile lacks this file.")

(defvar total-review--switching-profile nil
  "Non-nil while updating buffer modes after a profile switch.")

(defun total-review--path (&optional file)
  "Return canonical path for FILE or current file, requiring a local file."
  (let ((path (or file buffer-file-name)))
    (unless (and path (not (file-remote-p path))
                 (file-regular-p path))
      (user-error "Total Review requires an existing local, regular file"))
    (file-truename path)))

(defun total-review--id (path)
  "Return a filesystem-safe identifier for PATH."
  (secure-hash 'sha256 path))

(defun total-review--profile-root ()
  "Return the root directory for the active review profile."
  (let ((name total-review-profile))
    (unless (and (stringp name)
                 (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" name))
      (error "Invalid Total Review profile name: %S" name))
    (expand-file-name
     (if (equal name "default")
         "default/"
       (concat "named/" name "/"))
     total-review-directory)))

(defun total-review--profile-label ()
  "Return a readable name for the active profile."
  total-review-profile)

(defun total-review--scope (&optional location)
  "Return review scope for LOCATION, or the current file or directory.
Return a plist with :directory, :kind and :root.  LOCATION is a file
or directory; its containing project determines the database."
  (let* ((location (or location buffer-file-name default-directory))
         (dir (if (file-directory-p location)
                  location (file-name-directory (expand-file-name location))))
         (project (and (not (file-remote-p dir)) (project-current nil dir)))
         (root (and project (file-truename (project-root project))))
         (profile (total-review--profile-root)))
    (if root
        (list :directory (expand-file-name
                          (concat "projects/" (total-review--id root) "/")
                          profile)
              :kind 'project :root root)
      (list :directory (expand-file-name "global/" profile)
            :kind 'global :root nil))))

(defun total-review--database (&optional path)
  "Return the selected database directory for PATH or current context."
  (plist-get (total-review--scope path) :directory))

(defun total-review--state-path (path &optional directory)
  "Return metadata path for canonical PATH in DIRECTORY."
  (expand-file-name (concat (total-review--id path) ".state")
                    (or directory (total-review--database path))))

(defun total-review--snapshot-path (state directory)
  "Return snapshot path from STATE under DIRECTORY, checking its name."
  (let ((name (plist-get state :snapshot)))
    (unless (and (stringp name)
                 (equal name (file-name-nondirectory name))
                 (string-suffix-p ".snapshot" name))
      (error "Invalid Total Review snapshot filename: %S" name))
    (expand-file-name name directory)))

(defun total-review--read-state-file (file directory)
  "Read and validate a version 2 review state from FILE in DIRECTORY."
  (let* ((state (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (read (current-buffer))))
         (path (plist-get state :path))
         (backend (plist-get state :backend)))
    (unless (and (listp state)
                 (equal (plist-get state :version) 2)
                 (stringp path) (file-name-absolute-p path)
                 (equal (file-name-nondirectory file)
                        (concat (total-review--id path) ".state"))
                 (pcase backend
                   ('snapshot
                    (and (stringp (plist-get state :flags))
                         (string-match-p "\\`[01]*\\'" (plist-get state :flags))
                         (file-regular-p
                          (total-review--snapshot-path state directory))))
                   ('git
                    (and (stringp (plist-get state :git-root))
                         (stringp (plist-get state :git-path))
                         (stringp (plist-get state :active))
                         (consp (plist-get state :versions))
                         (cl-some
                          (lambda (version)
                            (equal (plist-get version :blob)
                                   (plist-get state :active)))
                          (plist-get state :versions))
                         (cl-every
                          (lambda (version)
                            (and (stringp (plist-get version :blob))
                                 (stringp (plist-get version :commit))
                                 (stringp (plist-get version :flags))
                                 (string-match-p
                                  "\\`[01]*\\'" (plist-get version :flags))))
                          (plist-get state :versions))))
                   (_ nil)))
      (error "Invalid or incomplete Total Review state: %s" file))
    state))

(defun total-review--load (path &optional directory)
  "Load review state for canonical PATH, or nil if not tracked."
  (let* ((dir (or directory (total-review--database path)))
         (file (total-review--state-path path dir)))
    (when (file-exists-p file)
      (total-review--read-state-file file dir))))

(defun total-review--line-count ()
  "Count actual lines, excluding the imaginary line after a final LF."
  (save-restriction
    (widen)
    (if (= (point-min) (point-max))
        0
      (count-lines (point-min) (point-max)))))

(defun total-review--write-state (path state directory)
  "Atomically commit STATE metadata for PATH in DIRECTORY."
  (make-directory directory t)
  (let ((tmp (make-temp-file (expand-file-name ".review-state-" directory))))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";; Total Review state, version 2.\n")
            (let ((print-length nil) (print-level nil))
              (prin1 state (current-buffer)))
            (insert "\n"))
          (rename-file tmp (total-review--state-path path directory) t))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun total-review--save (path flags &optional old-state snapshot-changed directory)
  "Persist FLAGS for PATH, optionally updating the source snapshot.
OLD-STATE contains prior metadata.  If SNAPSHOT-CHANGED is non-nil,
create a new source snapshot from the current buffer before atomically
committing the state.  DIRECTORY overrides the resolved database."
  (let* ((dir (or directory (total-review--database path)))
         (old (and old-state (total-review--snapshot-path old-state dir)))
         (new (and snapshot-changed
                   (progn
                     (make-directory dir t)
                     (make-temp-file
                      (expand-file-name (concat (total-review--id path) "-") dir)
                      nil ".snapshot"))))
         (snapshot (or new old))
         (committed nil))
    (unless snapshot
      (error "Cannot save review state without a snapshot"))
    (unwind-protect
        (progn
          (when new
            (save-restriction
              (widen)
              (write-region (point-min) (point-max) new nil 'silent)))
          (total-review--write-state
           path (list :version 2 :backend 'snapshot :path path
                      :snapshot (file-name-nondirectory snapshot)
                      :flags flags)
           dir)
          (setq committed t)
          (when (and new old (file-exists-p old))
            (ignore-errors (delete-file old))))
      (when (and new (not committed) (file-exists-p new))
        (delete-file new)))))

(defun total-review--git-output (directory &rest args)
  "Return Git standard output in DIRECTORY for successful ARGS, else nil."
  (when (executable-find "git")
    (with-temp-buffer
      (when (eq 0 (apply #'process-file
                         "git" nil t nil "-C" directory args))
        (replace-regexp-in-string "\n\\'" "" (buffer-string))))))

(defun total-review--git-info (path)
  "Return HEAD's committed file identity for PATH, or nil.
The index and working tree are deliberately ignored."
  (let* ((directory (file-name-directory path))
         (root (total-review--git-output directory "rev-parse" "--show-toplevel")))
    (when root
      (let* ((root (file-name-as-directory (file-truename root)))
             (relative (file-relative-name path root))
             (head (total-review--git-output root "rev-parse" "--verify" "HEAD"))
             (blob (and head
                        (not (string-prefix-p "../" relative))
                        (total-review--git-output
                         root "rev-parse" "--verify" (concat "HEAD:" relative)))))
        (when (and blob (equal (total-review--git-output root "cat-file" "-t" blob)
                               "blob"))
          (list :root root :relative relative :head head :blob blob))))))

(defun total-review--git-blob-buffer (root blob)
  "Return a temporary buffer containing BLOB from Git repository ROOT.
Signal an error when Git cannot read the historical object."
  (let ((buffer (generate-new-buffer " *total-review-git-blob*")))
    (condition-case err
        (progn
          (with-current-buffer buffer
            (unless (eq 0 (process-file "git" nil t nil
                                        "-C" root "cat-file" "blob" blob))
              (error "Git object %s is unavailable" blob))
            (set-buffer-modified-p nil))
          buffer)
      (error
       (kill-buffer buffer)
       (signal (car err) (cdr err))))))

(defun total-review--git-state (path info flags)
  "Construct initial Git review state for PATH and committed INFO using FLAGS."
  (list :version 2 :backend 'git :path path
        :git-root (plist-get info :root)
        :git-path (plist-get info :relative)
        :active (plist-get info :blob)
        :versions (list (list :blob (plist-get info :blob)
                              :commit (plist-get info :head)
                              :flags flags))))

(defun total-review--git-version (state blob)
  "Return the saved review version in STATE for BLOB, or nil."
  (cl-find blob (plist-get state :versions)
           :key (lambda (entry) (plist-get entry :blob)) :test #'equal))

(defun total-review--git-ancestor-distance (root older newer)
  "Return commit distance from OLDER to NEWER in ROOT if ancestor, else nil."
  (when (eq 0 (process-file "git" nil nil nil "-C" root
                            "merge-base" "--is-ancestor" older newer))
    (let ((count (total-review--git-output
                  root "rev-list" "--count" (concat older ".." newer))))
      (and count (string-to-number count)))))

(defun total-review--git-inheritance-source (state info)
  "Find a saved version from which to inherit review progress for INFO.
Prefer the closest known ancestor; if none survives, use the last active
version, conservatively re-diffing it against the new committed content."
  (let ((versions (plist-get state :versions))
        (root (plist-get info :root))
        (head (plist-get info :head))
        chosen distance)
    (dolist (entry versions)
      (let ((n (total-review--git-ancestor-distance
                root (plist-get entry :commit) head)))
        (when (and n (or (not distance) (< n distance)))
          (setq chosen entry distance n))))
    (or chosen (total-review--git-version state (plist-get state :active))
        (car versions))))

(defun total-review--git-transfer (root old-blob old-flags new-blob)
  "Transfer OLD-FLAGS from OLD-BLOB to NEW-BLOB in ROOT.
If the historical source object has been garbage-collected, return all
pending flags rather than incorrectly guessing reviewed lines."
  (let ((new-buffer (total-review--git-blob-buffer root new-blob))
        old-buffer)
    (unwind-protect
        (let ((new-count (with-current-buffer new-buffer
                           (total-review--line-count))))
          (condition-case nil
              (setq old-buffer (total-review--git-blob-buffer root old-blob))
            (error nil))
          (if (not old-buffer)
              (make-string new-count ?0)
            (unless (= (length old-flags)
                       (with-current-buffer old-buffer (total-review--line-count)))
              (error "Stored Git review bits disagree with their baseline"))
            (with-current-buffer new-buffer
              (total-review--transfer
               old-flags new-count (total-review--hunks old-buffer)))))
      (when (buffer-live-p new-buffer) (kill-buffer new-buffer))
      (when (buffer-live-p old-buffer) (kill-buffer old-buffer)))))

(defun total-review--snapshot-to-git (path state info directory)
  "Convert a snapshot STATE to Git baseline INFO without keeping old formats.
The old snapshot is deleted only after new state metadata is written."
  (let* ((snapshot (total-review--snapshot-path state directory))
         (flags (plist-get state :flags))
         (committed (total-review--git-blob-buffer
                     (plist-get info :root) (plist-get info :blob)))
         new-flags)
    (unwind-protect
        (with-current-buffer committed
          (setq new-flags
                (total-review--transfer flags (total-review--line-count)
                                        (total-review--hunks snapshot))))
      (when (buffer-live-p committed) (kill-buffer committed)))
    (total-review--write-state path (total-review--git-state path info new-flags)
                               directory)
    (ignore-errors (delete-file snapshot))))

(defun total-review--line-map (old-count new-count hunks)
  "Map unchanged lines of NEW to their OLD zero-based indices.
Changed lines have nil mappings.  OLD-COUNT and NEW-COUNT are line counts."
  (let ((mapping (make-vector new-count nil))
        (old-position 0) (new-position 0))
    (dolist (hunk hunks)
      (pcase-let ((`(,old-start ,old-size ,new-start ,new-size) hunk))
        (let* ((old-before (if (zerop old-size) old-start (1- old-start)))
               (new-before (if (zerop new-size) new-start (1- new-start)))
               (old-gap (- old-before old-position))
               (new-gap (- new-before new-position)))
          (unless (and (>= old-gap 0) (= old-gap new-gap)
                       (<= (+ old-before old-size) old-count)
                       (<= (+ new-before new-size) new-count))
            (error "Inconsistent diff hunk: %S" hunk))
          (dotimes (i old-gap)
            (aset mapping (+ new-position i) (+ old-position i)))
          (setq old-position (+ old-before old-size)
                new-position (+ new-before new-size)))))
    (unless (= (- old-count old-position) (- new-count new-position))
      (error "Diff did not account for all lines"))
    (dotimes (i (- new-count new-position))
      (aset mapping (+ new-position i) (+ old-position i)))
    mapping))

(defun total-review--git-sync (path state directory info)
  "Reconcile Git STATE with current committed INFO and working buffer."
  (unless (and info
               (equal (plist-get state :git-root) (plist-get info :root))
               (equal (plist-get state :git-path) (plist-get info :relative)))
    (user-error "Git file not present at its original path in HEAD; review state kept"))
  (let* ((blob (plist-get info :blob))
         (version (total-review--git-version state blob))
         (changed nil))
    (unless version
      (let* ((from (total-review--git-inheritance-source state info))
             (flags (if from
                        (total-review--git-transfer
                         (plist-get info :root) (plist-get from :blob)
                         (plist-get from :flags) blob)
                      (let ((committed (total-review--git-blob-buffer
                                        (plist-get info :root) blob)))
                        (unwind-protect
                            (with-current-buffer committed
                              (make-string (total-review--line-count) ?0))
                          (kill-buffer committed))))))
        (setq version (list :blob blob :commit (plist-get info :head)
                            :flags flags))
        (setq state (plist-put state :versions
                               (cons version (plist-get state :versions))))
        (setq changed t)))
    (unless (equal (plist-get state :active) blob)
      (setq state (plist-put state :active blob)
            changed t))
    (when changed
      (total-review--write-state path state directory))
    (let ((committed (total-review--git-blob-buffer
                      (plist-get info :root) blob)))
      (unwind-protect
          (let* ((flags (plist-get version :flags))
                 (base-count (with-current-buffer committed
                               (total-review--line-count)))
                 (work-count (total-review--line-count))
                 (hunks (total-review--hunks committed))
                 (mapping (total-review--line-map base-count work-count hunks))
                 (working (make-string work-count ?-)))
            (unless (= base-count (length flags))
              (error "Stored Git flags disagree with committed baseline"))
            (dotimes (i work-count)
              (let ((old (aref mapping i)))
                (when old (aset working i (aref flags old)))))
            (setq total-review--status working
                  total-review--base-flags flags
                  total-review--mapping mapping)
            working)
        (kill-buffer committed)))))

(defun total-review--hunks (snapshot)
  "Return zero-context diff hunks from SNAPSHOT to current buffer.
Each hunk is (OLD-START OLD-COUNT NEW-START NEW-COUNT).  Fail closed
if diff reports binary changes, errors, or unexpected output."
  (let ((diff-buffer (generate-new-buffer " *total-review-diff*"))
        (diff-entire-buffers t)
        hunks)
    (unwind-protect
        (progn
          (diff-no-select snapshot (current-buffer) "-U0" t diff-buffer)
          (with-current-buffer diff-buffer
            (goto-char (point-min))
            (when (or (search-forward "Diff finished (diff error)" nil t)
                      (progn (goto-char (point-min))
                             (re-search-forward "^Binary files .* differ" nil t)))
              (error "Diff failed or reported binary files; review data unchanged"))
            (goto-char (point-min))
            (while (re-search-forward
                    "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                    nil t)
              (push (list (string-to-number (match-string 1))
                          (if (match-string 2)
                              (string-to-number (match-string 2)) 1)
                          (string-to-number (match-string 3))
                          (if (match-string 4)
                              (string-to-number (match-string 4)) 1))
                    hunks))
            (unless (or hunks
                        (progn (goto-char (point-min))
                               (search-forward "Diff finished (no differences)" nil t)))
              (error "Unexpected diff output; review data unchanged")))
          (nreverse hunks))
      (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer)))))

(defun total-review--transfer (old-flags new-count hunks)
  "Transfer OLD-FLAGS to NEW-COUNT lines using zero-context HUNKS.
Only unchanged lines keep their review status."
  (let ((new-flags (make-string new-count ?0))
        (old-processed 0)
        (new-processed 0)
        (old-count (length old-flags)))
    (dolist (hunk hunks)
      (pcase-let ((`(,old-start ,old-size ,new-start ,new-size) hunk))
        ;; Zero-size ranges are positioned after the given line.
        (let* ((old-before (if (zerop old-size) old-start (1- old-start)))
               (new-before (if (zerop new-size) new-start (1- new-start)))
               (old-gap (- old-before old-processed))
               (new-gap (- new-before new-processed)))
          (unless (and (>= old-gap 0) (= old-gap new-gap)
                       (<= (+ old-before old-size) old-count)
                       (<= (+ new-before new-size) new-count))
            (error "Inconsistent diff hunk: %S" hunk))
          (dotimes (j old-gap)
            (aset new-flags (+ new-processed j)
                  (aref old-flags (+ old-processed j))))
          (setq old-processed (+ old-before old-size)
                new-processed (+ new-before new-size)))))
    (let ((old-tail (- old-count old-processed))
          (new-tail (- new-count new-processed)))
      (unless (and (>= old-tail 0) (= old-tail new-tail))
        (error "Diff did not account for all source lines"))
      (dotimes (j old-tail)
        (aset new-flags (+ new-processed j)
              (aref old-flags (+ old-processed j)))))
    new-flags))

(defun total-review--sync ()
  "Reconcile the current buffer with its review baseline; return working flags.
Git baselines are immutable committed versions; snapshot baselines are
updated to match the buffer as before.  Changed Git lines are marked ?-."
  (let* ((path (total-review--path))
         (directory (total-review--database path))
         (state (total-review--load path directory)))
    (unless state
      (user-error "File is not tracked in profile %s" (total-review--profile-label)))
    (let ((info (total-review--git-info path)))
      (when (and (eq (plist-get state :backend) 'snapshot) info)
        (total-review--snapshot-to-git path state info directory)
        (setq state (total-review--load path directory)))
      (if (eq (plist-get state :backend) 'git)
          (total-review--git-sync path state directory info)
        (let* ((snapshot (total-review--snapshot-path state directory))
               (old-flags (plist-get state :flags))
               (new-count (total-review--line-count))
               (old-count (with-temp-buffer
                            (insert-file-contents snapshot)
                            (total-review--line-count))))
          (unless (= (length old-flags) old-count)
            (error "Review snapshot and flags disagree; refusing to overwrite"))
          (let* ((hunks (total-review--hunks snapshot))
                 (flags (if hunks
                            (total-review--transfer old-flags new-count hunks)
                          old-flags)))
            (unless (= (length flags) new-count)
              (error "Review status length does not match current buffer"))
            (when hunks (total-review--save path flags state t directory))
            (setq total-review--status flags
                  total-review--base-flags flags
                  total-review--mapping nil)
            flags))))))

(defun total-review--format-progress-counts (reviewed total)
  "Format REVIEWED / TOTAL baseline progress consistently."
  (format "Reviewed %d / %d lines (%.1f%%), %d pending"
          reviewed total
          (if (zerop total) 100.0 (* 100.0 (/ (float reviewed) total)))
          (- total reviewed)))

(defun total-review--format-progress (flags)
  "Format review progress for baseline FLAGS consistently."
  (total-review--format-progress-counts (cl-count ?1 flags) (length flags)))

(defun total-review--clear-highlights ()
  "Remove only Total Review's overlays."
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'total-review-overlay t)))

(defun total-review--draw (flags)
  "Highlight runs of unreviewed lines according to FLAGS."
  (total-review--clear-highlights)
  (when total-review-mode
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((run-start nil)
              (face (total-review--pending-face)))
          (dotimes (i (length flags))
            (let ((pending (= (aref flags i) ?0)))
              (when (and pending (not run-start))
                (setq run-start (point)))
              (when (and (not pending) run-start)
                (let ((ov (make-overlay run-start (point))))
                  (overlay-put ov 'face face)
                  (overlay-put ov 'total-review-overlay t)
                  (overlay-put ov 'priority -40))
                (setq run-start nil))
              (forward-line 1)))
          (when run-start
            (let ((ov (make-overlay run-start (point-max))))
              (overlay-put ov 'face face)
              (overlay-put ov 'total-review-overlay t)
              (overlay-put ov 'priority -40))))))))

(defun total-review--refresh-pending-faces (&rest _)
  "Recolor pending overlays after a theme change.
Preserve overlays cleared by edits; do not synchronize review data."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when total-review-mode
        (save-restriction
          (widen)
          (let ((face (total-review--pending-face)))
            (dolist (ov (overlays-in (point-min) (point-max)))
              (when (overlay-get ov 'total-review-overlay)
                (overlay-put ov 'face face)))))))))

(add-hook 'enable-theme-functions #'total-review--refresh-pending-faces)
(add-hook 'disable-theme-functions #'total-review--refresh-pending-faces)

(defun total-review--after-edit (&rest _)
  "Clear stale overlays after edits, until the next refresh or save."
  (setq total-review--status nil
        total-review--base-flags nil
        total-review--mapping nil)
  (total-review--clear-highlights))

(defun total-review--after-save ()
  "Refresh the current tracked buffer after saving."
  (condition-case err
      (total-review-refresh)
    (error (message "Total Review refresh failed: %s"
                    (error-message-string err)))))

;;;###autoload
(define-minor-mode total-review-mode
  "Highlight unreviewed source lines in this tracked buffer.
Highlights are cleared after edits and rebuilt on save or refresh."
  :lighter " Review"
  (if total-review-mode
      (condition-case err
          (progn
            (total-review-refresh)
            (setq total-review--wanted t)
            (add-hook 'after-change-functions #'total-review--after-edit nil t)
            (add-hook 'after-save-hook #'total-review--after-save nil t))
        (error
         (setq total-review-mode nil)
         (signal (car err) (cdr err))))
    (remove-hook 'after-change-functions #'total-review--after-edit t)
    (remove-hook 'after-save-hook #'total-review--after-save t)
    (unless total-review--switching-profile
      (setq total-review--wanted nil))
    (setq total-review--status nil
          total-review--base-flags nil
          total-review--mapping nil)
    (total-review--clear-highlights)))

(defun total-review--turn-on ()
  "Enable highlighting for a tracked local file, if applicable."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (file-regular-p buffer-file-name)
             (total-review--load (file-truename buffer-file-name)))
    (total-review-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-total-review-mode
  total-review-mode total-review--turn-on
  :group 'total-review)

(defun total-review--activate-file (path)
  "Activate highlights in a visiting buffer for newly tracked PATH."
  (let ((buffer (find-buffer-visiting path)))
    (when buffer
      (with-current-buffer buffer
        (when (and (or global-total-review-mode total-review--wanted)
                   (not total-review-mode))
          (total-review-mode 1))))))

(defun total-review--track-path (file)
  "Track local FILE if not tracked, using HEAD when available.
A file not committed in Git uses a private snapshot fallback."
  (let* ((path (total-review--path file))
         (directory (total-review--database path)))
    (unless (total-review--load path directory)
      (let ((info (total-review--git-info path)))
        (if info
            (let ((committed (total-review--git-blob-buffer
                              (plist-get info :root) (plist-get info :blob))))
              (unwind-protect
                  (total-review--write-state
                   path (total-review--git-state
                         path info (with-current-buffer committed
                                     (make-string (total-review--line-count) ?0)))
                   directory)
                (kill-buffer committed)))
          (let ((buffer (find-buffer-visiting path)))
            (if buffer
                (with-current-buffer buffer
                  (total-review--save
                   path (make-string (total-review--line-count) ?0)
                   nil t directory))
              (with-temp-buffer
                (insert-file-contents path)
                (total-review--save
                 path (make-string (total-review--line-count) ?0)
                 nil t directory))))))
      (total-review--activate-file path)
      t)))

;;;###autoload
(defun total-review-track ()
  "Track the current file; existing review data is preserved."
  (interactive)
  (let ((path (total-review--path)))
    (if (total-review--track-path path)
        (message "Total Review: tracking %s" path)
      (message "Total Review: already tracking %s" path))
    (when total-review-mode (total-review-refresh))))

(defun total-review--forget-state (path state directory)
  "Discard review STATE for PATH in DIRECTORY, without changing the source.
Also turn off pending-line highlighting in any buffer visiting PATH."
  ;; Delete metadata first so a failed snapshot deletion can only leave an
  ;; orphaned snapshot, not metadata pointing at a missing snapshot.
  (delete-file (total-review--state-path path directory))
  (when (eq (plist-get state :backend) 'snapshot)
    (delete-file (total-review--snapshot-path state directory)))
  (let ((buffer (find-buffer-visiting path)))
    (when buffer
      (with-current-buffer buffer
        (when total-review-mode
          (total-review-mode -1))
        (setq total-review--wanted nil
              total-review--status nil
              total-review--base-flags nil
              total-review--mapping nil)
        (total-review--clear-highlights)))))

;;;###autoload
(defun total-review-untrack ()
  "Stop tracking the current file in the active review profile.
After confirmation, delete its review state and snapshot, but never
modify the source file or data belonging to other profiles."
  (interactive)
  (let* ((path (total-review--path))
         (directory (total-review--database path))
         (state (total-review--load path directory)))
    (unless state
      (user-error "File is not tracked in profile %s"
                  (total-review--profile-label)))
    (when (yes-or-no-p
           (format "Discard Total Review progress for %s in profile %s? "
                   (abbreviate-file-name path)
                   (total-review--profile-label)))
      (total-review--forget-state path state directory)
      (message "Total Review: stopped tracking %s" path)
      t)))

;;;###autoload
(defun total-review-track-dired ()
  "Track Dired's marked regular files, or the file at point.
Explicitly selected files bypass the automatic project-file filter."
  (interactive)
  (require 'dired)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command must be run in Dired"))
  (let ((files (dired-get-marked-files)) (added 0) (skipped 0))
    (dolist (file files)
      (if (and (file-regular-p file) (not (file-remote-p file)))
          (when (total-review--track-path file)
            (cl-incf added))
        (cl-incf skipped)))
    (message "Total Review: added %d file(s); %d non-regular/remote skipped"
             added skipped)))

;;;###autoload
(defun total-review-untrack-dired ()
  "Untrack Dired's marked files, or the file at point.
Ask once before discarding review data for tracked files.  Never
modify the source files or review data in other profiles."
  (interactive)
  (require 'dired)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command must be run in Dired"))
  (let (tracked seen)
    (dolist (file (dired-get-marked-files))
      (when (and (not (file-remote-p file)) (file-regular-p file))
        (let ((path (file-truename file)))
          (unless (member path seen)
            (push path seen)
            (let* ((directory (total-review--database path))
                   (state (total-review--load path directory)))
              (when state
                (push (list path state directory) tracked)))))))
    (if (null tracked)
        (message "Total Review: no marked files are tracked in profile %s"
                 (total-review--profile-label))
      (when (yes-or-no-p
             (format "Discard Total Review progress for %d marked file(s) in profile %s? "
                     (length tracked) (total-review--profile-label)))
        (dolist (entry tracked)
          (pcase-let ((`(,path ,state ,directory) entry))
            (total-review--forget-state path state directory)))
        (message "Total Review: stopped tracking %d file(s)" (length tracked))))))

(defconst total-review--binary-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "ico" "bmp" "tif" "tiff"
    "pdf" "zip" "gz" "xz" "zst" "7z" "so" "o" "a"
    "elc" "eln" "exe" "dll" "dylib" "pyc" "class"
    "mp3" "wav" "ogg" "flac" "mp4" "mov" "sqlite")
  "Common binary extensions excluded from automatic project tracking.")

(defun total-review--project-file-skip-reason (file)
  "Return why FILE is excluded from automatic tracking, or nil if eligible.
This uses the same rules as `total-review--text-file-p'."
  (condition-case err
      (cond
       ((file-remote-p file) "remote file")
       ((file-symlink-p file) "symbolic link")
       ((not (file-regular-p file)) "not a regular file")
       ((and (file-directory-p total-review-directory)
             (file-in-directory-p file total-review-directory))
        "inside the Total Review database")
       (t
        (let ((size (file-attribute-size (file-attributes file))))
          (cond
           ((> size (* 1024 1024)) "larger than 1 MiB")
           ((member (downcase (or (file-name-extension file) ""))
                    total-review--binary-extensions)
            "binary file extension")
           ((with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally file nil 0 (min size 8192))
              (goto-char (point-min))
              (search-forward (string 0) nil t))
            "NUL byte in first 8 KiB")
           (t nil)))))
    (error (format "cannot inspect file: %s" (error-message-string err)))))

(defun total-review--text-file-p (file)
  "Return non-nil if FILE is suitable for automatic project tracking.
Reject files over 1 MiB, common binary extensions, and files containing
NUL bytes in their first 8 KiB.  Do not use external detection programs."
  (not (total-review--project-file-skip-reason file)))

(defun total-review--project-selection (project)
  "Return (ELIGIBLE . SKIPPED) from PROJECT's `project-files'.
ELIGIBLE contains absolute paths.  SKIPPED contains (FILE . REASON)
pairs, with absolute FILE paths.  Both lists preserve project order."
  (let ((root (project-root project))
        eligible skipped)
    (dolist (file (project-files project))
      (let* ((path (expand-file-name file root))
             (reason (total-review--project-file-skip-reason path)))
        (if reason
            (push (cons path reason) skipped)
          (push path eligible))))
    (cons (nreverse eligible) (nreverse skipped))))

(defun total-review--show-skipped-project-files (root skipped)
  "Display SKIPPED files and reasons, relative to project ROOT."
  (with-help-window "*Total Review Skipped*"
    (princ (format "Total Review: %d file(s) skipped in %s\n\n"
                   (length skipped) root))
    (dolist (entry skipped)
      (princ (format "%s — %s\n"
                     (file-relative-name (car entry) root)
                     (cdr entry))))
    (princ "\nOnly files returned by `project-files' are included.\n")))

;;;###autoload
(defun total-review-list-skipped-project-files ()
  "List files filtered out by automatic tracking in the current project.
Does not change review state.  Files excluded by the project backend
itself (such as Git-ignored files) are not included."
  (interactive)
  (let* ((project (or (project-current nil)
                      (user-error "Not in a recognized Emacs project")))
         (skipped (cdr (total-review--project-selection project))))
    (if skipped
        (total-review--show-skipped-project-files
         (project-root project) skipped)
      (message "Total Review: no files skipped by the project filter"))))

;;;###autoload
(defun total-review-track-project ()
  "Track eligible files in the current project, using `project-files'.
Project exclusions are respected.  Confirm the selection before adding.
Show the skipped file names and reasons in a separate buffer."
  (interactive)
  (let* ((project (or (project-current nil)
                      (user-error "Not in a recognized Emacs project")))
         (root (project-root project))
         (forgotten (plist-get
                     (total-review--enumerate (total-review--database root))
                     :forgotten))
         (selection (total-review--project-selection project))
         (eligible (car selection))
         (excluded (cdr selection))
         (skipped (length excluded))
         (added 0)
         (existing 0))
    (when (and eligible
               (y-or-n-p
                (format "Track %d eligible file(s) in %s? (%d skipped) "
                        (length eligible) root skipped)))
      (dolist (file (delete-dups eligible))
        (if (total-review--track-path file)
            (cl-incf added)
          (cl-incf existing)))
      (message "Total Review: %d added, %d already tracked, %d excluded%s"
               added existing skipped
               (if (zerop forgotten) ""
                 (format ", %d deleted forgotten" forgotten))))
    (unless eligible
      (message "Total Review: no eligible files (%d excluded)%s" skipped
               (if (zerop forgotten) ""
                 (format "; %d deleted forgotten" forgotten))))
    (when excluded
      (total-review--show-skipped-project-files root excluded))))

;;;###autoload
(defun total-review-refresh ()
  "Map the current file against its baseline and repaint highlights."
  (interactive)
  (let ((flags (total-review--sync)))
    (total-review--draw flags)
    (when (called-interactively-p 'interactive)
      (message "Total Review: %s"
               (total-review--format-progress total-review--base-flags)))))

(defun total-review--line-range (count)
  "Return zero-based (FIRST . LAST) selected line indices within COUNT."
  (when (zerop count) (user-error "File has no lines to mark"))
  (save-restriction
    (widen)
    (if (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (first (1- (line-number-at-pos beg)))
               (last (1- (line-number-at-pos (max beg (1- end))))))
          (cons (min (1- count) first) (min (1- count) last)))
      (let ((line (min (1- count) (1- (line-number-at-pos)))))
        (cons line line)))))

(defun total-review--set-lines (value)
  "Set current or selected reviewable lines to VALUE (?1 or ?0).
Skip changed Git lines, which have no corresponding committed review bits.
If the selection has no reviewable lines, do nothing silently."
  (let* ((working (copy-sequence (total-review--sync)))
         (path (total-review--path))
         (directory (total-review--database path))
         (state (total-review--load path directory))
         (baseline (copy-sequence total-review--base-flags))
         (mapping total-review--mapping)
         (range (unless (zerop (length working))
                  (total-review--line-range (length working))))
         (reviewable 0))
    (when range
      (cl-loop for i from (car range) to (cdr range)
               for old = (if mapping (aref mapping i) i)
               when old
               do (progn
                    (aset baseline old value)
                    (aset working i value)
                    (cl-incf reviewable))))
    (when (> reviewable 0)
      (if (eq (plist-get state :backend) 'git)
          (let* ((blob (plist-get state :active))
                 (version (total-review--git-version state blob)))
            (unless version (error "Active Git review version missing"))
            (setf (plist-get version :flags) baseline)
            ;; VERSION is shared with STATE's :versions list.
            (total-review--write-state path state directory))
        (total-review--write-state
         path (list :version 2 :backend 'snapshot :path path
                    :snapshot (plist-get state :snapshot) :flags baseline)
         directory)))
    (setq total-review--status working
          total-review--base-flags baseline)
    (total-review--draw working)
    (when (> reviewable 0)
      (message "Total Review: %s" (total-review--format-progress baseline)))))

;;;###autoload
(defun total-review-mark ()
  "Mark reviewable lines at point or in the region as reviewed.
Silently skip lines changed relative to the committed baseline."
  (interactive)
  (total-review--set-lines ?1))

;;;###autoload
(defun total-review-unmark ()
  "Mark reviewable lines at point or in the region as unreviewed.
Silently skip lines changed relative to the committed baseline."
  (interactive)
  (total-review--set-lines ?0))

(defun total-review--jump (index)
  "Jump to zero-based line INDEX in the current source buffer."
  (widen)
  (goto-char (point-min))
  (forward-line index)
  (recenter))

;;;###autoload
(defun total-review-next ()
  "Jump to the next unreviewed line in the current file, wrapping around."
  (interactive)
  (let* ((flags (total-review--sync))
         (n (length flags))
         (line (save-restriction (widen) (1- (line-number-at-pos))))
         (found nil))
    (when (zerop n) (user-error "File has no lines"))
    (let ((step 0))
      (while (and (< step n) (not found))
        (setq step (1+ step))
        (let ((i (mod (+ line step) n)))
          (when (= (aref flags i) ?0) (setq found i)))))
    (unless found
      (user-error "No unchanged pending lines in the buffer (%d pending in baseline)"
                  (cl-count ?0 total-review--base-flags)))
    (total-review--draw flags)
    (total-review--jump found)))

;;;###autoload
(defun total-review-random ()
  "Jump to a random unreviewed line in the current file."
  (interactive)
  (let* ((flags (total-review--sync))
         (pending (cl-loop for i below (length flags)
                           when (= (aref flags i) ?0) collect i)))
    (unless pending
      (user-error "No unchanged pending lines in the buffer (%d pending in baseline)"
                  (cl-count ?0 total-review--base-flags)))
    (total-review--draw flags)
    (total-review--jump (nth (random (length pending)) pending))))

;;;###autoload
(defun total-review-progress ()
  "Display baseline review progress for the current tracked file."
  (interactive)
  (total-review--sync)
  (message "Total Review: %s"
           (total-review--format-progress total-review--base-flags)))

(defun total-review--enumerate (directory)
  "List tracked files in DIRECTORY; forget deleted snapshot-backed files.
Return :paths, :errors and :forgotten.  Missing Git-backed paths are
retained to protect review states across branch switches.  An inaccessible
parent directory is not evidence that a snapshot-backed file was deleted."
  (let (paths errors (forgotten 0))
    (when (file-directory-p directory)
      (dolist (file (directory-files directory t "\\.state\\'" t))
        (condition-case err
            (let* ((state (total-review--read-state-file file directory))
                   (path (plist-get state :path)))
              (cond
               ((file-exists-p path)
                (push path paths))
               ((eq (plist-get state :backend) 'git)
                ;; A branch switch may remove this path.  Never delete its
                ;; committed review history based on worktree absence.
                (push (format "%s: missing from worktree; Git review state preserved"
                              path)
                      errors))
               ((file-accessible-directory-p (file-name-directory path))
                (total-review--forget-state path state directory)
                (cl-incf forgotten))
               (t
                (push (format "%s: parent directory inaccessible; review data preserved"
                              path)
                      errors))))
          (error (push (format "%s: %s" file (error-message-string err))
                       errors)))))
    (list :paths (nreverse paths)
          :errors (nreverse errors)
          :forgotten forgotten)))

(defun total-review--sync-path (path)
  "Return the review summary for PATH, considering visiting buffers.
The returned baseline flags are independent of uncommitted edits."
  (let ((buffer (find-buffer-visiting path)))
    (if buffer
        (with-current-buffer buffer
          (let ((working (total-review--sync)))
            (when total-review-mode (total-review--draw working))
            (list :working working :baseline total-review--base-flags
                  :mapping total-review--mapping)))
      (with-temp-buffer
        (insert-file-contents path)
        ;; Only dynamically bind the visiting name during synchronization.
        (let* ((buffer-file-name path)
               (working (total-review--sync)))
          (list :working working :baseline total-review--base-flags
                :mapping total-review--mapping))))))

(defun total-review--collect (directory)
  "Collect progress using committed or snapshot baselines in DIRECTORY.
The review denominator is the full baseline, including committed lines
that are no longer visible in the working buffer.  :navigable counts
pending lines currently present and unchanged in the working buffer."
  (let* ((enumeration (total-review--enumerate directory))
         (paths (plist-get enumeration :paths))
         (errors (plist-get enumeration :errors))
         entries (reviewed 0) (total 0) (navigable 0)
         (changed 0) (hidden 0))
    (dolist (path paths)
      (condition-case err
          (let* ((progress (total-review--sync-path path))
                 (baseline (plist-get progress :baseline))
                 (working (plist-get progress :working))
                 (mapping (plist-get progress :mapping))
                 (n (length baseline))
                 (r (cl-count ?1 baseline))
                 (visible (cl-count ?0 working))
                 (new (cl-count ?- working))
                 (old-hidden (if mapping
                                 (- n (cl-count-if #'identity mapping))
                               0)))
            (push (list path r (- n r) n visible new old-hidden) entries)
            (cl-incf reviewed r)
            (cl-incf total n)
            (cl-incf navigable visible)
            (cl-incf changed new)
            (cl-incf hidden old-hidden))
        (error (push (format "%s: %s" path (error-message-string err))
                     errors))))
    (list :entries (sort entries (lambda (a b) (string-lessp (car a) (car b))))
          :errors (nreverse errors)
          :forgotten (plist-get enumeration :forgotten)
          :reviewed reviewed :total total :pending (- total reviewed)
          :navigable navigable :working-changed changed :baseline-hidden hidden)))

(defun total-review--pending-index (flags ordinal)
  "Return the index of the ORDINAL-th unreviewed line in FLAGS."
  (let ((index 0))
    (while (and (< index (length flags))
                (or (/= (aref flags index) ?0) (> ordinal 0)))
      (when (= (aref flags index) ?0)
        (setq ordinal (1- ordinal)))
      (setq index (1+ index)))
    (unless (< index (length flags))
      (error "Pending line count changed during navigation"))
    index))

;;;###autoload
(defun total-review-random-project ()
  "Jump to a random visible, unchanged, pending line in this review scope.
Committed lines absent or modified in the working buffer cannot be visited.
Every navigable pending line has an equal chance of selection."
  (interactive)
  (let* ((scope (total-review--scope))
         (stats (total-review--collect (plist-get scope :directory)))
         (pending (plist-get stats :navigable))
         (target (and (> pending 0) (random pending)))
         chosen)
    (when (plist-get stats :errors)
      (message "Total Review: %d tracked file(s) could not be read"
               (length (plist-get stats :errors))))
    (unless target
      (user-error "No reviewable pending lines in this %s database (%d pending in baselines)"
                  (plist-get scope :kind) (plist-get stats :pending)))
    (dolist (entry (plist-get stats :entries))
      (when (and (not chosen) (< target (nth 4 entry)))
        (setq chosen (cons (car entry) target)))
      (unless chosen (setq target (- target (nth 4 entry)))))
    (let* ((path (car chosen))
           (progress (total-review--sync-path path))
           (index (total-review--pending-index
                   (plist-get progress :working) (cdr chosen))))
      (find-file path)
      (unless total-review-mode (total-review-mode 1))
      (total-review--jump index)
      (message "Total Review: unreviewed line %d of %s"
               (1+ index) (abbreviate-file-name path)))))

(defun total-review--report-visit (button)
  "Visit the source file stored on BUTTON."
  (find-file (button-get button 'total-review-file)))

;;;###autoload
(defun total-review-report ()
  "Show a report for the current project or global fallback database.
Progress counts committed baselines for Git files and the current
snapshot for other files, even when visiting buffers have unsaved edits."
  (interactive)
  (let* ((scope (total-review--scope))
         (dir (plist-get scope :directory))
         (stats (total-review--collect dir))
         (entries (plist-get stats :entries))
         (errors (plist-get stats :errors))
         (reviewed (plist-get stats :reviewed))
         (total (plist-get stats :total))
         (root (plist-get scope :root))
         (source-dir default-directory)
         (out (get-buffer-create "*Total Review Report*")))
    (with-current-buffer out
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory (or root source-dir))
        (insert (format "Total Review — %s profile\n"
                        (total-review--profile-label)))
        (insert (format "Scope: %s\nDatabase: %s\n\n"
                        (or root "Global (files outside projects)") dir))
        (insert (format "Tracked files: %d\n" (length entries)))
        (insert (total-review--format-progress-counts reviewed total) "\n")
        (when (or (> (or (plist-get stats :working-changed) 0) 0)
                  (> (or (plist-get stats :baseline-hidden) 0) 0))
          (insert (format "Working changes: %d new/modified buffer lines, %d committed lines absent/modified\n"
                          (or (plist-get stats :working-changed) 0)
                          (or (plist-get stats :baseline-hidden) 0))))
        (insert "\n")
        (when (> (or (plist-get stats :forgotten) 0) 0)
          (insert (format "Forgot %d deleted tracked file(s).\n\n"
                          (plist-get stats :forgotten))))
        (when errors
          (insert (format "WARNING: %d file(s) could not be processed; totals exclude them.\n\n"
                          (length errors)))
          (insert "Errors (not counted as reviewed):\n")
          (dolist (error-message errors)
            (insert "  " error-message "\n"))
          (insert "\n"))
        (insert (format "%-9s %-9s %-9s %s\n"
                        "Reviewed" "Pending" "Total" "File"))
        (insert (make-string 65 ?-) "\n")
        (dolist (entry entries)
          (pcase-let ((`(,path ,r ,p ,n) entry))
            (insert (format "%-9d %-9d %-9d " r p n))
            (insert-text-button
             (if root (file-relative-name path root)
               (abbreviate-file-name path))
             'follow-link t
             'total-review-file path
             'action #'total-review--report-visit)
            (insert "\n")))
        (insert (make-string 65 ?-) "\n")
        (insert (format "%-9d %-9d %-9d %s\n"
                        reviewed (plist-get stats :pending) total "Totals"))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer out)))

(defun total-review--known-profiles ()
  "Return names of existing profiles, including default."
  (let ((named-dir (expand-file-name "named/" total-review-directory)))
    (cons "default"
          (when (file-directory-p named-dir)
            (cl-remove-if-not
             (lambda (name)
               (and (file-directory-p (expand-file-name name named-dir))
                    (string-match-p
                     "\\`[[:alnum:]][[:alnum:]_.-]*\\'" name)))
             (directory-files named-dir nil "^[^.].*" t))))))

(defun total-review--refresh-open-buffers ()
  "Re-evaluate review mode in open buffers after a profile change."
  (let ((total-review--switching-profile t))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when buffer-file-name
          (when total-review-mode (total-review-mode -1))
          (when (and (or total-review--wanted global-total-review-mode)
                     (not (file-remote-p buffer-file-name))
                     (file-regular-p buffer-file-name)
                     (total-review--load (file-truename buffer-file-name)))
            (total-review-mode 1)))))))

;;;###autoload
(defun total-review-switch-profile (name)
  "Switch to review profile NAME, updating already open review buffers.
NAME is a string, for example default or first-pass.  New names are
created when tracking the first file.  Each profile maintains its own
databases per project and for non-project files."
  (interactive
   (list (completing-read "Total Review profile (default or name): "
                          (total-review--known-profiles) nil nil nil nil
                          (total-review--profile-label))))
  (let ((total-review-profile name))
    (total-review--profile-root))
  (setq total-review-profile name)
  (total-review--refresh-open-buffers)
  (message "Total Review profile: %s" (total-review--profile-label)))

(provide 'total-review)
;;; total-review.el ends here
