;;; total-review.el --- Track line review coverage using diffs  -*- lexical-binding: t; -*-

;; Version: 0.2.6
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience

;;; Commentary:
;;
;; Total Review stores a plain-text snapshot and a string of 0/1 review
;; flags for each tracked local file.  GNU diff, through `diff-no-select',
;; carries flags over unchanged lines whenever the source changes.
;; New and modified lines become unreviewed.  The source is never edited
;; or saved by this package.
;;
;; Database profiles: "default" or a string such as "first-pass".  Each profile
;; has an independent database for each `project.el' project, plus a
;; shared fallback database for files outside projects.  No data is
;; stored in source repositories.
;;
;; A local GNU-compatible `diff' executable is required.  This is an
;; experimental package; back up review data while testing it.

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
  "Status flags last loaded for this buffer.")

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
  (expand-file-name (concat (total-review--id path) ".el")
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
  "Read and validate FILE in DIRECTORY, returning its state plist."
  (let* ((state (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (read (current-buffer))))
         (path (plist-get state :path)))
    (unless (and (listp state)
                 (equal (plist-get state :version) 1)
                 (stringp path) (file-name-absolute-p path)
                 (equal (file-name-nondirectory file)
                        (concat (total-review--id path) ".el"))
                 (stringp (plist-get state :flags))
                 (string-match-p "\\`[01]*\\'" (plist-get state :flags))
                 (file-regular-p (total-review--snapshot-path state directory)))
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
            (insert ";; Total Review state, version 1.\n")
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
           path (list :version 1 :path path
                      :snapshot (file-name-nondirectory snapshot)
                      :flags flags)
           dir)
          (setq committed t)
          (when (and new old (file-exists-p old))
            (ignore-errors (delete-file old))))
      (when (and new (not committed) (file-exists-p new))
        (delete-file new)))))

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
  "Synchronize current buffer with its saved snapshot; return flags.
Unsaved buffer changes are included in the new snapshot if diffed.
The source file itself is never saved."
  (let* ((path (total-review--path))
         (directory (total-review--database path))
         (state (total-review--load path directory)))
    (unless state
      (user-error "File is not tracked in profile %s" (total-review--profile-label)))
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
        (when hunks
          (total-review--save path flags state t directory))
        (setq total-review--status flags)
        flags))))

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
  (setq total-review--status nil)
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
    (setq total-review--status nil)
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
  "Track local FILE if not already tracked.  Return t if newly added.
Use a visiting buffer when available, preserving its unsaved changes."
  (let* ((path (total-review--path file))
         (directory (total-review--database path)))
    (unless (total-review--load path directory)
      (let ((buffer (find-buffer-visiting path)))
        (if buffer
            (with-current-buffer buffer
              (total-review--save
               path (make-string (total-review--line-count) ?0)
               nil t directory))
          (with-temp-buffer
            ;; This buffer is not visiting PATH: assigning `buffer-file-name'
            ;; would prompt to save when the modified temp buffer is killed.
            (insert-file-contents path)
            (total-review--save
             path (make-string (total-review--line-count) ?0)
             nil t directory))))
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
      (message "Total Review: %d added, %d already tracked, %d excluded"
               added existing skipped))
    (unless eligible
      (message "Total Review: no eligible files (%d excluded)" skipped))
    (when excluded
      (total-review--show-skipped-project-files root excluded))))

;;;###autoload
(defun total-review-refresh ()
  "Diff the current file against its snapshot and repaint highlights."
  (interactive)
  (let ((flags (total-review--sync)))
    (total-review--draw flags)
    (when (called-interactively-p 'interactive)
      (message "Total Review: %d/%d lines reviewed"
               (cl-count ?1 flags) (length flags)))))

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
  "Set current or selected lines to VALUE, either ?1 or ?0."
  (let* ((flags (copy-sequence (total-review--sync)))
         (path (total-review--path))
         (directory (total-review--database path))
         (state (total-review--load path directory))
         (range (total-review--line-range (length flags))))
    (cl-loop for i from (car range) to (cdr range)
             do (aset flags i value))
    (total-review--write-state
     path (list :version 1 :path path
                :snapshot (plist-get state :snapshot) :flags flags)
     directory)
    (setq total-review--status flags)
    (total-review--draw flags)
    (message "Total Review: %d/%d lines reviewed"
             (cl-count ?1 flags) (length flags))))

;;;###autoload
(defun total-review-mark ()
  "Mark the current line or the selected lines reviewed."
  (interactive)
  (total-review--set-lines ?1))

;;;###autoload
(defun total-review-unmark ()
  "Mark the current line or the selected lines unreviewed."
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
    (unless found (user-error "All %d lines reviewed" n))
    (total-review--draw flags)
    (total-review--jump found)))

;;;###autoload
(defun total-review-random ()
  "Jump to a random unreviewed line in the current file."
  (interactive)
  (let* ((flags (total-review--sync))
         (pending (cl-loop for i below (length flags)
                           when (= (aref flags i) ?0) collect i)))
    (unless pending (user-error "No unreviewed lines remain"))
    (total-review--draw flags)
    (total-review--jump (nth (random (length pending)) pending))))

;;;###autoload
(defun total-review-progress ()
  "Display review progress for the current tracked file."
  (interactive)
  (let* ((flags (total-review--sync))
         (reviewed (cl-count ?1 flags))
         (total (length flags)))
    (message "Total Review: %d/%d lines reviewed (%.1f%%)"
             reviewed total
             (if (zerop total) 100.0 (* 100.0 (/ (float reviewed) total))))))

;;;###autoload
(defun total-review-show-diff ()
  "Show the raw unified diff against the snapshot, without syncing."
  (interactive)
  (let* ((path (total-review--path))
         (directory (total-review--database path))
         (state (or (total-review--load path directory)
                    (user-error "File is not tracked")))
         (diff-entire-buffers t))
    (display-buffer
     (diff-no-select (total-review--snapshot-path state directory)
                     (current-buffer) "-U0" t
                     (get-buffer-create "*Total Review Diff*")))))

(defun total-review--enumerate (directory)
  "Return (PATHS . ERRORS) for database DIRECTORY.
ERRORS contains messages for invalid metadata; never silently count it
as reviewed."
  (let (paths errors)
    (when (file-directory-p directory)
      (dolist (file (directory-files directory t "\\.el\\'" t))
        (condition-case err
            (push (plist-get (total-review--read-state-file file directory)
                             :path)
                  paths)
          (error (push (format "%s: %s" file (error-message-string err))
                       errors)))))
    (cons (nreverse paths) (nreverse errors))))

(defun total-review--sync-path (path)
  "Return synchronized review flags for PATH, including unsaved edits.
An already visiting buffer is used when possible.  Otherwise a temporary
buffer loads the file from disk without visiting it permanently."
  (let ((buffer (find-buffer-visiting path)))
    (if buffer
        (with-current-buffer buffer
          (let ((flags (total-review--sync)))
            (when total-review-mode (total-review--draw flags))
            flags))
      (with-temp-buffer
        (insert-file-contents path)
        ;; `total-review--sync' expects a visiting filename.  Bind it only
        ;; during the call so cleanup cannot prompt to kill a modified buffer.
        (let ((buffer-file-name path))
          (total-review--sync))))))

(defun total-review--collect (directory)
  "Collect project/global progress information from DIRECTORY.
Return a plist with :entries, :errors, :reviewed, :total, :pending.
Synchronize all tracked files first; a failure leaves that file out of
the totals and is explicitly recorded under :errors."
  (let* ((enumeration (total-review--enumerate directory))
         (paths (car enumeration))
         (errors (cdr enumeration))
         entries (reviewed 0) (total 0))
    (dolist (path paths)
      (condition-case err
          (let* ((flags (total-review--sync-path path))
                 (n (length flags))
                 (r (cl-count ?1 flags)))
            (push (list path r (- n r) n) entries)
            (cl-incf reviewed r)
            (cl-incf total n))
        (error (push (format "%s: %s" path (error-message-string err))
                     errors))))
    (list :entries (sort entries (lambda (a b) (string-lessp (car a) (car b))))
          :errors (nreverse errors)
          :reviewed reviewed :total total :pending (- total reviewed))))

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
  "Jump to a random unreviewed line in any tracked file in this scope.
The scope is the current project's database, or the global fallback if
outside a project.  Each pending line has the same chance of selection."
  (interactive)
  (let* ((scope (total-review--scope))
         (stats (total-review--collect (plist-get scope :directory)))
         (pending (plist-get stats :pending))
         (target (and (> pending 0) (random pending)))
         chosen)
    (when (plist-get stats :errors)
      (message "Total Review: %d tracked file(s) could not be read"
               (length (plist-get stats :errors))))
    (unless target
      (user-error "No unreviewed lines in this %s database"
                  (plist-get scope :kind)))
    (dolist (entry (plist-get stats :entries))
      (when (and (not chosen) (< target (nth 2 entry)))
        (setq chosen (cons (car entry) target)))
      (unless chosen (setq target (- target (nth 2 entry)))))
    (let* ((path (car chosen))
           (flags (total-review--sync-path path))
           (index (total-review--pending-index flags (cdr chosen))))
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
Synchronizes all tracked files, including live unsaved buffers."
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
        (insert (format "Reviewed %d / %d lines (%.1f%%), %d pending\n\n"
                        reviewed total
                        (if (zerop total) 100.0
                          (* 100.0 (/ (float reviewed) total)))
                        (plist-get stats :pending)))
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
