;;; total-review-tests.el --- ERT tests for Total Review  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'total-review)

(defmacro total-review-tests--sandbox (&rest body)
  "Execute BODY with a fresh Total Review database and source directory."
  (declare (indent 0) (debug t))
  `(let* ((total-review-directory (make-temp-file "total-review-db-" t))
          (total-review-profile "default")
          (total-review-tests--sources (make-temp-file "total-review-src-" t)))
     (unwind-protect
         (progn ,@body)
       (when (bound-and-true-p global-total-review-mode)
         (global-total-review-mode -1))
       (delete-directory total-review-directory t)
       (delete-directory total-review-tests--sources t))))

(defun total-review-tests--write (path text)
  "Write TEXT to PATH, returning PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert text))
  path)

(ert-deftest total-review-transfer-insertion ()
  (should (equal (total-review--transfer "101" 4 '((1 0 2 1)))
                 "1001")))

(ert-deftest total-review-transfer-deletion ()
  (should (equal (total-review--transfer "101" 2 '((2 1 1 0)))
                 "11")))

(ert-deftest total-review-transfer-replacement ()
  (should (equal (total-review--transfer "111" 3 '((2 1 2 1)))
                 "101")))

(ert-deftest total-review-transfer-all-inserted ()
  (should (equal (total-review--transfer "" 2 '((0 0 1 2)))
                 "00")))

(ert-deftest total-review-transfer-all-deleted ()
  (should (equal (total-review--transfer "11" 0 '((1 2 0 0)))
                 "")))

(ert-deftest total-review-transfer-invalid-diff ()
  (should-error (total-review--transfer "111" 4 '((1 1 3 1)))))

(ert-deftest total-review-final-newline-and-empty-file ()
  (with-temp-buffer
    (should (= (total-review--line-count) 0))
    (insert "one\n")
    (should (= (total-review--line-count) 1))
    (insert "two")
    (should (= (total-review--line-count) 2))))

(ert-deftest total-review-live-sync ()
  "The snapshot follows edits while preserving unchanged review flags."
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "sample.el" total-review-tests--sources)
                  "alpha\nbeta\ngamma\n"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (should (equal (total-review--sync) "000"))
            (forward-line 1)
            (total-review-mark)
            (should (equal (total-review--sync) "010"))
            (goto-char (point-min))
            (insert "new\n")
            (should (equal (total-review--sync) "0010"))
            (goto-char (point-min))
            (forward-line 2)
            (insert "changed-")
            (should (equal (total-review--sync) "0000"))
            (should (equal
                     (plist-get (total-review--load (total-review--path)) :flags)
                     "0000")))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest total-review-untrack-deletes-review-data-not-source ()
  "Untracking removes metadata, snapshot, and highlights, but not the file."
  (total-review-tests--sandbox
    (let* ((text "alpha\nbeta\n")
           (path (total-review-tests--write
                  (expand-file-name "sample.el" total-review-tests--sources)
                  text))
           (buffer (find-file-noselect path))
           (confirmations 0))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mode 1)
            (total-review-mark)
            (let* ((canonical (total-review--path))
                   (directory (total-review--database canonical))
                   (state (total-review--load canonical directory))
                   (metadata (total-review--state-path canonical directory))
                   (snapshot (total-review--snapshot-path state directory)))
              (should (file-exists-p metadata))
              (should (file-exists-p snapshot))
              (should total-review-mode)
              (should (equal (plist-get state :flags) "10"))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (&rest _)
                           (cl-incf confirmations)
                           t)))
                (should (total-review-untrack)))
              (should (= confirmations 1))
              (should-not (file-exists-p metadata))
              (should-not (file-exists-p snapshot))
              (should-not (total-review--load canonical directory))
              (should-not total-review-mode)
              (should-not total-review--wanted)
              (should-not total-review--status)
              (should-not
               (cl-some (lambda (ov) (overlay-get ov 'total-review-overlay))
                        (overlays-in (point-min) (point-max))))
              (should (equal (with-temp-buffer
                               (insert-file-contents-literally path)
                               (buffer-string))
                             text))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-untrack-cancel-preserves-everything ()
  "Declining confirmation leaves tracking and highlighting unchanged."
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "sample.el" total-review-tests--sources)
                  "alpha\nbeta\n"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mode 1)
            (let* ((canonical (total-review--path))
                   (directory (total-review--database canonical))
                   (state (total-review--load canonical directory))
                   (metadata (total-review--state-path canonical directory))
                   (snapshot (total-review--snapshot-path state directory)))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                (should-not (total-review-untrack)))
              (should (file-exists-p metadata))
              (should (file-exists-p snapshot))
              (should (total-review--load canonical directory))
              (should total-review-mode)
              (should (cl-some
                       (lambda (ov) (overlay-get ov 'total-review-overlay))
                       (overlays-in (point-min) (point-max))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-untrack-untracked-file-errors ()
  "An untracked file cannot be accidentally removed by this command."
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "sample.el" total-review-tests--sources)
                  "alpha\n"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _)
                         (ert-fail "Should not prompt for an untracked file"))))
              (should-error (total-review-untrack) :type 'user-error))
            (should (file-exists-p path)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-untrack-preserves-other-profiles ()
  "Untracking in one profile does not delete another profile's data."
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "sample.el" total-review-tests--sources)
                  "alpha\nbeta\n"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mark)
            (let* ((canonical (total-review--path))
                   (default-directory-path (total-review--database canonical))
                   (default-state (total-review--load canonical)))
              (let ((total-review-profile "second-pass"))
                (total-review-track)
                (should (equal (plist-get (total-review--load canonical) :flags)
                               "00"))
                (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                  (total-review-untrack))
                (should-not (total-review--load canonical)))
              (should (equal (total-review--database canonical)
                             default-directory-path))
              (should (equal (total-review--load canonical) default-state))
              (should (equal (total-review--sync) "10"))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-deleted-file-forgotten-on-collect ()
  "A missing file is removed from the active database on the next scan."
  (total-review-tests--sandbox
    (let* ((missing (total-review-tests--write
                     (expand-file-name "missing.el" total-review-tests--sources)
                     "one\ntwo\n"))
           (survivor (total-review-tests--write
                      (expand-file-name "survivor.el" total-review-tests--sources)
                      "three\n")))
      (total-review--track-path missing)
      (total-review--track-path survivor)
      (let* ((directory (total-review--database missing))
             (state (total-review--load missing directory))
             (metadata (total-review--state-path missing directory))
             (snapshot (total-review--snapshot-path state directory)))
        (delete-file missing)
        (let ((stats (total-review--collect directory)))
          (should (= (plist-get stats :forgotten) 1))
          (should (= (plist-get stats :total) 1))
          (should (= (length (plist-get stats :entries)) 1))
          (should-not (plist-get stats :errors)))
        (should-not (file-exists-p metadata))
        (should-not (file-exists-p snapshot))
        (should (total-review--load survivor directory))
        (should (= (plist-get (total-review--collect directory) :forgotten) 0))))))

(ert-deftest total-review-deleted-file-preserves-other-profile ()
  "Missing file cleanup only touches the profile being scanned."
  (total-review-tests--sandbox
    (let ((path (total-review-tests--write
                 (expand-file-name "vanished.el" total-review-tests--sources)
                 "value\n")))
      (total-review--track-path path)
      (let ((second-profile-state
             (let ((total-review-profile "second-pass"))
               (total-review--track-path path)
               (total-review--state-path path))))
        (delete-file path)
        (should (= (plist-get (total-review--collect
                              (total-review--database path)) :forgotten) 1))
        (should (file-exists-p second-profile-state))
        (let ((total-review-profile "second-pass"))
          (should (= (plist-get (total-review--collect
                                (total-review--database path)) :forgotten) 1)))
        (should-not (file-exists-p second-profile-state))))))

(ert-deftest total-review-deleted-file-with-inaccessible-parent-kept ()
  "A missing path without an accessible parent is not pruned."
  (total-review-tests--sandbox
    (let ((path (total-review-tests--write
                 (expand-file-name "unavailable.el" total-review-tests--sources)
                 "value\n")))
      (total-review--track-path path)
      (let ((metadata (total-review--state-path path)))
        (delete-file path)
        (cl-letf (((symbol-function 'file-accessible-directory-p)
                   (lambda (&rest _) nil)))
          (let ((stats (total-review--collect (total-review--database path))))
            (should (= (plist-get stats :forgotten) 0))
            (should (= (length (plist-get stats :errors)) 1))))
        (should (file-exists-p metadata))))))

(ert-deftest total-review-deleted-file-disables-visiting-highlights ()
  "A deleted tracked file is forgotten even if its buffer is still open."
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "opened.el" total-review-tests--sources)
                  "value\n"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mode 1)
            (should total-review-mode)
            (delete-file path)
            (should (= (plist-get (total-review--collect
                                   (total-review--database path)) :forgotten)
                       1))
            (should-not total-review-mode)
            (should-not total-review--wanted)
            (should-not
             (cl-some (lambda (ov) (overlay-get ov 'total-review-overlay))
                      (overlays-in (point-min) (point-max)))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest total-review-empty-file-is-not-forgotten ()
  "An existing zero-line file must remain tracked."
  (total-review-tests--sandbox
    (let ((path (total-review-tests--write
                 (expand-file-name "empty.el" total-review-tests--sources)
                 "value\n")))
      (total-review--track-path path)
      (total-review-tests--write path "")
      (let ((stats (total-review--collect (total-review--database path))))
        (should (= (plist-get stats :forgotten) 0))
        (should (= (plist-get stats :total) 0))
        (should (total-review--load path))))))

(ert-deftest total-review-untrack-dired-marked-files ()
  "Bulk untrack confirms once and only deletes marked tracked metadata."
  (total-review-tests--sandbox
    (let* ((one (total-review-tests--write
                 (expand-file-name "one.el" total-review-tests--sources)
                 "one\n"))
           (two (total-review-tests--write
                 (expand-file-name "two.el" total-review-tests--sources)
                 "two\n"))
           (extra (total-review-tests--write
                   (expand-file-name "extra.el" total-review-tests--sources)
                   "extra\n"))
           (confirmations 0))
      (mapc #'total-review--track-path (list one two extra))
      (require 'dired)
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                ((symbol-function 'dired-get-marked-files)
                 (lambda (&rest _) (list one two one)))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _)
                   (cl-incf confirmations)
                   t)))
        (total-review-untrack-dired))
      (should (= confirmations 1))
      (should-not (total-review--load one))
      (should-not (total-review--load two))
      (should (total-review--load extra))
      (should (file-exists-p one))
      (should (file-exists-p two)))))

(ert-deftest total-review-untrack-dired-declined ()
  "Bulk untrack cancellation never removes progress."
  (total-review-tests--sandbox
    (let ((path (total-review-tests--write
                 (expand-file-name "one.el" total-review-tests--sources)
                 "one\n")))
      (total-review--track-path path)
      (require 'dired)
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                ((symbol-function 'dired-get-marked-files)
                 (lambda (&rest _) (list path)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (total-review-untrack-dired))
      (should (total-review--load path)))))

(ert-deftest total-review-project-and-global-scopes ()
  "Project roots partition databases, and unprojected files share global."
  (total-review-tests--sandbox
    (let* ((root (file-name-as-directory
                  (expand-file-name "project-a" total-review-tests--sources)))
           (other (file-name-as-directory
                   (expand-file-name "other" total-review-tests--sources)))
           (file-a (total-review-tests--write
                    (expand-file-name "one.el" root) "a\n"))
           (file-b (total-review-tests--write
                    (expand-file-name "two.el" root) "b\n"))
           (file-c (total-review-tests--write
                    (expand-file-name "outside.el" other) "c\n")))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&optional _prompt dir)
                   (when (string-prefix-p root dir) 'fake-project)))
                ((symbol-function 'project-root)
                 (lambda (_project) root)))
        (should (equal (total-review--database file-a)
                       (total-review--database file-b)))
        (should-not (equal (total-review--database file-a)
                           (total-review--database file-c)))
        (let ((default-dir (total-review--database file-a)))
          (setq total-review-profile "first-pass")
          (should-not (equal default-dir (total-review--database file-a)))
          (should (equal (total-review--database file-a)
                         (total-review--database file-b))))))))

(ert-deftest total-review-profile-is-a-string ()
  "Profile settings use a plain string and retain existing database paths."
  (total-review-tests--sandbox
    (should (equal total-review-profile "default"))
    (should (eq (get 'total-review-profile 'custom-type) 'string))
    (should (equal (total-review--profile-root)
                   (expand-file-name "default/" total-review-directory)))
    (setq total-review-profile "first-pass")
    (should (equal (total-review--profile-root)
                   (expand-file-name "named/first-pass/" total-review-directory)))
    (should (equal (total-review--profile-label) "first-pass"))
    (dolist (bad '("" "../escape" "nested/name" default (named "old-style")))
      (let ((total-review-profile bad))
        (should-error (total-review--profile-root))))))

(ert-deftest total-review-profiles-independent ()
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "sample.el" total-review-tests--sources)
                  "first\nsecond\n"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mark)
            (should (equal (total-review--sync) "10"))
            (total-review-switch-profile "second-pass")
            (should (equal total-review-profile "second-pass"))
            (should-not (total-review--load (total-review--path)))
            (total-review-track)
            (should (equal (total-review--sync) "00"))
            (total-review-switch-profile "default")
            (should (equal total-review-profile "default"))
            (should (equal (total-review--sync) "10")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-profile-switch-restores-manual-highlighting ()
  "Manual highlighting returns when a previously tracked profile is restored."
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "sample.el" total-review-tests--sources)
                  "alpha\nbeta\n"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mode 1)
            (should total-review-mode)
            (total-review-switch-profile "other-pass")
            (should-not total-review-mode)
            (should total-review--wanted)
            (total-review-switch-profile "default")
            (should total-review-mode)
            (total-review-mode -1))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-pending-index ()
  (should (= (total-review--pending-index "10100" 0) 1))
  (should (= (total-review--pending-index "10100" 1) 3))
  (should (= (total-review--pending-index "10100" 2) 4))
  (should-error (total-review--pending-index "10100" 3)))

(ert-deftest total-review-binary-diff-fails-closed ()
  "An explicitly tracked binary file must never silently transfer flags."
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "binary.dat" total-review-tests--sources)
                  (concat "alpha" (string 0) "beta")))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (let* ((before (total-review--load (total-review--path)))
                   (snapshot (plist-get before :snapshot)))
              (goto-char (point-min))
              (insert "changed")
              (should-error (total-review--sync))
              (let ((after (total-review--load (total-review--path))))
                (should (equal (plist-get after :snapshot) snapshot))
                (should (equal (plist-get after :flags)
                               (plist-get before :flags))))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest total-review-file-filter ()
  (total-review-tests--sandbox
    (let ((txt (total-review-tests--write
                (expand-file-name "sample.el" total-review-tests--sources)
                "hello\n"))
          (binary (total-review-tests--write
                   (expand-file-name "mystery" total-review-tests--sources)
                   (concat "abc" (string 0) "def")))
          (png (total-review-tests--write
                (expand-file-name "logo.PNG" total-review-tests--sources)
                "plain bytes, but common binary extension"))
          (large (total-review-tests--write
                  (expand-file-name "huge.txt" total-review-tests--sources)
                  (make-string (1+ (* 1024 1024)) ?x))))
      (should (total-review--text-file-p txt))
      (should-not (total-review--text-file-p binary))
      (should-not (total-review--text-file-p png))
      (should-not (total-review--text-file-p large)))))

(ert-deftest total-review-project-file-skip-reasons ()
  "Automatic exclusions identify a specific reason for every skipped file."
  (total-review-tests--sandbox
    (let* ((root total-review-tests--sources)
           (txt (total-review-tests--write (expand-file-name "source.el" root)
                                           "code\n"))
           (binary (total-review-tests--write (expand-file-name "image.png" root)
                                              "plain text with binary extension"))
           (nul (total-review-tests--write (expand-file-name "mystery" root)
                                           (concat "abc" (string 0) "def")))
           (large (total-review-tests--write (expand-file-name "large.txt" root)
                                             (make-string (1+ (* 1024 1024)) ?a)))
           (link (expand-file-name "link.el" root)))
      (make-symbolic-link txt link)
      (should-not (total-review--project-file-skip-reason txt))
      (should (equal (total-review--project-file-skip-reason binary)
                     "binary file extension"))
      (should (equal (total-review--project-file-skip-reason nul)
                     "NUL byte in first 8 KiB"))
      (should (equal (total-review--project-file-skip-reason large)
                     "larger than 1 MiB"))
      (should (equal (total-review--project-file-skip-reason link)
                     "symbolic link")))))

(ert-deftest total-review-list-skipped-project-files ()
  "The inspection command reports skipped paths and reasons without tracking."
  (total-review-tests--sandbox
    (let* ((root (file-name-as-directory
                  (expand-file-name "project" total-review-tests--sources)))
           (source (total-review-tests--write (expand-file-name "main.el" root)
                                              "foo\n"))
           (binary (total-review-tests--write (expand-file-name "icon.png" root)
                                              "xyz"))
           (default-directory root))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) 'fake-project))
                ((symbol-function 'project-root)
                 (lambda (&rest _) root))
                ((symbol-function 'project-files)
                 (lambda (&rest _) (list source binary))))
        (unwind-protect
            (progn
              (total-review-list-skipped-project-files)
              (with-current-buffer "*Total Review Skipped*"
                (should (string-match-p
                         "icon\\.png — binary file extension"
                         (buffer-string)))
                (should-not (string-match-p "main\\.el —"
                                            (buffer-string))))
              (should-not (total-review--load source)))
          (when (get-buffer "*Total Review Skipped*")
            (kill-buffer "*Total Review Skipped*")))))))

(ert-deftest total-review-project-tracking-reports-skipped-files ()
  "Bulk tracking displays files rejected by the automatic filter."
  (total-review-tests--sandbox
    (let* ((root (file-name-as-directory
                  (expand-file-name "project" total-review-tests--sources)))
           (source (total-review-tests--write (expand-file-name "main.el" root)
                                              "foo\n"))
           (binary (total-review-tests--write (expand-file-name "icon.png" root)
                                              "xyz"))
           (default-directory root)
           reported-root reported-skips)
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) 'fake-project))
                ((symbol-function 'project-root)
                 (lambda (&rest _) root))
                ((symbol-function 'project-files)
                 (lambda (&rest _) (list source binary)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'total-review--show-skipped-project-files)
                 (lambda (project-root excluded)
                   (setq reported-root project-root
                         reported-skips excluded))))
        (total-review-track-project)
        (should (equal reported-root root))
        (should (equal reported-skips
                       (list (cons binary "binary file extension"))))
        (should (total-review--load source))
        (should-not (total-review--load binary))))))

(ert-deftest total-review-project-tracking-and-collection ()
  (total-review-tests--sandbox
    (let* ((root (file-name-as-directory
                  (expand-file-name "project" total-review-tests--sources)))
           (one (total-review-tests--write (expand-file-name "one.el" root)
                                           "a\nb\n"))
           (two (total-review-tests--write (expand-file-name "two.el" root)
                                           "c\n"))
           (binary (total-review-tests--write (expand-file-name "logo.png" root)
                                              "not text"))
           (default-directory root))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) 'fake-project))
                ((symbol-function 'project-root)
                 (lambda (&rest _) root))
                ((symbol-function 'project-files)
                 (lambda (&rest _) (list one two binary)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (total-review-track-project)
        (let* ((stats (total-review--collect (total-review--database one)))
               (entries (plist-get stats :entries)))
          (should (= (length entries) 2))
          (should (= (plist-get stats :pending) 3))
          (should (= (plist-get stats :total) 3)))))))

(ert-deftest total-review-track-project-forgets-missing-files ()
  "Project tracking scans its database even when no new files are added."
  (total-review-tests--sandbox
    (let* ((root (file-name-as-directory
                  (expand-file-name "project" total-review-tests--sources)))
           (old (total-review-tests--write (expand-file-name "old.el" root)
                                           "old\n"))
           (current (total-review-tests--write
                     (expand-file-name "current.el" root) "current\n"))
           (default-directory root))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) 'fake-project))
                ((symbol-function 'project-root) (lambda (&rest _) root))
                ((symbol-function 'project-files)
                 (lambda (&rest _) (list current)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (total-review--track-path old)
        (total-review--track-path current)
        (delete-file old)
        (total-review-track-project)
        (should-not (total-review--load old))
        (should (total-review--load current))))))

(ert-deftest total-review-project-tracking-does-not-prompt-for-temp-buffers ()
  "Bulk tracking and progress collection must not kill a temp visiting a file.
A modified temp buffer visiting a file triggers a save/kill confirmation."
  (total-review-tests--sandbox
    (let* ((root (file-name-as-directory
                  (expand-file-name "project" total-review-tests--sources)))
           (one (total-review-tests--write
                 (expand-file-name "one.el" root) "one\n"))
           (two (total-review-tests--write
                 (expand-file-name "two.el" root) "two\n"))
           (default-directory root)
           (original-kill-buffer (symbol-function 'kill-buffer))
           (temp-kills 0))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) 'fake-project))
                ((symbol-function 'project-root)
                 (lambda (&rest _) root))
                ((symbol-function 'project-files)
                 (lambda (&rest _) (list one two)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'kill-buffer)
                 (lambda (&optional buffer-or-name)
                   (let ((target (get-buffer (or buffer-or-name
                                                 (current-buffer)))))
                     (when (and target
                                (string-prefix-p " *temp" (buffer-name target)))
                       (cl-incf temp-kills)
                       (should-not (buffer-local-value 'buffer-file-name
                                                       target))))
                   (funcall original-kill-buffer buffer-or-name))))
        (total-review-track-project)
        (should (total-review--load one))
        (should (total-review--load two))
        ;; Also exercise the temporary buffer used by `--sync-path'.
        (should (= (plist-get (total-review--collect
                               (total-review--database one)) :total)
                   2))
        (should (>= temp-kills 4))))))

(ert-deftest total-review-overlays-highlight-pending ()
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "sample.el" total-review-tests--sources)
                  "a\nb\n"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mode 1)
            (should (cl-some (lambda (ov)
                               (overlay-get ov 'total-review-overlay))
                             (overlays-in (point-min) (point-max))))
            (goto-char (point-min))
            (total-review-mark)
            (should (equal (total-review--sync) "10"))
            (total-review-mode -1)
            (should-not (cl-some (lambda (ov)
                                   (overlay-get ov 'total-review-overlay))
                                 (overlays-in (point-min) (point-max)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-global-mode-activates-tracked-files ()
  (total-review-tests--sandbox
    (let* ((path (total-review-tests--write
                  (expand-file-name "sample.el" total-review-tests--sources)
                  "a\nb\n"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (global-total-review-mode 1)
            (should (bound-and-true-p total-review-mode))
            (global-total-review-mode -1)
            (should-not (bound-and-true-p total-review-mode)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-random-scope-total ()
  "Project-wide data covers multiple files and stores pending counts."
  (total-review-tests--sandbox
    (let* ((one (total-review-tests--write
                 (expand-file-name "one.el" total-review-tests--sources)
                 "a\nb\n"))
           (two (total-review-tests--write
                 (expand-file-name "two.el" total-review-tests--sources)
                 "c\n"))
           (buf1 (find-file-noselect one))
           (buf2 (find-file-noselect two)))
      (unwind-protect
          (progn
            (with-current-buffer buf1
              (total-review-track)
              (total-review-mark))
            (with-current-buffer buf2 (total-review-track))
            (let* ((scope (total-review--database one))
                   (stats (total-review--collect scope)))
              (should (= (plist-get stats :reviewed) 1))
              (should (= (plist-get stats :pending) 2))
              (should (= (length (plist-get stats :entries)) 2))))
        (dolist (buffer (list buf1 buf2))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

;;; Pending-line face tests

(ert-deftest total-review-pending-face-custom-override ()
  "A user-defined face takes precedence over computed colors."
  (cl-letf (((symbol-function 'facep)
             (lambda (name) (eq name 'total-review-pending-face))))
    (should (eq (total-review--pending-face)
                'total-review-pending-face))))

(ert-deftest total-review-pending-face-dark-theme ()
  "Dark backgrounds are lightened for the pending underline."
  (cl-letf (((symbol-function 'facep) (lambda (&rest _) nil))
            ((symbol-function 'face-background)
             (lambda (&rest _) "#202020"))
            ((symbol-function 'color-name-to-rgb)
             (lambda (&rest _) '(0.125 0.125 0.125)))
            ((symbol-function 'color-lighten-name)
             (lambda (name percent)
               (should (equal name "#202020"))
               (should (= percent 20))
               "#444444")))
    (should (equal (total-review--pending-face)
                   '(:underline "#444444")))))

(ert-deftest total-review-pending-face-light-theme ()
  "Light backgrounds are darkened for the pending underline."
  (cl-letf (((symbol-function 'facep) (lambda (&rest _) nil))
            ((symbol-function 'face-background)
             (lambda (&rest _) "#eeeeee"))
            ((symbol-function 'color-name-to-rgb)
             (lambda (&rest _) '(0.93 0.93 0.93)))
            ((symbol-function 'color-darken-name)
             (lambda (name percent)
               (should (equal name "#eeeeee"))
               (should (= percent 20))
               "#cccccc")))
    (should (equal (total-review--pending-face)
                   '(:underline "#cccccc")))))

(ert-deftest total-review-pending-face-no-color ()
  "Unknown backgrounds fall back to a foreground-colored underline."
  (cl-letf (((symbol-function 'facep) (lambda (&rest _) nil))
            ((symbol-function 'face-background) (lambda (&rest _) nil)))
    (should (equal (total-review--pending-face)
                   '(:underline t)))))

(ert-deftest total-review-pending-face-draws-every-run ()
  "All pending runs share the computed face and retain overlay identity."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let ((total-review-mode t)
          (face '(:underline "#444444")))
      (cl-letf (((symbol-function 'total-review--pending-face)
                 (lambda () face)))
        (total-review--draw "010"))
      (let ((overlays (cl-remove-if-not
                       (lambda (ov) (overlay-get ov 'total-review-overlay))
                       (overlays-in (point-min) (point-max)))))
        (should (= (length overlays) 2))
        (dolist (ov overlays)
          (should (equal (overlay-get ov 'face) face))
          (should (= (overlay-get ov 'priority) -40)))))))

(ert-deftest total-review-pending-face-theme-refresh ()
  "Theme changes recolor existing overlays without tracking new lines."
  (let ((buffer (generate-new-buffer " *total-review-theme-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "unreviewed\n")
          (setq-local total-review-mode t)
          (cl-letf (((symbol-function 'total-review--pending-face)
                     (lambda () '(:underline "#222222"))))
            (total-review--draw "0"))
          (let ((overlay (car (overlays-in (point-min) (point-max)))))
            (should overlay)
            (cl-letf (((symbol-function 'total-review--pending-face)
                       (lambda () '(:underline "#eeeeee"))))
              (total-review--refresh-pending-faces 'changed-theme))
            (should (eq overlay
                        (car (overlays-in (point-min) (point-max)))))
            (should (equal (overlay-get overlay 'face)
                           '(:underline "#eeeeee")))))
      (kill-buffer buffer))))

(ert-deftest total-review-pending-face-theme-refresh-after-edit ()
  "A theme change must not recreate highlights cleared by edits."
  (let ((buffer (generate-new-buffer " *total-review-edit-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "unreviewed\n")
          (setq-local total-review-mode t)
          (cl-letf (((symbol-function 'total-review--pending-face)
                     (lambda () '(:underline t))))
            (total-review--draw "0"))
          (total-review--after-edit)
          (total-review--refresh-pending-faces 'changed-theme)
          (should-not (overlays-in (point-min) (point-max))))
      (kill-buffer buffer))))

(ert-deftest total-review-report-layout-and-totals ()
  "Show tracked-file count first and a percentage-free totals row last."
  (let ((report (get-buffer-create "*Total Review Report*")))
    (unwind-protect
        (cl-letf (((symbol-function 'total-review--scope)
                   (lambda (&optional _location)
                     (list :directory "/tmp/total-review-report-test/"
                           :kind 'global :root nil)))
                  ((symbol-function 'total-review--collect)
                   (lambda (_directory)
                     (list :entries '(("/tmp/one.el" 2 1 3)
                                      ("/tmp/two.el" 0 1 1))
                           :errors '("/tmp/missing.el: unreadable")
                           :reviewed 2 :pending 2 :total 4)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer)))
          (total-review-report)
          (with-current-buffer report
            (let* ((body (buffer-string))
                   (tracked (string-match "Tracked files: 2" body))
                   (summary (string-match
                             (regexp-quote
                              "Reviewed 2 / 4 lines (50.0%), 2 pending")
                             body))
                   (table (string-match "Reviewed  Pending" body))
                   (footer (concat
                            (make-string 65 ?-) "\n"
                            (format "%-9d %-9d %-9d %s\n"
                                    2 2 4 "Totals"))))
              (should tracked)
              (should summary)
              (should table)
              (should (< tracked summary))
              (should (< summary table))
              (should (string-match-p
                       (regexp-quote "Errors (not counted as reviewed):")
                       body))
              (should-not (string-match-p "Readable tracked files" body))
              (should (string-suffix-p footer body))
              (should-not (string-match-p "%" footer)))))
      (when (buffer-live-p report)
        (kill-buffer report)))))


;;; Git-backed review tests

(defun total-review-tests--git (root &rest args)
  "Invoke Git with ARGS in ROOT; fail the test on an error."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil t nil "-C" root args)))
      (unless (eq status 0)
        (ert-fail (format "Git %S failed (%S): %s" args status (buffer-string))))
      (buffer-string))))

(defun total-review-tests--git-repo ()
  "Create a small Git repository inside the sandbox sources."
  (unless (executable-find "git") (ert-skip "Git is unavailable"))
  (let ((root (expand-file-name "git-repo" total-review-tests--sources)))
    (make-directory root t)
    (total-review-tests--git root "init" "-q")
    root))

(defun total-review-tests--git-commit (root message)
  "Commit the current index and worktree changes in ROOT."
  (total-review-tests--git root "add" "--all")
  (total-review-tests--git root "-c" "user.name=Total Review Tests"
                           "-c" "user.email=total-review@example.invalid"
                           "-c" "commit.gpgsign=false"
                           "commit" "-q" "-m" message))

(ert-deftest total-review-git-lines-based-on-head-not-working-tree ()
  "Unsaved edits never change Git review bits or the baseline line count."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           (path (total-review-tests--write
                  (expand-file-name "sample.el" root) "alpha\nbeta\ngamma\n"))
           buffer)
      (total-review-tests--git-commit root "initial")
      (setq buffer (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (let ((state (total-review--load (total-review--path))))
              (should (eq (plist-get state :backend) 'git))
              (should (equal (plist-get state :active)
                             (plist-get (total-review--git-info path) :blob)))
              (should (equal (plist-get (car (plist-get state :versions)) :flags)
                             "000")))
            (total-review-mark)
            (should (equal total-review--base-flags "100"))
            (goto-char (point-min))
            (insert "new\n")
            (should (equal (total-review--sync) "-100"))
            (should (equal total-review--base-flags "100"))
            (should (equal (append total-review--mapping nil)
                           '(nil 0 1 2)))
            (goto-char (point-min))
            (should-error (total-review-mark) :type 'user-error)
            (should (equal total-review--base-flags "100"))
            (should (equal (plist-get
                            (total-review--git-version
                             (total-review--load (total-review--path))
                             (plist-get (total-review--git-info path) :blob))
                            :flags)
                           "100")))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest total-review-git-erasing-and-restoring-buffer-keeps-progress ()
  "Erase/save/reinsert cannot destroy the stored committed review bits."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           (text "one\ntwo\nthree\n")
           (path (total-review-tests--write
                  (expand-file-name "sample.el" root) text))
           buffer)
      (total-review-tests--git-commit root "initial")
      (setq buffer (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (forward-line 1)
            (total-review-mark)
            (should (equal (total-review--sync) "010"))
            (erase-buffer)
            (save-buffer)
            (should (equal (total-review--sync) ""))
            (should (equal total-review--base-flags "010"))
            (insert text)
            (save-buffer)
            (should (equal (total-review--sync) "010"))
            (should (= (length (plist-get
                                (total-review--load (total-review--path))
                                :versions)) 1)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest total-review-git-new-commit-transfers-only-unchanged-lines ()
  "A new commit creates version bits, preserving unchanged reviewed lines."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           (path (total-review-tests--write
                  (expand-file-name "sample.el" root) "one\ntwo\nthree\n"))
           buffer)
      (total-review-tests--git-commit root "initial")
      (setq buffer (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mark)
            (forward-line 2)
            (total-review-mark)
            (should (equal (total-review--sync) "101"))
            (goto-char (point-min))
            (delete-region (line-beginning-position) (line-end-position))
            (insert "modified")
            (save-buffer)
            (should (equal (total-review--sync) "-01"))
            (should (equal total-review--base-flags "101"))
            (total-review-tests--git-commit root "change first line")
            (should (equal (total-review--sync) "001"))
            (should (equal total-review--base-flags "001"))
            (let ((state (total-review--load (total-review--path))))
              (should (= (length (plist-get state :versions)) 2))
              (should (eq (plist-get state :backend) 'git))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest total-review-git-switch-branches-restores-versions ()
  "Previously visited committed versions retain separate review progress."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           (path (total-review-tests--write
                  (expand-file-name "sample.el" root) "first\nsecond\nthird\n"))
           buffer)
      (total-review-tests--git-commit root "initial")
      (setq buffer (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mark)
            (should (equal (total-review--sync) "100"))
            (total-review-tests--git root "switch" "-q" "-c" "feature")
            (goto-char (point-min))
            (forward-line 1)
            (delete-region (line-beginning-position) (line-end-position))
            (insert "feature-change")
            (save-buffer)
            (total-review-tests--git-commit root "feature")
            (should (equal (total-review--sync) "100"))
            (goto-char (point-max))
            (forward-line -1)
            (total-review-mark)
            (should (equal total-review--base-flags "101"))
            (total-review-tests--git root "switch" "-q" "-")
            (revert-buffer :ignore-auto :noconfirm)
            (should (equal (total-review--sync) "100"))
            (total-review-tests--git root "switch" "-q" "feature")
            (revert-buffer :ignore-auto :noconfirm)
            (should (equal (total-review--sync) "101"))
            (should (= (length (plist-get
                                (total-review--load (total-review--path))
                                :versions)) 2)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest total-review-git-untracked-file-falls-back-and-converts ()
  "An uncommitted file uses snapshots until its first commit."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           path buffer)
      (total-review-tests--write (expand-file-name "anchor" root) "anchor\n")
      (total-review-tests--git-commit root "initial")
      (setq path (total-review-tests--write
                  (expand-file-name "new.el" root) "first\nsecond\n"))
      (setq buffer (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (should (eq (plist-get (total-review--load (total-review--path))
                                   :backend) 'snapshot))
            (total-review-mark)
            (should (equal (total-review--sync) "10"))
            (total-review-tests--git-commit root "first appearance of new.el")
            (should (equal (total-review--sync) "10"))
            (let ((state (total-review--load (total-review--path))))
              (should (eq (plist-get state :backend) 'git))
              (should (equal (plist-get (car (plist-get state :versions)) :flags)
                             "10"))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-git-missing-working-file-preserves-state ()
  "Missing Git-backed paths aren't auto-untracked by the database scan."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           (path (total-review-tests--write
                  (expand-file-name "sample.el" root) "value\n")))
      (total-review-tests--git-commit root "initial")
      (total-review--track-path path)
      (let ((metadata (total-review--state-path path)))
        (delete-file path)
        (let ((stats (total-review--collect (total-review--database path))))
          (should (= (plist-get stats :forgotten) 0))
          (should (= (length (plist-get stats :errors)) 1)))
        (should (file-exists-p metadata))))))

(ert-deftest total-review-git-progress-messages-consistent ()
  "Progress, mark and refresh messages use the same baseline summary."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           (path (total-review-tests--write
                  (expand-file-name "sample.el" root) "one\ntwo\n"))
           buffer log)
      (total-review-tests--git-commit root "initial")
      (setq buffer (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) log))))
              (total-review-mark)
              (total-review-progress)
              (total-review-refresh))
            (should (= (length log) 2))
            ;; `refresh' only prints progress when called interactively.
            (should (equal (car log) (cadr log)))
            (should (string-match-p
                     "Reviewed 1 / 2 lines (50.0%), 1 pending" (car log))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-line-map-keeps-only-unchanged-lines ()
  (should (equal (append (total-review--line-map 3 4 '((1 0 2 1))) nil)
                 '(0 nil 1 2)))
  (should (equal (append (total-review--line-map 3 3 '((2 1 2 1))) nil)
                 '(0 nil 2)))
  (should (equal (append (total-review--line-map 3 0 '((1 3 0 0))) nil)
                 nil)))


(ert-deftest total-review-git-commit-while-emacs-not-running ()
  "A commit made while a review buffer is closed is detected on reopening."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           (path (total-review-tests--write
                  (expand-file-name "sample.el" root) "one\ntwo\n"))
           buffer)
      (total-review-tests--git-commit root "initial")
      (setq buffer (find-file-noselect path))
      (with-current-buffer buffer
        (total-review-track)
        (total-review-mark))
      (kill-buffer buffer)
      (total-review-tests--write path "one\ntwo\nthree\n")
      (total-review-tests--git-commit root "external commit")
      (setq buffer (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal (total-review--sync) "100"))
            (should (= (length (plist-get
                                (total-review--load (total-review--path))
                                :versions)) 2)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-git-commit-with-unchanged-blob-reuses-bits ()
  "A commit touching another file doesn't duplicate this file's versions."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           (path (total-review-tests--write
                  (expand-file-name "sample.el" root) "one\ntwo\n"))
           buffer)
      (total-review-tests--git-commit root "initial")
      (setq buffer (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (total-review-mark)
            (total-review-tests--write (expand-file-name "other" root) "other\n")
            (total-review-tests--git-commit root "unrelated file")
            (should (equal (total-review--sync) "10"))
            (should (= (length (plist-get
                                (total-review--load (total-review--path))
                                :versions)) 1)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest total-review-git-selection-with-changed-lines-is-atomic ()
  "Marking a region with changed lines must not modify any committed bits."
  (total-review-tests--sandbox
    (let* ((root (total-review-tests--git-repo))
           (path (total-review-tests--write
                  (expand-file-name "sample.el" root) "a\nb\nc\n"))
           buffer)
      (total-review-tests--git-commit root "initial")
      (setq buffer (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buffer
            (total-review-track)
            (goto-char (point-min))
            (insert "new\n")
            (goto-char (point-min))
            (set-mark (point))
            (goto-char (point-max))
            (activate-mark)
            (should-error (total-review-mark) :type 'user-error)
            (should (equal total-review--base-flags "000"))
            (should (equal (plist-get
                            (total-review--git-version
                             (total-review--load (total-review--path))
                             (plist-get (total-review--git-info path) :blob))
                            :flags) "000")))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(provide 'total-review-tests)
;;; total-review-tests.el ends here
