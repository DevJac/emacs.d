# Total Review 0.3.1

Total Review is an experimental Emacs package for tracking which lines of a
text file you have personally reviewed. It leaves source files untouched, uses
Emacs's diff integration to map unchanged lines across edits, and keeps review
information in a central directory (not in Git).

**Version 0.3.0 changes the meaning of a review baseline for Git-committed
files.** A file that exists in `HEAD` is reviewed against its last committed
contents, rather than an independently stored snapshot. Worktree edits cannot
silently erase review progress. Files not yet committed to Git (or outside Git)
continue using the earlier snapshot method.

This version **does not read or migrate version 1 databases** from 0.2.x. It
uses `.state` metadata files instead of the old `.el` metadata files, leaving
existing data and snapshots in place but inactive. Back up the database before
experimentation. There is no automatic migration.

## Installation

Put `total-review.el` in your `load-path`, then load it or configure it as a
local package. This version targets Emacs 29.1 or later and uses external Git
and a GNU-compatible `diff` command.

```elisp
(add-to-list 'load-path "~/.config/emacs/user-lisp/total-review/")
(require 'total-review)
(global-total-review-mode 1)
```

The global mode highlights **already tracked** files; it does not
indiscriminately add every opened file to the review database.

## Commands

| Command | Purpose |
|---|---|
| `total-review-track` | Start tracking the current file |
| `total-review-untrack` | Discard the current file's review state, with confirmation |
| `total-review-track-project` | Add eligible project files, with confirmation |
| `total-review-track-dired` | Add marked Dired files |
| `total-review-untrack-dired` | Untrack marked Dired files, with confirmation |
| `total-review-list-skipped-project-files` | View project-filter exclusions and reasons |
| `total-review-mode` | Toggle pending-line highlighting in the current buffer |
| `global-total-review-mode` | Highlight files already tracked in the active profile |
| `total-review-mark` | Mark current/selected unchanged lines as reviewed |
| `total-review-unmark` | Undo a review mark on current/selected unchanged lines |
| `total-review-next` | Jump to next visible pending line in this file |
| `total-review-random` | Jump to a random visible pending line in this file |
| `total-review-random-project` | Random pending line across eligible visible lines in the scope |
| `total-review-progress` | Show current file's baseline review percentage |
| `total-review-report` | Report progress in the project or global scope |
| `total-review-refresh` | Re-map lines against the baseline and refresh highlighting |
| `total-review-switch-profile` | Select a profile and refresh open buffers |

Marking and unmarking a region **update all unchanged, reviewable lines**,
while silently skipping new or modified Git lines. A selection containing only
changed lines does nothing; it does not raise an error or write review state.
The same behavior applies when operating on just one line.

## Git-backed review model

When a file exists as an ordinary file in the current Git `HEAD`, Total Review
reads the committed version from Git and uses it as the immutable baseline.
It stores one `0`/`1` bit per **committed line**:

- `1` — reviewed
- `0` — pending review

The live working buffer may have added, deleted, or modified lines. A unified
diff maps unchanged committed lines to their present positions. Working-buffer
lines that don't correspond to a committed line have an **unpersisted** third
state, `changed`, and cannot be marked. Deleted committed lines have no display
position but remain in the baseline and its review denominator.

**The Git index/staging area is ignored.** This package always compares the
committed `HEAD` version with the *current Emacs buffer*, even with unsaved
edits. It neither stages nor commits source files.

After a new commit is detected (including one made while Emacs was closed),
Total Review checks whether it has stored flags for that committed file
version. If so, it restores them. Otherwise it selects the closest known
ancestor commit as its transfer source, or the last used reviewed version
when no known ancestor exists, then transfers review bits across unchanged
lines using a diff. New or modified committed lines start pending.

Several committed versions of a file may have separate review states in the
same database. Switching back to a version already seen **restores its own
saved bits** rather than overwriting them with the last branch's review data.
Identical committed file contents share the same version within a tracked path,
even when reached through different commits.

Moved or copied text has no special treatment. Diff's ordinary unchanged-line
matching applies; moved blocks generally start pending again. Ambiguous or
repeated text may be matched differently by the diff algorithm.

Git uses immutable object IDs for committed contents. Those IDs are an internal
storage detail of Total Review; users interact with files and commits normally.
If Git eventually garbage-collects an old, unreferenced version, an attempted
transfer from that missing version will conservatively start with all lines
pending. A previously saved version of the *current* committed file can still
restore its stored bits.

**For a missing Git-backed path**, the database retains its review history.
This avoids losing progress when switching branches. Missing files are omitted
from report totals and reported as unprocessable until they reappear. Use
`total-review-untrack` or `total-review-untrack-dired` while the file exists to
explicitly remove its history. There is no dedicated missing-path untrack
command yet.

### What progress percentages mean

Percentages are based on **the full baseline**, not only lines currently
visible in the working tree. For example, if a 100-line committed file has 80
reviewed bits and 20 pending bits, its progress remains 80% even when you
remove or rewrite several lines without committing. The report separately
shows changed working lines and committed lines absent/modified in the buffer.
It counts replacements in both categories, not as a single changed line.

Navigation targets only pending committed lines that remain unchanged in the
working buffer. This means the number of navigable pending lines can be smaller
than the pending total shown in progress and reports.

The same progress wording is used in `mark`, `unmark`, `progress`, `refresh`
(when interactive), and the report summary:

```text
Reviewed 85 / 120 lines (70.8%), 35 pending
```

## Snapshot fallback

For a file outside Git or not yet committed in the repository's `HEAD`, Total
Review uses its original approach: it saves a plain-text snapshot plus review
bits, updating them when the buffer changes. Unsaved edits also participate
in this comparison. New or changed lines become unreviewed, and unchanged
lines inherit their bits. An empty file is still tracked.

When a snapshot-backed file first appears in a Git commit, the next review
operation transfers its existing bits to that committed baseline, switches its
backend to Git, and removes the old snapshot after successfully saving the new
metadata. No explicit conversion command is needed.

Snapshot-backed files confirmed missing are still automatically forgotten when
`total-review-report`, `total-review-random-project`, or
`total-review-track-project` scans their database, provided the parent directory
is accessible. Missing Git-backed files are *not* automatically forgotten.

## Appearance

Unreviewed, unchanged lines have a subtle underline. By default its color is
derived from the theme's `default` background (20% lighter for dark themes,
20% darker for light themes). It does not replace syntax highlighting or text
backgrounds, and theme changes recolor existing overlays. Changed working
lines have no Total Review overlay.

To override the generated face, define `total-review-pending-face` before
loading Total Review:

```elisp
(defface total-review-pending-face
  '((t (:underline "#fabd2f")))
  "Custom appearance of pending Total Review lines.")
```

## Database and profiles

`total-review-profile` is a plain string, defaulting to `"default"`.
Use `total-review-switch-profile` to change it interactively. The location of
all databases remains centralized:

```text
~/.config/emacs/total-review/
    default/
        global/
        projects/<hash-of-project-root>/
    named/
        first-pass/
            global/
            projects/<hash-of-project-root>/
```

The location is controlled by `total-review-directory`. No files are written
in `.git/` or inside source repositories by Total Review. Each version 2
metadata file is named `<hash-of-absolute-file-path>.state` and contains a Lisp
property list. Git records contain a path, repository root, Git-relative path,
an active committed object ID, and a list of per-version flags and the commit
where that version was first encountered. Snapshot records instead contain a
snapshot filename and the current flags.

A single tracked Git record may grow as more committed versions are encountered;
there is currently no automatic pruning of historical review states.

## Project tracking filter

`total-review-track-project` uses `project-files` and respects exclusions from
the project's backend. It skips remote, symlink, non-regular, oversized
(above 1 MiB), or likely binary files (by extension or NUL bytes in the first
8 KiB). It also excludes Total Review's own central database directory.

When there are exclusions, the minibuffer reports **counts only**. Filenames
and reasons appear in `*Total Review Skipped*`, also available on demand using
`total-review-list-skipped-project-files`. Explicit tracking from a file or
Dired bypasses the binary/size filter.

## Limitations

- Files are identified by canonical absolute pathname; source renames are not
  automatically carried over.
- Multiple Emacs instances editing one database concurrently are unsupported.
- Git working-tree filters, alternative encodings, and line-ending conversion
  can create unexpected diff mappings. Test those workflows before depending
  on their review coverage.
- The old 0.2.x `.el` metadata and snapshots are ignored, not deleted or
  migrated. You can keep a backup or remove them manually later.
- Git commands and GNU diff run as subprocesses. Reports over very large
  repositories may be slower than with the snapshot-only backend.

## Tests

From the extracted package directory:

```sh
emacs -Q --batch -L . -l total-review-tests.el -f ert-run-tests-batch-and-exit
```

Or within Emacs:

```elisp
(load-library "total-review-tests")
(ert-run-tests-interactively "^total-review-")
```
