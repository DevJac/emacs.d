# Total Review 0.2.2 (experimental)

Total Review tracks which lines you've examined in source files, without adding annotations or altering the source. It uses a **plain-text snapshot** plus **review flags** for each file. On refresh, Emacs runs a **zero-context unified diff** against the current buffer and carries review status forward across unchanged lines. Inserted or modified lines become unreviewed.

Compatible with Emacs 29.1+ (designed for Emacs 31). No third-party Emacs packages or SQLite required. Relies on the `diff` executable used by Emacs's built-in `diff.el` (on Fedora, usually GNU diffutils). Supports local files, not TRAMP.

**This is an experimental release. Keep a backup of your review database.** The accompanying ERT tests have been authored but could not be executed in the build environment because an Emacs executable was not available.

## Installation

Put `total-review.el` in a directory on `load-path`. With Emacs 31's `user-lisp/` setup, a convenient layout is:

```text
~/.config/emacs/user-lisp/total-review/
    total-review.el
    total-review-tests.el
```

A minimal `use-package` declaration:

```elisp
(use-package total-review
  :ensure nil)
```

To automatically show pending-line highlighting in every **tracked** source buffer, use:

```elisp
(use-package total-review
  :ensure nil
  :demand t
  :config
  (global-total-review-mode 1))
```

The global mode does not track every file; it enables the local display mode only for files that have been added to the active review database.

## Commands

| Command | Function |
|---|---|
| `total-review-track` | Track current file, initially all pending |
| `total-review-track-project` | Discover project files using `project-files`, confirm bulk tracking, and display skipped files and reasons |
| `total-review-track-dired` | Track files marked in Dired (or file at point) |
| `total-review-list-skipped-project-files` | Show which project files the automatic filter skips, and why, without tracking anything |
| `total-review-mode` | Toggle pending-line highlighting in this buffer |
| `global-total-review-mode` | Automatically enable highlighting in tracked file buffers |
| `total-review-mark` | Mark current line or selected lines reviewed |
| `total-review-unmark` | Mark lines pending again |
| `total-review-next` | Next unreviewed line in the current file |
| `total-review-random` | Random unreviewed line in the current file |
| `total-review-random-project` | Random unreviewed line across tracked files in the current project/global fallback |
| `total-review-progress` | Current-file review percentage |
| `total-review-report` | Full project/global report, with clickable filenames |
| `total-review-refresh` | Reconcile snapshot and current source using `diff` |
| `total-review-show-diff` | Inspect the raw `-U0` diff without synchronizing |
| `total-review-switch-profile` | Change to default or a named review profile and refresh open buffers |
| `total-review-import-review-lines` | Import data from the old Review Lines 0.1 prototype |

### Workflow

1. In a project, `M-x total-review-track-project`. Emacs asks for confirmation and shows how many files will be excluded; if any are skipped, `*Total Review Skipped*` lists their names and reasons.
2. `M-x global-total-review-mode` once (or enable it in your init file). Pending lines receive an overline and underline.
3. Select lines with Evil Visual mode (or an ordinary active region), then `M-x total-review-mark`.
4. `M-x total-review-random-project` picks a **pending line**, uniformly over all pending lines across tracked files in the current scope.
5. `M-x total-review-report` gives an overall progress breakdown, with source-file links.
6. Edit any source. Temporary highlights clear until save, refresh, marking, or navigation. Saving and these commands re-diff the source to carry status across unchanged lines.

To bulk-track just a selection, mark files in Dired and invoke `M-x total-review-track-dired`. Explicit file tracking does not apply the automatic binary/size filter.

## Pending-line appearance

Unreviewed lines are marked with **both an overline and an underline**;
reviewed lines are left alone.  By default, the line color is derived from
the current theme's `default` background: it is lightened by 20% on dark
themes, or darkened by 20% on light themes.  This affects the lines only;
syntax highlighting, the text foreground, and the text background are not
changed.  If a terminal does not provide a usable background color, the
lines instead use the foreground color.  Changing themes recolors existing
pending overlays without modifying review state.

**Optional face override:** If you define the face
`total-review-pending-face`, Total Review uses it *instead* of generating
its automatic colors.  The package deliberately does **not** define this
face itself, so the fallback remains available.  For example, evaluate
this in your init file **before** loading Total Review:

```elisp
(defface total-review-pending-face
  '((t (:overline "#fabd2f" :underline "#fabd2f")))
  "Custom appearance of pending Total Review lines.")
```

After adding or removing a face override, `M-x total-review-refresh`
updates the pending overlays.  If you have loaded an older Total Review
version that defined this face automatically, restart Emacs after installing
0.2.2 to remove the old face definition before trying the fallback.

## Database profiles and location

`total-review-profile` is one of:

```elisp
(setq total-review-profile 'default)
(setq total-review-profile '(named "first-pass"))
```

Use `M-x total-review-switch-profile` for an **interactive, immediate switch** that also updates highlighted open buffers. Existing named profiles are suggested; typing a new name is allowed. Profile names accept letters, numbers, underscores, periods, and hyphens, beginning with a letter or number.

A profile automatically selects the current project database using Emacs `project.el`. Files outside recognized projects share that profile's **global** database. Each tracked file is associated with exactly one database in that profile. Profiles don't merge review flags with one another.

By default, data is stored under `(locate-user-emacs-file "total-review/")`, for example:

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

Each database contains `<hash-of-source-path>.el` metadata files and `<hash-of-source-path>-<random>.snapshot` snapshots. This is usually outside the project being reviewed. If your **Emacs configuration itself is a Git repository**, you may want to add `/total-review/` to its `.gitignore`. Automatic tracking explicitly excludes the Total Review database directory to prevent recursively tracking its own snapshots. To relocate the database root (not the selection strategy), set `total-review-directory`.

### Import the original Review Lines prototype

If you previously used `review-lines.el`, remove its mode/configuration so the two packages don't both highlight buffers. Then run:

```text
M-x total-review-import-review-lines
```

It prompts for the old database directory (default `~/.config/emacs/review-lines/` if that is your `user-emacs-directory`), copies the snapshots and flags to the selected project's/default or named databases **without modifying or deleting your old data**, and skips files already tracked in Total Review. The next refresh will diff imported snapshots against current files.

## Automatic project tracking filter

`total-review-track-project` obtains its file list from `project-files`, which usually respects the project's ignore/exclusion rules (depending on the project backend). It skips:

- Non-regular or remote files, symlinks, and Total Review's own database files.
- Files larger than **1 MiB**.
- A small hardcoded collection of common binary extensions.
- Files with NUL bytes in their first **8 KiB**.
- Files that can't be read.

To inspect skipped files again at any time, run `M-x total-review-list-skipped-project-files`.  It does not change review state.  The skipped list includes only files returned by `project-files`; files hidden by Git ignore rules or other project backend exclusions cannot appear in it.

No external file-classification utility is invoked, and the heuristic is deliberately not customizable. You can explicitly track a file that it would skip with `total-review-track` or `total-review-track-dired`.

If an explicitly tracked file produces a **binary diff**, the package refuses to synchronize that file instead of risking incorrect review flags. The project report displays files that couldn't be synchronized rather than counting them as reviewed.

## How synchronization works

State is saved as a Lisp property list:

```elisp
(:version 1
 :path "/home/user/code/example.el"
 :snapshot "<id>-<suffix>.snapshot"
 :flags "110010")
```

The flags correspond to lines in the snapshot: `1` means reviewed and `0` means pending. A GNU unified diff (`-U0`) identifies old/new line ranges; unchanged ranges preserve their flags, while new and replaced ranges start pending. Newly produced snapshots are committed by atomically replacing the corresponding metadata file. Old snapshots are deleted after the metadata commit; an interrupted write may leave an unused snapshot but should not leave metadata referring to half-written data.

Important caveats:

- **Unsaved changes count**: synchronizing uses the **live buffer**, even if it hasn't been saved. The package never saves the actual source file.
- Ambiguous duplicate lines and reordered blocks can be matched unexpectedly by `diff`. Diff is not a stable semantic identity scheme.
- A line changed then later reverted after a synchronization does not automatically get its old review status back.
- File renames change canonical path identity; a renamed file is not automatically migrated.
- The report and cross-file random command synchronize all tracked files in scope. Missing/unreadable/unsupported files appear as errors in the report; totals exclude them.
- No multi-process write coordination. Do not use multiple Emacs instances modifying the same review database at the same time.

## Tests

```sh
cd ~/.config/emacs/user-lisp/total-review
emacs -Q --batch -L . -l total-review-tests.el -f ert-run-tests-batch-and-exit
```

Or, in Emacs:

```elisp
(load-library "total-review-tests")
(ert-run-tests-interactively "^total-review-")
```

If an ERT test fails, copy its backtrace and the output to investigate. `total-review-show-diff` is useful when you're curious about exactly how `diff` matched changed lines.
