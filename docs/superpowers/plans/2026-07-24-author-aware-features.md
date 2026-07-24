# Author-aware features — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let a reviewer set the current author interactively, see and filter changes by author in the overview, and accept or reject one author's changes in a batch.

**Architecture:** All authorship is already in the buffer — a change's author is the `@ID` prefix of its comment group, read by the existing `org-change--split-comment`. Two internal helpers (`org-change--change-author`, `org-change--authors-present`) expose it; everything else (a set-author command, an overview column + filter, and by-author accept/reject) builds on those two. No markup or file-format change.

**Tech Stack:** Emacs Lisp; ERT for tests, run in batch.

## Global Constraints

- Elisp only; target `emacs "29.1"` (the package's `Package-Requires`).
- Every change keeps a clean byte-compile: `rm -f org-change.elc && emacs -batch -L . -f batch-byte-compile org-change.el` must print no `warn`/`error` lines.
- The full suite must stay green: `emacs -batch -L . -l test/org-change-tests.el -f ert-run-tests-batch-and-exit` ends `... results as expected, 0 unexpected`.
- Follow the existing file's style: tab indentation, docstrings on every defun/defvar, comments explaining *why*.
- A change's author is nil when it has no comment or the comment has no `@ID`; such a change is *unattributed*. The empty change `{!!}{!!}` is skipped everywhere, as elsewhere in the package.
- Commit message trailer on every commit: `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.

**Test run helper** (used throughout; substitute the selector):

```bash
emacs -batch -L . -l test/org-change-tests.el \
  --eval '(ert-run-tests-batch-and-exit "SELECTOR")' 2>&1 \
  | grep -v Fontif | grep -E "^Ran|FAILED|passed"
```

New code in `org-change.el` goes just before the `;;; Counting changes` section (where the overview code already lives), unless a step says otherwise. New tests append to `test/org-change-tests.el` before the final `(provide 'org-change-tests)`.

---

## Task 1: Shared author helpers

**Files:**
- Modify: `org-change.el` — add two defuns in the overview area, just before `(defun org-change--change-summary ...)`.
- Test: `test/org-change-tests.el`

**Interfaces:**
- Produces:
  - `(org-change--change-author)` → author id string, or nil. Must be called with the match data of `org-change--regexp` set (right after a search). Saves the match data.
  - `(org-change--authors-present)` → cons `(IDS . UNATTRIBUTED)`: `IDS` a list of distinct author id strings in first-seen order; `UNATTRIBUTED` non-nil if any change has no author.
- Consumes: existing `org-change--decode`, `org-change--split-comment`, `org-change--search-forward`.

- [ ] **Step 1: Write the failing tests**

Append to `test/org-change-tests.el` before `(provide 'org-change-tests)`:

```elisp
;;; Author helpers

(defun org-change-tests--author-at (text pos)
  "Return the author of the change at POS in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char pos)
    (org-change--at-change)
    (org-change--change-author)))

(ert-deftest org-change-test-change-author-reads-the-id ()
  "The author is the @ID prefix of the comment."
  (should (equal (org-change-tests--author-at "a {!x!}{!y!}{!@SG note!} b" 6)
		 "SG")))

(ert-deftest org-change-test-change-author-nil-without-id ()
  "A comment without an @ID leaves the change unattributed."
  (should-not (org-change-tests--author-at "a {!x!}{!y!}{!just a note!} b" 6)))

(ert-deftest org-change-test-change-author-nil-without-comment ()
  "A change with no comment is unattributed."
  (should-not (org-change-tests--author-at "a {!x!}{!y!} b" 6)))

(ert-deftest org-change-test-change-author-keeps-match-data ()
  "Reading the author does not disturb a caller walking the buffer."
  (with-temp-buffer
    (insert "a {!x!}{!y!}{!@SG!} b {!p!}{!q!} c")
    (goto-char (point-min))
    (org-change--search-forward nil t)
    (let ((beg (match-beginning 0)))
      (org-change--change-author)
      ;; match data still points at the first change
      (should (= (match-beginning 0) beg))
      (should (equal (match-string 1) "x")))))

(ert-deftest org-change-test-authors-present-lists-in-order ()
  "Distinct authors are returned in first-seen order, with the flag."
  (with-temp-buffer
    (insert "{!a!}{!!}{!@MR!} x {!b!}{!!}{!@SG!} y {!c!}{!!} z {!d!}{!!}{!@MR!}")
    (should (equal (org-change--authors-present) '(("MR" "SG") . t)))))

(ert-deftest org-change-test-authors-present-skips-empty-change ()
  "The empty change is not counted as an unattributed change."
  (with-temp-buffer
    (insert "{!a!}{!!}{!@SG!} x {!!}{!!}")
    (should (equal (org-change--authors-present) '(("SG"))))))
```

- [ ] **Step 2: Run the tests, verify they fail**

Run the helper with selector `"change-author\\|authors-present"`.
Expected: FAIL — `org-change--change-author` / `org-change--authors-present` undefined (void-function).

- [ ] **Step 3: Implement the helpers**

In `org-change.el`, immediately before `(defun org-change--change-summary ()`:

```elisp
(defun org-change--change-author ()
  "Return the author id of the change just matched, or nil.
The author is the @ID prefix of the comment group, read the way the
on-screen comment and the exporters read it.  Uses the match data of
`org-change--regexp', so it must be called right after a search; it
saves that data, so a caller walking the buffer can call it between
searches."
  (save-match-data
    (car (org-change--split-comment
	  (org-change--decode (or (match-string-no-properties 3) ""))))))

(defun org-change--authors-present ()
  "Return (IDS . UNATTRIBUTED) for the changes in the current buffer.
IDS is the distinct author ids that occur, in first-seen order.
UNATTRIBUTED is non-nil when at least one change has no author.  The
empty change is skipped, as it is everywhere else."
  (save-excursion
    (goto-char (point-min))
    (let ((ids nil) (unattr nil))
      (while (org-change--search-forward nil t)
	(let ((new (match-string-no-properties 1))
	      (old (match-string-no-properties 2))
	      (author (org-change--change-author)))
	  (unless (and (equal new "") (equal old ""))
	    (if author
		(unless (member author ids) (push author ids))
	      (setq unattr t)))))
      (cons (nreverse ids) unattr))))
```

- [ ] **Step 4: Run the tests, verify they pass**

Run the helper with selector `"change-author\\|authors-present"`. Expected: all `passed`.

- [ ] **Step 5: Byte-compile and full suite**

```bash
rm -f org-change.elc && emacs -batch -L . -f batch-byte-compile org-change.el 2>&1 | grep -iE "warn|error"; rm -f org-change.elc
emacs -batch -L . -l test/org-change-tests.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E "^Ran"
```
Expected: no warnings; `... 0 unexpected`.

- [ ] **Step 6: Commit**

```bash
git add org-change.el test/org-change-tests.el
git commit -m "Add author helpers: --change-author and --authors-present

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 2: `org-change-set-author` and the shared author label

**Files:**
- Modify: `org-change.el` — add `org-change--author-label` (before `org-change--change-author`) and `org-change-set-author` (in the accept/reject area, after `org-change-reject`).
- Test: `test/org-change-tests.el`

**Interfaces:**
- Produces:
  - `(org-change--author-label ID)` → display string `"ID  (Name)"`, or just `"ID"` when the id has no name in `org-change-authors`.
  - `(org-change-set-author)` — interactive; sets `org-change-author` to a chosen id, a freely-typed id, or nil.
- Consumes: `org-change-authors`, `org-change-author` (both already defined).

- [ ] **Step 1: Write the failing tests**

Append before `(provide 'org-change-tests)`:

```elisp
;;; Setting the author

(ert-deftest org-change-test-author-label-shows-the-name ()
  "A registered author is labelled id and name; an unknown id is bare."
  (let ((org-change-authors '(("SG" :name "Stefano Ghirlanda" :color "blue"))))
    (should (equal (org-change--author-label "SG") "SG  (Stefano Ghirlanda)"))
    (should (equal (org-change--author-label "ZZ") "ZZ"))))

(defun org-change-tests--set-author-with (answer)
  "Run `org-change-set-author' with `completing-read' returning ANSWER."
  (let ((org-change-author "start"))
    (cl-letf (((symbol-function 'completing-read)
	       (lambda (&rest _) answer)))
      (org-change-set-author))
    org-change-author))

(ert-deftest org-change-test-set-author-picks-a-registered-id ()
  "Choosing the label of a registered author sets that id."
  (let ((org-change-authors '(("SG" :name "Stefano Ghirlanda"))))
    (should (equal (org-change-tests--set-author-with "SG  (Stefano Ghirlanda)")
		   "SG"))))

(ert-deftest org-change-test-set-author-accepts-a-new-id ()
  "Typing an id that is not registered sets it verbatim."
  (let ((org-change-authors '(("SG" :name "Stefano Ghirlanda"))))
    (should (equal (org-change-tests--set-author-with "MR") "MR"))))

(ert-deftest org-change-test-set-author-can-clear ()
  "Choosing No author clears the author."
  (should-not (org-change-tests--set-author-with "No author")))
```

- [ ] **Step 2: Run the tests, verify they fail**

Selector `"author-label\\|set-author"`. Expected: FAIL (void-function).

- [ ] **Step 3: Implement the label helper**

In `org-change.el`, immediately before `(defun org-change--change-author ()`:

```elisp
(defun org-change--author-label (id)
  "Return a completion label for author ID: \"ID  (Name)\", or \"ID\".
The name comes from `org-change-authors'; an id that is not
registered there is shown on its own."
  (let ((name (plist-get (cdr (assoc id org-change-authors)) :name)))
    (if (and name (not (string-empty-p name)))
	(format "%s  (%s)" id name)
      id)))
```

- [ ] **Step 4: Implement the command**

In `org-change.el`, immediately after `(defun org-change-reject ...)` (its closing paren):

```elisp
(defun org-change-set-author ()
  "Set `org-change-author', the id stamped on changes you make.
Completes over `org-change-authors', each shown as its id and name,
with a No author choice to clear the author.  A match is not
required: an id typed on the spot is used as-is, and simply has no
color until you add it to `org-change-authors'."
  (interactive)
  (let* ((alist (append
		 (mapcar (lambda (entry)
			   (cons (org-change--author-label (car entry))
				 (car entry)))
			 org-change-authors)
		 '(("No author" . nil))))
	 (choice (completing-read "Author: " (mapcar #'car alist) nil nil))
	 (id (if (assoc choice alist)
		 (cdr (assoc choice alist))
	       (unless (string-empty-p choice) choice))))
    (setq org-change-author id)
    (message (if id (format "Author set to %s" id) "Author cleared"))))
```

- [ ] **Step 5: Run the tests, verify they pass**

Selector `"author-label\\|set-author"`. Expected: all `passed`.

- [ ] **Step 6: Byte-compile and full suite** (as Task 1 Step 5).

- [ ] **Step 7: Commit**

```bash
git add org-change.el test/org-change-tests.el
git commit -m "Add org-change-set-author and the shared author label

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 3: Author column in the overview

**Files:**
- Modify: `org-change.el` — `org-change--overview-entries` (add author slot) and `org-change-overview--render` (render the column).
- Test: `test/org-change-tests.el`

**Interfaces:**
- Consumes: `org-change--change-author`, `org-change--change-face` (existing).
- Produces: overview entries are now `(MARKER LINE SUMMARY AUTHOR)`; each rendered line is `"LINE  AUTHOR  SUMMARY"` with the author column blank when unattributed and omitted entirely when no change in the list has an author.

The existing helper `org-change-tests--overview-lines` strips a fixed 6-character prefix; with a variable author column it no longer isolates the summary. This task adds author-aware assertions and leaves that helper for the tests that used it (which have no authors, so the column is absent and the 6-char prefix still holds).

- [ ] **Step 1: Write the failing tests**

Append before `(provide 'org-change-tests)`:

```elisp
;;; Author column in the overview

(ert-deftest org-change-test-overview-shows-authors ()
  "The overview names the author of each change, blank when there is none."
  (org-change-tests--with-overview
      "{!a!}{!!}{!@SG!}\n{!b!}{!!}{!@MR note!}\n{!c!}{!!}"
    (should (equal (split-string (buffer-substring-no-properties
				  (point-min) (point-max))
				 "\n" t)
		   '("   1  SG  a" "   2  MR  b" "   3      c")))))

(ert-deftest org-change-test-overview-has-no-author-column-without-authors ()
  "With no attributed change, the list has no author column at all."
  (org-change-tests--with-overview "{!a!}{!!}\n{!b!}{!!}"
    (should (equal (split-string (buffer-substring-no-properties
				  (point-min) (point-max))
				 "\n" t)
		   '("   1  a" "   2  b")))))

(ert-deftest org-change-test-overview-colors-the-author ()
  "The author id carries that author's face."
  (let ((org-change-authors '(("SG" :name "Stefano" :color "blue"))))
    (org-change-tests--with-overview "{!a!}{!!}{!@SG!}"
      (goto-char (point-min))
      (search-forward "SG")
      (should (equal (get-text-property (match-beginning 0) 'face)
		     (org-change--change-face "SG"))))))
```

- [ ] **Step 2: Run the tests, verify they fail**

Selector `"overview-shows-authors\\|no-author-column\\|colors-the-author"`.
Expected: FAIL — entries have no author, so lines read `"   1  a"` etc. and the author assertions do not match.

- [ ] **Step 3: Add the author slot to the entries**

In `org-change--overview-entries`, replace the `let*` body that builds each entry:

```elisp
	(while (org-change--search-forward nil t)
	  (let* ((beg (match-beginning 0))
		 ;; Author before summary: `--change-author' saves the match
		 ;; data, so `--change-summary' still sees this change.
		 (author (org-change--change-author))
		 (summary (org-change--change-summary)))
	    (push (list (copy-marker beg) (line-number-at-pos beg) summary author)
		  entries)))
```

- [ ] **Step 4: Render the author column**

Replace the body of `org-change-overview--render` from `(erase-buffer)` through the `(dolist ...)` that draws the lines with:

```elisp
    (erase-buffer)
    (if (null entries)
	(insert "No changes")
      (let ((width (apply #'max 0
			  (mapcar (lambda (e) (length (or (nth 3 e) "")))
				  entries))))
	(dolist (entry entries)
	  (let ((start (point))
		(author (nth 3 entry)))
	    (insert (format "%4d  " (nth 1 entry)))
	    (when (> width 0)
	      (let ((astart (point)))
		(insert (format (format "%%-%ds  " width) (or author "")))
		(when author
		  (put-text-property astart (+ astart (length author))
				     'face (org-change--change-face author)))))
	    (insert (nth 2 entry) "\n")
	    (put-text-property start (point) 'org-change-marker (car entry))))))
```

Leave the rest of `org-change-overview--render` (the `let*` bindings above, and the point-restoring tail) unchanged.

- [ ] **Step 5: Run the tests, verify they pass**

Selector `"overview-shows-authors\\|no-author-column\\|colors-the-author"`. Expected: all `passed`.

- [ ] **Step 6: Byte-compile and full suite.** The earlier overview tests (no authors) must still pass — the author column is absent for them.

- [ ] **Step 7: Commit**

```bash
git add org-change.el test/org-change-tests.el
git commit -m "Show the author of each change in the overview

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 4: Filter the overview by author

**Files:**
- Modify: `org-change.el` — add `org-change-overview--filter` defvar-local (next to `org-change-overview--source`), `org-change-overview--filter-entries`, `org-change-overview--header`; wire both into `org-change-overview--render`; add `org-change-overview-filter`; bind `a` in the mode map; reset the filter in `org-change-overview`; mention `a` in the mode docstring.
- Test: `test/org-change-tests.el`

**Interfaces:**
- Consumes: `org-change--authors-present`, `org-change--author-label`, `org-change-overview--render`, `org-change-overview--source`.
- Produces:
  - buffer-local `org-change-overview--filter` — id string, symbol `unattributed`, or nil (all).
  - `(org-change-overview-filter)` — interactive; bound to `a` in the overview.
  - `(org-change-overview--filter-entries ENTRIES)` — the sublist matching the current filter.
  - `(org-change-overview--header)` — the `header-line-format` string.

- [ ] **Step 1: Write the failing tests**

Append before `(provide 'org-change-tests)`:

```elisp
;;; Filtering the overview by author

(defun org-change-tests--filter-to (answer)
  "Run `org-change-overview-filter' with `completing-read' → ANSWER."
  (cl-letf (((symbol-function 'completing-read)
	     (lambda (&rest _) answer)))
    (org-change-overview-filter)))

(ert-deftest org-change-test-overview-filters-to-an-author ()
  "Filtering to an author shows only that author's changes."
  (org-change-tests--with-overview
      "{!a!}{!!}{!@SG!}\n{!b!}{!!}{!@MR!}\n{!c!}{!!}{!@SG!}"
    (org-change-tests--filter-to "SG  (SG)")
    (should (equal (split-string (buffer-substring-no-properties
				  (point-min) (point-max)) "\n" t)
		   '("   1  SG  a" "   3  SG  c")))
    (should (equal header-line-format "Author: SG"))))

(ert-deftest org-change-test-overview-filters-to-unattributed ()
  "Filtering to (unattributed) shows only changes with no author."
  (org-change-tests--with-overview
      "{!a!}{!!}{!@SG!}\n{!b!}{!!}\n{!c!}{!!}{!@SG!}"
    (org-change-tests--filter-to "(unattributed)")
    (should (equal (split-string (buffer-substring-no-properties
				  (point-min) (point-max)) "\n" t)
		   '("   2  b")))
    (should (equal header-line-format "Author: (unattributed)"))))

(ert-deftest org-change-test-overview-filter-all-restores ()
  "Filtering to All shows everything again."
  (org-change-tests--with-overview
      "{!a!}{!!}{!@SG!}\n{!b!}{!!}{!@MR!}"
    (org-change-tests--filter-to "SG  (SG)")
    (org-change-tests--filter-to "All")
    (should (equal (split-string (buffer-substring-no-properties
				  (point-min) (point-max)) "\n" t)
		   '("   1  SG  a" "   2  MR  b")))
    (should (equal header-line-format "Author: all"))))

(ert-deftest org-change-test-overview-filter-accepts-within-author ()
  "Accepting on a filtered list acts on that change in the source."
  (org-change-tests--with-overview
      "x {!a!}{!!}{!@SG!} y {!b!}{!!}{!@MR!} z"
    (org-change-tests--filter-to "SG  (SG)")
    (org-change-overview-accept)
    (should (equal (with-current-buffer source
		     (buffer-substring-no-properties (point-min) (point-max)))
		   "x a y {!b!}{!!}{!@MR!} z"))))

(ert-deftest org-change-test-overview-filter-survives-a-refresh ()
  "A refresh from the text buffer keeps the active filter."
  (org-change-tests--with-overview
      "x {!a!}{!!}{!@SG!} y {!b!}{!!}{!@MR!} z {!c!}{!!}{!@SG!} w"
    (org-change-tests--filter-to "SG  (SG)")
    (with-current-buffer source
      (goto-char 4)			; the first SG change
      (org-change-accept))
    ;; still filtered to SG: only the remaining SG change shows
    (should (equal (org-change-tests--overview-lines) '("c")))
    (should (equal header-line-format "Author: SG"))))
```

Note: `org-change-tests--overview-lines` strips 6 characters; on the last test the visible line is `"   3  SG  c"`, and stripping 6 leaves `"SG  c"` — so change that test's expectation accordingly:

Replace the last `should` above with:

```elisp
    (should (equal (mapcar (lambda (l) (string-trim (substring l 6)))
			   (split-string (buffer-substring-no-properties
					  (point-min) (point-max)) "\n" t))
		   '("SG  c")))
```

- [ ] **Step 2: Run the tests, verify they fail**

Selector `"overview-filter\\|filters-to"`. Expected: FAIL (`org-change-overview-filter` void, `header-line-format` nil).

- [ ] **Step 3: Add the filter variable**

In `org-change.el`, immediately after `(defvar-local org-change-overview--source nil ...)`:

```elisp
(defvar-local org-change-overview--filter nil
  "The author the overview is filtered to.
An id string shows only that author's changes; the symbol
`unattributed' shows only changes with no author; nil shows every
change.")
```

- [ ] **Step 4: Add filter-entries and header, wire into render**

In `org-change.el`, immediately before `(defun org-change-overview--render ()`:

```elisp
(defun org-change-overview--filter-entries (entries)
  "Return the ENTRIES that match `org-change-overview--filter'.
Each entry is (MARKER LINE SUMMARY AUTHOR)."
  (let ((filter org-change-overview--filter))
    (cond
     ((null filter) entries)
     ((eq filter 'unattributed)
      (seq-filter (lambda (e) (null (nth 3 e))) entries))
     (t (seq-filter (lambda (e) (equal (nth 3 e) filter)) entries)))))

(defun org-change-overview--header ()
  "Return the header line naming the active author filter."
  (concat "Author: "
	  (cond ((null org-change-overview--filter) "all")
		((eq org-change-overview--filter 'unattributed) "(unattributed)")
		(t org-change-overview--filter))))
```

Then, in `org-change-overview--render`, change the `entries` binding to filter, and set the header. Replace:

```elisp
  (let* ((source org-change-overview--source)
	 (entries (and (buffer-live-p source)
		       (org-change--overview-entries source)))
```

with:

```elisp
  (let* ((source org-change-overview--source)
	 (entries (and (buffer-live-p source)
		       (org-change-overview--filter-entries
			(org-change--overview-entries source))))
```

and, immediately after `(inhibit-read-only t))` opens the body (before `(erase-buffer)`), add:

```elisp
    (setq header-line-format (org-change-overview--header))
```

- [ ] **Step 5: Add the command and bind `a`**

In `org-change.el`, immediately before `(defun org-change-overview-goto ()`:

```elisp
(defun org-change-overview-filter ()
  "Show only the changes of one author, or of none, or all again.
Prompts for an author present in the buffer, with a (unattributed)
choice when there are un-tagged changes and an All choice to clear
the filter."
  (interactive)
  (unless (buffer-live-p org-change-overview--source)
    (user-error "The buffer this overview describes is gone"))
  (let* ((present (with-current-buffer org-change-overview--source
		    (org-change--authors-present)))
	 (alist (append
		 (mapcar (lambda (id)
			   (cons (org-change--author-label id) id))
			 (car present))
		 (when (cdr present) '(("(unattributed)" . unattributed)))
		 '(("All" . nil))))
	 (choice (completing-read "Show author: " (mapcar #'car alist) nil t)))
    (setq org-change-overview--filter (cdr (assoc choice alist)))
    (org-change-overview--render)))
```

In the `org-change-overview-mode-map` definition, add after the `(define-key map (kbd "RET") ...)` line:

```elisp
    (define-key map "a" #'org-change-overview-filter)
```

- [ ] **Step 6: Reset the filter on open, document `a`**

In `org-change-overview`, where it does `(setq org-change-overview--source source)`, add on the next line:

```elisp
      (setq org-change-overview--filter nil)
```

In the `org-change-overview-mode` docstring, change the sentence listing keys to include the filter — replace `press \\[org-change-overview-goto] to go\nto the change on this line,` with:

```elisp
press \\[org-change-overview-goto] to go
to the change on this line, \\[org-change-overview-filter] to show
only one author's changes,
```

- [ ] **Step 7: Run the tests, verify they pass**

Selector `"overview-filter\\|filters-to"`. Expected: all `passed`.

- [ ] **Step 8: Byte-compile and full suite.**

- [ ] **Step 9: Commit**

```bash
git add org-change.el test/org-change-tests.el
git commit -m "Filter the overview by author

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 5: Accept / reject by author

**Files:**
- Modify: `org-change.el` — add `org-change--author-match-p`, `org-change--count-by-author`, `org-change--apply-by-author`, `org-change--read-present-author`, `org-change--by-author`, and the two commands `org-change-accept-by-author` / `org-change-reject-by-author`. Put them in the accept/reject area, after `org-change-accept-reject-all`.
- Test: `test/org-change-tests.el`

**Interfaces:**
- Consumes: `org-change--change-author`, `org-change--apply-change`, `org-change--authors-present`, `org-change--author-label`, `org-change--overview-update`, `org-change--search-forward`.
- Produces:
  - `(org-change--author-match-p AUTHOR)` — non-nil if the change just matched belongs to AUTHOR (an id string, or the symbol `unattributed`). Saves match data.
  - `(org-change--count-by-author AUTHOR BEG END)` → integer.
  - `(org-change--apply-by-author ACCEPT AUTHOR BEG END)` → integer count applied.
  - `(org-change-accept-by-author)` / `(org-change-reject-by-author)` — interactive.

- [ ] **Step 1: Write the failing tests**

Append before `(provide 'org-change-tests)`:

```elisp
;;; Accepting and rejecting by author

(defun org-change-tests--by-author (fn author yes text)
  "Insert TEXT, run FN with the author prompt → AUTHOR and y-or-n-p → YES.
Return the resulting buffer text."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (org-change-mode 1)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) author))
	      ((symbol-function 'y-or-n-p) (lambda (&rest _) yes)))
      (funcall fn))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest org-change-test-accept-by-author-takes-only-that-author ()
  "Accepting by author acts on that author's changes and no others."
  (should (equal (org-change-tests--by-author
		  #'org-change-accept-by-author "SG  (SG)" t
		  "a {!x!}{!!}{!@SG!} b {!y!}{!!}{!@MR!} c {!z!}{!!}{!@SG!} d")
		 "a x b {!y!}{!!}{!@MR!} c z d")))

(ert-deftest org-change-test-reject-by-author-takes-only-that-author ()
  "Rejecting by author restores that author's old text only."
  (should (equal (org-change-tests--by-author
		  #'org-change-reject-by-author "MR  (MR)" t
		  "a {!x!}{!o!}{!@SG!} b {!y!}{!p!}{!@MR!} c")
		 "a {!x!}{!o!}{!@SG!} b p c")))

(ert-deftest org-change-test-by-author-declined-does-nothing ()
  "Answering no to the confirmation leaves the buffer untouched."
  (should (equal (org-change-tests--by-author
		  #'org-change-accept-by-author "SG  (SG)" nil
		  "a {!x!}{!!}{!@SG!} b")
		 "a {!x!}{!!}{!@SG!} b")))

(ert-deftest org-change-test-by-author-targets-unattributed ()
  "The (unattributed) choice acts on the un-tagged changes."
  (should (equal (org-change-tests--by-author
		  #'org-change-accept-by-author "(unattributed)" t
		  "a {!x!}{!!}{!@SG!} b {!y!}{!!} c")
		 "a {!x!}{!!}{!@SG!} b y c")))

(ert-deftest org-change-test-by-author-respects-the-region ()
  "With a region active, only changes inside it are considered."
  (with-temp-buffer
    (org-mode)
    (insert "a {!x!}{!!}{!@SG!} b {!z!}{!!}{!@SG!} c")
    (org-change-mode 1)
    (org-change-tests--mark-region 1 18)	; only the first SG change
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "SG  (SG)"))
	      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (org-change-accept-by-author))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "a x b {!z!}{!!}{!@SG!} c"))))

(ert-deftest org-change-test-by-author-reports-the-count ()
  "The confirmation prompt names how many changes will be affected."
  (with-temp-buffer
    (org-mode)
    (insert "a {!x!}{!!}{!@SG!} b {!z!}{!!}{!@SG!} c")
    (org-change-mode 1)
    (let (asked)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "SG  (SG)"))
		((symbol-function 'y-or-n-p)
		 (lambda (prompt &rest _) (setq asked prompt) nil)))
	(org-change-accept-by-author))
      (should (equal asked "Accept 2 changes by SG? ")))))
```

- [ ] **Step 2: Run the tests, verify they fail**

Selector `"by-author"`. Expected: FAIL (commands void).

- [ ] **Step 3: Implement the predicate, count, and apply walk**

In `org-change.el`, immediately after `(defun org-change-accept-reject-all ...)` (its closing paren, before `;;; Comments`):

```elisp
(defun org-change--author-match-p (author)
  "Non-nil if the change just matched belongs to AUTHOR.
AUTHOR is an id string, or the symbol `unattributed' for a change
with no author.  Saves the match data."
  (let ((this (org-change--change-author)))
    (if (eq author 'unattributed)
	(null this)
      (equal this author))))

(defun org-change--count-by-author (author beg end)
  "Return the number of AUTHOR's changes between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((limit (copy-marker end t)) (count 0))
      (while (org-change--search-forward limit t)
	(when (org-change--author-match-p author)
	  (setq count (1+ count))))
      (set-marker limit nil)
      count)))

(defun org-change--apply-by-author (accept author beg end)
  "Accept or reject AUTHOR's changes between BEG and END.
ACCEPT is as in `org-change--apply-change'.  Return the number of
changes acted on.  Positions are markers, so the text shifting under
the walk does not derail it."
  (let ((limit (copy-marker end t)) (count 0))
    (save-excursion
      (goto-char beg)
      (while (org-change--search-forward limit t)
	(let ((matches (org-change--author-match-p author))
	      (change-beg (match-beginning 0))
	      (stop (match-end 0)))
	  (if matches
	      (let ((done (progn (goto-char change-beg)
				 (org-change--apply-change accept))))
		(if done
		    (progn (setq count (1+ count))
			   ;; `--apply-change' leaves point put; step past
			   ;; the replacement so the walk moves on.
			   (goto-char done))
		  (goto-char (1+ change-beg))))
	    (goto-char stop)))))
    (set-marker limit nil)
    count))
```

- [ ] **Step 4: Implement the reader and the interactive glue**

Immediately after `org-change--apply-by-author`:

```elisp
(defun org-change--read-present-author (prompt)
  "Read one of the authors present in the current buffer for PROMPT.
Return an id string, or the symbol `unattributed'.  Signal a
`user-error' when there is nothing to choose."
  (let* ((present (org-change--authors-present))
	 (alist (append
		 (mapcar (lambda (id)
			   (cons (org-change--author-label id) id))
			 (car present))
		 (when (cdr present) '(("(unattributed)" . unattributed))))))
    (unless alist
      (user-error "No changes to select"))
    (cdr (assoc (completing-read prompt (mapcar #'car alist) nil t) alist))))

(defun org-change--by-author (accept)
  "Accept (ACCEPT is t) or reject (ACCEPT is nil) one author's changes.
Acts on the active region if there is one, otherwise the whole
buffer, after confirming how many changes will be affected."
  (let* ((verb (if accept "Accept" "Reject"))
	 (author (org-change--read-present-author
		  (format "%s changes by author: " verb)))
	 (region (use-region-p))
	 (beg (if region (region-beginning) (point-min)))
	 (end (if region (region-end) (point-max)))
	 (count (org-change--count-by-author author beg end)))
    (if (zerop count)
	(message "No changes to %s" (downcase verb))
      (when (y-or-n-p
	     (if (eq author 'unattributed)
		 (format "%s %d unattributed change%s? "
			 verb count (if (= count 1) "" "s"))
	       (format "%s %d change%s by %s? "
		       verb count (if (= count 1) "" "s") author)))
	(let ((done (org-change--apply-by-author accept author beg end)))
	  (when region (deactivate-mark))
	  (org-change--overview-update)
	  (message "%d change%s %s"
		   done (if (= done 1) "" "s")
		   (if accept "accepted" "rejected")))))))

(defun org-change-accept-by-author ()
  "Accept every change by one author, in the region or the whole buffer."
  (interactive)
  (org-change--by-author t))

(defun org-change-reject-by-author ()
  "Reject every change by one author, in the region or the whole buffer."
  (interactive)
  (org-change--by-author nil))
```

- [ ] **Step 5: Run the tests, verify they pass**

Selector `"by-author"`. Expected: all `passed`.

- [ ] **Step 6: Byte-compile and full suite.**

- [ ] **Step 7: Commit**

```bash
git add org-change.el test/org-change-tests.el
git commit -m "Add accept-by-author and reject-by-author

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 6: Documentation and version bump

**Files:**
- Modify: `README.org` (new subsection + the overview key list), `org-change.el` (commentary + `;; Version:`).

- [ ] **Step 1: Document the overview filter key**

In `README.org`, in the overview key list under "A bird's eye view of the changes", add after the `RET` line:

```org
- ~a~ :: filter the list to one author, to the unattributed changes, or back to all.
```

- [ ] **Step 2: Add an "Authors and reviewing by author" subsection**

In `README.org`, in the "Authors" section, after the existing text, add:

```org
You can set the current author interactively with ~M-x org-change-set-author~, which completes over ~org-change-authors~ and offers a "No author" choice; an id you type that is not in the list is used as-is, with no color until you register it.

When several people have marked changes, you can review one at a time. In the overview (see [[A bird's eye view of the changes]]) each change shows its author, and ~a~ filters the list to a chosen author, to the changes that carry no author, or back to all. To dispose of a whole author's changes at once, ~M-x org-change-accept-by-author~ and ~M-x org-change-reject-by-author~ prompt for an author, count the matching changes in the region (or the whole buffer), and ask for confirmation before acting.
```

- [ ] **Step 3: Update the commentary**

In `org-change.el`, in the `;;; Commentary:` block, after the sentence about `org-change-info`/`org-change-overview`, add:

```elisp
;; Attribute changes to authors with org-change-author (set it with
;; org-change-set-author), review one author at a time in the overview,
;; and accept or reject a whole author's changes with
;; org-change-accept-by-author and org-change-reject-by-author.
```

- [ ] **Step 4: Bump the version**

In `org-change.el`, change `;; Version: 0.11.2` to `;; Version: 0.12.0` (a feature release).

- [ ] **Step 5: Byte-compile and full suite** one last time.

- [ ] **Step 6: Commit and push**

```bash
git add org-change.el README.org
git commit -m "Document author-aware features; bump to 0.12.0

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
git push
```

---

## Self-review notes

- **Spec coverage:** helpers (Task 1) ↔ spec §1; set-author (Task 2) ↔ §2; overview column (Task 3) + filter (Task 4) ↔ §3; by-author (Task 5) ↔ §4; testing woven through each task ↔ spec Testing; docs/version (Task 6). Interaction model (text stays modeless, `n`/`p` untouched) — respected: no task alters navigation. Out-of-scope items are not implemented.
- **Type consistency:** entry shape `(MARKER LINE SUMMARY AUTHOR)` introduced in Task 3 and consumed by `--filter-entries` (Task 4). `--author-match-p` accepts id-or-`unattributed`, matching `--read-present-author`'s return and `--count/apply-by-author`'s parameter. Picker return values (`cdr (assoc ...)`) are id string / `unattributed` / nil consistently.
- **Filter value nil:** in the overview picker, the `All` entry maps to nil and `completing-read` uses `require-match t`, so `(cdr (assoc choice alist))` is unambiguously the filter value.
