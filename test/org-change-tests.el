;;; org-change-tests.el --- Tests for org-change -*- lexical-binding: t; -*-

;; Run with:
;;   emacs -batch -l ert -l org-change.el -l test/org-change-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ox)
(require 'org-change)

(defun org-change-tests--displays ()
  "Return the `display' properties of all org-change overlays in the buffer."
  (delq nil
	(mapcar (lambda (ov) (overlay-get ov 'display))
		(overlays-in (point-min) (point-max)))))

(defun org-change-tests--type (char)
  "Insert CHAR as `self-insert-command' would, running its hooks.
`execute-kbd-macro' cannot be used here: in batch mode it empties
the buffer."
  (let ((last-command-event char))
    (self-insert-command 1 char)))

(defun org-change-tests--messages-while (thunk)
  "Return the list of messages `message' emits while THUNK runs."
  (let ((collected '()))
    (cl-letf (((symbol-function 'message)
	       (lambda (format-string &rest args)
		 (push (apply #'format format-string args) collected))))
      (funcall thunk))
    (nreverse collected)))

(defun org-change-tests--mark-region (beg end)
  "Make BEG to END an active region that `use-region-p' recognizes."
  (setq-local transient-mark-mode t)
  (goto-char beg)
  (set-mark beg)
  (goto-char end)
  (activate-mark))

(defun org-change-tests--materialize (markup group)
  "Reduce MARKUP to one side of its changes.
GROUP 1 keeps the new text (accept all), GROUP 2 the old text
\(reject all); equal text is left as is."
  (with-temp-buffer
    (insert markup)
    (goto-char (point-min))
    (while (org-change--search-forward nil t)
      (replace-match (org-change--decode (or (match-string group) "")) t t))
    (buffer-string)))

;;; Fontification of the empty change {!!}{!!}

(ert-deftest org-change-test-empty-change-is-not-shown-as-deletion ()
  "An empty change is an empty addition, not a deletion.
It must not display `org-change-deleted-marker'."
  (with-temp-buffer
    (insert "a {!!}{!!} b")
    (org-change-mode 1)
    (goto-char 5)			; inside the change
    (org-change-fontify)
    (should-not (member org-change-deleted-marker
			(org-change-tests--displays)))))

(ert-deftest org-change-test-empty-change-is-shown-as-a-space ()
  "An empty change looks like a fresh `org-change-add' placeholder."
  (with-temp-buffer
    (insert "a {!!}{!!} b")
    (org-change-mode 1)
    (goto-char 5)
    (org-change-fontify)
    (should (member " " (org-change-tests--displays)))))

(ert-deftest org-change-test-real-deletion-is-still-shown-as-deleted ()
  "A deletion with old text keeps displaying `org-change-deleted-marker'."
  (with-temp-buffer
    (insert "a {!!}{!gone!} b")
    (org-change-mode 1)
    (org-change-fontify)
    (should (member org-change-deleted-marker
		    (org-change-tests--displays)))))

;;; Cleanup of the empty change

(ert-deftest org-change-test-empty-change-removed-when-point-outside ()
  "An empty change is deleted from the buffer once point leaves it."
  (with-temp-buffer
    (insert "a {!!}{!!} b")
    (org-change-mode 1)
    (goto-char (point-max))
    (org-change--cleanup-empty)
    (should (equal (buffer-string) "a  b"))))

(ert-deftest org-change-test-empty-change-kept-when-point-inside ()
  "An empty change survives while point is inside it, so typing resumes it."
  (with-temp-buffer
    (insert "a {!!}{!!} b")
    (org-change-mode 1)
    (goto-char 5)			; where point sits after backspacing
    (org-change--cleanup-empty)
    (should (equal (buffer-string) "a {!!}{!!} b"))))

(ert-deftest org-change-test-empty-change-removed-at-buffer-edges ()
  "Cleanup works when the empty change starts the buffer."
  (with-temp-buffer
    (insert "{!!}{!!}tail")
    (org-change-mode 1)
    (goto-char (point-max))
    (org-change--cleanup-empty)
    (should (equal (buffer-string) "tail"))))

(ert-deftest org-change-test-empty-change-with-comment-is-kept ()
  "An empty change carrying a comment is left alone: never destroy typed text."
  (with-temp-buffer
    (insert "a {!!}{!!}{!note!} b")
    (org-change-mode 1)
    (goto-char (point-max))
    (org-change--cleanup-empty)
    (should (equal (buffer-string) "a {!!}{!!}{!note!} b"))))

(ert-deftest org-change-test-several-empty-changes-removed-at-once ()
  "Cleanup removes every empty change point is not inside."
  (with-temp-buffer
    (insert "a {!!}{!!} b {!!}{!!} c")
    (org-change-mode 1)
    (goto-char (point-max))
    (org-change--cleanup-empty)
    (should (equal (buffer-string) "a  b  c"))))

(ert-deftest org-change-test-cleanup-leaves-real-changes-alone ()
  "Cleanup never touches changes that carry text."
  (with-temp-buffer
    (insert "{!new!}{!old!} and {!!}{!gone!} and {! !}{!!}")
    (org-change-mode 1)
    (goto-char (point-max))
    (org-change--cleanup-empty)
    (should (equal (buffer-string)
		   "{!new!}{!old!} and {!!}{!gone!} and {! !}{!!}"))))

;;; The typing placeholder inserted by org-change-add and org-change-replace

(ert-deftest org-change-test-placeholder-space-removed-on-typing ()
  "Typing on the placeholder replaces it rather than adding to it."
  (with-temp-buffer
    (org-change-mode 1)
    (org-change-add)
    (org-change-tests--type ?x)
    (should (equal (buffer-string) "{!x!}{!!}"))))

(ert-deftest org-change-test-placeholder-does-not-eat-a-distant-character ()
  "Typing elsewhere after `org-change-add' must not delete an innocent char."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-min))
    (org-change-mode 1)
    (org-change-add)			; buffer is now "{! !}{!!}hello"
    (goto-char 11)			; between the h and the e
    (org-change-tests--type ?z)
    (should (equal (buffer-string) "{! !}{!!}hzello"))))

(ert-deftest org-change-test-replace-placeholder-space-removed-on-typing ()
  "The `org-change-replace' placeholder behaves like the `org-change-add' one."
  (with-temp-buffer
    (insert "old")
    (org-change-mode 1)
    (org-change-tests--mark-region (point-min) (point-max))
    (org-change-replace)
    (org-change-tests--type ?x)
    (should (equal (buffer-string) "{!x!}{!old!}"))))

;;; Progress reporting

(ert-deftest org-change-test-typing-reports-no-progress ()
  "Refontifying after a keystroke must not talk to the echo area."
  (with-temp-buffer
    (insert "a {!new!}{!old!} b")
    (org-change-mode 1)
    (goto-char (point-max))
    (should-not (org-change-tests--messages-while
		 (lambda () (org-change-tests--type ?x))))))

(ert-deftest org-change-test-cleanup-reports-no-progress ()
  "Sweeping an empty change must not talk to the echo area either."
  (with-temp-buffer
    (insert "a {!!}{!!} b")
    (org-change-mode 1)
    (goto-char (point-max))
    (should-not (org-change-tests--messages-while
		 #'org-change--cleanup-empty))))

(ert-deftest org-change-test-marking-a-change-reports-no-progress ()
  "Creating a change fontifies only the new markup: nothing to report."
  (with-temp-buffer
    (insert "text")
    (org-change-mode 1)
    (goto-char (point-max))
    (should-not (org-change-tests--messages-while #'org-change-add))))

(ert-deftest org-change-test-fontifying-whole-buffer-reports-progress ()
  "Fontifying the buffer on request still reports progress."
  (with-temp-buffer
    (insert "a {!new!}{!old!} b")
    (org-change-mode 1)
    (should (org-change-tests--messages-while #'org-change-fontify))))

;;; Export

(ert-deftest org-change-test-empty-change-exports-to-nothing ()
  "An empty change must not export as an empty deletion or addition."
  (with-temp-buffer
    (insert "a {!!}{!!} b")
    (org-change--before-processing 'latex)
    (should (equal (buffer-string) "a  b"))))

(ert-deftest org-change-test-deletion-still-exports-as-deleted ()
  "Regression guard: a real deletion still exports with \\deleted."
  (with-temp-buffer
    (insert "a {!!}{!gone!} b")
    (org-change--before-processing 'latex)
    (should (equal (buffer-string) "a @@latex:\\deleted{gone}@@ b"))))

;;; Adjacent changes (comment/pair ambiguity)

(ert-deftest org-change-test-adjacent-changes-are-two-changes ()
  "Two changes that touch must not merge: the second must not be read
as a comment of the first."
  (with-temp-buffer
    (insert "{!new1!}{!old1!}{!new2!}{!old2!}")
    (org-change--before-processing 'latex)
    (should (equal (buffer-string)
		   (concat "@@latex:\\replaced{new1}{old1}@@"
			   "@@latex:\\replaced{new2}{old2}@@")))))

(ert-deftest org-change-test-at-change-picks-the-second-of-two-adjacent ()
  "Point in the second of two adjacent changes finds only that change."
  (with-temp-buffer
    (insert "{!new1!}{!old1!}{!new2!}{!old2!}")
    (org-change-mode 1)
    (goto-char 20)			; inside new2
    (let ((span (org-change--at-change)))
      (should span)
      (should (equal (buffer-substring-no-properties (car span) (cdr span))
		     "{!new2!}{!old2!}")))))

(ert-deftest org-change-test-reject-second-of-two-adjacent-changes ()
  "Rejecting the second of two adjacent changes leaves the first intact."
  (with-temp-buffer
    (insert "{!new1!}{!old1!}{!new2!}{!old2!}")
    (org-change-mode 1)
    (goto-char 20)			; inside new2
    (org-change-reject)
    (should (equal (buffer-string) "{!new1!}{!old1!}old2"))))

(ert-deftest org-change-test-reject-first-of-two-adjacent-changes ()
  "Rejecting the first of two adjacent changes leaves the second intact."
  (with-temp-buffer
    (insert "{!new1!}{!old1!}{!new2!}{!old2!}")
    (org-change-mode 1)
    (goto-char 4)			; inside new1
    (org-change-reject)
    (should (equal (buffer-string) "old1{!new2!}{!old2!}"))))

(ert-deftest org-change-test-accepting-a-deletion-then-rejecting-neighbor ()
  "The reported scenario: accepting a deletion abuts its neighbours,
and rejecting one of them must still touch only that one."
  (with-temp-buffer
    (insert "{!A!}{!B!}{!!}{!gone!}{!C!}{!D!}")
    (org-change-mode 1)
    (goto-char 12)			; inside the deletion
    (org-change-accept)			; removes it -> the pairs now abut
    (should (equal (buffer-string) "{!A!}{!B!}{!C!}{!D!}"))
    (goto-char 4)			; inside the first change
    (org-change-reject)
    (should (equal (buffer-string) "B{!C!}{!D!}"))))

(ert-deftest org-change-test-comment-still-captured-at-end ()
  "A trailing comment on a lone change is still captured."
  (with-temp-buffer
    (insert "{!new!}{!old!}{!cmt!}")
    (org-change--before-processing 'latex)
    (should (equal (buffer-string)
		   "@@latex:\\replaced[comment=cmt]{new}{old}@@"))))

(ert-deftest org-change-test-comment-then-spaced-change-both-captured ()
  "A comment followed by whitespace and another change stays a comment."
  (with-temp-buffer
    (insert "{!new!}{!old!}{!cmt!} {!new2!}{!old2!}")
    (org-change--before-processing 'latex)
    (should (equal (buffer-string)
		   (concat "@@latex:\\replaced[comment=cmt]{new}{old}@@"
			   " @@latex:\\replaced{new2}{old2}@@")))))

;;; Escaping delimiters in change content

(ert-deftest org-change-test-encode-decode-round-trips ()
  "Encoding then decoding returns the original string."
  (dolist (s '("plain text"
	       "has a close !} in it"
	       "has an open {! in it"
	       "both {! and !} here"
	       "the tricky {!} sequence"
	       "\\LaTeX and \\(a=1\\) backslashes"))
    (should (equal (org-change--decode (org-change--encode s)) s))))

(ert-deftest org-change-test-encode-leaves-bare-latex-backslashes-alone ()
  "Encoding touches only the delimiters, never other backslashes."
  (should (equal (org-change--encode "\\(a=1\\)") "\\(a=1\\)")))

(ert-deftest org-change-test-encoded-content-has-no-bare-delimiters ()
  "Encoded content must not contain the bare delimiters that would
confuse the parser."
  (let ((enc (org-change--encode "x!}y{!z")))
    (should-not (string-match-p (regexp-quote "!}") enc))
    (should-not (string-match-p (regexp-quote "{!") enc))))

(ert-deftest org-change-test-delete-region-containing-close-delimiter ()
  "Deleting a region that contains !} produces valid markup, and
rejecting restores the text exactly."
  (with-temp-buffer
    (insert "a x!}y b")
    (org-change-mode 1)
    (org-change-tests--mark-region 3 7)	; "x!}y"
    (org-change-delete)
    ;; The change parses as exactly one change.
    (goto-char 4)
    (should (org-change--at-change))
    (org-change-reject)
    (should (equal (buffer-string) "a x!}y b"))))

(ert-deftest org-change-test-replace-region-containing-open-delimiter ()
  "Replacing a region that contains {! and rejecting restores it."
  (with-temp-buffer
    (insert "a {!x b")
    (org-change-mode 1)
    (org-change-tests--mark-region 3 6)	; "{!x"
    (org-change-replace)
    (goto-char 4)
    (org-change-reject)
    (should (equal (buffer-string) "a {!x b"))))

(ert-deftest org-change-test-accept-decodes-new-text ()
  "Accepting an addition whose new text contained !} restores it."
  (with-temp-buffer
    (insert "p!}q")
    (org-change-mode 1)
    (org-change-tests--mark-region (point-min) (point-max))
    (org-change-add)
    (goto-char 4)
    (org-change-accept)
    (should (equal (buffer-string) "p!}q"))))

(ert-deftest org-change-test-export-decodes-content ()
  "Export materializes the real content, not its escaped form."
  (with-temp-buffer
    (insert "{!a!\\}b!}{!old!}")	; new text is the escaped "a!}b"
    (org-change--before-processing 'latex)
    (should (equal (buffer-string) "@@latex:\\replaced{a!}b}{old}@@"))))

(ert-deftest org-change-test-escaped-delimiter-does-not-merge-adjacent ()
  "An escaped !} in one change's content must not swallow the next change."
  (with-temp-buffer
    (insert "{!!}{!x!\\}y!}{!A!}{!B!}")	; deletion of "x!}y", then a replacement
    (org-change--before-processing 'latex)
    (should (equal (buffer-string)
		   (concat "@@latex:\\deleted{x!}y}@@"
			   "@@latex:\\replaced{A}{B}@@")))))

(ert-deftest org-change-test-latex-backslashes-survive-a-change ()
  "Marking LaTeX as a deletion and rejecting restores it verbatim."
  (with-temp-buffer
    (insert "see \\(a=1\\) now")
    (org-change-mode 1)
    (org-change-tests--mark-region 5 11)	; "\\(a=1\\)"
    (org-change-delete)
    (goto-char 6)
    (org-change-reject)
    (should (equal (buffer-string) "see \\(a=1\\) now"))))

;;; Navigation between changes

;; Buffer used below: "aa {!n1!}{!o1!} bb {!n2!}{!o2!} cc"
;; change 1 begins at position 4, change 2 begins at position 20.

(ert-deftest org-change-test-next-goes-to-first-change ()
  "From before any change, next moves to the first change."
  (with-temp-buffer
    (insert "aa {!n1!}{!o1!} bb {!n2!}{!o2!} cc")
    (org-change-mode 1)
    (goto-char (point-min))
    (org-change-next-change)
    (should (= (point) 4))))

(ert-deftest org-change-test-next-skips-the-current-change ()
  "From inside a change, next moves to the following change, not this one."
  (with-temp-buffer
    (insert "aa {!n1!}{!o1!} bb {!n2!}{!o2!} cc")
    (org-change-mode 1)
    (goto-char 6)			; inside change 1
    (org-change-next-change)
    (should (= (point) 20))))

(ert-deftest org-change-test-next-at-last-change-messages-and-stays ()
  "With no change ahead, next reports it and leaves point put."
  (with-temp-buffer
    (insert "aa {!n1!}{!o1!} bb {!n2!}{!o2!} cc")
    (org-change-mode 1)
    (goto-char 22)			; inside change 2, the last one
    (should (member "No next change"
		    (org-change-tests--messages-while #'org-change-next-change)))
    (should (= (point) 22))))

(ert-deftest org-change-test-previous-goes-to-previous-change ()
  "From inside a change, previous moves to the change before it."
  (with-temp-buffer
    (insert "aa {!n1!}{!o1!} bb {!n2!}{!o2!} cc")
    (org-change-mode 1)
    (goto-char 22)			; inside change 2
    (org-change-previous-change)
    (should (= (point) 4))))

(ert-deftest org-change-test-previous-at-first-change-messages-and-stays ()
  "With no change behind, previous reports it and leaves point put."
  (with-temp-buffer
    (insert "aa {!n1!}{!o1!} bb {!n2!}{!o2!} cc")
    (org-change-mode 1)
    (goto-char 6)			; inside change 1, the first one
    (should (member "No previous change"
		    (org-change-tests--messages-while #'org-change-previous-change)))
    (should (= (point) 6))))

;;; Generating changes from two versions

(ert-deftest org-change-test-diff-identical-produces-no-markup ()
  "Diffing a version against itself leaves the text untouched."
  (let ((s "the quick brown fox\njumped over"))
    (should (equal (org-change--diff-to-markup s s) s))))

(ert-deftest org-change-test-diff-replacement ()
  "A changed word becomes a replacement."
  (should (equal (org-change--diff-to-markup "the cat" "the dog")
		 "the {!dog!}{!cat!}")))

(ert-deftest org-change-test-diff-round-trips-to-new-and-old ()
  "The markup accepts to the new version and rejects to the old one."
  (dolist (pair '(("the cat sat" . "the dog sat")	; replacement
		  ("a c" . "a b c")			; addition
		  ("a b c" . "a c")			; deletion
		  ("hello world" . "hi there")		; multi-word change
		  ("keep\nold line\nkeep" . "keep\nnew line\nkeep") ; multi-line
		  ("" . "all new")			; from empty
		  ("all gone" . "")))			; to empty
    (let* ((old (car pair))
	   (new (cdr pair))
	   (markup (org-change--diff-to-markup old new)))
      (should (equal (org-change-tests--materialize markup 1) new))
      (should (equal (org-change-tests--materialize markup 2) old)))))

(ert-deftest org-change-test-diff-encodes-delimiters-in-content ()
  "Diffed text that contains the delimiters still round-trips."
  (let* ((old "a plain b")
	 (new "a x!}y{!z b")
	 (markup (org-change--diff-to-markup old new)))
    (should (equal (org-change-tests--materialize markup 1) new))
    (should (equal (org-change-tests--materialize markup 2) old))))

(ert-deftest org-change-test-from-diff-file-replaces-buffer ()
  "`org-change-from-diff' shows the incoming file version as changes
over the buffer: the buffer is the base, so it becomes the old text
and rejecting restores it."
  (let ((new-file (make-temp-file "org-change-new")))
    (unwind-protect
	(progn
	  (with-temp-file new-file (insert "the cat"))
	  (with-temp-buffer
	    (insert "the dog")		; the base version, in the buffer
	    (setq buffer-file-name nil)
	    (org-change-from-diff (cons 'file new-file))
	    (should (equal (buffer-string) "the {!cat!}{!dog!}"))
	    (should org-change-mode)
	    ;; Rejecting keeps the buffer's original text.
	    (should (equal (org-change-tests--materialize (buffer-string) 2)
			   "the dog"))))
      (delete-file new-file))))

(ert-deftest org-change-test-from-diff-git-bails-without-a-file ()
  "Git mode reports clearly when the buffer visits no file."
  (with-temp-buffer
    (insert "text")
    (should-error (org-change-from-diff (cons 'git "HEAD"))
		  :type 'user-error)))

;;; Revealing folded changes when navigating

(defun org-change-tests--change-pos (n)
  "Return the buffer position of the Nth change (1-based)."
  (save-excursion
    (goto-char (point-min))
    (let ((i 0) (pos nil))
      (while (and (< i n) (org-change--search-forward nil t))
	(setq i (1+ i) pos (match-beginning 0)))
      pos)))

(ert-deftest org-change-test-jump-reveals-a-folded-change ()
  "Jumping to a change under a folded heading reveals it."
  (with-temp-buffer
    (org-mode)
    (insert "* A\nintro {!x!}{!y!} end\n* B\ntail\n")
    (org-change-mode 1)
    (org-fold-hide-sublevels 1)
    (let ((change (org-change-tests--change-pos 1)))
      (should (org-fold-folded-p change))	; folded to begin with
      (goto-char (point-min))
      (org-change-next-change)
      (should (org-change--at-change))		; landed on the change
      (should-not (org-fold-folded-p (point)))))) ; and it is visible

(ert-deftest org-change-test-next-jump-restores-the-previous-reveal ()
  "Stepping to the next change re-folds the one the last step opened."
  (with-temp-buffer
    (org-mode)
    (insert "* A\nintro {!x!}{!y!} end\n* B\nmore {!p!}{!q!} tail\n")
    (org-change-mode 1)
    (org-fold-hide-sublevels 1)
    (goto-char (point-min))
    (org-change-next-change)			; reveal change in A
    (let ((first (point)))
      (should-not (org-fold-folded-p first))
      (org-change-next-change)			; move to B, restore A
      (should-not (org-fold-folded-p (point)))	; B revealed
      (should (org-fold-folded-p first)))))	; A folded again

(ert-deftest org-change-test-reveal-is-a-noop-outside-org ()
  "Navigation still works in a non-org buffer, with nothing to reveal."
  (with-temp-buffer
    (insert "a {!n1!}{!o1!} b {!n2!}{!o2!} c")
    (org-change-mode 1)
    (goto-char (point-min))
    (org-change-next-change)
    (should (= (point) 3))
    (should-not org-change--fold-restore)))

;;; Multi-author support

(ert-deftest org-change-test-split-comment ()
  "The @id prefix is parsed out of a comment as its author."
  (should (equal (org-change--split-comment "@SG") '("SG" . "")))
  (should (equal (org-change--split-comment "@SG needs a citation")
		 '("SG" . "needs a citation")))
  (should (equal (org-change--split-comment "just a note")
		 '(nil . "just a note")))
  (should (equal (org-change--split-comment "") '(nil . ""))))

(ert-deftest org-change-test-creation-stamps-the-author ()
  "Creating a change while `org-change-author' is set records it."
  (let ((org-change-author "SG"))
    (with-temp-buffer
      (insert "gone")
      (org-change-mode 1)
      (org-change-tests--mark-region (point-min) (point-max))
      (org-change-delete)
      (should (equal (buffer-string) "{!!}{!gone!}{!@SG!}")))))

(ert-deftest org-change-test-creation-without-author-adds-no-comment ()
  "With no current author, changes carry no author comment."
  (let ((org-change-author nil))
    (with-temp-buffer
      (insert "gone")
      (org-change-mode 1)
      (org-change-tests--mark-region (point-min) (point-max))
      (org-change-delete)
      (should (equal (buffer-string) "{!!}{!gone!}")))))

(ert-deftest org-change-test-change-face-uses-author-color ()
  "The change face tints the text in the author's color."
  (let ((org-change-authors '(("SG" :name "Stefano" :color "blue"))))
    (should (equal (org-change--change-face "SG")
		   (list :inherit org-change-face :foreground "blue")))
    (should (eq (org-change--change-face "unknown") org-change-face))
    (should (eq (org-change--change-face nil) org-change-face))))

(ert-deftest org-change-test-latex-export-with-author ()
  "LaTeX export maps the author to the changes package id."
  (should (equal (org-change--export-latex "old" "new" "@SG")
		 "@@latex:\\replaced[id=SG]{new}{old}@@"))
  (should (equal (org-change--export-latex "" "new" "@SG")
		 "@@latex:\\added[id=SG]{new}@@"))
  (should (equal (org-change--export-latex "old" "new" "@SG my note")
		 "@@latex:\\replaced[id=SG, comment=my note]{new}{old}@@")))

(ert-deftest org-change-test-latex-export-comment-only-unchanged ()
  "A plain comment still exports as before, with no id."
  (should (equal (org-change--export-latex "old" "new" "cmt")
		 "@@latex:\\replaced[comment=cmt]{new}{old}@@")))

(ert-deftest org-change-test-latex-author-definitions ()
  "Registered authors become changes package definitions."
  (let ((org-change-authors '(("SG" :name "Stefano" :color "blue")
			      ("AB" :name "Alex" :color "red"))))
    (should (equal (org-change--latex-author-defs)
		   (concat "\\definechangesauthor[name={Stefano}, color={blue}]{SG}\n"
			   "\\definechangesauthor[name={Alex}, color={red}]{AB}\n")))))

(ert-deftest org-change-test-before-processing-with-author ()
  "Exporting an authored change through the buffer works end to end.
Guards against the exporter's `string-match' clobbering the match
data `replace-match' needs."
  (with-temp-buffer
    (insert "x {!new!}{!old!}{!@SG!} y")
    (org-change--before-processing 'latex)
    (should (equal (buffer-string)
		   "x @@latex:\\replaced[id=SG]{new}{old}@@ y"))))

(ert-deftest org-change-test-html-export-with-author ()
  "HTML export tags the spans with the author class."
  (should (equal (org-change--export-html "old" "new" "@SG")
		 (concat "@@html:"
			 "<span class=\"org-change-added org-change-author-SG\">new</span>"
			 "<span class=\"org-change-deleted org-change-author-SG\">old</span>"
			 "@@"))))

(provide 'org-change-tests)

;;; org-change-tests.el ends here
