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

(ert-deftest org-change-test-sweep-reveals-each-folded-change ()
  "The buffer sweep reveals each change under a folded heading in turn.
It must not leave a change hidden while asking whether to accept it,
and it must re-fold what it opened once it moves on."
  (with-temp-buffer
    (org-mode)
    (insert "* A\nintro {!x!}{!y!} end\n* B\nmore {!p!}{!q!} tail\n")
    (org-change-mode 1)
    (org-fold-hide-sublevels 1)
    (let ((a (org-change-tests--change-pos 1))
	  (b (org-change-tests--change-pos 2))
	  (seen nil))
      (should (org-fold-folded-p a))
      (should (org-fold-folded-p b))
      ;; Skip both changes, recording whether each was visible, and
      ;; whether the previous one had been re-folded, at its prompt.
      (cl-letf (((symbol-function 'read-char)
		 (lambda (&rest _)
		   (push (list (not (org-fold-folded-p (point)))
			       (and (org-fold-folded-p a) t))
			 seen)
		   ?\s)))
	(org-change-accept-reject-all))
      (setq seen (nreverse seen))
      ;; First prompt: change A visible.
      (should (equal (nth 0 seen) '(t nil)))
      ;; Second prompt: change B visible, and A folded again.
      (should (equal (nth 1 seen) '(t t)))
      ;; Afterwards the buffer is folded as it started.
      (should (org-fold-folded-p a))
      (should (org-fold-folded-p b)))))

(ert-deftest org-change-test-reveal-is-a-noop-outside-org ()
  "Navigation still works in a non-org buffer, with nothing to reveal."
  (with-temp-buffer
    (insert "a {!n1!}{!o1!} b {!n2!}{!o2!} c")
    (org-change-mode 1)
    (goto-char (point-min))
    (org-change-next-change)
    (should (= (point) 3))
    (should-not org-change--fold-restore)))

;;; Comments

(ert-deftest org-change-test-join-comment ()
  "Author and note are joined back into a comment string."
  (should (equal (org-change--join-comment nil "note") "note"))
  (should (equal (org-change--join-comment nil "") ""))
  (should (equal (org-change--join-comment "SG" "") "@SG"))
  (should (equal (org-change--join-comment "SG" "note") "@SG note")))

(ert-deftest org-change-test-comment-adds-to-a-change ()
  "`org-change-comment' adds a comment to a change that has none."
  (with-temp-buffer
    (insert "{!new!}{!old!}")
    (org-change-mode 1)
    (goto-char 4)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "my note")))
      (org-change-comment))
    (should (equal (buffer-string) "{!new!}{!old!}{!my note!}"))))

(ert-deftest org-change-test-comment-edits-an-existing-comment ()
  "`org-change-comment' replaces an existing comment."
  (with-temp-buffer
    (insert "{!new!}{!old!}{!old note!}")
    (org-change-mode 1)
    (goto-char 4)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "new note")))
      (org-change-comment))
    (should (equal (buffer-string) "{!new!}{!old!}{!new note!}"))))

(ert-deftest org-change-test-comment-preserves-the-author ()
  "Editing a comment keeps the change's author."
  (with-temp-buffer
    (insert "{!new!}{!old!}{!@SG old!}")
    (org-change-mode 1)
    (goto-char 4)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "revised")))
      (org-change-comment))
    (should (equal (buffer-string) "{!new!}{!old!}{!@SG revised!}"))))

(ert-deftest org-change-test-comment-empty-input-removes-it ()
  "An empty comment removes the comment group."
  (with-temp-buffer
    (insert "{!new!}{!old!}{!note!}")
    (org-change-mode 1)
    (goto-char 4)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  ")))
      (org-change-comment))
    (should (equal (buffer-string) "{!new!}{!old!}"))))

(ert-deftest org-change-test-comment-display ()
  "The author and note are shown as \"author: note\"."
  (should (equal (org-change--comment-display "SG" "my note") "SG: my note"))
  (should (equal (org-change--comment-display "SG" "") "SG"))
  (should (equal (org-change--comment-display nil "my note") "my note"))
  (should (equal (org-change--comment-display nil "") "")))

(ert-deftest org-change-test-comment-shown-in-italic-with-author ()
  "A change's comment is shown as an italic \"author: note\" after-string."
  (with-temp-buffer
    (insert "a {!new!}{!old!}{!@SG see note!} b")
    (org-change-mode 1)
    (let ((shown nil))
      (dolist (ov (overlays-in (point-min) (point-max)))
	(let ((as (overlay-get ov 'after-string)))
	  (when (and as (string-match-p "see note" as))
	    (setq shown as))))
      (should shown)
      ;; author is shown as "SG: ", not the raw "@SG"
      (should (string-match-p "SG: see note" shown))
      (should-not (string-match-p "@" shown))
      (should (eq (get-text-property (1- (length shown)) 'face shown)
		  'org-change-comment-face)))))

(ert-deftest org-change-test-show-deleted-displays-old-text ()
  "With `org-change-show-deleted', the deleted text is shown after the change.
Guards the after-string overlays against being deleted for being empty."
  (let ((org-change-show-deleted t))
    (with-temp-buffer
      (insert "a {!!}{!gone!} b")
      (org-change-mode 1)
      (let ((shown nil))
	(dolist (ov (overlays-in (point-min) (point-max)))
	  (let ((as (overlay-get ov 'after-string)))
	    (when (and as (string-match-p "gone" as))
	      (setq shown t))))
	(should shown)))))

(ert-deftest org-change-test-comment-shown-only-once ()
  "Adding a comment must not stack two copies of the after-string."
  (with-temp-buffer
    (insert "x {!a!}{!b!} y")
    (org-change-mode 1)
    (goto-char 4)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "note")))
      (org-change-comment))
    (let ((n 0))
      (dolist (ov (overlays-in (point-min) (point-max)))
	(let ((as (overlay-get ov 'after-string)))
	  (when (and as (string-match-p "note" as))
	    (setq n (1+ n)))))
      (should (= n 1)))))

;;; Re-fontifying after edits inside multi-line changes

(ert-deftest org-change-test-editing-inside-multiline-change-keeps-markup-hidden ()
  "Editing a later line of a change that spans blank lines must not
strip the change's overlays: the closing markup stays hidden."
  (with-temp-buffer
    (insert "before {!para one\n\npara two!}{!!} after")
    (org-change-mode 1)
    ;; type a character on the change's last line
    (goto-char (point-min))
    (re-search-forward "para two")
    (goto-char (match-beginning 0))
    (insert "X")				; fires `org-change--after-change'
    ;; the closing !}{!!} must still be hidden by an org-change overlay
    (goto-char (point-min))
    (re-search-forward "!}{!!}")
    (should (get-char-property (match-beginning 0) 'invisible))))

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

;;; Help

(ert-deftest org-change-test-help-string ()
  "The help lists the current keys with a description each."
  (let ((s (org-change--help-string)))
    (should (string-match-p "C-` a" s))
    (should (string-match-p "addition" s))
    (should (string-match-p "C-` i" s))
    (should (string-match-p "C-` h" s))
    ;; a description, not the function name
    (should-not (string-match-p "org-change-add\\b" s))))

(ert-deftest org-change-test-help-follows-custom-keys ()
  "The help reflects a customized key binding."
  (let ((org-change-add-key (kbd "C-c q")))
    (should (string-match-p "C-c q" (org-change--help-string)))))

(ert-deftest org-change-test-help-pops-up-a-buffer ()
  "`org-change-help' displays a help buffer with the bindings."
  (with-temp-buffer
    (org-change-mode 1)
    (save-window-excursion
      (org-change-help))
    (should (get-buffer "*Org Change Help*"))
    (with-current-buffer "*Org Change Help*"
      (should (string-match-p "Reject" (buffer-string))))
    (kill-buffer "*Org Change Help*")))

;;; Counting changes

(ert-deftest org-change-test-counts ()
  "Changes are counted by kind, ignoring the empty change."
  (with-temp-buffer
    (insert "{!a!}{!!} x {!!}{!b!} y {!c!}{!d!} z {!e!}{!!} {!!}{!!}")
    (should (equal (org-change--counts) '(2 1 1)))))

(ert-deftest org-change-test-info-message ()
  "`org-change-info' reports the counts in the minibuffer."
  (with-temp-buffer
    (insert "{!a!}{!!} {!!}{!b!} {!c!}{!d!}")
    (should (member "1 addition, 1 deletion, 1 replacement"
		    (org-change-tests--messages-while #'org-change-info)))))

(ert-deftest org-change-test-info-plurals ()
  "The message pluralizes each count."
  (with-temp-buffer
    (insert "{!a!}{!!} {!e!}{!!}")
    (should (member "2 additions, 0 deletions, 0 replacements"
		    (org-change-tests--messages-while #'org-change-info)))))

(ert-deftest org-change-test-info-no-changes ()
  "With no changes, `org-change-info' says so."
  (with-temp-buffer
    (insert "just plain text")
    (should (member "No changes"
		    (org-change-tests--messages-while #'org-change-info)))))

;;; Accepting and rejecting over a region

(defun org-change-tests--over-region (text beg end fn)
  "Insert TEXT, mark BEG to END, call FN, return the resulting buffer."
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert text)
    (org-change-tests--mark-region beg end)
    (funcall fn)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest org-change-test-region-accept-leaves-changes-outside-alone ()
  "Only changes inside the region are accepted."
  (should (equal (org-change-tests--over-region
		  "one {!A!}{!!} two {!B!}{!!} three"
		  1 15 #'org-change-accept)
		 "one A two {!B!}{!!} three")))

(ert-deftest org-change-test-region-reject-leaves-changes-outside-alone ()
  "Only changes inside the region are rejected."
  (should (equal (org-change-tests--over-region
		  "one {!!}{!A!} two {!!}{!B!} three"
		  1 15 #'org-change-reject)
		 "one A two {!!}{!B!} three")))

(ert-deftest org-change-test-region-handles-every-change-in-it ()
  "A region containing several changes processes all of them.
Point sits at the end of a change, which must not shadow the
region and reduce the command to that one change."
  (should (equal (org-change-tests--over-region
		  "x {!A!}{!!} y {!B!}{!!} z {!C!}{!!} w"
		  3 24 #'org-change-accept)
		 "x A y B z {!C!}{!!} w")))

(ert-deftest org-change-test-region-ignores-a-change-it-only-partly-covers ()
  "A change straddling the region is left alone when point is not in it.
The region is marked backwards, so that point sits at its
beginning, outside any change."
  (should (equal (org-change-tests--over-region
		  "one {!A!}{!!} two {!B!}{!!} three"
		  20 1 #'org-change-accept)
		 "one A two {!B!}{!!} three")))

(ert-deftest org-change-test-region-takes-the-change-at-point-whole ()
  "The change under point is acted on even if the region only reaches into it."
  (should (equal (org-change-tests--over-region
		  "one {!A!}{!!} two {!B!}{!!} three"
		  1 20 #'org-change-accept)
		 "one A two B three")))

(ert-deftest org-change-test-region-takes-a-change-starting-before-it ()
  "The change under point counts even when it starts before the region.
Marked backwards, so point lands inside the first change while
the region reaches to the right."
  (should (equal (org-change-tests--over-region
		  "one {!A!}{!!} two {!B!}{!!} three"
		  26 7 #'org-change-accept)
		 "one A two {!B!}{!!} three")))

(ert-deftest org-change-test-region-with-no-changes-is-harmless ()
  "A region without changes leaves the buffer alone."
  (should (equal (org-change-tests--over-region
		  "nothing to see here"
		  1 8 #'org-change-accept)
		 "nothing to see here")))

(ert-deftest org-change-test-region-reports-how-many-it-did ()
  "Operating on a region says how many changes it touched."
  (should (member "2 changes accepted"
		  (org-change-tests--messages-while
		   (lambda ()
		     (org-change-tests--over-region
		      "x {!A!}{!!} y {!B!}{!!} z"
		      1 24 #'org-change-accept))))))

(ert-deftest org-change-test-accept-without-region-still-works ()
  "Without a region, accept still applies to the change at point."
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert "one {!A!}{!!} two {!B!}{!!} three")
    (goto-char 7)
    (org-change-accept)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "one A two {!B!}{!!} three"))))

;;; Point does not move when a change is accepted or rejected

(ert-deftest org-change-test-accept-keeps-point-on-the-same-character ()
  "Accepting from inside the new text leaves point on that character."
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert "a {!brown!}{!red!} b")
    (goto-char 8)			; the `w' of "brown"
    (should (equal (char-after) ?w))
    (org-change-accept)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "a brown b"))
    (should (equal (char-after) ?w))))

(ert-deftest org-change-test-accept-does-not-jump-to-the-end ()
  "Point must not land past the accepted text."
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert "a {!brown!}{!red!} b")
    (goto-char 5)			; the `b' of "brown"
    (org-change-accept)
    (should (equal (point) 3))))

(ert-deftest org-change-test-reject-keeps-point-inside-the-old-text ()
  "Rejecting puts point at the same offset in the restored text."
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert "a {!brown!}{!red!} b")
    (goto-char 6)			; second character of the new text
    (org-change-reject)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "a red b"))
    (should (equal (char-after) ?e))))

(ert-deftest org-change-test-accept-clamps-point-to-shorter-text ()
  "Point past the end of the replacement lands at its end, not beyond."
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert "a {!brown!}{!red!} b")
    (goto-char 10)			; near the end of "brown"
    (org-change-reject)			; "red" is shorter
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "a red b"))
    (should (<= (point) 6))))

(ert-deftest org-change-test-accept-elsewhere-leaves-point-alone ()
  "Point outside the changes rides along with its own text.
It ends up on the same character it started on, shifted only by
what the accepted change removed before it."
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert "start {!A!}{!!} end")
    (org-change-tests--mark-region 1 18)	; point lands on the `n' of "end"
    (should (equal (char-after) ?n))
    (org-change-accept)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "start A end"))
    (should (equal (char-after) ?n))))

(ert-deftest org-change-test-region-accept-terminates ()
  "Keeping point put must not make the region loop revisit a change.
The accepted text itself contains change delimiters, which a
restarted search could match again."
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert "x {!a{\\!b!}{!!} y")
    (org-change-tests--mark-region (point-min) (point-max))
    (with-timeout (5 (ert-fail "org-change-accept did not terminate"))
      (org-change-accept))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "x a{!b y"))))

;;; Accepting or rejecting and moving on

(ert-deftest org-change-test-accept-and-next-moves-to-the-next-change ()
  "Accept-and-next applies the change and lands on the one after it."
  (with-temp-buffer
    (org-mode)
    (insert "aa {!n1!}{!o1!} bb {!n2!}{!o2!} cc")
    (org-change-mode 1)
    (goto-char 6)			; inside change 1
    (org-change-accept-and-next)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "aa n1 bb {!n2!}{!o2!} cc"))
    (should (= (point) 10))))

(ert-deftest org-change-test-reject-and-next-moves-to-the-next-change ()
  "Reject-and-next restores the old text and lands on the next change."
  (with-temp-buffer
    (org-mode)
    (insert "aa {!n1!}{!o1!} bb {!n2!}{!o2!} cc")
    (org-change-mode 1)
    (goto-char 6)			; inside change 1
    (org-change-reject-and-next)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "aa o1 bb {!n2!}{!o2!} cc"))
    (should (= (point) 10))))

(ert-deftest org-change-test-accept-and-next-at-the-last-change ()
  "With nothing after it, accept-and-next still accepts, and says so."
  (with-temp-buffer
    (org-mode)
    (insert "aa {!n1!}{!o1!} cc")
    (org-change-mode 1)
    (goto-char 6)
    (should (member "No next change"
		    (org-change-tests--messages-while
		     #'org-change-accept-and-next)))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "aa n1 cc"))))

(ert-deftest org-change-test-accept-and-next-over-a-region ()
  "Over a region, accept-and-next takes the region's changes, then moves on."
  (with-temp-buffer
    (org-mode)
    (insert "aa {!n1!}{!o1!} bb {!n2!}{!o2!} cc {!n3!}{!o3!} dd")
    (org-change-mode 1)
    (org-change-tests--mark-region 1 31)	; changes 1 and 2, not 3
    (org-change-accept-and-next)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "aa n1 bb n2 cc {!n3!}{!o3!} dd"))
    (should (= (point) 16))))

;;; Rejecting restores the text exactly

(defun org-change-tests--reject-round-trip (text make)
  "Mark every region of TEXT with MAKE, reject it, return offending cases.
Each case is a list of the region, the markup, and what rejecting
left behind.  An empty list means every region round-tripped."
  (let ((bad nil))
    (dotimes (i (length text))
      (dotimes (j (length text))
	(let ((beg (1+ i)) (end (1+ j)))
	  (when (< beg end)
	    (with-temp-buffer
	      (org-mode)
	      (insert text)
	      (org-change-mode 1)
	      (org-change-tests--mark-region beg end)
	      (funcall make)
	      (let ((markup (buffer-substring-no-properties
			     (point-min) (point-max))))
		(org-change-tests--mark-region (point-min) (point-max))
		(org-change-reject)
		(let ((after (buffer-substring-no-properties
			      (point-min) (point-max))))
		  (unless (equal after text)
		    (push (list (substring text (1- beg) (1- end)) markup after)
			  bad)))))))))
    bad))

(ert-deftest org-change-test-rejecting-a-deletion-restores-the-text ()
  "Rejecting a deletion puts back exactly what was there, spaces and all."
  (should-not (org-change-tests--reject-round-trip
	       "the quick brown fox" #'org-change-delete)))

(ert-deftest org-change-test-rejecting-a-replacement-restores-the-text ()
  "Rejecting a replacement puts back exactly what was there.
In particular the space `org-change-replace' leaves as a typing
placeholder must not survive into the restored text."
  (should-not (org-change-tests--reject-round-trip
	       "the quick brown fox" #'org-change-replace)))

(ert-deftest org-change-test-rejecting-a-kill-restores-the-text ()
  "Rejecting a killed region puts back exactly what was there."
  (should-not (org-change-tests--reject-round-trip
	       "the quick brown fox" #'org-change-kill)))

(ert-deftest org-change-test-rejecting-a-diff-restores-the-old-version ()
  "Rejecting every change from a diff gives back the old version verbatim.
Word-level diffs split text on space runs, so a mishandled
boundary would show up here as a doubled or missing space."
  (let* ((words '("the" "quick" "brown" "fox" "jumps"))
	 (old (string-join words " ")))
    (dotimes (i (length words))
      (dolist (new (list
		    ;; drop word i, replace it, insert before it, drop two
		    (string-join (append (seq-take words i)
					 (seq-drop words (1+ i))) " ")
		    (string-join (append (seq-take words i) (list "new")
					 (seq-drop words (1+ i))) " ")
		    (string-join (append (seq-take words i) (list "new")
					 (seq-drop words i)) " ")
		    (string-join (append (seq-take words i)
					 (seq-drop words (+ i 2))) " ")))
	(with-temp-buffer
	  (org-mode)
	  (insert (org-change--diff-to-markup old new))
	  (org-change-mode 1)
	  (org-change-tests--mark-region (point-min) (point-max))
	  (org-change-reject)
	  (should (equal (buffer-substring-no-properties (point-min) (point-max))
			 old)))))))

;;; Spaces left behind by accepting or rejecting

(defun org-change-tests--apply-at (text pos accept)
  "Insert TEXT, accept or reject the change at POS, return the buffer."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (org-change-mode 1)
    (goto-char pos)
    (if accept (org-change-accept) (org-change-reject))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest org-change-test-accept-joins-spaces-around-a-deletion ()
  "Accepting a deletion selected without its spaces leaves one space."
  (should (equal (org-change-tests--apply-at "the {!!}{!quick!} brown" 7 t)
		 "the brown")))

(ert-deftest org-change-test-reject-joins-spaces-around-an-addition ()
  "Rejecting an addition selected without its spaces leaves one space."
  (should (equal (org-change-tests--apply-at "the {!quick!}{!!} brown" 7 nil)
		 "the brown")))

(ert-deftest org-change-test-accept-drops-an-abandoned-placeholder-space ()
  "The space `org-change-replace' leaves for typing must not survive.
Moving point away abandons the placeholder, so the space stays in
the markup; accepting it would otherwise triple the space."
  (should (equal (org-change-tests--apply-at "the {! !}{!quick!} brown" 7 t)
		 "the brown")))

(ert-deftest org-change-test-accept-keeps-indentation ()
  "A run of spaces the change did not create is left alone.
Only spaces that end up next to each other because of the accept
are joined, so indentation survives."
  (should (equal (org-change-tests--apply-at "   {!new!}{!old!} x" 6 t)
		 "   new x")))

(ert-deftest org-change-test-accept-keeps-a-space-run-inside-the-new-text ()
  "Spaces written inside a change are the author's, and are kept."
  (should (equal (org-change-tests--apply-at "a {!x  y!}{!z!} b" 6 t)
		 "a x  y b")))

(ert-deftest org-change-test-region-accept-joins-spaces ()
  "Joining spaces also happens when accepting over a region."
  (should (equal (org-change-tests--over-region
		  "the {!!}{!quick!} brown {!!}{!red!} fox"
		  1 40 #'org-change-accept)
		 "the brown fox")))

;;; Newlines left behind by accepting or rejecting

(ert-deftest org-change-test-accept-closes-the-gap-left-by-a-deleted-line ()
  "Deleting a line without its newlines must not leave an empty line."
  (should (equal (org-change-tests--apply-at "a\n{!!}{!b!}\nc" 5 t)
		 "a\nc")))

(ert-deftest org-change-test-reject-closes-the-gap-left-by-an-added-line ()
  "Rejecting an added line must not leave an empty line either."
  (should (equal (org-change-tests--apply-at "a\n{!b!}{!!}\nc" 5 nil)
		 "a\nc")))

(ert-deftest org-change-test-accept-keeps-the-paragraph-break ()
  "Deleting a paragraph leaves one blank line, not two."
  (should (equal (org-change-tests--apply-at "p1\n\n{!!}{!p2!}\n\np3" 8 t)
		 "p1\n\np3")))

(ert-deftest org-change-test-accept-keeps-the-wider-of-the-two-gaps ()
  "The larger of the two gaps wins: a paragraph break outranks a newline."
  (should (equal (org-change-tests--apply-at "a\n{!!}{!b!}\n\nc" 5 t)
		 "a\n\nc")))

(ert-deftest org-change-test-accept-keeps-indentation-of-the-next-line ()
  "Closing a gap keeps the indentation the following line carries."
  (should (equal (org-change-tests--apply-at
		  "def f():\n    a\n    {!!}{!b!}\n    c" 24 t)
		 "def f():\n    a\n    c")))

(ert-deftest org-change-test-accept-keeps-blank-lines-inside-a-change ()
  "Blank lines written inside a change are the author's, and are kept."
  (should (equal (org-change-tests--apply-at "a\n{!x\n\ny!}{!z!}\nb" 5 t)
		 "a\nx\n\ny\nb")))

(ert-deftest org-change-test-accept-after-a-join-finds-the-next-change ()
  "Closing a gap must not make the region loop step over what follows.
Here the gap kept is shorter than the one dropped, so the position
the loop resumes from moves; a stale position would skip the second
change."
  (should (equal (org-change-tests--over-region
		  "a   {!!}{!b!}\n{!!}{!c!}d" 1 25 #'org-change-accept)
		 "a\nd")))

;;; The overview side window

(defmacro org-change-tests--with-overview (text &rest body)
  "Insert TEXT in a buffer, open its overview, and run BODY there.
Inside BODY, `source' is the buffer holding TEXT, and the current
buffer is the overview.  Both buffers, and the window the overview
opened, are gone afterwards."
  (declare (indent 1))
  `(let ((source (generate-new-buffer " *org-change-test-source*")))
     (unwind-protect
	 (save-window-excursion
	   (with-current-buffer source
	     (org-mode)
	     (insert ,text)
	     (org-change-mode 1)
	     (goto-char (point-min))
	     (org-change-overview))
	   (with-current-buffer org-change-overview-buffer-name
	     ,@body))
       (kill-buffer source)
       (when (get-buffer org-change-overview-buffer-name)
	 (kill-buffer org-change-overview-buffer-name)))))

(defun org-change-tests--overview-lines ()
  "Return the overview's lines, without their line numbers."
  (mapcar (lambda (line) (string-trim (substring line (min 6 (length line)))))
	  (split-string (buffer-substring-no-properties (point-min) (point-max))
			"\n" t)))

(ert-deftest org-change-test-overview-lists-every-change ()
  "The overview shows one line per change, in buffer order."
  (org-change-tests--with-overview
      "a {!new one!}{!old one!} b\n{!added!}{!!}\n{!!}{!dropped!}\n"
    (should (equal (org-change-tests--overview-lines)
		   (list "new one" "added" "✗dropped")))))

(ert-deftest org-change-test-overview-shows-the-first-line-only ()
  "A change spanning several lines is summarized by its first line."
  (org-change-tests--with-overview "{!first\nsecond\nthird!}{!!}"
    (should (equal (org-change-tests--overview-lines) '("first")))))

(ert-deftest org-change-test-overview-gives-the-line-of-each-change ()
  "Each entry is prefixed with the line the change is on."
  (org-change-tests--with-overview "one\n\n{!two!}{!!}\n{!three!}{!!}"
    (should (equal (split-string (buffer-substring-no-properties
				  (point-min) (point-max))
				 "\n" t)
		   '("   3  two" "   4  three")))))

(ert-deftest org-change-test-overview-says-when-there-is-nothing ()
  "A buffer without changes gets an overview that says so."
  (org-change-tests--with-overview "nothing to see here"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "No changes"))))

(ert-deftest org-change-test-overview-jumps-to-the-change ()
  "RET moves point in the source buffer to the change on this line."
  (org-change-tests--with-overview "a {!x!}{!y!} b {!p!}{!q!} c"
    (forward-line 1)			; the second change
    (org-change-overview-goto)
    (should (eq (current-buffer) source))
    (should (equal (org-change--at-change) (cons 16 26)))))

(ert-deftest org-change-test-overview-accepts-in-the-source-buffer ()
  "The accept key acts on the change in the buffer being reviewed."
  (org-change-tests--with-overview "a {!x!}{!y!} b {!p!}{!q!} c"
    (org-change-overview-accept)
    (should (equal (with-current-buffer source
		     (buffer-substring-no-properties (point-min) (point-max)))
		   "a x b {!p!}{!q!} c"))))

(ert-deftest org-change-test-overview-rejects-in-the-source-buffer ()
  "The reject key acts on the change in the buffer being reviewed."
  (org-change-tests--with-overview "a {!x!}{!y!} b {!p!}{!q!} c"
    (org-change-overview-reject)
    (should (equal (with-current-buffer source
		     (buffer-substring-no-properties (point-min) (point-max)))
		   "a y b {!p!}{!q!} c"))))

(ert-deftest org-change-test-overview-refreshes-after-accepting ()
  "The dealt-with change leaves the list, and the cursor stays put.
The next change moves up into its place, so accepting repeatedly
works down the list without having to move."
  (org-change-tests--with-overview "a {!x!}{!y!} b {!p!}{!q!} c"
    (org-change-overview-accept)
    (should (equal (org-change-tests--overview-lines) '("p")))
    (should (= (line-number-at-pos) 1))
    (org-change-overview-accept)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "No changes"))
    (should (equal (with-current-buffer source
		     (buffer-substring-no-properties (point-min) (point-max)))
		   "a x b p c"))))

(ert-deftest org-change-test-overview-keeps-the-source-point-put ()
  "Reviewing from the overview does not move point in the source buffer."
  (org-change-tests--with-overview "a {!x!}{!y!} b {!p!}{!q!} c"
    (with-current-buffer source (goto-char (point-max)))
    (org-change-overview-accept)
    (should (= (with-current-buffer source (point))
	       (with-current-buffer source (point-max))))))

(ert-deftest org-change-test-overview-opens-a-side-window ()
  "The overview is shown in a side window, which `q' closes."
  (org-change-tests--with-overview "a {!x!}{!y!} b"
    (let ((window (get-buffer-window (current-buffer))))
      (should window)
      (should (window-parameter window 'window-side))
      (should (eq (selected-window) window))
      (quit-window)
      (should-not (get-buffer-window org-change-overview-buffer-name)))))

(ert-deftest org-change-test-overview-survives-a-dead-source ()
  "With the buffer it describes gone, the overview says so rather than fails."
  (org-change-tests--with-overview "a {!x!}{!y!} b"
    (kill-buffer source)
    (should-error (org-change-overview-accept) :type 'user-error)))

(ert-deftest org-change-test-overview-follows-the-source-buffer ()
  "Accepting in the text refreshes the list, which no longer shows it."
  (org-change-tests--with-overview "a {!x!}{!y!} b {!p!}{!q!} c"
    (with-current-buffer source
      (goto-char 4)
      (org-change-accept))
    (should (equal (org-change-tests--overview-lines) '("p")))))

(ert-deftest org-change-test-overview-follows-a-region-accept ()
  "Accepting a whole region in the text refreshes the list too."
  (org-change-tests--with-overview "a {!x!}{!y!} b {!p!}{!q!} c"
    (with-current-buffer source
      (org-change-tests--mark-region (point-min) (point-max))
      (org-change-accept))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   "No changes"))))

(ert-deftest org-change-test-overview-refresh-keeps-the-cursor-line ()
  "A refresh from the other buffer leaves the cursor where it was."
  (org-change-tests--with-overview
      "a {!x!}{!y!} b {!p!}{!q!} c {!m!}{!n!} d"
    (forward-line 2)			; the third change
    (with-current-buffer source
      (goto-char 4)			; accept the first
      (org-change-accept))
    (should (equal (org-change-tests--overview-lines) '("p" "m")))
    ;; Line 3 is gone with the change that was on line 1, so the cursor
    ;; sits on the last line rather than past the end of the list.
    (should (= (line-number-at-pos) 2))))

(ert-deftest org-change-test-overview-accepts-without-the-prefix ()
  "The accept and reject keys work bare in the overview.
There is nothing to type there, so the prefix is not needed."
  (org-change-tests--with-overview "a {!x!}{!y!} b"
    (should (eq (key-binding (org-change--bare-key org-change-accept-key))
		#'org-change-overview-accept))
    (should (eq (key-binding (org-change--bare-key org-change-reject-key))
		#'org-change-overview-reject))
    ;; and the full sequences still work
    (should (eq (key-binding org-change-accept-key)
		#'org-change-overview-accept))))

(ert-deftest org-change-test-bare-key-takes-the-last-event ()
  "The bare binding follows a customized key rather than a fixed letter."
  (should (equal (key-description (org-change--bare-key (kbd "C-` k"))) "k"))
  (should (equal (key-description (org-change--bare-key (kbd "C-c C-v"))) "C-v"))
  (should-not (org-change--bare-key "")))

;;; Plain text export

(defun org-change-tests--ascii (text)
  "Return TEXT with its change markup rewritten for plain text export."
  (with-temp-buffer
    (insert text)
    (org-change--before-processing 'ascii)
    (buffer-string)))

(ert-deftest org-change-test-ascii-exports-an-addition ()
  "An addition becomes CriticMarkup's insertion."
  (should (equal (org-change-tests--ascii "a {!new!}{!!} b")
		 "a @@ascii:{++new++}@@ b")))

(ert-deftest org-change-test-ascii-exports-a-deletion ()
  "A deletion becomes CriticMarkup's deletion."
  (should (equal (org-change-tests--ascii "a {!!}{!gone!} b")
		 "a @@ascii:{--gone--}@@ b")))

(ert-deftest org-change-test-ascii-exports-a-replacement ()
  "A replacement becomes CriticMarkup's substitution, old text first."
  (should (equal (org-change-tests--ascii "a {!new!}{!old!} b")
		 "a @@ascii:{~~old~>new~~}@@ b")))

(ert-deftest org-change-test-ascii-exports-a-comment ()
  "A comment follows the change as CriticMarkup's comment."
  (should (equal (org-change-tests--ascii "a {!new!}{!old!}{!a note!} b")
		 "a @@ascii:{~~old~>new~~}{>>a note<<}@@ b")))

(ert-deftest org-change-test-ascii-exports-the-author-of-a-comment ()
  "An @ID prefix names the author, as it does on screen."
  (should (equal (org-change-tests--ascii "a {!new!}{!old!}{!@SG a note!} b")
		 "a @@ascii:{~~old~>new~~}{>>SG: a note<<}@@ b")))

(ert-deftest org-change-test-ascii-exports-an-author-without-a-comment ()
  "An author alone still shows, with nothing after the colon to say."
  (should (equal (org-change-tests--ascii "a {!new!}{!old!}{!@SG!} b")
		 "a @@ascii:{~~old~>new~~}@@ b")))

(ert-deftest org-change-test-ascii-decodes-escaped-delimiters ()
  "The escaping of the org-change delimiters is undone on export."
  (should (equal (org-change-tests--ascii "{!!}{!a!\\}b!}")
		 "@@ascii:{--a!}b--}@@")))

(ert-deftest org-change-test-ascii-export-runs-end-to-end ()
  "A document exports to plain text with its changes marked up."
  (require 'ox-ascii)
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert "Some {!new!}{!old!} text and an {!added!}{!!} word.\n")
    (should (equal (org-export-as 'ascii nil nil t)
		   (concat "Some {~~old~>new~~} text and an {++added++} word.\n")))))

(ert-deftest org-change-test-ascii-final-export-drops-the-markup ()
  "With `org-change-final' set, plain text export gives a clean document."
  (require 'ox-ascii)
  (with-temp-buffer
    (org-mode)
    (org-change-mode 1)
    (insert "Some {!new!}{!old!} text and a {!!}{!dropped!} word.\n")
    (let ((org-change-final t))
      ;; Org fills the text on export, so the gap the deletion leaves
      ;; closes by itself.
      (should (equal (org-export-as 'ascii nil nil t)
		     "Some new text and a word.\n")))))

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
    (org-change-tests--filter-to "SG")
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
    (org-change-tests--filter-to "SG")
    (org-change-tests--filter-to "All")
    (should (equal (split-string (buffer-substring-no-properties
				  (point-min) (point-max)) "\n" t)
		   '("   1  SG  a" "   2  MR  b")))
    (should (equal header-line-format "Author: all"))))

(ert-deftest org-change-test-overview-filter-accepts-within-author ()
  "Accepting on a filtered list acts on that change in the source."
  (org-change-tests--with-overview
      "x {!a!}{!!}{!@SG!} y {!b!}{!!}{!@MR!} z"
    (org-change-tests--filter-to "SG")
    (org-change-overview-accept)
    (should (equal (with-current-buffer source
		     (buffer-substring-no-properties (point-min) (point-max)))
		   "x a y {!b!}{!!}{!@MR!} z"))))

(ert-deftest org-change-test-overview-filter-survives-a-refresh ()
  "A refresh from the text buffer keeps the active filter."
  (org-change-tests--with-overview
      "x {!a!}{!!}{!@SG!} y {!b!}{!!}{!@MR!} z {!c!}{!!}{!@SG!} w"
    (org-change-tests--filter-to "SG")
    (with-current-buffer source
      (goto-char 4)			; the first SG change
      (org-change-accept))
    (should (equal (mapcar (lambda (l) (string-trim (substring l 6)))
			   (split-string (buffer-substring-no-properties
					  (point-min) (point-max)) "\n" t))
		   '("SG  c")))
    (should (equal header-line-format "Author: SG"))))

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
		  #'org-change-accept-by-author "SG" t
		  "a {!x!}{!!}{!@SG!} b {!y!}{!!}{!@MR!} c {!z!}{!!}{!@SG!} d")
		 "a x b {!y!}{!!}{!@MR!} c z d")))

(ert-deftest org-change-test-reject-by-author-takes-only-that-author ()
  "Rejecting by author restores that author's old text only."
  (should (equal (org-change-tests--by-author
		  #'org-change-reject-by-author "MR" t
		  "a {!x!}{!o!}{!@SG!} b {!y!}{!p!}{!@MR!} c")
		 "a {!x!}{!o!}{!@SG!} b p c")))

(ert-deftest org-change-test-by-author-declined-does-nothing ()
  "Answering no to the confirmation leaves the buffer untouched."
  (should (equal (org-change-tests--by-author
		  #'org-change-accept-by-author "SG" nil
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
    (org-change-tests--mark-region 1 21)	; only the first SG change
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "SG"))
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
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "SG"))
		((symbol-function 'y-or-n-p)
		 (lambda (prompt &rest _) (setq asked prompt) nil)))
	(org-change-accept-by-author))
      (should (equal asked "Accept 2 changes by SG? ")))))

(provide 'org-change-tests)

;;; org-change-tests.el ends here
