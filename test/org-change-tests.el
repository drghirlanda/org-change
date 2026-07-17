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

(defun org-change-tests--mark-region (beg end)
  "Make BEG to END an active region that `use-region-p' recognizes."
  (setq-local transient-mark-mode t)
  (goto-char beg)
  (set-mark beg)
  (goto-char end)
  (activate-mark))

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

(provide 'org-change-tests)

;;; org-change-tests.el ends here
