;;; org-change.el --- Annotate changes in text files -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Stefano Ghirlanda

;; Version: 0.5.0
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/drghirlanda/org-change
;; Keywords: wp, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; org-change is a minor mode to annotate changes in text files using
;; a custom markup syntax: {!new text!}{!old text!}.  It works in any
;; major mode.  Mark additions with org-change-add (C-` a), deletions
;; with org-change-delete (C-` d), and replacements with
;; org-change-replace (C-` r).  Accept or reject changes with
;; org-change-accept (C-` k) and org-change-reject (C-` x).  When
;; used in org-mode, LaTeX and HTML export are available.  To change
;; key bindings and other settings, run M-x customize-group RET
;; org-change.  More information at the package URL.

;;; Code:

;; Customization group and options (defined early so functions can use them)

(defgroup org-change nil
  "Customization options for Org Change."
  :group 'wp)

(defcustom org-change-show-deleted nil
  "If non-nil, show deleted/replaced text alongside new text.

The deleted/replaced text is shown in the face
 `org-change-deleted-face', which defaults to gray and can also
 be customized."
  :type 'boolean
  :group 'org-change)

(defcustom org-change-deleted-marker "✗"
  "Placeholder for deleted text."
  :type 'string
  :group 'org-change)

(defcustom org-change-add-key (kbd "C-` a")
  "Keybinding for `org-change-add'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-delete-key (kbd "C-` d")
  "Keybinding for `org-change-delete'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-kill-key (kbd "C-` w")
  "Keybinding for `org-change-kill'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-yank-key (kbd "C-` y")
  "Keybinding for `org-change-yank'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-replace-key (kbd "C-` r")
  "Keybinding for `org-change-replace'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-accept-key (kbd "C-` k")
  "Keybinding for `org-change-accept'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-reject-key (kbd "C-` x")
  "Keybinding for `org-change-reject'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-accept-reject-all-key (kbd "C-` b")
  "Keybinding for `org-change-accept-reject-all'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-fontify-key (kbd "C-` f")
  "Keybinding for `org-change-fontify'."
  :type 'key-sequence
  :group 'org-change)

(defface org-change-link-face
  '((t (:background "lavender blush" :underline nil)))
  "Face for Org Change links."
  :group 'org-change)

(defface org-change-deleted-face
  '((t (:foreground "gray")))
  "Face for Org Change deleted/replaced text."
  :group 'org-change)

(defcustom org-change-face 'org-change-link-face
  "Face for Org Change links."
  :type 'face
  :group 'org-change)

;; Internal variables

(defvar org-change-mode)  ; defined by define-minor-mode below

(defvar org-change--extra-space-pos nil
  "Marker at the space inserted as a typing placeholder, or nil.")

;; Regexp to match change markup: {!new!}{!old!} with optional {!comment!}
;; Group 1 = new text, Group 2 = old text, Group 3 = comment (optional)
(defvar org-change--regexp
  "{!\\(\\(?:.\\|\n\\)*?\\)!}{!\\(\\(?:.\\|\n\\)*?\\)!}\\(?:{!\\(\\(?:.\\|\n\\)*?\\)!}\\)?"
  "Regexp to match change markup.")

(defvar org-change--comment-regexp
  "{!\\(\\(?:.\\|\n\\)*?\\)!}"
  "Regexp to match the comment part of change markup.")

(defconst org-change--empty-markup "{!!}{!!}"
  "Markup of a change with neither new nor old text.")

(defun org-change--get-region ()
  "Return content of active region or nil."
  (when (use-region-p)
    (buffer-substring-no-properties
     (region-beginning)
     (region-end))))

;;; Overlay-based display

(defun org-change--remove-overlays (&optional rbeg rend)
  "Remove all org-change overlays in region RBEG to REND."
  (remove-overlays (or rbeg (point-min)) (or rend (point-max))
		   'org-change-overlay t))

(defun org-change--make-overlay (beg end &rest properties)
  "Create an org-change overlay from BEG to END with PROPERTIES."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'org-change-overlay t)
    (overlay-put ov 'evaporate t)
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    ov))

(defun org-change-fontify (&optional rbeg rend)
  "Fontify change markup using overlays.
Called automatically when Org Change mode starts.  Optional
arguments RBEG and REND delimit the region to fontify.  If nil,
RBEG is set to buffer beginning and REND to buffer end.

Fontifying the whole buffer can take a while, so it reports
progress in the echo area.  Fontifying a region the caller names
is quick, and stays silent: it happens on every keystroke, and the
echo area is needed for other things."
  (interactive)
  (let ((quiet (or rbeg rend)))
    (setq rbeg (or rbeg (point-min))
	  rend (or rend (point-max)))
    (org-change--remove-overlays rbeg rend)
    (save-excursion
      (goto-char rbeg)
      (while (re-search-forward org-change--regexp rend t)
	(let* ((full-beg (match-beginning 0))
	       (full-end (match-end 0))
	       (new-text (match-string 1))
	       (old-text (match-string 2))
	       (new-beg (match-beginning 1))
	       (new-end (match-end 1))
	       (open-beg full-beg)        ; {!
	       (open-end (+ full-beg 2))  ; after {!
	       (mid-beg new-end))         ; start of !}{!old!}...
	  (unless quiet
	    (message "Fontifying changes (%d%%)"
		     (* 100 (/ (float full-end) (point-max)))))
	  (cond
	   ;; Empty change: neither new nor old text.  Show the same
	   ;; placeholder `org-change-add' starts out with, so that typing
	   ;; resumes the addition.  `org-change--cleanup-empty' removes
	   ;; the markup once point leaves it.
	   ((and (equal new-text "") (equal old-text ""))
	    (org-change--make-overlay full-beg full-end
				      'display " "
				      'face 'org-change-link-face))
	   ;; Deletion: new text is empty
	   ((equal new-text "")
	    ;; Hide everything, show deleted marker
	    (org-change--make-overlay full-beg full-end
				      'display org-change-deleted-marker
				      'face 'org-change-link-face)
	    (when org-change-show-deleted
	      ;; Also show old text after the marker
	      (let ((ov (make-overlay full-end full-end nil t nil)))
		(overlay-put ov 'org-change-overlay t)
		(overlay-put ov 'evaporate t)
		(overlay-put ov 'after-string
			     (propertize old-text
					 'face 'org-change-deleted-face)))))
	   ;; Addition or replacement: show new text
	   (t
	    ;; Hide {! before new text
	    (org-change--make-overlay open-beg open-end 'invisible t)
	    ;; Face on new text
	    (org-change--make-overlay new-beg new-end
				      'face 'org-change-link-face)
	    ;; Hide !}{!old!} and optional {!comment!}
	    (org-change--make-overlay mid-beg full-end 'invisible t)
	    (when (and org-change-show-deleted (not (equal old-text "")))
	      ;; Show old text after the change
	      (let ((ov (make-overlay full-end full-end nil t nil)))
		(overlay-put ov 'org-change-overlay t)
		(overlay-put ov 'evaporate t)
		(overlay-put ov 'after-string
			     (propertize old-text
					 'face 'org-change-deleted-face))))))
	  (goto-char full-end))))
    (unless quiet
      (message "Fontifying changes (100%%)"))))

(defun org-change--after-change (beg end _len)
  "Re-fontify around changes after buffer modification.
BEG and END are the modified region boundaries."
  (when org-change-mode
    (save-excursion
      (let ((rbeg (progn (goto-char beg) (line-beginning-position)))
	    (rend (progn (goto-char end) (line-end-position))))
	(org-change-fontify rbeg rend)))))

;;; Change creation functions

(defun org-change--mark-change (old-text new-text)
  "Delete region and insert change markup with OLD-TEXT and NEW-TEXT."
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (let ((beg (point)))
    (insert (format "{!%s!}{!%s!}" new-text old-text))
    (org-change-fontify beg (point))))

(defun org-change-replace ()
  "Mark active region as replaced text.
The region becomes old text and point is placed where you can
type the new text."
  (interactive "")
  (let ((old-text (org-change--get-region)))
    (if (not old-text)
	(user-error "Select text to be replaced")
      (when (use-region-p)
	(delete-region (region-beginning) (region-end)))
      (let ((beg (point)))
	(insert (format "{! !}{!%s!}" old-text))
	(org-change-fontify beg (point))
	;; place point inside the new text, on the space
	(goto-char (+ beg 2))
	(org-change--mark-extra-space)))))

(defun org-change-delete ()
  "Mark active region as deleted text."
  (interactive "")
  (let ((old-text (org-change--get-region)))
    (if (equal old-text nil)
	(user-error "Select text to be deleted")
      (org-change--mark-change old-text ""))))

(defun org-change-kill ()
  "Like `org-change-delete', but kill (cut) rather than delete text.
Used together with `org-change-yank' to move text around."
  (interactive)
  (when (use-region-p)
    (kill-ring-save (region-beginning) (region-end)))
  (org-change-delete))

(defun org-change-yank ()
  "Yank (paste) text and mark it as an addition.
Used together with `org-change-kill' to move text around."
  (interactive)
  (let ((beg (point)))
    (insert "{!")
    (yank)
    (insert "!}{!!}")
    (org-change-fontify beg (point))))

(defun org-change-add ()
  "Mark the active region as new text.
If there is no active region, insert an empty addition for typing."
  (interactive "")
  (let ((new-text (or (org-change--get-region) " ")))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (let ((beg (point)))
      (insert (format "{!%s!}{!!}" new-text))
      (org-change-fontify beg (point))
      (when (equal new-text " ")
	;; place point on the space for typing
	(goto-char (+ beg 2))
	(org-change--mark-extra-space)))))

(defun org-change--mark-extra-space ()
  "Record the space at point as a typing placeholder."
  (setq org-change--extra-space-pos (copy-marker (point))))

(defun org-change--erase-extra-space ()
  "Remove the space added by `org-change-add' or `org-change-replace'.
The space is removed only if the character just typed landed on
it.  Typing anywhere else means the placeholder was abandoned, and
the space must be left where it is: deleting a character at point
would eat text the user meant to keep."
  (when (and org-change-mode
	     org-change--extra-space-pos
	     (eq (marker-buffer org-change--extra-space-pos) (current-buffer)))
    (when (and (= (point) (1+ (marker-position org-change--extra-space-pos)))
	       (eq (char-after) ?\s))
      (delete-char 1))
    (org-change--forget-extra-space)))

(defun org-change--forget-extra-space ()
  "Forget the typing placeholder, if any."
  (when org-change--extra-space-pos
    (set-marker org-change--extra-space-pos nil)
    (setq org-change--extra-space-pos nil)))

;;; Cleanup of empty changes

(defun org-change--empty-markup-end (pos)
  "Return the end of the empty change starting at POS, or nil to keep it.
POS is the start of `org-change--empty-markup'.  The end covers a
trailing empty comment, if any.  A change carrying a comment with
text is kept: deleting it would destroy what the user wrote."
  (save-excursion
    (goto-char (+ pos (length org-change--empty-markup)))
    (if (looking-at org-change--comment-regexp)
	(and (equal (match-string 1) "") (match-end 0))
      (point))))

(defun org-change--cleanup-empty ()
  "Delete the empty changes that point is not inside.
An empty change, `{!!}{!!}', says nothing: it is what
`org-change-add' leaves behind when its new text is erased again.
It is kept while point is inside it, so typing resumes the
addition, and deleted as soon as point leaves."
  (when org-change-mode
    (let ((pos (point-marker)))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward org-change--empty-markup nil t)
	  (let* ((beg (match-beginning 0))
		 (end (org-change--empty-markup-end beg)))
	    (when (and end
		       (not (and (> (marker-position pos) beg)
				 (< (marker-position pos) end))))
	      (org-change--remove-overlays beg end)
	      (delete-region beg end)))))
      (set-marker pos nil))))

;;; Accept/reject functions

(defun org-change--at-change ()
  "If point is inside a change, return (BEG . END) of the match.
Also sets match data for `org-change--regexp'."
  (save-excursion
    (let ((pos (point))
	  (limit (max (point-min) (- (point) 1000))))
      ;; Search backward then forward to find a change containing point
      (goto-char limit)
      (catch 'found
	(while (re-search-forward org-change--regexp nil t)
	  (when (and (<= (match-beginning 0) pos)
		     (>= (match-end 0) pos))
	    (throw 'found (cons (match-beginning 0) (match-end 0))))
	  (when (> (match-beginning 0) pos)
	    (throw 'found nil)))
	nil))))

(defun org-change--accept-or-reject (accept)
  "Accept (ACCEPT is t) or reject (ACCEPT is nil) change at point.
If there is no change at point, accept or reject all changes in
the active region."
  (let ((change-position (org-change--at-change))
	(inhibit-read-only t))
    (if change-position
	(let ((new-text (match-string-no-properties 1))
	      (old-text (match-string-no-properties 2))
	      (beg (car change-position))
	      (end (cdr change-position)))
	  (org-change--remove-overlays beg end)
	  (delete-region beg end)
	  (if accept
	      (unless (equal new-text "")
		(insert new-text))
	    (insert old-text)))
      (when (use-region-p)
	(save-excursion
	  (save-restriction
	    (goto-char (region-beginning))
	    (while (re-search-forward org-change--regexp nil t)
	      (let ((beg (match-beginning 0)))
		(goto-char beg)
		(org-change--accept-or-reject accept)
		(goto-char beg)))))))))

(defun org-change-accept ()
  "Accept change at point."
  (interactive "")
  (org-change--accept-or-reject t))

(defun org-change-reject ()
  "Reject change at point."
  (interactive "")
  (org-change--accept-or-reject nil))

(defun org-change-accept-reject-all ()
  "Go through all changes, prompting to accept or reject each one.
With an active region, only process changes in the region,
otherwise process the whole buffer."
  (interactive)
  (let* ((beg 1)
	 (end (buffer-end 1)))
    (save-mark-and-excursion
      (when (use-region-p)
	(setq beg (use-region-beginning)
	      end (use-region-end))
	(set-mark end))
      (goto-char beg)
      (while (re-search-forward org-change--regexp end t)
	(let ((answer (read-char "Accept change? [y/n] or SPC to skip, C-g to quit")))
	  (cond
	   ((char-equal answer ?y)
	    (org-change--accept-or-reject t))
	   ((char-equal answer ?n)
	    (org-change--accept-or-reject nil))
	   ((char-equal answer ?\s)) ; skip
	   (t
	    (goto-char (match-beginning 0))))))
      (deactivate-mark)))
  (message "No more changes"))

(defun org-change-toggle-deleted-text ()
  "Show/hide deleted text."
  (interactive)
  (setq org-change-show-deleted (not org-change-show-deleted))
  (org-change-fontify))

;;; Converting from old link syntax

(defun org-change-convert-from-links ()
  "Convert old change link syntax to new markup syntax in the buffer.
The old syntax is [[change:old text][new text]], used in versions
before 0.5.  This function converts all occurrences in the buffer
to the new {!new text!}{!old text!} syntax.

This requires `org-mode' to be available for `org-link-unescape'."
  (interactive)
  (require 'ol)
  (let ((old-regexp "\\[\\[change:\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]")
	(count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward old-regexp nil t)
	(let* ((old-text (org-link-unescape (match-string 1)))
	       (raw-new (org-link-unescape (match-string 2)))
	       (mbeg (match-beginning 0))
	       (mend (match-end 0))
	       ;; Extract comment from **comment** at end of new text
	       (has-comment (string-match "\\(.*\\)\\*\\*\\(.+\\)\\*\\*$" raw-new))
	       (new-text (if has-comment (match-string 1 raw-new) raw-new))
	       (comment (if has-comment (match-string 2 raw-new) nil))
	       ;; Deletion: new text is the deleted marker
	       (new-text (if (equal new-text org-change-deleted-marker) "" new-text))
	       (replacement
		(concat (format "{!%s!}{!%s!}" new-text old-text)
			(if comment (format "{!%s!}" comment) ""))))
	  (delete-region mbeg mend)
	  (goto-char mbeg)
	  (insert replacement)
	  (setq count (1+ count)))))
    (when (> count 0)
      (org-change-fontify))
    (message "Converted %d change link%s" count (if (= count 1) "" "s"))))

;;; Export mechanism (requires org-mode)

(declare-function org-link-unescape "ol")
(declare-function org-export-derived-backend-p "ox")
(defvar org-export-before-processing-functions)
(defvar org-export-filter-final-output-functions)

(defun org-change--export-latex (old-text new-text comment)
  "Export a change to LaTeX.
OLD-TEXT, NEW-TEXT, and COMMENT are the elements of the change.
The result is wrapped in @@latex:...@@ so org exports it verbatim."
  (let ((comment (if (equal comment "") "" (format "[comment=%s]" comment))))
    (format "@@latex:%s@@"
	    (cond ((equal old-text "")
		   (format "\\added%s{%s}" comment new-text))
		  ((equal new-text "")
		   (format "\\deleted%s{%s}" comment old-text))
		  (t
		   (format "\\replaced%s{%s}{%s}" comment new-text old-text))))))

(defun org-change--make-span (class text)
  "Return string <span class=\"CLASS\">TEXT</span> for HTML export."
    (if (equal text "")
	""
      (format "<span class=\"%s\">%s</span>" class text)))

(defun org-change--export-html (old-text new-text comment)
  "Export a change to HTML.
OLD-TEXT, NEW-TEXT, and COMMENT are the elements of the change.
The result is wrapped in @@html:...@@ so org exports it verbatim."
  (format "@@html:%s@@"
	  (cond ((equal old-text "")
		 (org-change--make-span
		  "org-change-added"
		  (concat new-text (org-change--make-span
				    "org-change-comment"
				    comment))))
		((equal new-text "")
		 (org-change--make-span
		  "org-change-deleted"
		  (concat old-text (org-change--make-span
				    "org-change-comment" comment))))
		(t
		 (concat
		  (org-change--make-span
		   "org-change-added"
		   (concat new-text (org-change--make-span
				     "org-change-comment" comment)))
		  (org-change--make-span "org-change-deleted" old-text))))))

(defvar org-change--exporters
  '((latex . org-change--export-latex)
    (html . org-change--export-html))
  "List of exporters known to Org Change.")

(defun org-change-add-export-backend (backend exporter)
  "Add export backend to Org Change.
The EXPORTER function must take arguments old-text, new-text, and
comment, and return a string appropriate to BACKEND."
  (add-to-list 'org-change--exporters (cons backend exporter)))

(defvar org-change-final
  nil
  "If nil, include changes when exporting, otherwise include only new text.")

(defun org-change--before-processing (backend)
  "Replace change markup in buffer before org parses it for BACKEND.
This runs on a temporary copy of the buffer via
`org-export-before-processing-functions', so modifications are
safe and do not affect the original buffer."
  (goto-char (point-min))
  (while (re-search-forward org-change--regexp nil t)
    (let* ((new-text (match-string 1))
	   (old-text (match-string 2))
	   (comment (or (match-string 3) ""))
	   (replacement
	    (cond
	     ;; An empty change has nothing to export
	     ((and (equal new-text "") (equal old-text "") (equal comment ""))
	      "")
	     (org-change-final
	      (if (equal new-text "") "" new-text))
	     (t
	      (let ((exporter (alist-get
			       backend
			       org-change--exporters
			       nil nil
			       #'org-export-derived-backend-p)))
		(if exporter
		    (funcall exporter old-text new-text comment)
		  (user-error "Change markup not supported in %s export"
			      backend)))))))
      (replace-match replacement t t))))

(defun org-change-filter-final-output (text backend _)
  "Add the changes.sty package to the LaTeX preamble.
TEXT is the whole document and BACKEND is checked for being
\\='latex or derived from \\='latex."
  (if (and (org-export-derived-backend-p backend 'latex)
	   (not org-change-final))
      (replace-regexp-in-string
       "\\\\begin{document}"
       (concat
	"\\\\usepackage"
	(when (boundp 'org-change-latex-options) org-change-latex-options)
	"{changes}\n\\\\begin{document}")
       text)
    text))

(defun org-change--register-export-hooks ()
  "Register org export hooks for change markup processing."
  (add-to-list 'org-export-before-processing-functions
	       #'org-change--before-processing)
  (add-to-list 'org-export-filter-final-output-functions
	       #'org-change-filter-final-output))

(defun org-change--setup-export ()
  "Set up export hooks, deferring if ox is not yet loaded."
  (if (featurep 'ox)
      (org-change--register-export-hooks)
    (with-eval-after-load 'ox
      (org-change--register-export-hooks))))

;;; Minor mode definition

(define-minor-mode org-change-mode
  "Minor mode for annotating changes in text files."
  :lighter " Chg"
  :group 'org-change
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map org-change-add-key #'org-change-add)
            (define-key map org-change-delete-key #'org-change-delete)
            (define-key map org-change-kill-key #'org-change-kill)
            (define-key map org-change-yank-key #'org-change-yank)
            (define-key map org-change-replace-key #'org-change-replace)
            (define-key map org-change-accept-key #'org-change-accept)
            (define-key map org-change-reject-key #'org-change-reject)
            (define-key map org-change-accept-reject-all-key #'org-change-accept-reject-all)
            (define-key map org-change-fontify-key #'org-change-fontify)
            map)
  (if org-change-mode
      (progn
	(add-hook 'post-self-insert-hook #'org-change--erase-extra-space 0 t)
	(add-hook 'after-change-functions #'org-change--after-change nil t)
	(add-hook 'post-command-hook #'org-change--cleanup-empty nil t)
	(org-change--setup-export)
	(setq-local org-change--extra-space-pos nil)
	(org-change-fontify))
    (remove-hook 'post-self-insert-hook #'org-change--erase-extra-space t)
    (remove-hook 'after-change-functions #'org-change--after-change t)
    (remove-hook 'post-command-hook #'org-change--cleanup-empty t)
    (org-change--forget-extra-space)
    (org-change--remove-overlays)))

(provide 'org-change)

;;; org-change.el ends here
