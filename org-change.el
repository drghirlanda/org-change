;;; org-change.el --- Annotate changes in text files -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Stefano Ghirlanda

;; Version: 0.11.1
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
;; org-change-accept (C-` k) and org-change-reject (C-` x), or with
;; C-` K and C-` X to move to the next change as well.  Comment
;; on a change with org-change-comment (C-` c).  Move between changes
;; with org-change-next-change (C-` n) and
;; org-change-previous-change (C-` p).  Count them with
;; org-change-info (C-` i), or list them in a side window with
;; org-change-overview (C-` o).  Press C-` h for a summary of the key
;; bindings.  Generate change markup from
;; two versions of a document with org-change-from-diff.  When
;; used in org-mode, LaTeX, HTML, and plain text export are
;; available.  To change
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

(defcustom org-change-author nil
  "Identifier of the current author, or nil.
When non-nil, changes you create are attributed to this author: it
is stored as an @-prefixed token in the change's comment.  Look up
the author's name and color in `org-change-authors'."
  :type '(choice (const :tag "No author" nil) string)
  :group 'org-change)

(defcustom org-change-authors nil
  "Registry of change authors.
Each entry is (ID :name NAME :color COLOR), where ID is the short
identifier stored in changes (see `org-change-author'), NAME is a
readable name, and COLOR names a color known to both Emacs and, for
LaTeX export, the xcolor package (for example \"blue\" or \"red\").
The color tints the author's changes in the buffer and is used on
export."
  :type '(alist :key-type string
		:value-type (plist :options ((:name string) (:color string))))
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

(defcustom org-change-accept-and-next-key (kbd "C-` K")
  "Keybinding for `org-change-accept-and-next'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-reject-and-next-key (kbd "C-` X")
  "Keybinding for `org-change-reject-and-next'."
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

(defcustom org-change-comment-key (kbd "C-` c")
  "Keybinding for `org-change-comment'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-next-key (kbd "C-` n")
  "Keybinding for `org-change-next-change'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-previous-key (kbd "C-` p")
  "Keybinding for `org-change-previous-change'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-overview-key (kbd "C-` o")
  "Keybinding for `org-change-overview'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-info-key (kbd "C-` i")
  "Keybinding for `org-change-info'."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-help-key (kbd "C-` h")
  "Keybinding for `org-change-help'."
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

(defface org-change-comment-face
  '((t (:slant italic)))
  "Face for the comment shown after a change."
  :group 'org-change)

(defcustom org-change-overview-width 40
  "Width, in columns, of the side window `org-change-overview' opens."
  :type 'integer
  :group 'org-change)

(defcustom org-change-overview-side 'right
  "Side of the frame `org-change-overview' opens its window on."
  :type '(choice (const left) (const right) (const top) (const bottom))
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

(defvar org-change--pair-regexp
  "{!\\(\\(?:.\\|\n\\)*?\\)!}{!\\(\\(?:.\\|\n\\)*?\\)!}"
  "Regexp to match change markup without its optional comment.")

(defvar org-change--comment-regexp
  "{!\\(\\(?:.\\|\n\\)*?\\)!}"
  "Regexp to match the comment part of change markup.")

(defconst org-change--empty-markup "{!!}{!!}"
  "Markup of a change with neither new nor old text.")

(defun org-change--search-forward (&optional bound noerror)
  "Search forward for the next change, like `re-search-forward'.
Match data is set as for `org-change--regexp'.

Resolves the ambiguity between a comment and a directly following
change.  Change markup is a pair `{!new!}{!old!}', so the third
`{!...!}' group in `{!a!}{!b!}{!c!}' could be either a comment of
the first pair or the start of a second pair.  A lone trailing
group is a comment; a group that is itself followed by another
`{!...!}' group belongs to the next change.  Without this,
`{!a!}{!b!}{!c!}{!d!}' would be misread as one change whose
comment is `c', swallowing the `{!c!}{!d!}' change and leaving a
stray `{!d!}'."
  (when (re-search-forward org-change--regexp bound noerror)
    (when (and (match-beginning 3)
	       (save-excursion
		 (goto-char (match-end 0))
		 (looking-at-p org-change--comment-regexp)))
      ;; The captured comment is really the next change's new text.
      ;; Re-match the pair alone, so the comment stays with its change.
      (goto-char (match-beginning 0))
      (re-search-forward org-change--pair-regexp bound noerror))
    t))

(defun org-change--encode (text)
  "Escape the change delimiters `{!' and `!}' inside TEXT.
Content selected for a change may itself contain the delimiter
sequences.  A backslash is inserted to break the two-character
adjacency the parser scans for, so `{!' becomes `{\\!' and `!}'
becomes `!\\}'.  Only those exact sequences are touched; every
other backslash is left alone, so LaTeX such as \\=\\(a=1\\=\\) inside a
change survives unchanged.  `org-change--decode' reverses this."
  (string-replace "!}" "!\\}"
		  (string-replace "{!" "{\\!" text)))

(defun org-change--decode (text)
  "Undo `org-change--encode' on TEXT, restoring `{!' and `!}'."
  (string-replace "{\\!" "{!"
		  (string-replace "!\\}" "!}" text)))

;;; Authors

(defun org-change--split-comment (comment)
  "Split COMMENT into a (AUTHOR . TEXT) cons.
A leading @ID token names the author of the change; the rest,
trimmed, is the comment text.  With no @ID token AUTHOR is nil and
TEXT is COMMENT unchanged."
  (if (string-match
       "\\`@\\([[:alnum:]_-]+\\)\\(?:[ \t]+\\(\\(?:.\\|\n\\)*\\)\\)?\\'"
       comment)
      (cons (match-string 1 comment) (or (match-string 2 comment) ""))
    (cons nil comment)))

(defun org-change--join-comment (author note)
  "Build a comment string from AUTHOR (or nil) and NOTE.
The inverse of `org-change--split-comment'."
  (cond ((and author (not (equal note ""))) (format "@%s %s" author note))
	(author (format "@%s" author))
	(t note)))

(defun org-change--comment-display (author note)
  "Return the text shown after a change for AUTHOR and NOTE.
Combined as \"AUTHOR: NOTE\", so the author is legible without
relying on color; either part may be absent."
  (cond ((and author (not (equal note ""))) (format "%s: %s" author note))
	(author author)
	(t note)))

(defun org-change--author-markup ()
  "Return comment markup stamping the current author, or an empty string.
Empty when `org-change-author' is nil or blank."
  (if (and org-change-author (not (string-empty-p org-change-author)))
      (format "{!@%s!}" (org-change--encode org-change-author))
    ""))

(defun org-change--change-face (author)
  "Return the face for a change made by AUTHOR (a string or nil).
When AUTHOR has a color in `org-change-authors', tint the text with
it; otherwise use `org-change-face'."
  (let ((color (and author
		    (plist-get (cdr (assoc author org-change-authors)) :color))))
    (if color
	(list :inherit org-change-face :foreground color)
      org-change-face)))

(defun org-change--get-region ()
  "Return content of active region or nil."
  (when (use-region-p)
    (buffer-substring-no-properties
     (region-beginning)
     (region-end))))

;;; Overlay-based display

(defun org-change--remove-overlays (&optional rbeg rend)
  "Remove all org-change overlays in region RBEG to REND."
  (let ((rbeg (or rbeg (point-min)))
	(rend (or rend (point-max))))
    (remove-overlays rbeg rend 'org-change-overlay t)
    ;; `remove-overlays' leaves a zero-length overlay sitting exactly at
    ;; REND (such as a comment or deleted-text after-string); delete it
    ;; too, so re-fontifying does not stack a second copy on top.
    (dolist (ov (overlays-in rend rend))
      (when (and (overlay-get ov 'org-change-overlay)
		 (= (overlay-start ov) (overlay-end ov)))
	(delete-overlay ov)))))

(defun org-change--make-overlay (beg end &rest properties)
  "Create an org-change overlay from BEG to END with PROPERTIES."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'org-change-overlay t)
    (overlay-put ov 'evaporate t)
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    ov))

(defun org-change--after-string-overlay (pos string)
  "Add an org-change overlay at POS showing STRING after it.
Unlike `org-change--make-overlay', this must not set `evaporate':
the overlay is empty (zero length), and an empty evaporating
overlay is deleted at once.  It is cleaned up by
`org-change--remove-overlays' instead."
  (let ((ov (make-overlay pos pos nil t nil)))
    (overlay-put ov 'org-change-overlay t)
    (overlay-put ov 'after-string string)
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
      (while (org-change--search-forward rend t)
	(let* ((full-beg (match-beginning 0))
	       (full-end (match-end 0))
	       (new-text (match-string 1))
	       (old-text (match-string 2))
	       (new-beg (match-beginning 1))
	       (new-end (match-end 1))
	       (open-beg full-beg)        ; {!
	       (open-end (+ full-beg 2))  ; after {!
	       (mid-beg new-end)          ; start of !}{!old!}...
	       (split (org-change--split-comment
		       (org-change--decode (or (match-string 3) ""))))
	       (author (car split))
	       (note (cdr split))
	       (face (org-change--change-face author)))
	  (unless quiet
	    (message "Fontifying changes (%d%%)"
		     (* 100 (/ (float full-end) (point-max)))))
	  ;; Mark the whole change, so `org-change--after-change' can
	  ;; grow its region to cover a change spanning several lines
	  ;; when only one of them is edited.
	  (org-change--make-overlay full-beg full-end
				    'org-change-extent t)
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
				      'face face)
	    (when org-change-show-deleted
	      ;; Also show old text after the marker
	      (org-change--after-string-overlay
	       full-end
	       (propertize old-text 'face 'org-change-deleted-face))))
	   ;; Addition or replacement: show new text
	   (t
	    ;; Hide {! before new text
	    (org-change--make-overlay open-beg open-end 'invisible t)
	    ;; Face on new text
	    (org-change--make-overlay new-beg new-end
				      'face face)
	    ;; Hide !}{!old!} and optional {!comment!}
	    (org-change--make-overlay mid-beg full-end 'invisible t)
	    (when (and org-change-show-deleted (not (equal old-text "")))
	      ;; Show old text after the change
	      (org-change--after-string-overlay
	       full-end
	       (propertize old-text 'face 'org-change-deleted-face)))))
	  ;; Show the author and comment, if any, in italic after the
	  ;; change, as "author: note" -- so the author is legible
	  ;; without having to remember which color is whose.
	  (let ((shown (org-change--comment-display author note)))
	    (unless (equal shown "")
	      (org-change--after-string-overlay
	       full-end
	       (propertize (concat " " shown)
			   'face 'org-change-comment-face))))
	  (goto-char full-end))))
    (unless quiet
      (message "Fontifying changes (100%%)"))))

(defun org-change--after-change (beg end _len)
  "Re-fontify around changes after buffer modification.
BEG and END are the modified region boundaries.  The region is
grown to whole lines, and then to the full extent of any change
overlapping it, so editing one line of a change that spans several
lines re-fontifies the whole change rather than truncating it."
  (when org-change-mode
    (save-excursion
      (let ((rbeg (progn (goto-char beg) (line-beginning-position)))
	    (rend (progn (goto-char end) (line-end-position))))
	(dolist (ov (overlays-in rbeg rend))
	  (when (overlay-get ov 'org-change-extent)
	    (setq rbeg (min rbeg (overlay-start ov))
		  rend (max rend (overlay-end ov)))))
	(org-change-fontify rbeg rend)))))

;;; Change creation functions

(defun org-change--consume-region ()
  "Delete the active region, if any, and deactivate the mark.
The mark has to go: once its text has been turned into a change,
a still-active region would make the next accept or reject act on
the region rather than on the change just made."
  (when (use-region-p)
    (delete-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun org-change--mark-change (old-text new-text)
  "Delete region and insert change markup with OLD-TEXT and NEW-TEXT."
  (org-change--consume-region)
  (let ((beg (point)))
    (insert (format "{!%s!}{!%s!}"
		    (org-change--encode new-text)
		    (org-change--encode old-text))
	    (org-change--author-markup))
    (org-change-fontify beg (point))))

(defun org-change-replace ()
  "Mark active region as replaced text.
The region becomes old text and point is placed where you can
type the new text."
  (interactive "")
  (let ((old-text (org-change--get-region)))
    (if (not old-text)
	(user-error "Select text to be replaced")
      (org-change--consume-region)
      (let ((beg (point)))
	(insert (format "{! !}{!%s!}" (org-change--encode old-text))
		(org-change--author-markup))
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
    (let ((yank-beg (point)))
      (yank)
      ;; Escape any delimiters in the yanked text in place.
      (let ((text (org-change--encode
		   (buffer-substring-no-properties yank-beg (point)))))
	(delete-region yank-beg (point))
	(insert text)))
    (insert "!}{!!}" (org-change--author-markup))
    (org-change-fontify beg (point))))

(defun org-change-add ()
  "Mark the active region as new text.
If there is no active region, insert an empty addition for typing."
  (interactive "")
  (let ((new-text (or (org-change--get-region) " ")))
    (org-change--consume-region)
    (let ((beg (point)))
      (insert (format "{!%s!}{!!}" (org-change--encode new-text))
	      (org-change--author-markup))
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
	(while (org-change--search-forward nil t)
	  (when (and (<= (match-beginning 0) pos)
		     (>= (match-end 0) pos))
	    (throw 'found (cons (match-beginning 0) (match-end 0))))
	  (when (> (match-beginning 0) pos)
	    (throw 'found nil)))
	nil))))

(defun org-change--apply-change (accept)
  "Accept (ACCEPT is t) or reject (ACCEPT is nil) the change at point.
Point does not move: if it was inside the change, it is put back on
the same spot of the text that replaces it.  Where the two sides of
the change come together, a doubled space is reduced to one, as
`org-change--join-whitespace' explains.  Return the position just after
the text that replaces the change, or nil if there was no change at
point."
  (let ((change-position (org-change--at-change))
	(inhibit-read-only t))
    (when change-position
      (let* ((new-text (org-change--decode (match-string-no-properties 1)))
	     (old-text (org-change--decode (match-string-no-properties 2)))
	     (beg (car change-position))
	     (end (cdr change-position))
	     (text (if accept new-text old-text))
	     ;; How far into the change point sits.  The text shown starts
	     ;; two characters in, past the opening `{!', so that is where
	     ;; the offset is measured from.  Nil when point is elsewhere:
	     ;; then it must not be touched at all.
	     (offset (and (<= beg (point)) (<= (point) end)
			  (min (max 0 (- (point) beg 2))
			       (length text))))
	     (stop nil))
	(org-change--remove-overlays beg end)
	(save-excursion
	  (goto-char beg)
	  (delete-region beg end)
	  (unless (equal text "")
	    (insert text))
	  (setq stop (point)))
	(when offset
	  (goto-char (+ beg offset)))
	;; Join the two seams, the later one first so that the earlier
	;; position stays valid.  `stop' has to be a marker across the join
	;; at `beg', which can delete text before it.  When the change left
	;; no text behind, the two seams are one, and joining it twice would
	;; eat a gap that belongs to the text.
	(let ((stop-marker (copy-marker stop t)))
	  (org-change--join-whitespace stop)
	  (unless (= stop beg)
	    (org-change--join-whitespace beg))
	  (setq stop (marker-position stop-marker))
	  (set-marker stop-marker nil))
	stop))))

(defconst org-change--whitespace " \t\n"
  "The characters `org-change--join-whitespace' treats as a gap.")

(defun org-change--join-whitespace (pos)
  "Close the gap that accepting or rejecting opened at POS.
POS is a seam: what is before it and what is after it used to be
separated by change markup, and each side may end or begin with
whitespace of its own.  Once the markup is gone the two runs sit
next to each other, so one of them has to go: deleting a word
without its spaces would otherwise leave a double space, and
deleting a line without its newlines an empty line.  The wider run
is the one kept, so that a paragraph break survives next to a plain
newline, and the narrower one is deleted.

When only one side carries whitespace nothing was joined and the
text is left alone, which is what keeps indentation, and the runs
an author wrote inside a change, intact."
  (save-excursion
    (goto-char pos)
    (let (before-start after-end)
      (skip-chars-backward org-change--whitespace)
      (setq before-start (point))
      (goto-char pos)
      (skip-chars-forward org-change--whitespace)
      (setq after-end (point))
      (when (and (< before-start pos) (< pos after-end))
	(if (org-change--wider-gap-p
	     (buffer-substring-no-properties before-start pos)
	     (buffer-substring-no-properties pos after-end))
	    (delete-region pos after-end)
	  (delete-region before-start pos))))))

(defun org-change--newlines (string)
  "Return the number of newlines in STRING."
  (- (length string) (length (string-replace "\n" "" string))))

(defun org-change--wider-gap-p (a b)
  "Return non-nil if the gap A is at least as wide as the gap B.
Width is the number of newlines first -- a paragraph break is wider
than a line break, which is wider than a space -- and the length of
the run to settle a tie."
  (let ((na (org-change--newlines a))
	(nb (org-change--newlines b)))
    (or (> na nb)
	(and (= na nb) (>= (length a) (length b))))))

(defun org-change--apply-region (accept rbeg rend)
  "Accept or reject the changes between RBEG and REND.
ACCEPT is as in `org-change--apply-change'.  A change is acted on
when it lies entirely within the region, and also when point is
inside it: the cursor says what you mean, so the change under it
is taken whole even if the region only reaches into it.  Every
other change is left alone.  Return the number of changes acted
on."
  ;; Positions have to be markers: accepting or rejecting shortens or
  ;; lengthens the text, and plain positions would drift.
  (let* ((at-point (org-change--at-change))
	 (end (copy-marker rend t))
	 (point-beg (and at-point (copy-marker (car at-point))))
	 ;; The change under point may start before the region or run past
	 ;; it, so widen the search to take it in.
	 (start (if at-point (min rbeg (car at-point)) rbeg))
	 (limit (copy-marker (if at-point (max rend (cdr at-point)) rend) t))
	 (count 0))
    (save-excursion
      (goto-char start)
      (while (org-change--search-forward limit t)
	(let ((beg (match-beginning 0))
	      (stop (match-end 0)))
	  (if (or (<= stop (marker-position end))
		  (and point-beg (= beg (marker-position point-beg))))
	      (let ((done (progn (goto-char beg)
				 (org-change--apply-change accept))))
		(if done
		    (progn
		      (setq count (1+ count))
		      ;; `org-change--apply-change' leaves point where it was,
		      ;; so step past the replacement by hand.
		      (goto-char done))
		  ;; Should not happen, but never loop forever on it.
		  (goto-char (1+ beg))))
	    (goto-char stop)))))
    (set-marker end nil)
    (set-marker limit nil)
    (when point-beg (set-marker point-beg nil))
    count))

(defun org-change--accept-or-reject (accept)
  "Accept (ACCEPT is t) or reject (ACCEPT is nil) changes.
With an active region, act on every change inside it, and on no
other; otherwise act on the change at point.  An overview open on
this buffer is refreshed, so it never lists a change that is gone."
  (if (use-region-p)
      (let ((count (org-change--apply-region
		    accept (region-beginning) (region-end))))
	(deactivate-mark)
	(message "%d change%s %s"
		 count
		 (if (= count 1) "" "s")
		 (if accept "accepted" "rejected")))
    (unless (org-change--apply-change accept)
      (message "No change at point")))
  (org-change--overview-update))

(defun org-change-accept ()
  "Accept the change at point, or every change in the active region."
  (interactive "")
  (org-change--accept-or-reject t))

(defun org-change-reject ()
  "Reject the change at point, or every change in the active region."
  (interactive "")
  (org-change--accept-or-reject nil))

(defun org-change-accept-and-next ()
  "Accept the change at point, or the region's changes, then move on.
Like `org-change-accept', but afterwards point goes to the next
change, so that a document can be reviewed with one key."
  (interactive "")
  (org-change--accept-or-reject t)
  (org-change-next-change))

(defun org-change-reject-and-next ()
  "Reject the change at point, or the region's changes, then move on.
Like `org-change-reject', but afterwards point goes to the next
change, so that a document can be reviewed with one key."
  (interactive "")
  (org-change--accept-or-reject nil)
  (org-change-next-change))

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
      ;; A marker, so that accepting or rejecting a change does not make
      ;; the bound drift over the text that follows.
      (setq end (copy-marker end t))
      (goto-char beg)
      (while (org-change--search-forward end t)
	(let ((answer (read-char "Accept change? [y/n] or SPC to skip, C-g to quit")))
	  (cond
	   ;; Step past the replacement: `org-change--apply-change' leaves
	   ;; point where it was, which would prompt for the same change again.
	   ((char-equal answer ?y)
	    (goto-char (or (org-change--apply-change t) (point))))
	   ((char-equal answer ?n)
	    (goto-char (or (org-change--apply-change nil) (point))))
	   ((char-equal answer ?\s)) ; skip
	   (t
	    (goto-char (match-beginning 0))))))
      (set-marker end nil)
      (deactivate-mark)
      (org-change--overview-update)))
  (message "No more changes"))

;;; Comments

(defun org-change-comment ()
  "Add or edit the comment on the change at point.
Prompt for the comment text, pre-filled with the existing one.  Any
author stamped on the change is kept; an empty comment removes it."
  (interactive)
  (let ((change (org-change--at-change)))
    (unless change
      (user-error "No change at point"))
    ;; Capture positions and the raw comment before prompting or calling
    ;; `org-change--split-comment', both of which clobber the match data.
    (let* ((beg (car change))
	   (end (cdr change))
	   (pair-end (+ (match-end 2) 2))	; just after {!new!}{!old!}
	   (split (org-change--split-comment
		   (org-change--decode (or (match-string 3) ""))))
	   (author (car split))
	   (new-note (string-trim (read-string "Comment: " (cdr split))))
	   (comment (org-change--join-comment author new-note))
	   (markup (if (equal comment "")
		       ""
		     (format "{!%s!}" (org-change--encode comment)))))
      (delete-region pair-end end)
      (save-excursion
	(goto-char pair-end)
	(insert markup))
      (org-change-fontify beg (+ pair-end (length markup))))))

;;; Navigation between changes

(declare-function org-fold-folded-p "org-fold" (&optional pos spec-or-alias))
(declare-function org-fold-show-set-visibility "org-fold" (detail))
(declare-function org-fold-core-get-regions "org-fold-core"
		  (&rest keyword-args))

;; `org-fold-core-regions' is a macro, not a function, so it has to be
;; loaded when this file is compiled: without it the call below is
;; compiled as a function call and fails at run time with
;; `invalid-function'.  A `declare-function' would not do.  org-fold-core
;; ships with Emacs, and the mode only calls it inside `org-mode'.
(eval-when-compile (require 'org-fold-core))
;; Called by the code that macro expands to.
(declare-function org-fold-core-region "org-fold-core"
		  (from to flag &optional spec-or-alias))

(defvar-local org-change--fold-restore nil
  "Function that restores the fold state saved before the last jump.
Nil when there is nothing to restore.  See `org-change--reveal'.")

(defun org-change--restore-fold ()
  "Restore the fold state saved by the previous jump, if any."
  (when org-change--fold-restore
    (funcall org-change--fold-restore)
    (setq org-change--fold-restore nil)))

(defun org-change--reveal ()
  "Reveal point if org folding hides it, saving the prior fold state.
The saved state is restored on the next jump, so stepping to the
next or previous change re-folds whatever this jump opened.  Does
nothing outside `org-mode'."
  (when (and (derived-mode-p 'org-mode)
	     (fboundp 'org-fold-folded-p)
	     (org-fold-folded-p (point)))
    (let ((saved (org-fold-core-get-regions :with-markers t)))
      (setq org-change--fold-restore
	    (lambda ()
	      (org-fold-core-regions saved :override t :clean-markers t))))
    (org-fold-show-set-visibility 'canonical)))

(defun org-change--goto-change (dest)
  "Move to DEST, restoring the previous reveal, revealing DEST, and
recentering the window as `recenter-top-bottom' (\\[recenter-top-bottom]) does."
  (org-change--restore-fold)
  (goto-char dest)
  (org-change--reveal)
  (when (get-buffer-window (current-buffer))
    ;; Center the change like C-l.  Bind `last-command' so that
    ;; pressing the jump key repeatedly keeps centering rather than
    ;; cycling `recenter-top-bottom' to the top and bottom.
    (let ((last-command nil))
      (recenter-top-bottom))))

(defun org-change-next-change ()
  "Move point to the beginning of the next change.
If point is inside a change, move to the one after it.  If there is
no later change, leave point where it is and say so.

When the target change is hidden by org folding, reveal it; the
previous reveal is restored, so each jump shows one change at a
time."
  (interactive)
  (let* ((origin (point))
	 (here (org-change--at-change))
	 ;; Start past the current change: searching from inside one, the
	 ;; parser could latch onto its second `{!' field and mis-span.
	 (start (if here (cdr here) origin))
	 (dest nil))
    (save-excursion
      (goto-char start)
      (when (org-change--search-forward nil t)
	(setq dest (match-beginning 0))))
    (if (and dest (> dest origin))
	(org-change--goto-change dest)
      (message "No next change"))))

(defun org-change-previous-change ()
  "Move point to the beginning of the previous change.
If point is inside a change, move to the one before it.  If there is
no earlier change, leave point where it is and say so.

When the target change is hidden by org folding, reveal it; the
previous reveal is restored, so each jump shows one change at a
time."
  (interactive)
  (let ((origin (point))
	(dest nil))
    (save-excursion
      (goto-char (point-min))
      (while (org-change--search-forward origin t)
	(when (< (match-beginning 0) origin)
	  (setq dest (match-beginning 0)))))
    (if dest
	(org-change--goto-change dest)
      (message "No previous change"))))

;;; Help

(defvar org-change--help-table
  '((org-change-add-key . "Mark the region as an addition (or start typing new text)")
    (org-change-delete-key . "Mark the region as a deletion")
    (org-change-replace-key . "Replace the region with new text")
    (org-change-kill-key . "Kill (cut) the region as a deletion")
    (org-change-yank-key . "Yank (paste) as an addition")
    (org-change-accept-key . "Accept the change at point (or in the region)")
    (org-change-reject-key . "Reject the change at point (or in the region)")
    (org-change-accept-and-next-key . "Accept the change, then go to the next one")
    (org-change-reject-and-next-key . "Reject the change, then go to the next one")
    (org-change-accept-reject-all-key . "Accept or reject every change, one by one")
    (org-change-comment-key . "Add or edit the change's comment")
    (org-change-next-key . "Go to the next change")
    (org-change-previous-key . "Go to the previous change")
    (org-change-overview-key . "List every change in a side window")
    (org-change-info-key . "Report the number of additions, deletions, replacements")
    (org-change-fontify-key . "Re-fontify the buffer")
    (org-change-help-key . "Show this help"))
  "Rows of (KEY-VARIABLE . DESCRIPTION) for `org-change-help'.")

(defun org-change--help-string ()
  "Return the current key bindings and their descriptions as text."
  (mapconcat
   (lambda (row)
     (format "  %-8s  %s"
	     (key-description (symbol-value (car row)))
	     (cdr row)))
   org-change--help-table "\n"))

(defun org-change-help ()
  "Display the Org Change key bindings, with a description of each."
  (interactive)
  (with-help-window "*Org Change Help*"
    (princ "Org Change key bindings:\n\n")
    (princ (org-change--help-string))
    (princ "\n")))

;;; Overview of the changes in a side window

(defvar org-change-overview-buffer-name "*Org Change Overview*"
  "Name of the buffer listing the changes of another buffer.")

(defvar-local org-change-overview--source nil
  "The buffer whose changes the overview lists.")

(defun org-change--change-summary ()
  "Return a one-line description of the change just matched.
Uses the match data of `org-change--regexp', so it has to be called
right after a search.  A deletion has no new text to show, so its
old text is shown behind `org-change-deleted-marker' instead."
  (let* ((new (org-change--decode (match-string-no-properties 1)))
	 (old (org-change--decode (match-string-no-properties 2)))
	 (text (if (equal new "")
		   (concat org-change-deleted-marker old)
		 new))
	 (line (car (split-string text "\n"))))
    (if (string-empty-p (string-trim line))
	"(empty)"
      (string-trim line))))

(defun org-change--overview-entries (buffer)
  "Return one entry per change in BUFFER, in order.
An entry is a list (MARKER LINE SUMMARY): where the change starts,
the line it is on, and a one-line description of it."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((entries nil))
	(while (org-change--search-forward nil t)
	  (let* ((beg (match-beginning 0))
		 ;; Before anything that could clobber the match data.
		 (summary (org-change--change-summary)))
	    (push (list (copy-marker beg) (line-number-at-pos beg) summary)
		  entries)))
	(nreverse entries)))))

(defun org-change-overview--render ()
  "Fill the overview buffer from the changes of its source.
Point stays on the line it was on, so that accepting or rejecting
a change leaves the cursor on the one that takes its place.  The
window showing the overview is what point is read from and written
back to: refreshing from the other buffer must not lose the place
the cursor holds on screen."
  (let* ((source org-change-overview--source)
	 (entries (and (buffer-live-p source)
		       (org-change--overview-entries source)))
	 (window (get-buffer-window (current-buffer) t))
	 (line (line-number-at-pos (if window (window-point window) (point))))
	 (inhibit-read-only t))
    (erase-buffer)
    (if (null entries)
	(insert "No changes")
      (dolist (entry entries)
	(let ((start (point)))
	  (insert (format "%4d  %s\n" (nth 1 entry) (nth 2 entry)))
	  (put-text-property start (point) 'org-change-marker (car entry)))))
    (goto-char (point-min))
    (forward-line (1- line))
    ;; The last line may have gone: do not sit past the end of the list.
    (when (and (eobp) (not (bobp)))
      (forward-line -1))
    (when window
      (set-window-point window (point)))
    (set-buffer-modified-p nil)))

(defun org-change--overview-update ()
  "Refresh the overview, if one is open on this buffer.
Accepting or rejecting in the text itself would otherwise leave the
list showing a change that is no longer there."
  (let ((buffer (get-buffer org-change-overview-buffer-name))
	(source (current-buffer)))
    (when (and buffer (not (eq buffer source)))
      (with-current-buffer buffer
	(when (eq org-change-overview--source source)
	  (org-change-overview--render))))))

(defun org-change-overview--marker ()
  "Return the marker of the change listed on this line."
  (or (get-text-property (line-beginning-position) 'org-change-marker)
      (user-error "No change on this line")))

(defun org-change-overview--source-window ()
  "Return a window showing the source buffer, displaying it if need be."
  (let ((source org-change-overview--source))
    (unless (buffer-live-p source)
      (user-error "The buffer this overview describes is gone"))
    (or (get-buffer-window source)
	(display-buffer source '(nil (inhibit-same-window . t))))))

(defun org-change-overview-goto ()
  "Go to the change listed on this line, in the buffer it belongs to."
  (interactive)
  (let ((marker (org-change-overview--marker))
	(window (org-change-overview--source-window)))
    (select-window window)
    (org-change--goto-change (marker-position marker))))

(defun org-change-overview--apply (accept)
  "Accept or reject the change listed on this line, in its own buffer.
ACCEPT is as in `org-change--apply-change'.  The overview is
refreshed, and the cursor stays put, so it ends up on the change
that moves into the place of the one just dealt with."
  (let ((marker (org-change-overview--marker))
	(source org-change-overview--source))
    (unless (buffer-live-p source)
      (user-error "The buffer this overview describes is gone"))
    (with-current-buffer source
      (save-excursion
	(goto-char marker)
	(org-change--apply-change accept)))
    (org-change-overview--render)
    (message "Change %s" (if accept "accepted" "rejected"))))

(defun org-change-overview-accept ()
  "Accept the change listed on this line, in the buffer it belongs to."
  (interactive)
  (org-change-overview--apply t))

(defun org-change-overview-reject ()
  "Reject the change listed on this line, in the buffer it belongs to."
  (interactive)
  (org-change-overview--apply nil))

(defun org-change--bare-key (key)
  "Return the last event of KEY on its own, as a key sequence.
The overview needs no prefix -- there is nothing to type there --
so `C-` k\=' also answers to plain `k\='.  Taking the last event
rather than a fixed letter keeps that true when the bindings are
customized."
  (let ((events (listify-key-sequence key)))
    (and events (vector (car (last events))))))

(defvar org-change-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'org-change-overview-goto)
    (dolist (row `((,org-change-accept-key . org-change-overview-accept)
		   (,org-change-reject-key . org-change-overview-reject)))
      (define-key map (car row) (cdr row))
      (let ((bare (org-change--bare-key (car row))))
	(when bare
	  (define-key map bare (cdr row)))))
    map)
  "Keymap of `org-change-overview-mode'.
Inherits `special-mode-map', which is where `q\=' and the usual
scrolling keys come from.  The accept and reject keys are bound
both as they are in the text and without their prefix.")

(define-derived-mode org-change-overview-mode special-mode "Org Change"
  "Major mode for the buffer listing the changes of another buffer.

Move with the arrow keys, press \\[org-change-overview-goto] to go
to the change on this line, \\[revert-buffer] to refresh the list,
and \\[quit-window] to close the window.  The accept and reject
keys act on the change listed on this line, in the buffer being
reviewed; there is nothing to type here, so they work without their
prefix as well."
  (setq truncate-lines t)
  (setq-local revert-buffer-function
	      (lambda (&rest _) (org-change-overview--render))))

(defun org-change-overview ()
  "List every change of this buffer, one line each, in a side window.
The window is selected, so you can move through the changes right
away.  See `org-change-overview-mode' for what the keys do."
  (interactive)
  (let ((source (current-buffer))
	(buffer (get-buffer-create org-change-overview-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-change-overview-mode)
	(org-change-overview-mode))
      ;; After the mode: turning it on kills the buffer-local variables.
      (setq org-change-overview--source source)
      (org-change-overview--render))
    (select-window
     (display-buffer buffer
		     `((display-buffer-in-side-window)
		       (side . ,org-change-overview-side)
		       (window-width . ,org-change-overview-width))))))

;;; Counting changes

(defun org-change--counts ()
  "Return the list (ADDITIONS DELETIONS REPLACEMENTS) for the buffer.
The empty change `{!!}{!!}' is not counted."
  (let ((add 0) (del 0) (rep 0))
    (save-excursion
      (goto-char (point-min))
      (while (org-change--search-forward nil t)
	(let ((new (match-string 1))
	      (old (match-string 2)))
	  (cond
	   ((and (equal new "") (equal old "")))	; empty change: skip
	   ((equal old "") (setq add (1+ add)))		; addition
	   ((equal new "") (setq del (1+ del)))		; deletion
	   (t (setq rep (1+ rep)))))))			; replacement
    (list add del rep)))

(defun org-change-info ()
  "Show the number of additions, deletions, and replacements."
  (interactive)
  (pcase-let ((`(,add ,del ,rep) (org-change--counts)))
    (if (zerop (+ add del rep))
	(message "No changes")
      (message "%d addition%s, %d deletion%s, %d replacement%s"
	       add (if (= add 1) "" "s")
	       del (if (= del 1) "" "s")
	       rep (if (= rep 1) "" "s")))))

(defun org-change-toggle-deleted-text ()
  "Show/hide deleted text."
  (interactive)
  (setq org-change-show-deleted (not org-change-show-deleted))
  (org-change-fontify))

;;; Generating changes from two versions

(defun org-change--tokenize (string)
  "Split STRING into a list of word and non-word runs.
Concatenating the result reproduces STRING.  Splitting on word
boundaries makes the diff align on words, so changes read
naturally rather than character by character."
  (let ((tokens nil)
	(start 0)
	(len (length string)))
    (while (< start len)
      (string-match "[[:word:]]+\\|[^[:word:]]+" string start)
      (push (substring string start (match-end 0)) tokens)
      (setq start (match-end 0)))
    (nreverse tokens)))

(defun org-change--diff-ops (old new)
  "Return diff operations turning token list OLD into token list NEW.
Each operation is a cons: (equal . TOKEN), (old . TOKEN) for a
token only in OLD, or (new . TOKEN) for a token only in NEW.  Uses
a longest-common-subsequence table, so the shared text is kept and
only the differences are marked."
  (let* ((a (vconcat old))
	 (b (vconcat new))
	 (n (length a))
	 (m (length b))
	 (w (1+ m))
	 (dp (make-vector (* (1+ n) w) 0))
	 (i (1- n)))
    ;; Fill the LCS-length table from the bottom-right corner.
    (while (>= i 0)
      (let ((j (1- m)))
	(while (>= j 0)
	  (aset dp (+ (* i w) j)
		(if (equal (aref a i) (aref b j))
		    (1+ (aref dp (+ (* (1+ i) w) (1+ j))))
		  (max (aref dp (+ (* (1+ i) w) j))
		       (aref dp (+ (* i w) (1+ j))))))
	  (setq j (1- j))))
      (setq i (1- i)))
    ;; Walk the table from the top-left to recover the operations.
    (let ((ops nil) (i 0) (j 0))
      (while (and (< i n) (< j m))
	(cond
	 ((equal (aref a i) (aref b j))
	  (push (cons 'equal (aref a i)) ops)
	  (setq i (1+ i) j (1+ j)))
	 ((>= (aref dp (+ (* (1+ i) w) j)) (aref dp (+ (* i w) (1+ j))))
	  (push (cons 'old (aref a i)) ops)
	  (setq i (1+ i)))
	 (t
	  (push (cons 'new (aref b j)) ops)
	  (setq j (1+ j)))))
      (while (< i n) (push (cons 'old (aref a i)) ops) (setq i (1+ i)))
      (while (< j m) (push (cons 'new (aref b j)) ops) (setq j (1+ j)))
      (nreverse ops))))

(defun org-change--change-markup (new old)
  "Return change markup for NEW replacing OLD.
Either string may be empty, giving an addition or a deletion; both
empty gives the empty string.  Content is escaped so delimiters in
the diffed text are safe."
  (cond
   ((and (equal old "") (equal new "")) "")
   ((equal old "") (format "{!%s!}{!!}" (org-change--encode new)))
   ((equal new "") (format "{!!}{!%s!}" (org-change--encode old)))
   (t (format "{!%s!}{!%s!}"
	      (org-change--encode new)
	      (org-change--encode old)))))

(defun org-change--diff-to-markup (old new)
  "Return the string NEW annotated with change markup relative to OLD.
OLD and NEW are strings.  Text only in NEW becomes an addition,
text only in OLD a deletion, and a changed span a replacement;
shared text is left verbatim.  Adjacent differing tokens are
coalesced into a single change.  The result round-trips: accepting
every change yields NEW, rejecting every change yields OLD."
  (let ((ops (org-change--diff-ops (org-change--tokenize old)
				   (org-change--tokenize new)))
	(pieces nil)
	(old-run "")
	(new-run ""))
    (dolist (op ops)
      (pcase (car op)
	('equal
	 (push (org-change--change-markup new-run old-run) pieces)
	 (setq old-run "" new-run "")
	 (push (cdr op) pieces))
	('old (setq old-run (concat old-run (cdr op))))
	('new (setq new-run (concat new-run (cdr op))))))
    (push (org-change--change-markup new-run old-run) pieces)
    (apply #'concat (nreverse pieces))))

(defun org-change--file-string (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-change--git-string (rev)
  "Return the current buffer's file contents at git revision REV.
Signal a `user-error' if git, the file, or the revision is not
available, so the buffer is left untouched."
  (unless (executable-find "git")
    (user-error "Git is not available; compare against a file instead"))
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file; compare against a file instead"))
    (let* ((default-directory (file-name-directory file))
	   (root (ignore-errors
		   (with-temp-buffer
		     (when (zerop (call-process "git" nil t nil
						"rev-parse" "--show-toplevel"))
		       (string-trim (buffer-string)))))))
      (unless root
	(user-error "%s is not in a git repository" file))
      (let ((rel (file-relative-name file root)))
	(with-temp-buffer
	  (unless (zerop (call-process "git" nil t nil
				       "show" (format "%s:%s" rev rel)))
	    (user-error "Cannot read %s at git revision %s" rel rev))
	  (buffer-string))))))

(defun org-change-from-diff (source)
  "Rewrite the current buffer to review the changes SOURCE brings in.
The current buffer is the base version; SOURCE provides the new
version, as a cons cell, either (file . FILENAME) or (git . REVISION).
The differences are shown as tracked changes over the buffer, so
accepting them adopts SOURCE's text and rejecting them keeps the
buffer as it was.

Interactively, prompt for a file holding the new version; with a
prefix argument, prompt instead for a git revision (default HEAD)
and use the buffer's own file as it was at that revision.  Git mode
signals an error, leaving the buffer untouched, when git or the
file is not available.

The buffer is replaced in a single undoable step, so `undo'
restores it as well."
  (interactive
   (list (if current-prefix-arg
	     (cons 'git (read-string "New version -- git revision: " "HEAD"))
	   (cons 'file (read-file-name "New version file: " nil nil t)))))
  (let* ((base (buffer-string))
	 (incoming (pcase source
		     (`(file . ,f) (org-change--file-string f))
		     (`(git . ,rev) (org-change--git-string rev))
		     (_ (user-error "Invalid diff source: %S" source))))
	 (markup (org-change--diff-to-markup base incoming)))
    (atomic-change-group
      (delete-region (point-min) (point-max))
      (insert markup)
      (goto-char (point-min)))
    (unless org-change-mode (org-change-mode 1))
    (org-change-fontify)
    (message "Showing changes from %s -- accept to adopt them, reject to keep yours"
	     (pcase source (`(file . ,f) f) (`(git . ,rev) rev)))))

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
		(concat (format "{!%s!}{!%s!}"
				(org-change--encode new-text)
				(org-change--encode old-text))
			(if comment
			    (format "{!%s!}" (org-change--encode comment))
			  ""))))
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

(defun org-change--latex-options (author note)
  "Return the LaTeX optional-argument string for AUTHOR and NOTE.
AUTHOR becomes the changes package `id', NOTE its `comment'.  Either
may be empty; the result is empty when both are."
  (let ((parts nil))
    (when (and author (not (equal author "")))
      (push (format "id=%s" author) parts))
    (when (and note (not (equal note "")))
      (push (format "comment=%s" note) parts))
    (if parts
	(format "[%s]" (mapconcat #'identity (nreverse parts) ", "))
      "")))

(defun org-change--export-latex (old-text new-text comment)
  "Export a change to LaTeX.
OLD-TEXT, NEW-TEXT, and COMMENT are the elements of the change; an
@ID prefix in COMMENT is exported as the changes package author id.
The result is wrapped in @@latex:...@@ so org exports it verbatim."
  (let* ((split (org-change--split-comment comment))
	 (opts (org-change--latex-options (car split) (cdr split))))
    (format "@@latex:%s@@"
	    (cond ((equal old-text "")
		   (format "\\added%s{%s}" opts new-text))
		  ((equal new-text "")
		   (format "\\deleted%s{%s}" opts old-text))
		  (t
		   (format "\\replaced%s{%s}{%s}" opts new-text old-text))))))

(defun org-change--latex-author-defs ()
  "Return \\definechangesauthor lines for every author in `org-change-authors'."
  (mapconcat
   (lambda (entry)
     (let ((id (car entry))
	   (props (cdr entry)))
       (format "\\definechangesauthor[name={%s}, color={%s}]{%s}\n"
	       (or (plist-get props :name) id)
	       (or (plist-get props :color) "black")
	       id)))
   org-change-authors ""))

(defun org-change--make-span (class text)
  "Return string <span class=\"CLASS\">TEXT</span> for HTML export."
    (if (equal text "")
	""
      (format "<span class=\"%s\">%s</span>" class text)))

(defun org-change--html-class (base author)
  "Return the span class BASE, with the AUTHOR class appended if any."
  (if (and author (not (equal author "")))
      (format "%s org-change-author-%s" base author)
    base))

(defun org-change--export-html (old-text new-text comment)
  "Export a change to HTML.
OLD-TEXT, NEW-TEXT, and COMMENT are the elements of the change; an
@ID prefix in COMMENT tags the spans with an org-change-author-ID
class.  The result is wrapped in @@html:...@@ so org exports it
verbatim."
  (let* ((split (org-change--split-comment comment))
	 (author (car split))
	 (note (cdr split))
	 (added (org-change--html-class "org-change-added" author))
	 (deleted (org-change--html-class "org-change-deleted" author)))
    (format "@@html:%s@@"
	    (cond ((equal old-text "")
		   (org-change--make-span
		    added
		    (concat new-text (org-change--make-span
				      "org-change-comment" note))))
		  ((equal new-text "")
		   (org-change--make-span
		    deleted
		    (concat old-text (org-change--make-span
				      "org-change-comment" note))))
		  (t
		   (concat
		    (org-change--make-span
		     added
		     (concat new-text (org-change--make-span
				       "org-change-comment" note)))
		    (org-change--make-span deleted old-text)))))))

(defun org-change--export-ascii (old-text new-text comment)
  "Export a change to plain text, as CriticMarkup.
OLD-TEXT, NEW-TEXT, and COMMENT are the elements of the change: an
addition becomes `{++new++}\=', a deletion `{--old--}\=', and a
replacement `{~~old~>new~~}\='.  A comment follows as `{>>note<<}\=',
carrying the author of an @ID prefix as `{>>ID: note<<}\=', which is
how Emacs shows it.  The result is wrapped in @@ascii:...@@ so org
exports it verbatim, without reading the markup as org syntax."
  (let* ((split (org-change--split-comment comment))
	 (author (car split))
	 (note (cdr split))
	 (text (cond ((equal old-text "")
		      (format "{++%s++}" new-text))
		     ((equal new-text "")
		      (format "{--%s--}" old-text))
		     (t
		      (format "{~~%s~>%s~~}" old-text new-text)))))
    (format "@@ascii:%s%s@@"
	    text
	    (if (equal note "")
		""
	      (format "{>>%s%s<<}"
		      (if (or (null author) (equal author ""))
			  ""
			(concat author ": "))
		      note)))))

(defvar org-change--exporters
  '((latex . org-change--export-latex)
    (html . org-change--export-html)
    (ascii . org-change--export-ascii))
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
  (while (org-change--search-forward nil t)
    (let* ((new-text (org-change--decode (match-string 1)))
	   (old-text (org-change--decode (match-string 2)))
	   (comment (org-change--decode (or (match-string 3) "")))
	   ;; The exporter may call `string-match' (for example to parse
	   ;; the author out of the comment), which would clobber the
	   ;; buffer match data that `replace-match' below relies on.
	   (replacement
	    (save-match-data
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
				backend))))))))
      (replace-match replacement t t))))

(defun org-change-filter-final-output (text backend _)
  "Add the changes.sty package and author definitions to the preamble.
TEXT is the whole document and BACKEND is checked for being
\\='latex or derived from \\='latex.  A `\\definechangesauthor' line
is emitted for each entry in `org-change-authors'."
  (if (and (org-export-derived-backend-p backend 'latex)
	   (not org-change-final))
      ;; Use a function replacement so backslashes in the injected
      ;; preamble are inserted verbatim, without `\N' processing.
      (replace-regexp-in-string
       "\\\\begin{document}"
       (lambda (_)
	 (concat
	  "\\usepackage"
	  (when (boundp 'org-change-latex-options) org-change-latex-options)
	  "{changes}\n"
	  (org-change--latex-author-defs)
	  "\\begin{document}"))
       text nil t)
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
            (define-key map org-change-accept-and-next-key #'org-change-accept-and-next)
            (define-key map org-change-reject-and-next-key #'org-change-reject-and-next)
            (define-key map org-change-accept-reject-all-key #'org-change-accept-reject-all)
            (define-key map org-change-fontify-key #'org-change-fontify)
            (define-key map org-change-comment-key #'org-change-comment)
            (define-key map org-change-next-key #'org-change-next-change)
            (define-key map org-change-previous-key #'org-change-previous-change)
            (define-key map org-change-overview-key #'org-change-overview)
            (define-key map org-change-info-key #'org-change-info)
            (define-key map org-change-help-key #'org-change-help)
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
    (setq org-change--fold-restore nil)
    (org-change--remove-overlays)))

(provide 'org-change)

;;; org-change.el ends here
