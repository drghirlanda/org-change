;;; org-change.el --- Track changes in org-mode files -*- lexical-binding: t;

;; Copyright (C) 2023 Stefano Ghirlanda

;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.3"))
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

;; This package provides a minor mode for tracking changes in org-mode
;; files, by defining a new type of link, the change: link. Using the
;; functions org-change-add, org-change-delete, and
;; org-change-replace, you can mark text as constitution an addiion,
;; deletion, or replacement to the text. These functions are bound by
;; default to C-` a, C-` d, and C-` r. Functions org-change-accept and
;; org-change-reject can be used to replace the change: link with the
;; new and old text, respectively. These are bound to C-` o and C-` x.
;;
;; To change key bindings and other settings, run M-x customize-group
;; RET org-change. When exporting to LaTeX, changes are rendered using
;; the "changes" package. See the package URL for more documentation.

;;; Code:

;; Code 

(defvar org-change--deleted-marker "((DELETED))"
  "Placeholder for deleted text.")

(defun org-change-replace ()
  "Mark the active region as old text that is being replaced by new
text. You will be prompted for the new text, and the old text
will be hidden in a change: link."
  (interactive "")
  (if (use-region-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(insert (format "[[change:%s][%s]]" 
			(buffer-substring-no-properties beg end)
			(read-string "New text: ")))
	(delete-region beg end))
    (message "Select text to be replaced")))

(defun org-change-delete ()
  "Mark the active region as text that is being deleted. The text
will be hidden in a change: link and a DELETED marker will be
shown in its place."
  (interactive "")
  (if (use-region-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(insert (format "[[change:%s][%s]]" 
			(buffer-substring-no-properties beg end)
			org-change--deleted-marker))
	(delete-region beg end))
    (message "Select text to be deleted")))

(defun org-change-add ()
  "Mark the active region as text that is being added. If the
region is not active, you will be prompted for text to add. The
added text is shown in a change: link."
  (interactive "")
  (if (use-region-p)
      (let ((beg (region-beginning))
	    (end (region-end) (point)))
	(insert (format "[[change:][%s]]" (buffer-substring-no-properties beg end)))
	(delete-region beg end))
    (insert (format "[[change:][%s]]" (read-string "New text: ")))))

(defun org-change--accept-or-reject (accept)
  "Internal function to accept (t argument) or reject (nil
argument)."
  (setq link-position (org-in-regexp "\\[\\[change:\\(.*\\)\\]\\[\\(.*\\)\\]\\]" 10))
  (if link-position
      (let ((old-text (match-string-no-properties 1))
	    ;; to get new-text we also discard comments, if present: 
	    (new-text (replace-regexp-in-string "\\*\\*.+\\*\\*$" "" (match-string-no-properties 2)))
	    (beg (car link-position))
	    (end (cdr link-position)))
	(delete-region beg end)
	(if accept
	    (unless (equal new-text org-change--deleted-marker)
	      (insert new-text))
	  (insert old-text)))
    (message "There is no change: link here")))

(defun org-change-accept ()
  "Accept the change at point, replacing the change: link with the
new text and erasing the old text or DELETE marker."
  (interactive "")
  (org-change--accept-or-reject t))

(defun org-change-reject ()
  "Reject the change at point, replacing the change: link with the
old text and erasing the new text or DELETE marker."
  (interactive "")
  (org-change--accept-or-reject nil))

;; Tell org-mode about change: links 

(defface org-change-link-face
  '((t (:foreground "blue violet" :underline nil)))
  "Face for org-change links.")

(org-link-set-parameters "change"
                         :follow #'org-change-open-link
                         :export #'org-change-export-link
                         :store #'org-change-store-link
			 :face 'org-change-link-face)

(defun org-change-open-link (path _)
    ""
  nil)

(defun org-change-store-link ()
    ""
    nil)

(defun org-change-export-link (old-text new-text-raw backend _)
  "Export a change link to a backend. This function operates within
the standard org-mode link export."
  (if (string-match "\\(.*\\)\\*\\*\\(.+\\)\\*\\*$" new-text-raw)
      (progn
	(setq new-text (match-string 1 new-text-raw))
	(setq comment  (match-string 2 new-text-raw)))
    (setq new-text new-text-raw)
    (setq comment ""))
  (cond ((org-export-derived-backend-p backend 'latex)
	 (progn
	   (if (not (equal comment ""))
	       (setq comment (format "[comment=%s]" comment)))
	   (cond ((equal old-text "")
		  (format "\\added%s{%s}" comment new-text))
		 ((equal new-text org-change--deleted-marker)
		  (format "\\deleted%s{%s}" comment old-text))
		 (t
		  (format "\\replaced%s{%s}{%s}" comment new-text old-text)))))
	(t
	 (error (format "Change links not supported in %s export" backend)))))

;; Customizations and minor mode definitions

(defgroup org-change nil
  "Customization options for org-change."
  :group 'org)

(defcustom org-change-add-key (kbd "C-` a")
  "Keybinding for org-change-add."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-delete-key (kbd "C-` d")
  "Keybinding for org-change-delete."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-replace-key (kbd "C-` r")
  "Keybinding for org-change-replace."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-accept-key (kbd "C-` k")
  "Keybinding for org-change-accept."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-accept-key (kbd "C-` x")
  "Keybinding for org-change-reject."
  :type 'key-sequence
  :group 'org-change)

(defcustom org-change-face 'org-change-link-face
  "Face for org-change links."
  :type 'face
  :group 'org-change)

(define-minor-mode org-change-mode
  "Minor mode for tracking changes in org-mode files."
  :lighter " Chg"
  :group 'org-change
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map org-change-add-key #'org-change-add)
            (define-key map org-change-delete-key #'org-change-delete)
            (define-key map org-change-replace-key #'org-change-replace)
            (define-key map org-change-accept-key #'org-change-accept)
            map))

(provide 'org-change)

;;; org-change.el ends here
