;;; org-change.el --- Track changes in org-mode files

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

;; This package provides a syntax for tracking changes in org-mode
;; files, using org links and the changes package for LaTeX export.

;;; Code:

;; 
(defgroup org-change nil
  "Customization options for org-mode change syntax."
  :group 'org)

;;
(defcustom org-change-delete-key (kbd "C-c d")
  "Keybinding for deleting the active region (hiding it into a
change link and showing **DELETED** in its place)."
  :type 'key-sequence
  :group 'org-change)

;;
(defcustom org-change-replace-key (kbd "C-c r")
  "Keybinding for replacing the active region (hiding it into a
change link and placing the point to insert replacement text)."
  :type 'key-sequence
  :group 'org-change)

;;
(defcustom org-change-add-key (kbd "C-c a")
  "Keybinding for tagging the active region as added text."
  :type 'key-sequence
  :group 'org-change)

(define-minor-mode org-change-mode
  "Minor mode for tracking changes in org-mode files."
  :lighter " Chg"
  :group 'org-change
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map org-change-add-key 'org-change-add)
            (define-key map org-change-delete-key 'org-change-delete)
            (define-key map org-change-replace-key 'org-change-replace)
            map))

;;
(defun org-change-replace ()
  ""
  (interactive "")
  (if (not (use-region-p))
      (message "Select text to be replaced")
    (let ((beg (region-beginning))
	  (end (region-end)))
      (insert (format "[[change:%s][%s]]" 
		      (buffer-substring-no-properties beg end)
		      (read-string "New text: ")))
      (delete-region beg end))))

;;
(defun org-change-delete ()
  ""
  (interactive "")
  (if (not (use-region-p))
      (message "Select text to be deleted")
    (let ((beg (region-beginning))
	  (end (region-end)))
      (insert (format "[[change:%s][**DELETED**]]" 
		      (buffer-substring-no-properties beg end)))
      (delete-region beg end))))

;;
(defun org-change-add ()
  "If the region is active, mark it as an addition. If the region is not active, prompt the user for text to add. The added text is embedded in a change link as [[change:][added text]]."
  (interactive "")
  (let	((beg (if (use-region-p) (region-beginning) (point)))
	 (end (if (use-region-p) (region-end) (point))))
    (insert (format "[[change:][%s]]" 
		    (if (use-region-p)
			(buffer-substring-no-properties beg end)
		      (read-string "New text: "))))
    (delete-region beg end)))

(defface org-change-link-face
  '((t (:foreground "blue" :underline nil)))
  "Face for org-change links.")

(defgroup org-change nil
  "Customization options for org-change."
  :group 'org
  :prefix "org-change-")

(defcustom org-change-face 'org-change-link-face
  "Face for org-change links."
  :type 'face
  :group 'org-change)

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
  ""
  (if (string-match "\\(.*\\)\\*\\*\\(.+\\)\\*\\*$" new-text-raw)
      (progn
	(setq new-text (match-string 1 new-text-raw))
	(setq comment  (format "[comment=%s]" (match-string 2 new-text-raw))))
    (progn
      (setq new-text new-text-raw)
      (setq comment "")))
  (cond ((org-export-derived-backend-p backend 'latex)
	 (cond ((equal old-text "")
		(format "\\added%s{%s}" comment new-text))
	       ((equal new-text "**DELETED**")
		(format "\\deleted%s{%s}" comment old-text))
	       (t
		(format "\\replaced%s{%s}{%s}" comment new-text old-text))))
	(t
	 (error (format "Change links not supported in %s export" backend)))))

(provide 'org-change)

;;; org-change.el ends here

