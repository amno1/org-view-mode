;;; org-view-mode.el --- Read-only viewer with less markup clutter in org mode files  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: convenience, outlines, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Arthur Miller
;; Version: 0.0.1
;; Keywords: tools convenience
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/amno1/org-view-mode

;;; Commentary:

;; A minor mode to help reduce clutter in org-mode files by
;; hiding/unhiding org-mode markup language
;;
;; To turn it on execute:
;;
;;          `M-x org-view-mode'.
;;
;; To turn it off execute the same command.

;;; Issues

;;; Code:
(require 'org)

(defgroup org-view nil
  "Hide tags in org-headings."
  :prefix "org-view-"
  :group 'org)

(defvar-local org-view-center-credentials nil
  "Whether to align title and author in center or not.

Centering is done pixel wise relative to window width.")

(defcustom org-view-diminish-mode t
  "Hide lighters for individual minor modes when org-view-mode is on."
  :type 'boolean
  :group 'org-view)

(defvar org-view-stars-re "^[ \t]*\\*+"
  "Regex used to recognize leading stars in org-headings.")

(defvar org-view-credentials-re "[ \t]*#\\+\\(TITLE\\|AUTHOR\\):"
  "Regex used to update author and title lines.")

(defun org-view--update-tags (visibility)
  "Update invisible property to VISIBILITY for tags in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (with-silent-modifications
      (while (re-search-forward org-view-stars-re nil t)
        (goto-char (line-end-position))
        (when (re-search-backward org-tag-line-re (line-beginning-position) t)
            (put-text-property
             (match-beginning 1) (match-end 1) 'invisible visibility))
          (forward-line)))))

(defun org-view--update-keywords (visibility)
  "Set VISIBILITY for each line starting with a keyword from KEYWORDS list."
  (org-with-wide-buffer
   (save-excursion
     (with-silent-modifications
       (goto-char (point-min))
       (while (re-search-forward "^[ \t]*#\\+.*$" nil t)
         (goto-char (match-beginning 0))
         (unless (looking-at-p org-view-credentials-re)
           (put-text-property
            (1- (match-beginning 0)) (match-end 0) 'invisible visibility))
         (goto-char (match-end 0)))))))

(defun org-view--update-properties (visibility)
  "Set invisible property to VISIBILITY for properties in the current buffer."
  (org-with-wide-buffer
   (save-excursion
     (with-silent-modifications
       (goto-char (point-min))
       (while (re-search-forward org-property-drawer-re nil t)
         (put-text-property
          (match-beginning 0) (match-end 0) 'invisible visibility))
       (goto-char (point-min))
       (while (re-search-forward "^[ \t]*#\\+PROPERTY:.*$" nil t)
         (put-text-property
          (match-beginning 0) (match-end 0) 'invisible visibility))))))

(defun org-view--update-stars (visibility)
  "Update invisible property to VISIBILITY for markers in the current buffer."
  (org-with-wide-buffer
   (save-excursion
     (goto-char (point-min))
     (with-silent-modifications
       (while (re-search-forward org-view-stars-re nil t)
         (put-text-property
          (match-beginning 0) (match-end 0) 'invisible visibility))))))

(defun org-view--update-credentials (visibility)
  "Set invisible property to VISIBILITY for export settings."
  (org-with-wide-buffer
   (save-excursion
     (with-silent-modifications
       (goto-char (point-min))
       (while (re-search-forward org-view-credentials-re nil t)
         (put-text-property
          (match-beginning 0) (match-end 0) 'invisible visibility)
         (when org-view-center-credentials
           (org-view--center-in-window visibility)))
       (goto-char (point-min))))))

(defun org-view--center-in-window (center)
  "Center a line in a window pixel wise."
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((end (line-end-position))
          (beg (line-beginning-position)))
      (if center
          (let* ((line (buffer-substring beg end))
                 (length (/ (string-pixel-width line) 2)))
            (put-text-property
             beg (1+ beg) 'display `(space :align-to (- center (,length)))))
        (remove-text-properties beg (1+ beg) '(display nil))))))

;;;###autoload
(define-minor-mode org-view-hide-tags-mode
  "Hide/show tags in org-headings."
  :global nil :lighter " org-htm"
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (org-view--update-tags org-view-hide-tags-mode))

;;;###autoload
(define-minor-mode org-view-hide-stars-mode
  "Hide/show leading stars in org-headings."
  :global nil :lighter " org-hsm"
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (org-view--update-stars org-view-hide-stars-mode))

;;;###autoload
(define-minor-mode org-view-hide-properties-mode
  "Hide/show properties and property drawers."
  :global nil :lighter " org-hpm"
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (org-view--update-properties org-view-hide-properties-mode))

;;;###autoload
(define-minor-mode org-view-pretty-credentials-mode
  "Prettify credentials in org-buffers."
  :global nil :lighter " org-pcm"
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (org-view--update-credentials org-view-pretty-credentials-mode))

(defun org-view-quit ()
  (interactive)
  (org-view-mode -1)
  (message "org-view mode disabled in current buffer"))

(defvar-keymap org-view-mode-map
  :doc "Keymap for ‘ORG-view-mode’"
  "c" #'org-view-quit
  "C" #'org-view-quit
  "e" #'org-view-quit
  "E" #'org-view-quit
  "q" #'org-view-quit
  "Q" #'org-view-quit)

;;;###autoload
(define-minor-mode org-view-mode
  "Hide/show babel source code blocks on demand."
  :global nil :lighter " org-view" :keymap org-view-mode-map
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (cond (org-view-mode
         (org-view-hide-tags-mode org-view-mode)
         (org-view-hide-stars-mode org-view-mode)
         (org-view-hide-keywords-mode org-view-mode)
         (org-view-hide-properties-mode org-view-mode)
         (org-view-pretty-credentials-mode org-view-mode)
         (when org-view-diminish-mode
           (dolist (mode '(org-view-hide-tags-mode
                           org-view-hide-stars-mode
                           org-view-hide-keywords-mode
                           org-view-hide-properties-mode
                           org-view-pretty-credentials-mode))
             (let ((mode-str (cdr (assq mode minor-mode-alist))))
               (setcar mode-str ""))))
         (view-mode +1))
        (t (view-mode -1)
           (org-view-hide-tags-mode -1)
           (org-view-hide-stars-mode -1)
           (org-view-hide-keywords-mode -1)
           (org-view-hide-properties-mode -1)
           (org-view-pretty-credentials-mode -1))))

(provide 'org-view-mode)

;;; org-view-mode.el ends here
