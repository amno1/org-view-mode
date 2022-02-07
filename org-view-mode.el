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

;; A read-only viewer with reduced clutter and prettier rendering in org files.
;; To enter the view-mode run: `M-x org-view-mode'.  To turn it off run the same
;; command.  The mode tries very hard to not do any changes to the buffer and
;; the content. However, it does use text properties for some effects, so at
;; least in theory, if you don't exit the viewer cleanly, via some of its exit
;; methods, it might happened that the view is not restored properly. In that
;; case you may re-enter, and exit the viewer again, or re-open the file. Text
;; properties are not saved, so the content of the buffer itself is never
;; modified.

;;; Issues

;;; Code:
(require 'org)

;;; Options
(defgroup org-view nil
  "Hide tags in org-headings."
  :prefix "org-view-"
  :group 'org)

(defcustom org-view-diminish-mode t
  "Hide lighters for individual minor modes when org-view-mode is on."
  :type 'boolean
  :group 'org-view)

(defcustom org-view-paragraph-indent "  "
  "Indentation string for paragraphs."
  :type 'string
  :group 'org-view)

(defcustom org-view-default-fill-column 80
  "Default size for fill-column in org-view mode.

This value is used only if fill-column has invalid size, such as <= 0 or nil."
  :type 'fixnum
  :group 'org-view)

(defcustom org-view-author-prefix "───"
  "String used to pretty-render author prefix."
  :type 'string
  :group 'org-view)

(defcustom org-view-verse-align 'left
  "Specify how verses will be aligned.

Alignement can be: left, right och middle. The alignement is done relative to
  the longest line in the paragrach. If present, the author line is excluded."
  :type 'symbol
  :group 'org-view)

(defcustom org-view-quote-align 'left
  "Specify how quotes will be aligned.

Alignement can be: left, right och middle. The alignement is done relative to
  the longest line in the paragrach. If present, the author line is excluded."
  :type 'symbol
  :group 'org-view)

(defvar org-view-stars-re "^[ \t]*\\*+"
  "Regex used to recognize leading stars in org-headings.")

(defvar org-view-credentials-re "[ \t]*#\\+\\(TITLE\\|AUTHOR\\|EMAIL\\):"
  "Regex used to update author and title lines.")

(defvar-local org-view-center-credentials nil
  "Whether to align title and author in center or not.

Centering is done pixel wise relative to window width.")

(defgroup org-view-faces nil
  "Faces for org-view mode."
  :group 'org-view)

(defface org-view-author-face
  '((t :slant italic :weight bold))
  "Face to display author names in paragraphs."
  :group 'org-view-faces)

(defface org-view-verse-face
  '((t :slant italic))
  "Face to display verses in paragraphs."
  :group 'org-view-faces)

(defface org-view-quote-face
  '((t :slant italic))
  "Face to display quotes in paragraphs."
  :group 'org-view-faces)

;;; Implementation
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
          (1- (match-beginning 0)) (1+ (match-end 0)) 'invisible visibility))))))

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
         (message "M: %s" (buffer-substring (match-beginning 0) (match-end 0)))
         (put-text-property
          (match-beginning 0) (match-end 0) 'invisible visibility)
         (when org-view-center-credentials
           (let ((left (point))
                 (right (line-end-position))
                 (width (org-view--fill-column-pixel-width)))
             (when (re-search-forward "[ \t]+$" (line-end-position) t)
               (setq left (point))
               (put-text-property
                (match-beginning 0) (match-end 0) 'invisible visibility))
             (org-view--center-region left right width visibility)))
           (forward-line))))))

(defun org-view--fill-column-pixel-width ()
  "Return width for fill-column in pixels.

This methods works only in buffers with live windows. This function is the only
instance where org-view modifies the content in original buffer in order to
calculate pixel width of a full line of text. The modification should not be
visible, but there is no guarantee, it really depends on Emacs rendering
algorithms.

Emacs 29 has function 'string-pixel-width' however it is not avialable in older
versions."
  (when (get-buffer-window (current-buffer))
    (save-excursion
      (with-silent-modifications
        (let ((width (if (> fill-column 0)
                         fill-column
                       org-view-default-fill-column))
              pixel-width)
          (goto-char (point-max))
          (insert (make-string width ?\s))
          (goto-char (line-beginning-position))
          (setq pixel-width
                (org-view--region-pixel-width
                 (line-beginning-position) (line-end-position)))
          (delete-region (line-beginning-position) (line-end-position))
          pixel-width)))))

(defun org-view--region-pixel-size (beg end)
  "Calculate pixel-size for a region."
  (save-restriction
    (narrow-to-region beg end)
    (window-text-pixel-size
     (get-buffer-window (current-buffer)) beg end)))

(defun org-view--region-pixel-width (beg end)
  "Calculate width in pixels for a region.

This should be cheaper than string-pixel-width introduced in 29.1 and accessible
in older versions as well. Limitation is that it works only for completely
visible buffer lines."
  (car (org-view--region-pixel-size beg end)))

(defun org-view--center-region (beg end line-width &optional center)
  "Render a line bounded by BEG and END centered in a window pixel wise.

LINE-WIDTH is the pixel width of the base line used to center region against,
typically fill-column pixel width, and it assumes width from the left text area
border."
  (if center
      (let* ((width (org-view--region-pixel-width beg end))
             (spacer (/ (- line-width width) 2)))
        (overlay-put
         (make-overlay beg (1+ beg))
         'before-string (propertize " " 'display `(space :width (,spacer)))))
    (remove-overlays beg end)))

;;; Paragraphs
(defvar org-view-paragraph-beg-re
  "^[ \t]*#\\+BEGIN_\\(QUOTE\\|CENTER\\|VERSE\\)[ \t]*$")
(defvar org-view-paragraph-end-re
  "^[ \t]*#\\+END_\\(QUOTE\\|CENTER\\|VERSE\\)[ \t]*$")
(defvar org-view--verse-ov nil
  "Overlay to prettify display of text marked as verse.")
(defvar org-view--quote-ov nil
  "Overlay to prettify display of text marked as quotes.")
(defvar org-view--author-ov nil
  "Overlay to prettify display of the author when an author is present.")
(defvar org-view--author-prefix-ov nil
  "Overlay to prettify display of the author prefix when an author is present.")

(defun org-view--line-lengths (beg end)
  "Return longest line in region."
  (save-excursion
    (let (positions left right)
      (goto-char beg)
      (while (< (point) end)
        (skip-chars-forward " \t")
        (setq left (point))
        (goto-char (line-end-position))
        (skip-chars-backward " \t")
        (setq right (point))
        (and left right (> (- right left) 0)
             (push (cons (- right left) (cons left right)) positions))
        (forward-line))
      positions)))

(defun org-view--align-paragraph (beg end align)
  "Align text in region with the longest line according to the ALIGN parameter.

ALIGN can be one of following: 'left, 'right, 'middle or nil.
Nil value means to remove the alignement."
  (save-excursion
    (let* ((lines (cl-sort (org-view--line-lengths beg end) #'> :key #'car))
           (longest (car lines))
           (lines (cdr lines))
           indent longest-pixel-width indent-pixel-width)
      (cond (align
             (goto-char (cadr longest))

             (cond ((eq align 'left)
                    (setq indent (buffer-substring-no-properties
                                  (line-beginning-position) (point)))
                    (dolist (line lines)
                      (goto-char (cadr line))
                      (put-text-property
                       (line-beginning-position) (point) 'invisible t)
                      (overlay-put
                       (make-overlay (line-beginning-position) (point))
                       'before-string indent)))

                   ((eq align 'right)
                    (dolist (line lines)
                      (setq
                       indent (org-view--region-pixel-width
                               (cadr longest)
                               (+ (cadr longest) (- (car longest) (car line)))))
                      (goto-char (cadr line))
                      (unless (= (char-before) ?\n)
                        (put-text-property
                         (line-beginning-position) (point) 'invisible t))
                      (unless (= (char-after (cddr line)) ?\n)
                        (put-text-property
                         (cddr line) (line-end-position) 'invisible t))
                      (overlay-put
                       (make-overlay (point) (1+ (point))) 'before-string
                       (propertize " " 'display `(space :width (,indent))))))

                   ((eq align 'middle)
                    (setq longest-pixel-width
                          (org-view--region-pixel-width
                           (cadr longest) (cddr longest))
                          indent-pixel-width
                          (org-view--region-pixel-width
                           (line-beginning-position) (point)))
                    (dolist (line lines)
                      (let* ((length (org-view--region-pixel-width
                                      (cadr line) (cddr line)))
                             (spacer (+ indent-pixel-width
                                        (/ (- longest-pixel-width length) 2))))
                        (goto-char (cadr line))
                        (unless (= (char-before) ?\n)
                          (put-text-property
                           (line-beginning-position) (point) 'invisible t))
                        (goto-char (line-beginning-position))
                        (overlay-put
                         (make-overlay (point) (1+ (point))) 'before-string
                         (propertize " " 'display `(space :width (,spacer)))))))))

             (t (remove-overlays beg end)
                (remove-text-properties beg end '(display t invisible t)))))))

(defun org-view--prettify-quote (beg end &optional prettify)
  (save-excursion
    (with-silent-modifications
      (if prettify
          (setq org-view--quote-ov (make-overlay beg end))
        (remove-overlays beg end))
      (goto-char beg)
      (if prettify
          (org-view--align-paragraph beg end org-view-quote-align))
      (while (< (point) end)
        (cond (prettify
               (overlay-put org-view--quote-ov 'face 'org-view-quote-face)
               (overlay-put (make-overlay (point) (1+ (point)))
                            'before-string org-view-paragraph-indent))
              (t (remove-overlays beg end)
                 (remove-text-properties beg end '(display t invisible t))))
        (forward-line)))))

(defun org-view--prettify-verse (beg end &optional prettify)
  "Prettify paragraphs marked as verses.

The author name should be separated by three dashes, '---' and put on a separate
  line after the paragraph content."
  (save-excursion
    (goto-char end)
    (when (search-backward "---" beg t)
      (setq end (line-beginning-position)))
    (cond (prettify
           (org-view--align-paragraph beg end org-view-verse-align)
           (overlay-put (make-overlay beg end) 'face 'org-view-verse-face))
          (t (remove-overlays beg end)
             (remove-text-properties beg end '(display t invisible t))))))

(defun org-view--prettify-center (beg end &optional pretty)
  "Render centered text.

Centering is done according to fill-column, or current window width if
fill-column is nil. This function is the only instance where org-view modifies
the content in original buffer in order to calculate pixel width of a full line
of text. The modification should not be visible, but there is no guarantee, it
really depends on Emacs rendering algorithms."
  (save-excursion
    (with-silent-modifications
      (let ((width (org-view--fill-column-pixel-width))
            left right)
        (goto-char beg)
        (while (< (point) end)
          (when (re-search-forward "^[ \t]+" (line-end-position) t)
            (put-text-property
             (match-beginning 0) (match-end 0) 'invisible t))
          (setq left (point))
          (goto-char (line-end-position))
          (when (re-search-backward "[ \t]*$" (line-beginning-position) t)
            (put-text-property
             (match-beginning 0) (match-end 0) 'invisible t))
          (setq right (point))
          (org-view--center-region left right width pretty)
          (forward-line))))))

(defun org-view--prettify-author (beg end &optional pretty)
  "Render author of a quote or verse in custom face.

Anything following three dashes to the end of line is recognized as author."
  (save-excursion
    (goto-char end)
    (when (search-backward "---" beg t)
      (put-text-property (point) (+ (point) 3) 'invisible pretty)
      (setq org-view--author-prefix-ov (make-overlay (point) (+ 3 (point)))
            org-view--author-ov (make-overlay (+ 3 (point)) (line-end-position)))
      (cond (pretty
             (overlay-put
              org-view--author-prefix-ov 'before-string org-view-author-prefix)
             (overlay-put org-view--author-ov 'face 'org-view-author-face))
            (t (remove-overlays (point) (line-end-position)))))))

(defun org-view--update-paragraphs (&optional pretty)
  "Prettify paragraphs and hide markers."
  (org-with-wide-buffer
   (save-excursion
     (with-silent-modifications
       (goto-char (point-min))
       (let (beg end type)
         (while (re-search-forward org-view-paragraph-beg-re nil t)
           (put-text-property
            (1- (line-beginning-position)) (line-end-position) 'invisible pretty)
           (forward-line)
           (setq beg (point))
           (when (re-search-forward org-view-paragraph-end-re nil t)
             (put-text-property
              (1- (line-beginning-position)) (line-end-position) 'invisible pretty)
             (setq end (1- (line-beginning-position)))
             (save-excursion
                 (re-search-backward
                  "\\(QUOTE\\|VERSE\\|CENTER\\)" (line-beginning-position) t)
                 (setq type (match-string 0))
                 (cond
                  ((equal "QUOTE" type)
                   (org-view--prettify-quote beg end pretty))
                  ((equal "VERSE" type)
                   (org-view--prettify-verse beg end pretty))
                  ((equal "CENTER" type)
                   (org-view--prettify-center beg end pretty)))
                 (org-view--prettify-author beg end pretty)))))))))

;;; User commands

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
(define-minor-mode org-view-hide-keywords-mode
  "Hide/show leading stars in org-headings."
  :global nil :lighter " org-hkm"
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (org-view--update-keywords org-view-hide-stars-mode))

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

;;;###autoload
(define-minor-mode org-view-pretty-paragraphs-mode
  "Prettify credentials in org-buffers."
  :global nil :lighter " org-ppm"
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (org-view--update-paragraphs org-view-pretty-paragraphs-mode))

(defun org-view-quit ()
  (interactive)
  (org-view-mode -1)
  (message "org-view mode disabled in current buffer"))

(defvar org-view-mode-map
  (let ((map (make-sparse-keymap)))
        (define-key map (kbd "c") #'org-view-quit)
        (define-key map (kbd "C") #'org-view-quit)
        (define-key map (kbd "e") #'org-view-quit)
        (define-key map (kbd "E") #'org-view-quit)
        (define-key map (kbd "q") #'org-view-quit)
        (define-key map (kbd "Q") #'org-view-quit)
    map)
    "Keymap for ‘ORG-view-mode’")

;;;###autoload
(define-minor-mode org-view-mode
  "Hide/show babel source code blocks on demand."
  :global nil :lighter " org-view" :keymap org-view-mode-map
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (cond (org-view-mode
         (org-font-lock-ensure 1 (point-max))
         (org-view-hide-tags-mode org-view-mode)
         (org-view-hide-stars-mode org-view-mode)
         (org-view-hide-keywords-mode org-view-mode)
         (org-view-hide-properties-mode org-view-mode)
         (org-view-pretty-paragraphs-mode org-view-mode)
         (org-view-pretty-credentials-mode org-view-mode)
         (when org-view-diminish-mode
           (dolist (mode '(org-view-hide-tags-mode
                           org-view-hide-stars-mode
                           org-view-hide-keywords-mode
                           org-view-hide-properties-mode
                           org-view-pretty-paragraphs-mode
                           org-view-pretty-credentials-mode))
             (let ((mode-str (cdr (assq mode minor-mode-alist))))
               (setcar mode-str ""))))
         (view-mode +1))
        (t (view-mode -1)
           (org-view-hide-tags-mode -1)
           (org-view-hide-stars-mode -1)
           (org-view-hide-keywords-mode -1)
           (org-view-hide-properties-mode -1)
           (org-view-pretty-paragraphs-mode -1)
           (org-view-pretty-credentials-mode -1))))

(provide 'org-view-mode)

;;; org-view-mode.el ends here
