;;; org-view-font.el --- Font switcher module for org-view-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Trevor Richards

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

;;; Commentary:

;; This file is a library that contains the necessary code for `org-view-mode'
;; to be able to toggle a "reading font" off and on.


;;; Code:
(require 'face-remap)
(require 'cl-seq)

(defgroup 'org-view-font nil
  "Special font customizations for `org-view-mode'."
  :group 'org-view)

;;;###autoload
(defcustom org-view-font-remaps
  '((default . (:family "Serif" :height 1.25))
    (org-document-title . (:height 1.2))
    (org-level-1 . (:height 1.4 :weight normal))
    (org-level-2 . (:height 1.3 :weight normal))
    (org-level-3 . (:height 1.2 :weight normal))
    (org-level-4 . (:height 1.1 :slant italic))
    (org-level-5 . (:height 1.0 :weight semibold))
    (org-level-6 . (:height 1.0 :weight semibold))
    (org-level-7 . (:height 1.0 :weight semibold))
    (org-level-8 . (:height 1.0 :weight semibold))
    (org-link . (:underline t :weight normal)))
  "Faces to remap, with attributes to remap."
  :type '(alist :key-type face
                :value-type (plist :key-type symbol
                                   :value-type sexp))
  :group 'org-view-font)

;;;###autoload
(defcustom org-view-font-no-remap
  '(org-block
    org-block-begin-line
    org-block-end-line
    org-document-info-keyword
    org-code
    org-latex-and-related
    org-checkbox
    org-meta-line
    org-table
    org-drawer
    org-special-keyword
    org-property-value
    org-verbatim)
  "Faces to avoid remapping."
  :type '(repeat face)
  :group 'org-view-font)

;;;###autoload
(defcustom org-view-font-enable nil
  "Enable the custom reading font settings for `org-view-mode'."
  :type 'boolean
  :group 'org-view-font)



(defvar org-view-font--cookies '()
  "A record of remapped faces used by `org-view-mode'.")

(defun org-view-font--apply-remap (setting)
  "Invoke `face-remap-add-relative' with a given face `SETTING'."
  (face-remap-add-relative (car setting) (cdr setting)))

(defun org-view-font--remap-faces (remaps)
  "Apply `REMAPS' and store a record of them in the cookie storage."
  (dolist (cookie (cl-mapcar 'org-view-font--apply-remap (reverse remaps)))
    (add-to-list 'org-view-font--cookies cookie)))

(defun org-view-font--to-face-props (face)
  "Reduce a `FACE' into a property list of its face properties."
  (cl-reduce
   `(lambda (props name)
      (plist-put props (car name) (face-attribute ',face (car name))))
   face-attribute-name-alist
   :initial-value '()))

(defun org-view-font--default-setting (face)
  "Create a default setting for a `FACE'.  Preserves pre-existing settings."
  (let ((props (org-view-font--to-face-props face)))
    (cons face (plist-put props :family (face-attribute 'default :family)))))

(defun org-view-font--clear-remaps (cookies)
  "Clear the remaps in a `COOKIES' list."
  (dolist (c cookies)
    (face-remap-remove-relative c)))



;;;###autoload
(defun org-view-font-enable-font ()
  "Enable the org-view-font feature."
  (org-view-font--remap-faces org-view-font-remaps)
  (org-view-font--remap-faces (cl-mapcar 'org-view-font--default-setting
                                         org-view-font-no-remap)))

;;;###autoload
(defun org-view-font-disable-font ()
  "Disable the org-view-font feature."
  (org-view-font--clear-remaps org-view-font--cookies)
  (setq org-view-font--cookies '()))

(provide 'org-view-font)

;;; org-view-font.el ends here
