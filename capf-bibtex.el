;;; capf-bibtex.el --- completion at point for bibtex --- *- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (parsebib "3.0") (org "9.5"))
;; Homepage: https://github.com/mclear-tools/capf-bibtex
;; Keywords: bibtex, convenience


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Completion at point functions for latex, markdown, and org.

;;; Code:
;;;; Requirements
(require 'cl-lib)
(require 'parsebib)

;; Declare org-related vars & functions
(defvar org-element-citation-key-re)
(defvar org-element-citation-prefix-re)
(declare-function org-element-at-point "ext:org-element")
(declare-function org-element-property "ext:org-element")
(declare-function org-element-type "ext:org-element")
(declare-function org-element-context "ext:org-element")

;;;; Variables
(defgroup capf-bibtex nil
  "Company backend for BibTeX bibliography keys."
  :group 'capf-bibtex)

(defcustom capf-bibtex-bibliography nil
  "List of bibtex files used for gathering completions."
  :group 'capf-bibtex
  :type '(choice (file :must-match t)
                 (repeat (file :must-match t))))

;;;; Functions

(defun capf-bibtex-candidates ()
  "Parse .bib file for candidates and return list of keys."
  (let ((bib-paths (if (listp capf-bibtex-bibliography)
                       capf-bibtex-bibliography
                     (list capf-bibtex-bibliography))))
    (with-temp-buffer
      (mapc #'insert-file-contents bib-paths)
      (mapcar (function (lambda (x) (capf-bibtex-build-candidate x)))
              (capf-bibtex-parse-bibliography)))))

(defun capf-bibtex-build-candidate (bibentry)
  "Build a string---the bibtex key---with author and title properties attached.
  This is drawn from BIBENTRY, an element in the list produced
  by `company-bibtex-parse-bibliography'."
  (let ((bibkey (cdr (assoc "=key=" bibentry)))
	    (author (cdr (assoc "author" bibentry)))
        (editor (cdr (assoc "editor" bibentry)))
	    (title  (cdr (assoc "title" bibentry)))
        (shorttitle  (cdr (assoc "shorttitle" bibentry))))
    (propertize bibkey :author (or author editor) :title (or shorttitle title))))

(defun capf-bibtex-parse-bibliography ()
  "Parse BibTeX entries listed in the current buffer.

  Return a list of entry keys in the order in which the entries
  appeared in the BibTeX files."
  (goto-char (point-min))
  (cl-loop
   for entry-type = (parsebib-find-next-item)
   while entry-type
   unless (member-ignore-case entry-type '("preamble" "string" "comment"))
   collect (mapcar (lambda (it)
                     (cons (downcase (car it)) (cdr it)))
                   (parsebib-read-entry entry-type))))

(defun capf-bibtex-get-annotations (candidate)
  "Get data from CANDIDATE for annotations."
  (let ((prefix-length 0))
    (concat
     (replace-regexp-in-string "{\\|}" ""
			                   (format "   %s "
				                       (get-text-property prefix-length :title candidate)))
     (replace-regexp-in-string "{\\|}" ""
			                   (format "   %s"
				                       (get-text-property prefix-length :author candidate))))))

;;;###autoload
(defun capf-bibtex (&optional arg &rest ignored)
  "Complete citation key at point for org, markdown, or latex."
  (let ((capf-bibtex-latex-regex "\\(?:cite\\(?:\\(?:[pt]\\*\\|[pt]\\)?{\\)\\)\\([[:alnum:]_-]*,\\)*\\([[:alnum:]_-]*\\)")
        (capf-bibtex-markdown-regexp (concat "-?@"                         ; @ preceded by optional -
                                             "\\(?:"
                                             "{\\(?1:.*?\\)}"              ; brace-delimited key
                                             "\\|"
                                             "\\(?1:[[:alnum:]_][[:alnum:]]*\\(?:[:.#$%&+?<>~/-][[:alnum:]]+\\)*\\)"
                                             "\\)")))
    (when
        (cond
         ;; org-mode
         ((and (derived-mode-p 'org-mode)
               (let ((element (org-element-at-point)))
                 (or (eq 'citation (org-element-type (org-element-context element)))
                     (and (or (eq ?@ (char-before))
                              (looking-back org-element-citation-key-re
                                            (line-beginning-position)))
                          (let ((origin (point)))
                            (save-excursion
                              (and (re-search-backward org-element-citation-prefix-re
                                                       (org-element-property
                                                        :begin element)
                                                       t)
                                   (not (search-forward "]" origin t))))))))))
         ;; markdown-mode
         ((and (derived-mode-p 'markdown-mode)
               (or (eq ?@ (char-before))
                   (looking-back capf-bibtex-markdown-regexp
                                 (line-beginning-position)))))

         ;; latex-mode
         ((derived-mode-p 'latex-mode)
          (looking-back capf-bibtex-latex-regex 2)))

      (let* ((candidates
              (capf-bibtex-candidates))
             (begin (save-excursion (backward-word) (point)))
             (end (point)))

        (list begin end candidates
              :annotation-function
              (lambda (str)
                (capf-bibtex-get-annotations str))
              :exit-function
              (lambda (str _status)
                ;; take completion str and replace with key
                (insert)))))))

;;;; Define Minor Mode
(define-minor-mode capf-bibtex-mode
  "Create a global minor mode for `capf-bibtex'.
This adds hooks and the `capf-bibtex' function to the relevant modes."
  :lighter ""
  :global t
  (cond (capf-bibtex-mode
         ;; add to completion framework
         (add-hook 'completion-at-point-functions #'capf-bibtex -90 t)
         (add-to-list 'completion-at-point-functions #'capf-bibtex))
        (t
         (remove-hook 'completion-at-point-functions #'capf-bibtex)
         (remove #'capf-bibtex completion-at-point-functions))))


(provide 'capf-bibtex)
;;; capf-bibtex.el ends here
