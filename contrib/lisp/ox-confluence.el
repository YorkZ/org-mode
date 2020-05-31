;;; ox-confluence --- Confluence Wiki Back-End for Org Export Engine

;; Copyright (C) 2012, 2020 Sébastien Delafond

;; Author: Sébastien Delafond <sdelafond@gmail.com>
;; Keywords: outlines, confluence, wiki

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ox-confluence.el lets you convert Org files to confluence files
;; using the ox.el export engine. Check out
;; https://confluence.atlassian.com/doc/confluence-wiki-markup-251003035.html
;; for a decription of the Confluence Wiki Markup syntax.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-confluence)
;;
;; Export Org files to confluence:
;; M-x org-confluence-export-as-confluence RET
;;
;;; Code:

(require 'ox)
(require 'ox-ascii)

;; Define the backend itself
(org-export-define-derived-backend 'confluence 'ascii
  :translate-alist '((bold . org-confluence-bold)
                     (code . org-confluence-code)
                     (example-block . org-confluence-example-block)
                     (fixed-width . org-confluence-fixed-width)
                     (footnote-definition . org-confluence-empty)
                     (footnote-reference . org-confluence-footnote-reference)
                     (headline . org-confluence-headline)
                     (italic . org-confluence-italic)
                     (item . org-confluence-item)
                     (link . org-confluence-link)
                     (plain-list . org-confluence-plain-list)
                     (paragraph . org-confluence-paragraph)
                     (property-drawer . org-confluence-property-drawer)
                     (quote-block . org-confluence-quote-block)
                     (section . org-confluence-section)
                     (src-block . org-confluence-src-block)
                     (strike-through . org-confluence-strike-through)
                     (table . org-confluence-table)
                     (table-cell . org-confluence-table-cell)
                     (table-row . org-confluence-table-row)
                     (template . org-confluence-template)
                     (timestamp . org-confluence-timestamp)
                     (underline . org-confluence-underline)
                     (verbatim . org-confluence-verbatim)
                     (inner-template . org-confluence-inner-template)
                     (verbatim . org-confluence-verbatim))
  :menu-entry
  '(?f "Export to Confluence"
       ((?f "As Confluence buffer" org-confluence-export-as-confluence))))

(defcustom org-confluence-lang-alist
  '(("sh" . "bash"))
  "Map from org-babel language name to confluence wiki language name"
  :type '(alist :key-type string :value-type string))

(defun org-confluence-escape-chars (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward "~" nil t)
      (replace-match "\\\\~"))
    (buffer-string)))

;; All the functions we use
(defun org-confluence-bold (bold contents info)
  (format "*%s*" contents))

(defun org-confluence-empty (empty contents info)
  "")

(defun org-confluence-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((footnote-number (org-export-get-footnote-number footnote-reference
							 info)))
    (format " ^[%d|#fn_%d]^ " footnote-number footnote-number)))

(defun org-confluence-inner-template (contents info)
  (concat
   ;; 1. Document's body.
   contents
   ;; 2. Footnote definitions.
   (let ((definitions (org-export-collect-footnote-definitions info)))
     (when definitions
       (concat
	"\n\n"
	"h1. Footnotes\n"
	(mapconcat
	 (lambda (ref)
	   (let ((footnote-number (car ref)))
	     (format "  %d. {anchor:fn_%d} %s" footnote-number footnote-number
		     (org-export-data (nth 2 ref) info))))
	 definitions "\n"))))))

(defun org-confluence-example-block (example-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let ((content (org-export-format-code-default example-block info)))
    (org-confluence--block "none" "Confluence" content)))

(defun org-confluence-italic (italic contents info)
  (format "_%s_" contents))

(defun org-confluence-item (item contents info)
  (let ((list-type (org-element-property :type (org-export-get-parent item))))
    (concat
     (make-string (1+ (org-confluence--li-depth item))
                  (if (eq list-type 'ordered) ?\# ?\-))
     " "
     (pcase (org-element-property :checkbox item)
       (`on "*{{(X)}}* ")
       (`off "*{{( )}}* ")
       (`trans "*{{(\\-)}}* "))
     (when (eq list-type 'descriptive)
       (concat "*"
	       (org-export-data (org-element-property :tag item) info)
	       "* - "))
     (org-trim contents))))

(defun org-confluence-fixed-width (fixed-width contents info)
  (org-confluence--block
   "none"
   "Confluence"
   (org-trim (org-element-property :value fixed-width))))

(defun org-confluence-verbatim (verbatim contents info)
  (format "\{\{%s\}\}" (org-element-property :value verbatim)))

(defun org-confluence-code (code contents info)
  (format "\{\{%s\}\}" (org-element-property :value code)))

(defun org-confluence-headline (headline contents info)
  (let* ((low-level-rank (org-export-low-level-p headline info))
	 (text (org-export-data (org-element-property :title headline)
				info))
	 (todo (org-export-data (org-element-property :todo-keyword headline)
				info))
	 (level (org-export-get-relative-level headline info))
	 (anchor (format "{anchor:%s}"
			 (org-export-get-headline-number headline info)))
	 (todo-text (if (or (not (plist-get info :with-todo-keywords))
			    (string= todo ""))
			""
		      (format "*{{%s}}* " todo))))
    (format "h%s. %s %s%s\n%s" level anchor todo-text text
            (if (org-string-nw-p contents) contents ""))))

(defun org-confluence-link (link desc info)
  (if (string= "radio" (org-element-property :type link))
      desc
    (let* ((raw-link (org-element-property :raw-link link))
           (type (org-element-property :type link))
           (path (org-element-property :path link))
           (link-bracket (if (org-export-inline-image-p link)
                             '("!" . "!")
                           '("[" . "]"))))
      (cond
       ((and (equal type "file") path)
        (setq raw-link (file-name-nondirectory path)))
       ((member type '("custom-id" "fuzzy" "id"))
        (let ((destination (if (string= type "fuzzy")
                               (org-export-resolve-fuzzy-link link info)
                             (org-export-resolve-id-link link info))))
          (cl-case (org-element-type destination)
            ;; TODO: Id link points to an external file.
            (plain-text)
            ;; TODO: Fuzzy link points nowhere.
            ((nil))
            ;; link points to a headline.
            (headline
             (setq raw-link
                   (format "#%s"
                           (org-export-get-headline-number destination info))))
            ;; Fuzzy link points to a target.
            (otherwise
             (setq raw-link
                   (format "#%s"
                           (org-export-get-reference destination info))))))))
      (concat (car link-bracket)
              (when (org-string-nw-p desc) (format "%s|" desc))
              raw-link
              (cdr link-bracket)))))

(defun org-confluence-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to confluence.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (if (org-element-property :name plain-list)
      ;; Add an anchor for the list that have a "#+NAME:" definition
      ;; to allow linking to it.
      (let* ((first-item (car (org-element-contents plain-list)))
	     (content-beg (- (org-element-property :contents-begin first-item)
			     (org-element-property :begin first-item))))
	(with-temp-buffer
	  (insert contents)
	  (goto-char (point-min))
	  (forward-char content-beg)
	  (insert (format "{anchor:%s} "
			  (org-export-get-reference plain-list info)))
	  (buffer-string)))
    contents))

(defun org-confluence-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element for Confluence.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (with-temp-buffer
    (insert (org-confluence-escape-chars contents))
    (let ((fill-column most-positive-fixnum))
      (fill-region (point-min) (point-max))
      (buffer-string))))

(defun org-confluence-property-drawer (property-drawer contents info)
  (and (org-string-nw-p contents)
       (format "\{\{%s\}\}" contents)))

(defun org-confluence-quote-block (quote-block contents info)
  (format "{quote}\n%s{quote}" contents))

(defun org-confluence-section (section contents info)
  contents)

(defun org-confluence-src-block (src-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let* ((lang (org-element-property :language src-block))
         (language (or (cdr (assoc lang org-confluence-lang-alist)) lang))
         (content (org-export-format-code-default src-block info)))
    (org-confluence--block language "Emacs" content)))

(defun org-confluence-strike-through (strike-through contents info)
  (format "-%s-" contents))

(defun org-confluence-table (table contents info)
  contents)

(defun org-confluence-table-row  (table-row contents info)
  (concat
   (if (org-string-nw-p contents) (format "|%s" contents)
     "")
   (when (org-export-table-row-ends-header-p table-row info)
     "|")))

(defun org-confluence-table-cell  (table-cell contents info)
  (let ((table-row (org-export-get-parent table-cell)))
    (concat (and (org-export-table-row-starts-header-p table-row info) "|")
	    (if (= (length contents) 0) " " contents)
	    "|")))

(defun org-confluence-template (contents info)
  (let ((depth (plist-get info :with-toc)))
    (concat (when depth "\{toc\}\n\n") contents)))

(defun org-confluence-timestamp (timestamp _contents _info)
  "Transcode a TIMESTAMP object from Org to Confluence.
CONTENTS and INFO are ignored."
  (let ((translated (org-trim (org-timestamp-translate timestamp))))
    (if (string-prefix-p "[" translated)
        (concat "(" (substring translated 1 -1) ")")
      translated)))

(defun org-confluence-underline (underline contents info)
  (format "+%s+" contents))

(defun org-confluence--block (language theme contents)
  (concat "\{code:theme=" theme
          (when language (format "|language=%s" language))
          "}\n"
          contents
          "\{code\}\n"))

(defun org-confluence--li-depth (item)
  "Return depth of a list item; -1 means not a list item"
  ;; FIXME check whether it's worth it to cache depth
  ;;       (it gets recalculated quite a few times while
  ;;       traversing a list)
  (let ((depth -1)
        (tag))
    (while (and item
                (setq tag (car item))
                (or (eq tag 'item) ; list items interleave with plain-list
                    (eq tag 'plain-list)))
      (when (eq tag 'item)
        (cl-incf depth))
      (setq item (org-export-get-parent item)))
    depth))

;; main interactive entrypoint
(defun org-confluence-export-as-confluence
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CONFLUENCE Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'confluence "*org CONFLUENCE Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(provide 'ox-confluence)
