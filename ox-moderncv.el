;;; ox-moderncv.el --- LaTeX moderncv Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Oscar Najera <hi AT oscarnajera.com DOT com>
;; Keywords: org, wp, tex

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library implements a LaTeX moderncv back-end, derived from the
;; LaTeX one.

;;; Code:
(require 'cl-lib)
(require 'ox-latex)
(require 'org-cv-utils)

;; Install a default set-up for moderncv export.
(unless (assoc "moderncv" org-latex-classes)
  (add-to-list 'org-latex-classes
               '("moderncv"
                 "\\documentclass{moderncv}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


;;; User-Configurable Variables

(defgroup org-export-cv nil
  "Options specific for using the moderncv class in LaTeX export."
  :tag "Org moderncv"
  :group 'org-export
  :version "25.3")

;;; Define Back-End
(org-export-define-derived-backend 'moderncv 'latex
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "moderncv" t)
    (:cvstyle "CVSTYLE" nil "classic" t)
    (:cvcolor "CVCOLOR" nil nil t)
    (:mobile "MOBILE" nil nil parse)
    (:homepage "HOMEPAGE" nil nil parse)
    (:address "ADDRESS" nil nil newline)
    (:photo "PHOTO" nil nil parse)
    (:gitlab "GITLAB" nil nil parse)
    (:github "GITHUB" nil nil parse)
    (:linkedin "LINKEDIN" nil nil parse)
    (:with-email nil "email" t t)
    )
  :translate-alist '((template . org-moderncv-template)
                     (headline . org-moderncv-headline)))


;;;; Template
;;
;; Template used is similar to the one used in `latex' back-end,
;; excepted for the table of contents and moderncv themes.

(defun org-moderncv-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
        (spec (org-latex--format-spec info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
          (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; cvstyle
     (let ((cvstyle (org-export-data (plist-get info :cvstyle) info)))
       (when cvstyle (format "\\moderncvstyle{%s}\n" cvstyle)))
     ;; cvcolor
     (let ((cvcolor (org-export-data (plist-get info :cvcolor) info)))
       (when (not (string-empty-p cvcolor)) (format "\\moderncvcolor{%s}\n" cvcolor)))
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
         (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
                        (let ((auth (plist-get info :author)))
                          (and auth (org-export-data auth info))))))
       (format "\\name{%s}{}\n" author))
     ;; photo
     (let ((photo (org-export-data (plist-get info :photo) info)))
       (when (org-string-nw-p photo)
         (format "\\photo{%s}\n" photo)))
     ;; email
     (let ((email (and (plist-get info :with-email)
                       (org-export-data (plist-get info :email) info))))
       (when (org-string-nw-p email)
         (format "\\email{%s}\n" email)))
     ;; phone
     (let ((mobile (org-export-data (plist-get info :mobile) info)))
       (when (org-string-nw-p mobile)
         (format "\\phone[mobile]{%s}\n" mobile)))
     ;; homepage
     (let ((homepage (org-export-data (plist-get info :homepage) info)))
       (when (org-string-nw-p homepage)
         (format "\\homepage{%s}\n" homepage)))
     ;; address
     (let ((address (org-export-data (plist-get info :address) info)))
       (when (org-string-nw-p address)
         (format "\\address%s\n" (mapconcat (lambda (line)
                                              (format "{%s}" line))
                                            (split-string address "\n") ""))))
     (mapconcat (lambda (social-network)
                  (let ((network (org-export-data
                                  (plist-get info (car social-network)) info)))
                    (when (org-string-nw-p network)
                      (format "\\social[%s]{%s}\n"
                              (nth 1 social-network) network))))
                '((:github "github")
                  (:gitlab "gitlab")
                  (:linkedin "linkedin"))
                "")

     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))

     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
            (formatted-subtitle
             (when subtitle
               (format (plist-get info :latex-subtitle-format)
                       (org-export-data subtitle info))))
            (separate (plist-get info :latex-subtitle-separate)))
       (concat
        (format "\\title{%s%s}\n" title
                (if separate "" (or formatted-subtitle "")))
        (when (and separate subtitle)
          (concat formatted-subtitle "\n"))))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
        (cond ((not (plist-get info :with-title)) nil)
              ((string= "" title) nil)
              ((not (stringp command)) nil)
              ((string-match "\\(?:[^%]\\|^\\)%s" command)
               (format command title))
              (t command))))
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
          (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))


(defun org-moderncv--format-cventry (headline contents info)
  "Format HEADLINE as as cventry.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let* ((title (org-export-data (org-element-property :title headline) info))
         (from-date (or (org-element-property :FROM headline) (error "No FROM property provided for cventry %s" title)))
         (to-date (org-element-property :TO headline))
         (employer (org-element-property :EMPLOYER headline))
         (location (or (org-element-property :LOCATION headline) ""))
         (note (or (org-element-property :NOTE headline) "")))
    (format "\\cventry{\\textbf{%s}}{%s}{%s}{%s}{%s}{%s}\n"
            (org-cv-utils--format-time-window from-date to-date)
            title employer location note contents)))


;;;; Headline
(defun org-moderncv-headline (headline contents info)
  "Transcode HEADLINE element into moderncv code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((environment (let ((env (org-element-property :CV_ENV headline)))
                         (or (org-string-nw-p env) "block"))))
      (cond
       ;; is a cv entry
       ((equal environment "cventry")
        (org-moderncv--format-cventry headline contents info))
       ((org-export-with-backend 'latex headline contents info))))))

(provide 'ox-moderncv)
;;; ox-moderncv ends here
