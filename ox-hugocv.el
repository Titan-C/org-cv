;;; ox-hugocv.el --- LaTeX hugocv Back-End for Org Export Engine -*- lexical-binding: t; -*-

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
;; This library implements a LaTeX hugocv back-end, derived from the
;; LaTeX one.

;;; Code:
(require 'ox-hugo)

;;; User-Configurable Variables

(defgroup org-export-hugocv nil
  "Options for exporting Org mode files to Hugo-compatible Markdown"
  :tag "Org Export Hugo CV"
  :group 'org-export
  :version "25.3")

;;; Define Back-End
(org-export-define-derived-backend 'hugocv 'hugo
  :options-alist
  '(
    (:mobile "MOBILE" nil nil parse)
    (:homepage "HOMEPAGE" nil nil parse)
    (:address "ADDRESS" nil nil newline)
    (:photo "PHOTO" nil nil parse)
    (:gitlab "GITLAB" nil nil parse)
    (:github "GITHUB" nil nil parse)
    (:linkedin "LINKEDIN" nil nil parse)
    (:with-email nil "email" t t)
    )
  :translate-alist '((headline . org-hugocv-headline)
                     (inner-template . org-hugocv-inner-template)))

(defun org-hugocv-timestamp-to-shortdate (date_str)
  "Format orgmode timestamp DATE_STR  into a short form date.

e.g. <2002-08-12 Mon> => Aug 2012"
  (let* ((abbreviate 't)
	 (dte (org-parse-time-string date_str))
	 (month (nth 4 dte))
	 (year (nth 5 dte)));;'(02 07 2015)))
    (concat (calendar-month-name month abbreviate)
	    " "
(number-to-string year))))

(defun org-hugocv--format-cventry (headline contents info)
  "Format HEADLINE as as cventry.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let ((from-date (org-element-property :FROM headline))
        (to-date (org-element-property :TO headline))
        (loffset (string-to-number (plist-get info :hugo-level-offset))) ;"" -> 0, "0" -> 0, "1" -> 1, ..
        (level (org-export-get-relative-level headline info))
        (title (org-export-data (org-element-property :title headline) info))
        (employer (org-element-property :EMPLOYER headline))
        (location (or (org-element-property :LOCATION headline) "")))
    (format "\n%s

<ul class=\"cventry\">
    <li class=\"fa fa-building\"> %s</li>
    <li class=\"fa fa-map-marker\"> %s</li>
    <li class=\"fa fa-calendar\"> %s</li>
</ul>

%s
"
            (concat (make-string (+ loffset level) ?#) " " title)
            employer
            location
            (concat (org-hugocv-timestamp-to-shortdate from-date)
                    " -- "
                    (org-hugocv-timestamp-to-shortdate to-date))
            contents)))


;;;; Headline
(defun org-hugocv-headline (headline contents info)
  "Transcode HEADLINE element into hugocv code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((environment (let ((env (org-element-property :CV_ENV headline)))
			 (or (org-string-nw-p env) "block"))))
      (cond
       ;; is a cv entry
       ((equal environment "cventry")
        (org-hugocv--format-cventry headline contents info))
       ((org-export-with-backend 'hugo headline contents info))))))

(defun org-hugocv-inner-template (contents info)
  "Return body of document after converting it to Hugo-compatible Markdown.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
    (concat "<ul id=\"cvcontacts\">\n"
            ;; email
            (let ((email (and (plist-get info :with-email)
                              (org-export-data (plist-get info :email) info))))
                 (when email (format "<li class=\"fa fa-envelope\"><a href=\"mailto:%s\"> %s</a></li>\n" email email)))
            ;; homepage
            (let ((homepage (org-export-data (plist-get info :homepage) info)))
                 (when homepage (format "<li class=\"fa fa-globe\"><a href=\"https://%s\"> %s</a></li>\n" homepage homepage)))
            ;; social media
            (mapconcat (lambda (social-network)

                         (let ((command (org-export-data (plist-get info
                                                                    (car social-network))

                                                         info)))

                           (and command (format "<li class=\"fa fa-%s\"><a href=\"https://%s/%s\"> %s</a></li>\n"
                                                (nth 1 social-network)
                                                (nth 2 social-network)
                                                command
                                                command))))

                        '((:github "github" "www.github.com")
                        (:gitlab "gitlab" "www.gitlab.com")
                        (:linkedin "linkedin" "www.linkedin.com/in"))
                        "")
            "</ul>\n\n"
            (org-hugo-inner-template contents info)))


(provide 'ox-hugocv)
;;; ox-hugocv ends here
