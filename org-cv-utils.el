;;; org-cv-utils.el --- Common utility functions for CV exporters -*- lexical-binding: t; -*-

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
;; This library implements some utility functions

;;; Code:
(require 'org)

(defun org-cv-utils-org-timestamp-to-shortdate (date_str)
"Format orgmode timestamp DATE_STR  into a short form date.
Other strings are just returned unmodified

e.g. <2002-08-12 Mon> => Aug 2012
today => today"
  (if (string-match (org-re-timestamp 'active) date_str)
      (let* ((abbreviate 't)
             (dte (org-parse-time-string date_str))
             (month (nth 4 dte))
             (year (nth 5 dte))) ;;'(02 07 2015)))
        (concat
         (calendar-month-name month abbreviate) " " (number-to-string year)))
    date_str))

(defun org-cv-utils--format-time-window (from-date to-date)
"Join date strings in a time window.
FROM-DATE -- TO-DATE
in case TO-DATE is nil return Present"
  (concat
   (org-cv-utils-org-timestamp-to-shortdate from-date)
   " -- "
   (if (not to-date) "Present"
     (org-cv-utils-org-timestamp-to-shortdate to-date))))

(provide 'org-cv-utils)
;;; org-cv-utils ends here
