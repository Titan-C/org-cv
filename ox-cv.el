(require 'cl-lib)
(require 'ox-latex)

;; Install a default set-up for Beamer export.
(unless (assoc "orgcv" org-latex-classes)
  (add-to-list 'org-latex-classes
	       '("orgcv"
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
(org-export-define-derived-backend 'orgcv 'latex
  :options-alist
  '(
    (:cvstyle "CVSTYLE" nil "classic" t)
    (:cvcolor "CVCOLOR" nil "blue" t)
    (:mobile "MOBILE" nil nil parse)
    (:homepage "HOMEPAGE" nil nil parse)
    (:address "ADDRESS" nil nil newline)
    (:gitlab "GITLAB" nil nil parse)
    (:github "GITHUB" nil nil parse)
    (:linkedin "LINKEDIN" nil nil parse)
    )
  :translate-alist '((template . org-cv-template)
		     (headline . org-cv-headline)))

(defun org-cv--add-latex-newlines (string)
  "Replace regular newlines with LaTeX newlines (i.e. `\\\\')"
  (let ((str (org-trim string)))
    (when (org-string-nw-p str)
      (concat (replace-regexp-in-string "\n" "\\\\\\\\\n" str) "\\\\"))))

;;;; Template
;;
;; Template used is similar to the one used in `latex' back-end,
;; excepted for the table of contents and Beamer themes.

(defun org-cv-template (contents info)
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
     ;;(org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; cvstyle
     (let ((cvstyle (org-export-data (plist-get info :cvstyle) info)))
       (when cvstyle (format "\\moderncvstyle{%s}\n" cvstyle)))
     ;; cvcolor
     (let ((cvcolor (org-export-data (plist-get info :cvcolor) info)))
       (when cvcolor (format "\\moderncvcolor{%s}\n" cvcolor)))
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info))))))
       (format "\\name{%s}{}\n" author))
     ;; email
     (let ((email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (when email (format "\\email{%s}\n" email)))
     ;; phone
     (let ((mobile (org-export-data (plist-get info :mobile) info)))
       (when mobile (format "\\phone[mobile]{%s}\n" mobile)))
     ;; homepage
     (let ((homepage (org-export-data (plist-get info :homepage) info)))
       (when homepage (format "\\homepage{%s}\n" homepage)))
     ;; address
     (let ((address (org-export-data (plist-get info :address) info)))
       (when address (format "\\address{%s}\n" (org-cv--add-latex-newlines address))))
     (mapconcat (lambda (social-network)
		  (let ((command (org-export-data (plist-get info
							     (car social-network))
						  info)))
		    (and command (format "\\social[%s]{%s}\n"
					 (nth 1 social-network)
					 command))))
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


(defun org-cv-timestamp-to-shortdate (date_str)
  "e.g. <2002-08-12 Mon> => Aug 2012"
  (let* ((abbreviate 't)
	 (dte (org-parse-time-string date_str))
	 (month (nth 4 dte))
	 (year (nth 5 dte)));;'(02 07 2015)))
    (concat (calendar-month-name month abbreviate)
	    " "
(number-to-string year))))

(defun org-cv-cventry (headline contents info)
  (let ((from-date (org-element-property :FROM headline))
        (to-date (org-element-property :TO headline))
        (location (org-element-property :LOCATION headline))
        (title (org-element-property :title headline))
        (note (org-element-property :NOTE headline))
        (employer (org-element-property :EMPLOYER headline)))
    (format "\\cventry{%s}{%s}{%s}{%s}{%s}{%s}\n"
            (concat (org-cv-timestamp-to-shortdate from-date)
                    " -- "
                    (org-cv-timestamp-to-shortdate to-date))
            title employer location note contents)))


;;;; Headline
(defun org-cv-headline (headline contents info)
  "Transcode HEADLINE element into Beamer code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((level (org-export-get-relative-level headline info))
	  (frame-level (org-beamer--frame-level headline info))
          (tags (org-export-get-tags headline info))

	  (environment (let ((env (org-element-property :CV_ENV headline)))
			 (or (org-string-nw-p env) "block"))))
      (cond
       ;; is a cv entry
       ((equal environment "cventry")
        (org-cv-cventry headline contents info))
       (t (concat (format "%s %s %s %s %s\n" level tags frame-level environment
                          contents)
                ))))))
