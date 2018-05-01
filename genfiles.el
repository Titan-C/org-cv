;; remember the current directory, find-file changes it
(defvar cwd default-directory)
(defvar workdir "/tmp/org-cv-exports/")
(find-file "/tmp/install-org.el")
(eval-buffer)

(add-to-list 'load-path cwd)

(require 'ox-moderncv)

(require 'ox-altacv)

(let ((readme (concat cwd "readme.org")))
  (find-file readme)
  (make-directory workdir t)
  (cd workdir)
  (org-babel-tangle))

(copy-file (concat cwd "doc/smile.png") workdir)

(defun export-latex (backend file)
  (let ((workfile (concat workdir file))
        (outfile (concat workdir file ".tex")))
    (message (format "%s exists: %s" workfile (file-exists-p workfile)))
    (find-file workfile)
    (org-mode)
    (org-export-to-file backend outfile)
    (shell-command (format "pdflatex %s" outfile) "*Messages*" "*Messages*")
    (copy-file (concat file ".pdf") (concat cwd "/doc/static/" (concat file ".pdf")))
    ))

(make-directory (concat cwd "/doc/static/") t)
(export-latex 'altacv "altacv.org")
(export-latex 'moderncv "moderncv.org")
