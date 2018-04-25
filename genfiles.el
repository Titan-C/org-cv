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

(defun export-latex (file)
  (let ((workfile (concat workdir file))
        (outfile (concat workdir file ".tex")))
    (message (format "%s exists: %s" workfile (file-exists-p workfile)))
    (find-file workfile)
    (org-mode)
    (org-export-to-file 'altacv outfile)
    (shell-command (format "latexmk -pdf %s" outfile))
    ))

(export-latex "altacv.org")
(copy-directory workdir cwd)
