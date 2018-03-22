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
