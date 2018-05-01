+++
title = "Latex Exporter"
author = ["Óscar Nájera"]
draft = false
weight = 1004
+++

## Using modern-cv {#using-modern-cv}

[moderncv](https://www.ctan.org/tex-archive/macros/latex/contrib/moderncv) is a standard \\(\LaTeX\\) package that you can find in many of your
latex distributions. For I maintain for personal purposes a fork of it to
better work with my use case at <https://github.com/Titan-C/moderncv.git>
Feel free to use any or even your personal fork for your desired use case.

To configure the export for moderncv you need the addition options in your
org file.

```org
# CV theme - options include: 'casual' (default), 'classic', 'oldstyle' and 'banking'
#+CVSTYLE: banking
# CV color - options include: 'blue' (default), 'orange', 'green', 'red', 'purple', 'grey' and 'black'
#+CVCOLOR: green
```

When exporting you can call the following function to get the latex file.

```emacs-lisp
(org-export-to-file 'moderncv "moderncv.tex")
(org-latex-compile "moderncv.tex")
```

<object data="moderncv.org.pdf" type="application/pdf" width="100%" height="500px">
<p>Alternative text - include a link <a href="moderncv.org.pdf">to the PDF!</a></p>
</object>


## Using alta-cv {#using-alta-cv}

[AltaCV](https://github.com/liantze/AltaCV) is another project to generate a CV, you will need to install it
yourself. I maintain a fork too at <https://github.com/Titan-C/AltaCV.git>
because I need extra features and I encourage to use this fork on the
`sections` branch.

The style of this CV is more involved and you need some configuration in
your org file to get it to work. First define the margins, the large margin
to the right is to allow for a second column.

```org
#+LATEX_HEADER: \geometry{left=1cm,right=9cm,marginparwidth=6.8cm,marginparsep=1.2cm,top=1.25cm,bottom=1.25cm}
```

Content on the right column has the same structure of a org file, but you
need to enclose it in the `\marginpar{}` command as shown next.

```org
#+latex: \marginpar{
```

```org
* Main Interests
- Free/Libre and Open Source Software (FLOSS)
- Free food
- Free beer

* Programming
- Python
- C/C++
- EmacsLisp
- Bash
- JavaScript
- PHP

* Languages

- *English*  Fluent
- *German*   Fluent
- *Spanish*  Native
- *French*   Intermediate
```

```org
#+latex: }
```

When exporting you can call the following function to get the latex file.

```emacs-lisp
(org-export-to-file 'altacv "altacv.tex")
(org-latex-compile "altacv.tex")
```

<object data="altacv.org.pdf" type="application/pdf" width="100%" height="500px">
<p>Alternative text - include a link <a href="altacv.org.pdf">to the PDF!</a></p>
</object>
