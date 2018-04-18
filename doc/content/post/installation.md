+++
title = "Installation"
author = ["Óscar Nájera"]
draft = false
weight = 1001
+++

This project is not on MELPA so you have to do a manual installation. First
clone this git repository.

```bash
git clone https://gitlab.com/Titan-C/org-cv.git
```

There are various modules to perform the export. As of now `ox-moderncv`,
`ox-altacv`, `ox-hugocv`. Choose any or all that you prefer for install. I
use `use-package` to manage the installation for example of `ox-moderncv`.

```emacs-lisp
(use-package ox-moderncv
    :load-path "path_to_repository/org-cv/"
    :init (require 'ox-moderncv))
```
