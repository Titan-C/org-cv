#!/usr/bin/env bash

wget https://github.com/gohugoio/hugo/releases/download/v0.39/hugo_0.39_Linux-64bit.deb
dpkg -i hugo*.deb
hugo version

# Latex
apt-get update && apt install -y fonts-font-awesome
git clone https://github.com/Titan-C/AltaCV.git /root/texmf-dist/tex/latex/AltaCV
wget http://mirrors.ctan.org/fonts/fontawesome/tex/fontawesome.sty
wget http://mirrors.ctan.org/fonts/fontawesome/tex/fontawesomesymbols-generic.tex
wget http://mirrors.ctan.org/fonts/fontawesome/tex/fontawesomesymbols-pdftex.tex

emacs --batch --load /tmp/install-org.el
cd org-cv-exports; latex -pdf altacv.org.tex
