#!/usr/bin/env bash

apt-get update && apt-get install -y fonts-font-awesome
wget https://github.com/gohugoio/hugo/releases/download/v0.39/hugo_0.39_Linux-64bit.deb
dpkg -i hugo*.deb

echo "Installed Hugo:"
hugo version

# Latex
echo "Install altacv"
git clone https://github.com/Titan-C/AltaCV.git /root/texmf/tex/latex/AltaCV
echo "Install fontawesome for latex"
fontdir=/root/texmf/tex/latex/fontawesome/
mkdir -p $fontdir

wget -P $fontdir http://mirrors.ctan.org/fonts/fontawesome/tex/fontawesome.sty
wget -P $fontdir http://mirrors.ctan.org/fonts/fontawesome/tex/fontawesomesymbols-generic.tex
wget -P $fontdir http://mirrors.ctan.org/fonts/fontawesome/tex/fontawesomesymbols-pdftex.tex
