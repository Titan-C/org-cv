#!/usr/bin/env bash

apt-get update && apt-get install -y fonts-font-awesome
wget https://github.com/gohugoio/hugo/releases/download/v0.39/hugo_0.39_Linux-64bit.deb
dpkg -i hugo*.deb

echo "Installed Hugo:"
hugo version

# Latex
latexdir=/root/texmf/tex/latex
echo "Install altacv"
wget https://github.com/Titan-C/AltaCV/archive/sections.zip
unzip -j sections.zip -d $latexdir/AltaCV
echo "Install moderncv"
wget https://github.com/Titan-C/moderncv/archive/master.zip
unzip -j master.zip -d $latexdir/moderncv
echo "Install fontawesome for latex"
fontdir=$latexdir/fontawesome/
mkdir -p $fontdir

wget -P $fontdir http://mirrors.ctan.org/fonts/fontawesome/tex/fontawesome.sty
wget -P $fontdir http://mirrors.ctan.org/fonts/fontawesome/tex/fontawesomesymbols-generic.tex
wget -P $fontdir http://mirrors.ctan.org/fonts/fontawesome/tex/fontawesomesymbols-pdftex.tex
