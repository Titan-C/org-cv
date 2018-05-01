#!/usr/bin/env bash

wget https://github.com/gohugoio/hugo/releases/download/v0.39/hugo_0.39_Linux-64bit.deb
dpkg -i hugo*.deb

echo "Installed Hugo:"
hugo version

# Latex
latexdir=/root/texmf/tex/latex
mkdir -p $latexdir
echo "Install altacv"
wget https://github.com/Titan-C/AltaCV/archive/sections.zip
unzip -j sections.zip -d $latexdir/AltaCV
echo "Install moderncv"
wget https://github.com/Titan-C/moderncv/archive/master.zip
unzip -j master.zip -d $latexdir/moderncv
