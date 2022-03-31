#!/bin/sh

scribble --prefix tex-prefix.tex --latex one-more-re-nightmare.scrbl
sed -i -e 's/^\\renewrmdefault//g' \
    -e 's/^\\packageTxfonts//g' \
    -e 's/\\hspace\*{\\fill}//g' \
    -e 's/\\HR{}\\\\/\\HR{}/g' \
    one-more-re-nightmare.tex
sed -i -e 's/^\\\\//g' one-more-re-nightmare.tex
pdflatex one-more-re-nightmare.tex
makeindex one-more-re-nightmare
pdflatex one-more-re-nightmare.tex
