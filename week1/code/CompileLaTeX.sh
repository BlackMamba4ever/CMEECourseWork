#!/bin/bash

if [ "$#" == 0 ]; then
  echo "need to input file"
  exit 1
fi

if [ ${1: -4} !=  ".tex" ]; then
  echo "need to input file with .tex extension"
  exit 1
fi

filename=${1%.tex}

pdflatex $filename.tex
bibtex $filename
pdflatex $filename.tex
pdflatex $filename.tex

mv "$filename.pdf" ../results
evince ../results/$filename.pdf &

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg