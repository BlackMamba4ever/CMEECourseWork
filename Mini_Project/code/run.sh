##### PU ZHAO
##### 2 Dec 2023
#!/bin/bash

####Sourcing the main R script
Rscript model_fit.R


pdflatex report.tex
bibtex report
pdflatex report.tex
pdflatex report.tex
mv report.pdf ../results
evince ../results/report.pdf &


## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg

exit
