## Introduction:

The aim of this mini project is to investigate and analyze bacterial growth data and perform model fitting on the
dataset.

## Tools Used in Project:

### R Language:

R language(R version 4.3.1 (2023-06-16)) & RStudio

#### Package Used in R:

(tidyverse): The tidyverse is a collection of multiple packages that provides a consistent, clear, and efficient set of
tools for data analysis. Two main packages from the tidyverse are utilized in my project:

1. ggplot2: This package is used for creating beautiful and highly customizable graphics. It allows users to generate
   complex data visualizations through a simple and intuitive syntax.
2. dplyr: This package offers a set of functions for data manipulation and operations, including data filtering,
   sorting, grouping, summarization, and more. It employs an intuitive syntax, making data manipulation tasks simpler
   and more readable

(minpack.lm): The minpack.lm package provides an implementation of the Nonlinear Least Squares (NLS) algorithm. The
project primarily utilized the nlsLM() function from this package.

### LaTeX:

LaTeX, Overleaf

#### Package Used in LaTeX:

1. **setspace:** This package allows you to change the line spacing settings in the document. By
   using `\usepackage{setspace}` and commands like `\onehalfspacing` or `\doublespacing`, it can
   easily adjust the line spacing.

2. **amsmath:** This is a package for enhancing mathematical typesetting. It provides additional mathematical features,
   environments, and symbols to make it more convenient to typeset mathematical formulas.

3. **geometry:** This package is used to set the page layout of the document, including margins and page dimensions. In
   my LaTeX file, the left, right, top, and bottom margins of the page are set.

4. **graphicx:** This package is used for inserting graphics into document. It allows to include various image
   files and provides options for adjusting the size and position of the graphics.

5. **subfig:** This package is used to support the typesetting of sub-figures. It allows to include multiple
   sub-figures within the same figure environment and add sub-captions to each sub-figure.

6. **float:** It provides additional control over floating objects (such as figures and tables). It can be used to
   adjust their placement within the document.

### Bash

Bash script

## Project structure:
```
.
├── README.md
├── code
│ ├── model_fit.R (This R script primarily conducts data analysis, model fitting, and generates visualized data plots.)
│ ├── ref.bib (Used to generate reference directories)
│ ├── report.tex (Project report)
│ └── run.sh (Bash script that runs all script)
├── data
│ ├── LogisticGrowthData.csv
│ └── LogisticGrowthMetaData.csv
└── results
```

## Author:
PU ZHAO
pu.zhao23@imperial.ac.uk

## More Information:
This project was created based on https://mhasoba.github.io/TheMulQuaBio/.