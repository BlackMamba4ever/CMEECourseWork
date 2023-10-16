# Week1 Project

## Introduction

#### This repository aims to explore some aspects of computing, including UNIX and Linux, shell scripting, version control with Git, and scientific document creation with LaTeX.

### UNIX and Linux

UNIX is a multi-user and multi-tasking operating system with powerful functions and comprehensive performance. It can be
applied on a variety of different platforms from supercomputers to ordinary PCs. It is the operating system with the
widest application and the greatest influence. Linux is an operating system that looks and performs the same as or
better than UNIX. Understanding their architecture, command-line interface, file system structure, and basic commands is
crucial.

### Shell Scripting

Shell scripting is a valuable skill for automating tasks and enhancing productivity within a command-line environment.
Shell in Linux can be understood as the user interface provided by the Linux system to users. Shell provides an
environment for users to enter commands and parameters and obtain the results of command execution. This section will
introduce you to the basics of writing shell scripts, enabling you to automate repetitive tasks, streamline workflows,
and customize your computing experience.

### Version Control with Git

A version control system (VCS for short) is a "database" that can help you save a complete snapshot of a project when
needed. When you need to view a previous snapshot (called a "version"), the version control system can display the
details of all changes between the current version and the previous version. Version control is pivotal in collaborative
software development. Git, a robust version control system, facilitates tracking changes, managing projects, and
enabling seamless collaboration. This section will provide comprehensive insights into Git, covering branching
strategies, merging, and effective collaboration techniques.

### Scientific Documents with LaTeX

Latex is a document preparation system for high-quality typesetting. It is most commonly used for medium to large
technical or scientific documents, but it can be used for almost any form of publishing. Mastering LaTeX empowers you to
create professional research papers, theses, reports, and technical documentation. This section will guide you through
creating and formatting scientific documents using LaTeX.

## Directory Structure

```
.
├── README.md
├── UnixPrac1.txt
├── code
│  ├── CompileLaTeX.sh
│  ├── ConcatenateTwoFiles.sh
│  ├── CountLines.sh
│  ├── FirstBiblio.bib
│  ├── FirstExample.tex
│  ├── MyExampleScript.sh
│  ├── boilerplate.sh
│  ├── csvtospace.sh
│  ├── csvtospace_by_dir.sh
│  ├── tabtocsv.sh
│  ├── tiff2png.sh
│  └── variables.sh
├── data
│  ├── Temperatures
│  │  ├── 1800.csv
│  │  ├── 1801.csv
│  │  ├── 1802.csv
│  │  └── 1803.csv
│  ├── fasta
│  │  ├── 407228326.fasta
│  │  ├── 407228412.fasta
│  │  └── E.coli.fasta
│  ├── file1.txt
│  ├── file2.txt
│  ├── file3.txt
│  ├── spawannxs.txt
│  └── tiffPic
│      ├── 1111.tif
│      └── 2222.tif
├── results
├── sandbox
│  ├── ListRootDir.txt
│  ├── TestFind
│  │  ├── Dir1
│  │  │  ├── Dir11
│  │  │  │  └── Dir111
│  │  │  │      └── File111.txt
│  │  │  ├── File1.csv
│  │  │  ├── File1.tex
│  │  │  └── File1.txt
│  │  ├── Dir2
│  │  │  ├── File2.tex
│  │  │  ├── File2.txt
│  │  │  └── file2.csv
│  │  └── Dir3
│  │      └── File3.txt
│  ├── TestWild
│  │  ├── Anotherfile.csv
│  │  ├── Anotherfile.txt
│  │  ├── File1.csv
│  │  ├── File1.txt
│  │  ├── File2.csv
│  │  ├── File2.txt
│  │  ├── File3.csv
│  │  ├── File3.txt
│  │  ├── File4.csv
│  │  └── File4.txt
│  ├── test.txt
│  └── test.txt.csv
├── tree.md
└── tree.txt
```

## How to Use

### UNIX and Linux

Use &nbsp; `mkdir dir`&nbsp; to create a directory.  
Use &nbsp; `man COMMAND`&nbsp; to show help page of a command.  
Use &nbsp; `pwd`&nbsp; to show the current directory.  
Use &nbsp; `ls`&nbsp; to list the files in the directory.  
Use &nbsp; `cd + DIRNAME or .. or /DIRNAME or FILENAME`&nbsp; to change directory, move one directory up, and go to the
directory you want.  
And so on ...

### Git

`git add` used to add files or modifications to the staging area  
`git commit` used to submit files in the staging area to the repository. Submitting means permanently saving your
changes to the Git repository.  
`git status` used to display the status of the current working directory and file change information, including
modified, staged, and untracked files.  
And so on ...

## Author

PU ZHAO

## More Information

This project was created based on https://mhasoba.github.io/TheMulQuaBio/.