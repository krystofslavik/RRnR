RECORD AND REPLAY DEBUGGING IN R

Master's thesis focused on implementation and integration of an experimental Record and Replay debugger into R programming language.

CONTENTS:
* Thesis.pdf - the text of the thesis
* thesis_src - LaTeX source code of the thesis including benchmark result data
* modified-R - fork of R 3.4.3 with the debugger implementation and necessary modifications
* results    - raw results of the benchmarks and the vignette testing

BUILD INSTRUCTIONS:
  cd modified-R
  ./configure
  make
  bin/R

THESIS LATEX BUILD INSTRUCTIONS:
  cd thesis_src
  make

AUTHOR:
Kryštof Slavík
Faculty of Information Technology, Czech Technical University in Prague
2018

URL:
https://github.com/krystofslavik/RRnR

R-PROJECT:
https://www.r-project.org/
