# R Benchmark 2.5 (06/2008) [Simon Urbanek]
# version 2.5: scaled to get roughly 1s per test, R 2.7.0 @ 2.6GHz Mac Pro
# R Benchmark 2.4 (06/2008) [Simon Urbanek]
# version 2.4 adapted to more recent Matrix package
# R Benchmark 2.3 (21 April 2004)
# Warning: changes are not carefully checked yet!
# version 2.3 adapted to R 1.9.0
# Many thanks to Douglas Bates (bates@stat.wisc.edu) for improvements!
# version 2.2 adapted to R 1.8.0
# version 2.1 adapted to R 1.7.0
# version 2, scaled to get 1 +/- 0.1 sec with R 1.6.2
# using the standard ATLAS library (Rblas.dll)
# on a Pentium IV 1.6 Ghz with 1 Gb Ram on Win XP pro

# revised and optimized for R v. 1.5.x, 8 June 2002
# Requires additionnal libraries: Matrix, SuppDists
# Author : Philippe Grosjean
# eMail  : phgrosjean@sciviews.org
# Web    : http://www.sciviews.org
# License: GPL 2 or above at your convenience (see: http://www.gnu.org)
#
# Several tests are adapted from the Splus Benchmark Test V. 2
# by Stephan Steinhaus (stst@informatik.uni-frankfurt.de) 
# Reference for Escoufier's equivalents vectors (test III.5):
# Escoufier Y., 1970. Echantillonnage dans une population de variables
# aleatoires réelles. Publ. Inst. Statis. Univ. Paris 19 Fasc 4, 1-47.
#
# type source("c:/<dir>/R2.R") to start the test

run <- function()
{
	a <- new("dgeMatrix", x = rnorm(1200*1200), Dim = as.integer(c(1200,1200)))
	b <- as.double(1:1200)
	c <- solve(crossprod(a), crossprod(a,b))
	
	print(rowSums(c))
}
