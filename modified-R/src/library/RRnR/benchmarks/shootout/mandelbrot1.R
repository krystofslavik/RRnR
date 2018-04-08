# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
#
# Original Loc: https://github.com/allr/fastr/tree/master/test/r/shootout/mandelbrot
# Modified to be compatible with rbenchmark interface
# ------------------------------------------------------------------


run<-function() {
    n <- 1400

    lim <- 2
    iter <- 50
    
    n_mod8 = n %% 8L
    pads <- if (n_mod8) rep.int(0, 8L - n_mod8) else integer(0)
    p <- rep(as.integer(rep.int(2, 8) ^ (7:0)), length.out=n)
    
    bin_con <- file(paste(sep="/", dir, "shootout/mandelbrot1.R.tmp2"), "wb")
    cat("P4\n", file = bin_con)
    cat(n, n, "\n", file = bin_con)
    for (y in 0:(n-1)) {
        c <- 2 * 0:(n-1) / n - 1.5 + 1i * (2 * y / n - 1)
        z <- rep(0+0i, n)
        i <- 0L
        while (i < iter) {  # faster than for loop
            z <- z * z + c
            i <- i + 1L
        }
        bits <- as.integer(abs(z) <= lim)
        bytes <- as.raw(colSums(matrix(c(bits * p, pads), 8L)))
        writeBin(bytes, bin_con)
        flush(bin_con)
    }
    close(bin_con)
    
    print(unname(md5sum(paste(sep="/", dir, "shootout/mandelbrot1.R.tmp2"))))
    file.remove(paste(sep="/", dir, "shootout/mandelbrot1.R.tmp2"))
    invisible()
}
