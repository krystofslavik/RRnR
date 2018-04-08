library(tools)
library(Matrix)

dir <- dirname(sys.frame(1)$ofile)
warming_runs <- 5
runs <- 100

tests_shootout <- c(
"binary-trees.R",
"fannkuch-redux.R",
"fasta-native.R",
"fastaredux.R",
"k-nucleotide.R",
"mandelbrot1.R",
"meteor-contest.R",
"nbody.R",
"pidigits.R",
"regexdna.R",
"revcomp-1.R",
"spectral-norm-alt.R"
)
attr(tests_shootout, "dir") <- "shootout"

tests_rbench <- c(
"A1-matrix-deformation.R",
"A2-matrix-power.R",
"A3-sort.R",
"A4-matrix-cross.R",
"A5-matrix-linreg.R",
"B1-fft.R",
"B2-eigenvalues.R",
"B3-determinant.R",
"B4-cholesky.R",
"B5-inverse.R",
"C1-fibonacci.R",
"C2-hilbert.R",
"C3-gcd.R",
"C4-toeplitz.R",
"C5-escoufier.R"
)
attr(tests_rbench, "dir") <- "R-benchmark"

tests_custom <- c(
"T1-C-sleep.R",
"T2-system-sleep.R",
"T3-download.R",
"T4-big-file.R"
)
attr(tests_custom, "dir") <- "custom"

run_bench <- function(dir, files)
{
  results <- list()
  results[[1]] <- data.frame(matrix(vector(), runs, length(files), dimnames=list(c(), files)), stringsAsFactors=F)
  results[[2]] <- data.frame(matrix(vector(), runs, length(files), dimnames=list(c(), files)), stringsAsFactors=F)
  results[[3]] <- data.frame(matrix(vector(), runs, length(files), dimnames=list(c(), files)), stringsAsFactors=F)
  results[[4]] <- data.frame(matrix(vector(), runs, length(files), dimnames=list(c(), files)), stringsAsFactors=F)
  
  i <- 1
  for(f in files)
  {
    file <- paste(sep="/", attr(files, "dir"), f)
    total_runs <- warming_runs + runs
    
    source(paste(sep="/", dir, file))
    
    for(j in 1:total_runs)
    {
      cat("Running", file, "in plain mode\n")
      
      invisible(gc())
      t <- proc.time()[[3]]
      set.seed(61210793)
      capture.output(invisible(run()), file=paste(sep="/", dir, paste(sep=".", file, j, "tmp")))
      t0 <- proc.time()[[3]] - t
      
      check <- md5sum(paste(sep="/", dir, paste(sep=".", file, "out"))) == md5sum(paste(sep="/", dir, paste(sep=".", file, j, "tmp")))
      if(check) file.remove(paste(sep="/", dir, paste(sep=".", file, j, "tmp")))
      else stop("FALSE check")
      cat(t0, "s plain\nCheck:", check, "\n\n")
      
      if(j > warming_runs)
        results[[1]][j - warming_runs, i] <- t0
    }

    for(j in 1:total_runs)
    {
      cat("Running", file, "in record mode\n")
      
      invisible(gc())
      t <- proc.time()[[3]]
      set.seed(61210793)
      capture.output(invisible(rec <- record(run())), file=paste(sep="/", dir, paste(sep=".", file, j, "tmp")))
      t1 <- proc.time()[[3]] - t
      t1_inner <- rec$debug$time
      
      check <- md5sum(paste(sep="/", dir, paste(sep=".", file, "out"))) == md5sum(paste(sep="/", dir, paste(sep=".", file, j, "tmp")))
      if(check) file.remove(paste(sep="/", dir, paste(sep=".", file, j, "tmp")))
      else stop("FALSE check")
      cat(t1, "s (", t1_inner,"s inner) record\nCheck:", check, "\n\n")
      
      if(j > warming_runs)
      {
        results[[2]][j - warming_runs, i] <- t1
        results[[3]][j - warming_runs, i] <- t1_inner
      }
    }
    
    invisible(gc())
    set.seed(61210793)
    capture.output(invisible(rec <- record(run())))

    for(j in 1:total_runs)
    {
      cat("Running", file, "in replay mode\n")
      
      invisible(gc())
      t <- proc.time()[[3]]
      capture.output(invisible({ replay(rec); }), file=paste(sep="/", dir, paste(sep=".", file, j, "tmp")))
      t2 <- proc.time()[[3]] - t
      
      check <- md5sum(paste(sep="/", dir, paste(sep=".", file, "out"))) == md5sum(paste(sep="/", dir, paste(sep=".", file, j, "tmp")))
      if(check) file.remove(paste(sep="/", dir, paste(sep=".", file, j, "tmp")))
      else stop("FALSE check")
      cat(t2, "s replay\nCheck:", check, "\n\n")
      
      if(j > warming_runs)
        results[[4]][j - warming_runs, i] <- t2
    }
    
    i <- i + 1
  }
  
  results
}

invoke_count <- function(dir, files)
{
  results <- list()
  results$invokes <- data.frame(matrix(vector(), 1, length(files), dimnames=list(c(), files)), stringsAsFactors=F)
  results$invokes_internal <- data.frame(matrix(vector(), 1, length(files), dimnames=list(c(), files)), stringsAsFactors=F)
  results$trace_size <- data.frame(matrix(vector(), 1, length(files), dimnames=list(c(), files)), stringsAsFactors=F)
  
  cat("Running invoke counting\n")
  
  i <- 1
  for(f in files)
  {
    file <- paste(sep="/", attr(files, "dir"), f)
    cat(file, "\n")
    
    source(paste(sep="/", dir, file))
    invisible(gc())
    capture.output(invisible({ set.seed(61210793); rec <- record(run()); }))
    
    results$invokes[1, i] <- rec$debug$invoke_cnt
    results$invokes_internal[1, i] <- rec$debug$invoke_internal_cnt
    results$trace_size[1, i] <- length(rec$trace)
    i <- i + 1
  }
  
  results
}

tests <- tests_shootout

results <- run_bench(dir, tests)
write.csv(results[[1]], paste(sep="/", dir, "results0.csv"))
write.csv(results[[2]], paste(sep="/", dir, "results1.csv"))
write.csv(results[[3]], paste(sep="/", dir, "results1_inner.csv"))
write.csv(results[[4]], paste(sep="/", dir, "results2.csv"))
invokes <- invoke_count(dir, tests)
write.csv(invokes$invokes, paste(sep="/", dir, "invokes.csv"))
write.csv(invokes$invokes_internal, paste(sep="/", dir, "invokes_internal.csv"))
write.csv(invokes$trace_size, paste(sep="/", dir, "trace_size.csv"))
