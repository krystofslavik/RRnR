library(tools)
par(ask=F)

testing_output_dir <- paste(dirname(sys.frame(1)$ofile), "vignettes", sep="/")
if(!dir.exists(testing_output_dir)) dir.create(testing_output_dir)

package_list <- scan(paste(testing_output_dir, "..", "packages.txt", sep="/"), character(), quote = "")

test_vignettes <- function(packages=intersect(package_list, rownames(installed.packages())), skip_first=FALSE)
{
  dir <- testing_output_dir
  
  state <- list()
  state$results <- list()
  state$processed_packages <- list()
  state$processed_vignettes <- list()
  
  if(file.exists(paste(dir, "state.RData", sep="/")))
    load(paste(dir, "state.RData", sep="/"))

  # start where we stopped last time
  packages <- setdiff(packages, as.character(state$processed_packages))
  
  installed_packages <- rownames(installed.packages())
  to_install <- setdiff(packages, installed_packages)
  
  if(length(to_install))
  {
    install.packages(to_install,
                     repos='http://cran.us.r-project.org',
                     #Ncpus=20,                            # run in parallel using 20 cores
                     #keep_outputs=T,                      # keeps outputs in ".out" files in current directory
                     INSTALL_opts=c(
                     "--byte-compile",                     # byte-compile packages
                     "--example",                          # extract and keep examples
                     "--install-tests",                    # copy and retain test directory for the package
                     "--with-keep.source",                 # keep line numbers
                     "--no-multiarch"),
                     dependencies = c("Depends",
                                      "Imports",
                                      "LinkingTo",
                                      "Suggests",
                                      "Enhances"))
  }
  
  for(package in packages)
  {
    write(paste("Testing vignettes for package: ", package, sep=""), stdout())
    
    vignettes_in_package <- vignette(package = package)$results[,3]
    
    # start where we stopped last time
    vignettes_in_package <- setdiff(vignettes_in_package, as.character(state$processed_vignettes))
    
    if(skip_first)
    {
      state$processed_vignettes[[length(state$processed_vignettes)+1]] <- vignettes_in_package[1]
      state$results[[length(state$results)+1]] <- list(pkg=package, vignette=vignettes_in_package[1], skipped=TRUE)
      vignettes_in_package <- vignettes_in_package[-1]
      skip_first <- FALSE
    }
    
    for(vignette_name in vignettes_in_package)
    {
      write(paste("Testing vignette ", vignette_name, " from package ", package, sep=""), stdout())
      
      # backup the state so that we can continue from this place if needed
      save(state, file=paste(dir, "state.RData", sep="/"))
      state$processed_vignettes[[length(state$processed_vignettes)+1]] <- vignette_name
      
      state$results[[length(state$results)+1]] <- test_one_vignette(dir, vignette_name, package)
    }
    
    state$processed_packages[[length(state$processed_packages)+1]] <- package
    state$processed_vignettes <- list()
  }
  
  # final save
  save(state, file=paste(dir, "state.RData", sep="/"))
  
  state
}

test_one_vignette <- function(dir, vignette_name, package)
{
      vignette <- vignette(vignette_name, package = package)
      vignette_source <- paste(vignette$Dir, "doc", vignette$R, sep="/")
      
      plain_filename <- paste(dir, paste(package, vignette_name, "plain.tmp", sep="-"), sep="/")
      record_filename <- paste(dir, paste(package, vignette_name, "record.tmp", sep="-"), sep="/")
      replay_filename <- paste(dir, paste(package, vignette_name, "replay.tmp", sep="-"), sep="/")
      
      plain_error <- NULL
      result <- list(pkg=package, vignette=vignette_name)
      
      # run plain mode 3 times to hot load everything
      for(i in 1:3) {
        write(paste("Running in plain mode ", i, "...", sep=""), stdout())
        tryCatch(capture.output(source(vignette_source), file=plain_filename), error=function(e) { plain_error <<- e })
      }
      
      if(!is.null(plain_error))
      {
        cat("Plain mode failed with error: ")
        print(plain_error)
        result$failed_in_plain <- TRUE
      }
      else
      {
        write("Running in record mode...", stdout())
        tryCatch(capture.output(rec <- record(source(vignette_source)), file=record_filename))
      
        write("Running in replay mode...", stdout())
        tryCatch(capture.output(replay(rec), file=replay_filename))
      
        record_check <- md5sum(plain_filename) == md5sum(record_filename)
        replay_check <- md5sum(plain_filename) == md5sum(replay_filename)
        replay_check2 <- md5sum(record_filename) == md5sum(replay_filename)
        
        result$output_size <- file.size(plain_filename)
        
        if(is.na(record_check) || !record_check)
        {
          write("record != plain", stdout())
          result$record_plain <- FALSE
          result$trace_size <- NA
        }
        else
        {
          write("record == plain", stdout())
          result$record_plain <- TRUE
          result$trace_size <- length(rec$trace)
        }
        if(is.na(replay_check) || !replay_check)
        {
          write("replay != plain", stdout())
          result$replay_plain <- FALSE
        }
        else
        {
          write("replay == plain", stdout())
          result$replay_plain <- TRUE
        }
        if(is.na(replay_check2) || !replay_check2)
        {
          write("record != replay", stdout())
          result$record_replay <- FALSE
        }
        else
        {
          write("record == replay", stdout())
          result$record_replay <- TRUE
        }
      }
      
      result
}

test_skipped_vignettes <- function(skip = c())
{
  state <- load_results()
  
  for(i in 1:length(state$results))
  {
    if(is.null(state$results[[i]]$skipped) || !state$results[[i]]$skipped || state$results[[i]]$vignette %in% skip) next()
    
    write(paste("Testing vignette ", state$results[[i]]$vignette, " from package ", state$results[[i]]$pkg, sep=""), stdout())
    
    result <- test_one_vignette(testing_output_dir, state$results[[i]]$vignette, state$results[[i]]$pkg)
    
    state$results[[i]] <- result
    save(state, file=paste(testing_output_dir, "state.RData", sep="/"))
    
    print(result)
  }
}

check_plain_mode_where_record_failed <- function(record_replay)
{
  state <- load_results()
  res <- list()
  
  for(i in 1:length(state$results))
  {
    if(!is.null(state$results[[i]]$skipped) && state$results[[i]]$skipped) next()
    if(!is.null(state$results[[i]]$failed_in_plain) && state$results[[i]]$failed_in_plain) next()
    if(state$results[[i]]$record_plain) next()
    if(record_replay != state$results[[i]]$record_replay) next()
    
    write(paste("Testing vignette ", state$results[[i]]$vignette, " from package ", state$results[[i]]$pkg, sep=""), stdout())
    
    vignette <- vignette(state$results[[i]]$vignette, package = state$results[[i]]$pkg)
    vignette_source <- paste(vignette$Dir, "doc", vignette$R, sep="/")

    plain_filename <- paste(testing_output_dir, "plain_test.tmp", sep="/")
    plain_error <- NULL
    
    for(x in 1:3)
    {
      if(!is.null(plain_error)) next()
      write("Testing in plain mode...", stdout())
      
      tryCatch(capture.output(source(vignette_source), file=plain_filename), error=function(e) { plain_error <<- e })
    }
    
    same <- TRUE
    for(x in 1:2)
    {
      if(!is.null(plain_error)) next()
      write("Testing in plain mode...", stdout())
      
      plain_filename2 <- paste(testing_output_dir, "plain_test2.tmp", sep="/")
      tryCatch(capture.output(source(vignette_source), file=plain_filename2), error=function(e) { plain_error <<- e })
      
      if(md5sum(plain_filename) != md5sum(plain_filename2))
      {
        same <- FALSE
        break()
      }
    }
    
    if(!is.null(plain_error))
      res[[length(res)+1]] <- list(v=state$results[[i]]$vignette, r="error")
    else
      res[[length(res)+1]] <- list(v=state$results[[i]]$vignette, r=same)

    save(res, file=paste(testing_output_dir, "res.RData", sep="/"))
  }
  
  res
}

load_results <- function() { load(paste(testing_output_dir, "state.RData", sep="/")); state; }

results_total_vignettes <- function(state) length(state$results)
results_skipped_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$skipped) && state$results[[i]]$skipped)
      n <- n + 1
  n
}
results_plain_failed_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$failed_in_plain) && state$results[[i]]$failed_in_plain)
      n <- n + 1
  n
}
results_record_tested_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_plain))
      n <- n + 1
  n
}
results_record_success_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_plain) && state$results[[i]]$record_plain)
      n <- n + 1
  n
}
results_replay_success_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_replay) && state$results[[i]]$record_replay)
      n <- n + 1
  n
}
results_record_fail_replay_fail_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_plain) && !state$results[[i]]$record_plain && !state$results[[i]]$record_replay)
      n <- n + 1
  n
}
results_record_fail_replay_success_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_plain) && !state$results[[i]]$record_plain && state$results[[i]]$record_replay)
      n <- n + 1
  n
}
results_record_success_replay_fail_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_plain) && state$results[[i]]$record_plain && !state$results[[i]]$record_replay)
      n <- n + 1
  n
}
results_record_replay_success_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_plain) && state$results[[i]]$record_plain && state$results[[i]]$record_replay)
      n <- n + 1
  n
}
results_success_with_output_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_plain) && state$results[[i]]$record_plain && state$results[[i]]$record_replay && state$results[[i]]$output_size > 0)
      n <- n + 1
  n
}
results_success_with_trace_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_plain) && state$results[[i]]$record_plain && state$results[[i]]$record_replay && state$results[[i]]$trace_size > 0)
      n <- n + 1
  n
}
results_success_with_output_and_trace_vignettes <- function(state) {
  n <- 0
  for(i in 1:length(state$results))
    if(!is.null(state$results[[i]]$record_plain) && state$results[[i]]$record_plain && state$results[[i]]$record_replay && state$results[[i]]$output_size > 0 && state$results[[i]]$trace_size > 0)
      n <- n + 1
  n
}
results_vignettes_list <- function(state) {
  lst <- list()
  
  for(i in 1:length(state$results))
  {
    if(!is.null(state$results[[i]]$skipped) && state$results[[i]]$skipped) next()
    if(!is.null(state$results[[i]]$failed_in_plain) && state$results[[i]]$failed_in_plain) next()
    
    output <- c(state$results[[i]]$pkg, state$results[[i]]$vignette, state$results[[i]]$record_plain, state$results[[i]]$record_replay)
    lst[[length(lst)+1]] <- output
  }
  
  lst
}
results_pie_chart <- function(state) {
  success <- results_record_replay_success_vignettes(state)
  success_with_output_and_trace <- results_success_with_output_and_trace_vignettes(state)
  success_without_output <- success - results_success_with_output_vignettes(state)
  
  record_fail_replay_fail <- results_record_fail_replay_fail_vignettes(state)
  record_success_replay_fail <- results_record_success_replay_fail_vignettes(state)
  record_fail_replay_success <- results_record_fail_replay_success_vignettes(state)
  
  slices <- c(success_with_output_and_trace, success_without_output, record_fail_replay_success, record_success_replay_fail, record_fail_replay_fail)
  labels <- c("Success", "Success with no output", "Replay success & record fail", "Record success & replay fail", "Fail")
  colors <- c("#69DC9E", "#D7E5C3", "#BFBFBF", "#FF7C66", "#E94F37")
  
  pct <- round(slices/sum(slices)*100)
  labels <- paste(labels, "\n", pct, "%", sep="")
  
  pie(slices, labels=labels, col=colors)
}
