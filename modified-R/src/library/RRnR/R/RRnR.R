recordExprEnv <- function(expr, env, options)
{
  # update default options with passed options
  default_options <- list(allow_graphs = TRUE, allow_prints = TRUE, allow_connections = FALSE, full_clone = FALSE)
  option_names <- names(default_options)
  option_updates <- options[option_names]
  option_updates[sapply(option_updates, is.null)] <- NULL
  default_options[names(option_updates)] <- option_updates
  
  # clone all relevant environments
  cloned_envs <- clone_environments(env, skip_replay_structures=TRUE, full_clone=default_options$full_clone)

  # record the trace
  old_time <- proc.time()[[3]]
  replay_struct <- tryCatch(
    .Call(C_do_record, expr, env, default_options),
    error = function(cond) { print(cond); r <- .Call(C_do_get_replay_struct, expr, env, default_options); r$result <- cond; r }
  )
  replay_struct$debug$time <- proc.time()[[3]] - old_time
  
  # save the environments
  replay_struct$cloned_envs <- cloned_envs
  
  attr(replay_struct, "RRnR_replay_structure") <- TRUE
  
  return(replay_struct)
}

record <- function(code, options = list())
{
  # save the expression without evaluating it
  recordExprEnv(as.expression(substitute(code)), parent.frame(), options)
}

recordFindBug <- function(code, detect = NULL, options = list())
{
  max_time <- 20
  total_time <- 0
  
  if(is.numeric(options$max_time))
    max_time <- options$max_time
  
  while(TRUE)
  {
    old_time <- proc.time()[[3]]
    rec <- recordExprEnv(as.expression(substitute(code)), parent.frame(), options)
    total_time <- total_time + (proc.time()[[3]] - old_time)
    
    if(is.null(detect) && is(rec$result, "error"))
      return(rec)
    if(is.function(detect) && detect(rec$result) == TRUE)
      return(rec)
    if(total_time >= max_time)
      return(NULL)
  }
}

recordTrace <- function(rec, name, where=topenv(parent.frame()), ...)
{
  i <- 1
  found <- NULL
  callback <- function(e)
  {
    if(identical(where, e)) found <<- i
    i <<- i + 1
  }
  
  iterate_environments(rec$environment, callback, rec$options$full_clone)
  
  if(is.null(found))
    stop("corresponding environment not found")

  env <- rec$cloned_envs[[found]]
  trace(name, where=env, ...)
}

replay <- function(replay_struct)
{
  if(!is.list(replay_struct)) stop("the replay structure is invalid (is not a list)")
  
  # clone all relevant environments so that we can later restore them to the state before replay
  cloned_envs <- clone_environments(parent.frame(), skip_replay_structures=FALSE, full_clone=replay_struct$options$full_clone)
  
  # replace all relevant environments with their before-record clone and remove them from the replay struct
  replace_environments(parent.frame(), replay_struct$cloned_envs, replay_struct$options$full_clone)
  replay_struct$cloned_envs <- NULL
  
  # replay the trace and return the result
  ret <- tryCatch(
    .Call(C_do_replay, replay_struct),
    error = function(cond) { print(cond); list(value=cond, visible=FALSE) }
  )
  
  # replace all relevant environments with their before-replay clone
  replace_environments(parent.frame(), cloned_envs, replay_struct$options$full_clone)
  
  # return visibly or invisibly depending on the recorded expression
  if(ret$visible)
    return(ret$value)
  else
    invisible(ret$value)
}

clone_environments <- function(start_env, skip_replay_structures, full_clone)
{
  envs <- list()
  i <- 1
  
  callback <- function(env)
  {
    new_env <- clone_environment(env, skip_replay_structures)
    envs[[i]] <<- new_env
    i <<- i + 1
  }
  
  iterate_environments(start_env, callback, full_clone)
  
  return(envs)
}

replace_environments <- function(start_env, envs, full)
{
  i <- 1
  
  callback <- function(env)
  {
    replace_environment(env, envs[[i]])
    i <<- i + 1
  }
  
  iterate_environments(start_env, callback, full)
}

clone_environment <- function(env, skip_replay_structures)
{
  new_env <- new.env()
  for(n in ls(envir=env, all.names=TRUE))
  {
    obj <- get(n, envir=env)
    
    if(!skip_replay_structures || !("RRnR_replay_structure" %in% names(attributes(obj))))
      assign(n, obj, envir=new_env)
  }
  parent.env(new_env) <- parent.env(env)
  
  for(n in ls(envir=env, all.names=TRUE))
    if(bindingIsLocked(n, env))
      lockBinding(n, new_env)

  if(environmentIsLocked(env))
    lockEnvironment(new_env, FALSE)
  
  return(new_env)
}

replace_environment <- function(dst_env, src_env)
{
  # must evaluate in advance as the params might be
  # inside the environment to be rewritten
  force(src_env)
  force(dst_env)
  
  .Call(C_do_unlock_env, dst_env)

  for(n in ls(envir=dst_env, all.names=TRUE))
  {
    unlockBinding(n, dst_env)
    rm(list=n, envir=dst_env)
  }
  
  for(n in ls(envir=src_env, all.names=TRUE))
  {
    assign(n, get(n, envir=src_env), envir=dst_env)
    
    if(bindingIsLocked(n, src_env))
      lockBinding(n, dst_env)
  }
  
  if(environmentIsLocked(src_env))
    lockEnvironment(dst_env, FALSE)
}

# go through all environments on the search path
# but skip the namespace and imports environments
# func > pkg ns > pkg imports > base ns > global > pkg > empty
iterate_environments <- function(start_env, callback, full)
{
  env <- start_env
  skip <- FALSE
  while(TRUE)
  {
    if(.Internal(isNamespaceEnv(env)))
      skip <- TRUE

    if(identical(env, .GlobalEnv))
      skip <- FALSE

    if(identical(env, baseenv()))
      break()

    if(!skip)
      callback(env)

    if(!full && identical(env, .GlobalEnv))
      break()

    env <- parent.env(env)
  }
}

lazyload_before <- function()
{
  .Call(C_do_lazyload_before)
}

lazyload_after <- function()
{
  .Call(C_do_lazyload_after)
}
