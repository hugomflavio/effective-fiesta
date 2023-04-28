obj <- function(){ 
  all.names <- .Internal(ls(envir=.GlobalEnv, all.names=FALSE, sorted=TRUE))
  setdiff(all.names, lsf.str(envir=.GlobalEnv)) 
}
# Only lists objects, not the user-defined functions