#' Apply a function to all the matching files in a directory
#' 
#' @param path Directory path to the target files
#' @param pattern Group of characters specific to the target files' names
#' @param FUN Functions to be applied at file import
#' 
dirWrap <- function(path = getwd(), pattern, FUN){
	FUN <- match.fun(FUN)
  if (file_test("-d", path)) {
	  file.list <- list.files(path, pattern = pattern)
	} else {
		stop("Could not find target path.\n")
	}
	if (length(file.list) == 0){
		stop("No files were found that match the pattern.\n")
	}
	recipient <- list()
	for(i in file.list){
		recipient[[length(recipient)+1]] <- forceAndCall(1, FUN, paste(path, i, sep = "/"))
		names(recipient)[length(recipient)] <- i
	}
	return(recipient)
}