#' @param repo The GitHub repository.
#' @param branch The branch within the Github repository.
#' @param file The file to be loaded (from the specified branch)
#' 
#' @return Sources the file
#' 
git_source <- function(repo = "hugomflavio/effective-fiesta", branch = "main", folder = "Random_R_Functions", file) {
	repo_url <- paste0("https://github.com/", repo)
	if (!RCurl::url.exists(repo_url)) {
		stop("Could not find repository (", repo_url, ").", call. = FALSE)
	}

	branch_url <- paste0(repo_url, "/tree/", branch)
	if (!RCurl::url.exists(branch_url)) {
		stop("Could not find branch within repository (", branch_url, ").", call. = FALSE)
	}

	file_url <- paste0(repo_url, "/blob/", branch, "/", folder, "/", file)

	if (!RCurl::url.exists(file_url)) {
		stop("Could not find target file within branch (", file_url, ").", call. = FALSE)
	}

	raw_file_url <- paste0("https://raw.githubusercontent.com/", repo, "/", branch, "/", folder, "/", file)
	
	before_source <- ls(envir=.GlobalEnv)

	source(raw_file_url)

	after_source <- ls(envir=.GlobalEnv)

	new_objects <- after_source[!(after_source %in% before_source)]

	if (length(new_objects) == 0)
		warning("No new objects were loaded. Previously existing objects could have been overwritten.", call. = FALSE)
	
	if (length(new_objects) == 1)
		message("The following object was sourced: ", new_objects)
	
	if (length(new_objects) > 1)
		message("The following objects were sourced: ", paste(new_objects, collapse = ", "))
}
