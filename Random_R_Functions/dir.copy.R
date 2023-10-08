dir.copy <- function (from, to, add = FALSE) {
	if (dir.exists(to) & !add) {
		stop("'to' already exists. use add = TRUE to copy files to existing directory.")
	}
	if (!dir.exists(to)) {
		dir.create(to)
	}
	file.copy(list.files(from, full.names = TRUE), 
    	      to, 
        	  recursive = TRUE)
}
