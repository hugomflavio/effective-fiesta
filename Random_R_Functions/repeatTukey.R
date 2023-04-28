#' Perform multiple tukey tests and return the p-values for each comparison
#' 
#' @param m the model
#' @param v the variable being analysed
#' @param n the number of repetitions
#' 
#' @return a data table with the p values for each round
#' 
repeatTukey <- function(m, v, n) {
  pb <- txtProgressBar(min = 0, max = n, style = 3, width = 60)
	aux <- lapply(seq(1:n), function(i) {
		test <- paste0("summary(multcomp::glht(m, mcp('",v ,"' = 'Tukey')))")
		x <- suppressWarnings(eval(parse(text = test)))
		output <- as.vector(x$test$pvalues)
		names(output) <- attributes(x$test$tstat)$names
    setTxtProgressBar(pb, i)
		return(as.data.frame(t(output)))
	})
	close(pb)
	output <- data.table::rbindlist(aux)
	return(output)
}
