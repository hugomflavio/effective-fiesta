mean_table <- function(x, by, ci = c("none", "sd", "sem"), na.rm = FALSE) {
	sem <- function(x, na.rm) {sd(x, na.rm = na.rm) / sqrt(length(x))}
	
	ci <- match.arg(ci)
	output <- as.data.frame(aggregate(x, by, mean, na.rm = na.rm))
	colnames(output) <- c("group", "mean")

	output$n <- as.vector(aggregate(x, by, function(x) sum(!is.na(x)))$x)

	output <- output[, c("group", "n", "mean")]

	if (ci == "sd") {
		output$sd <- as.vector(aggregate(x, by, sd, na.rm = na.rm)$x)
		output$ci_up <- output$mean + output$sd
		output$ci_dn <- output$mean - output$sd
	}

	if (ci == "sem") {
		output$sem <- as.vector(aggregate(x, by, sem, na.rm = na.rm)$x)
		output$ci_up <- output$mean + output$sem
		output$ci_dn <- output$mean - output$sem
	}

	return(output)
}
