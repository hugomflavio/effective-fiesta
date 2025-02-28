mean_table <- function(x, by, ci = c("none", "sd", "sem", "95"),
					   na.rm = FALSE, digits = Inf) {
	sem <- function(x, na.rm) {sd(x, na.rm = na.rm) / sqrt(length(x))}
	ci <- match.arg(ci)

	output <- as.data.frame(aggregate(x, by, function(x) sum(!is.na(x))))
	colnames(output)[ncol(output)] <- "n"

	output$mean <- as.vector(aggregate(x, by, mean, na.rm = na.rm)$x)

	if (ci == "sd") {
		output$sd <- as.vector(aggregate(x, by, sd, na.rm = na.rm)$x)
		output$upper <- round(output$mean + output$sd, digits)
		output$lower <- round(output$mean - output$sd, digits)
		output$sd <- round(output$sd, digits)
	}

	if (ci == "sem") {
		output$sem <- as.vector(aggregate(x, by, sem, na.rm = na.rm)$x)
		output$upper <- round(output$mean + output$sem, digits)
		output$lower <- round(output$mean - output$sem, digits)
		output$sem <- round(output$sem, digits)
	}

	if (ci == "95") {
		output$upper <- round(as.vector(aggregate(x, by, quantile, 0.975)$x), digits)
		output$lower <- round(as.vector(aggregate(x, by, quantile, 0.025)$x), digits)
	}

	output$mean <- round(output$mean, digits)
	
	return(output)
}
