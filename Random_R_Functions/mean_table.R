mean_table <- function(x, by, ci = c("none", "sd", "sem", "95", "range"),
					   na.rm = FALSE, digits = Inf) {
	sem <- function(x, na.rm) {sd(x, na.rm = na.rm) / sqrt(length(x))}
	ci <- match.arg(ci)

	if (missing(by)) {
		by <- list(all = rep("all", length(x)))
	}
	if (!is.list(by)) {
		var_name <- deparse(substitute(by))
		by <- list(group = by)
		names(by) <- gsub("^[^\\$]*\\$", "", var_name)
	}

	output <- as.data.frame(aggregate(x, by, function(x) sum(!is.na(x))))
	colnames(output)[ncol(output)] <- "n"

	output$mean <- as.vector(aggregate(x, by, mean, na.rm = na.rm)$x)

	if (ci == "sd") {
		output$sd <- as.vector(aggregate(x, by, sd, na.rm = na.rm)$x)
		output$lower <- round(output$mean - output$sd, digits)
		output$upper <- round(output$mean + output$sd, digits)
		output$sd <- round(output$sd, digits)
	}

	if (ci == "sem") {
		output$sem <- as.vector(aggregate(x, by, sem, na.rm = na.rm)$x)
		output$lower <- round(output$mean - output$sem, digits)
		output$upper <- round(output$mean + output$sem, digits)
		output$sem <- round(output$sem, digits)
	}

	if (ci == "95") {
		output$lower <- round(
			as.vector(
				aggregate(x, by, quantile, 0.025, na.rm = na.rm)$x
			),
			digits
		)
		output$upper <- round(
			as.vector(
				aggregate(x, by, quantile, 0.975, na.rm = na.rm)$x
			),
			digits
		)
	}

	if (ci == "range") {
		output$min <- round(
			as.vector(
				aggregate(x, by, min, na.rm = na.rm)$x
			),
			digits
		)
		output$max <- round(
			as.vector(
				aggregate(x, by, max, na.rm = na.rm)$x
			),
			digits
		)
	}

	output$mean <- round(output$mean, digits)
	
	return(output)
}
