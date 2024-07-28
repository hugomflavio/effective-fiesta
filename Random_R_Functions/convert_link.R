convert_link <- function(P, link = c("identity", "sqrt", "inverse", "invsqrt", "log", "logit", "cloglog")) {
	link <- match.arg(link)
	if (any(grepl("se.fit", names(P)))) {
		output <- data.frame(
			fit = rep(NA_real_, length(P$fit)),
			upper = rep(NA_real_, length(P$fit)),
			lower = rep(NA_real_, length(P$fit)))
		calculate.CI <- TRUE
	} else {
		output <- data.frame(fit = rep(NA_real_, length(P$fit)))
		calculate.CI <- FALSE
	}
	if (link == "identity") {
		output$fit <- P$fit
		if (calculate.CI) {
			output$upper  <- P$fit + 1.96*P$se.fit
			output$lower <- P$fit - 1.96*P$se.fit
		}
	}
	if (link == "sqrt") {
		output$fit <- (P$fit)^2
		if (calculate.CI) {
			output$upper  <- (P$fit + 1.96*P$se.fit)^2
			output$lower <- (P$fit - 1.96*P$se.fit)^2
		}
	}
	if (link == "inverse") {
		output$fit <- (P$fit)^-1
		if (calculate.CI) {
			output$upper  <- (P$fit - 1.96*P$se.fit)^-1
			output$lower <- (P$fit + 1.96*P$se.fit)^-1
		}
	}
	if (link == "invsqrt") {
		output$fit <- (P$fit)^(-1/2)
		if (calculate.CI) {
			output$upper  <- (P$fit + 1.96*P$se.fit)^(-1/2)
			output$lower <- (P$fit - 1.96*P$se.fit)^(-1/2)
		}
	}
	if (link == "log") {
		output$fit <- exp(P$fit)
		if (calculate.CI) {
			output$upper  <- exp(P$fit + 1.96*P$se.fit)
			output$lower <- exp(P$fit - 1.96*P$se.fit)
		}
	}
	if (link == "logit") {
		output$fit <- exp(P$fit) / (1 + exp(P$fit))
		if (calculate.CI) {
			output$upper <- exp(P$fit + 1.96*P$se.fit) / (1 + exp(P$fit + 1.96*P$se.fit))
			output$lower <- exp(P$fit - 1.96*P$se.fit) / (1 + exp(P$fit - 1.96*P$se.fit))
		}
	}
	if (link == "cloglog") {
		output$fit <- 1 - exp(-exp(P$fit))
		if (calculate.CI) {
			output$upper <- 1 - exp(-exp(P$fit + 1.96*P$se.fit))
			output$lower <- 1 - exp(-exp(P$fit - 1.96*P$se.fit))
		}
	}
	return(output)
}