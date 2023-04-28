convertLink <- function(P, link = c("identity", "sqrt", "inverse", "invsqrt", "log", "logit", "cloglog")) {
	link <- match.arg(link)
	if (any(grepl("se.fit", names(P)))) {
		output <- data.frame(
			Fit = rep(NA_real_, length(P$fit)),
			CI.up = rep(NA_real_, length(P$fit)),
			CI.dn = rep(NA_real_, length(P$fit)))
		calculate.CI <- TRUE
	} else {
		output <- data.frame(Fit = rep(NA_real_, length(P$fit)))
		calculate.CI <- FALSE
	}
	if (link == "identity") {
		output$Fit <- P$fit
		if (calculate.CI) {
			output$CI.up  <- P$fit + 1.96*P$se.fit
			output$CI.dn <- P$fit - 1.96*P$se.fit
		}
	}
	if (link == "sqrt") {
		output$Fit <- (P$fit)^2
		if (calculate.CI) {
			output$CI.up  <- (P$fit + 1.96*P$se.fit)^2
			output$CI.dn <- (P$fit - 1.96*P$se.fit)^2
		}
	}
	if (link == "inverse") {
		output$Fit <- (P$fit)^-1
		if (calculate.CI) {
			output$CI.up  <- (P$fit - 1.96*P$se.fit)^-1
			output$CI.dn <- (P$fit + 1.96*P$se.fit)^-1
		}
	}
	if (link == "invsqrt") {
		output$Fit <- (P$fit)^(-1/2)
		if (calculate.CI) {
			output$CI.up  <- (P$fit + 1.96*P$se.fit)^(-1/2)
			output$CI.dn <- (P$fit - 1.96*P$se.fit)^(-1/2)
		}
	}
	if (link == "log") {
		output$Fit <- exp(P$fit)
		if (calculate.CI) {
			output$CI.up  <- exp(P$fit + 1.96*P$se.fit)
			output$CI.dn <- exp(P$fit - 1.96*P$se.fit)
		}
	}
	if (link == "logit") {
		output$Fit <- exp(P$fit) / (1 + exp(P$fit))
		if (calculate.CI) {
			output$CI.up <- exp(P$fit + 1.96*P$se.fit) / (1 + exp(P$fit + 1.96*P$se.fit))
			output$CI.dn <- exp(P$fit - 1.96*P$se.fit) / (1 + exp(P$fit - 1.96*P$se.fit))
		}
	}
	if (link == "cloglog") {
		output$Fit <- 1 - exp(-exp(P$fit))
		if (calculate.CI) {
			output$CI.up <- 1 - exp(-exp(P$fit + 1.96*P$se.fit))
			output$CI.dn <- 1 - exp(-exp(P$fit - 1.96*P$se.fit))
		}
	}
	return(output)
}