simpleCJS <- function(input, estimate = NULL){
	# The math below is based on 'Using mark-recapture models to estimate survival from telemetry data' by Perry et al. 2012
	# The model is based on mainputimum likelihood estimators derived from equations (i.e. the analytical approach)
	S <- rep(NA, ncol(input)-1)
	r <- z <- p <- m <- M <- rep(NA, ncol(input))
	for(i in 1:(ncol(input)-1)){
		# number of tags detected at i and downstream (r)
		tr <- input[input[, i] == 1, (i + 1) : ncol(input)]
		if (is.data.frame(tr))
			r[i] = sum(apply(tr, 1, function(f) any(f == 1)))
		if (is.vector(tr))
			r[i] = sum(tr)
		# number of tags NOT detected at i but detected downstream (z)
		tz <- input[input[, i] == 0, (i + 1) : ncol(input)]
		if (is.data.frame(tz))
			z[i] = sum(apply(tz, 1, function(f) any(f == 1)))
		if (is.vector(tz))
			z[i] = sum(tz)
		# probability of detection (p)
		p[i] <- r[i] / (r[i] + z[i])
		# number of detected tags at i (m)
		m[i] = sum(input[, i])
		# number of fish estimated alive at i (M)
		M[i] = round(m[i] / p[i], 0)
		if (i == 1 && M[i] > nrow(input))
			M[i] = nrow(input)
		if (i > 1 && M[i] > M[i - 1])
			M[i] = M[i - 1]
		# survival probability from i-1 to i (S)
		if (i > 1)
			S[i - 1] <- M[i] / M[i - 1]
		# lambda (last S * last P) 
		if (i == (ncol(input)-1))
			l <- r[i] / m[i]
	}
	m[ncol(input)] <- sum(input[, ncol(input)])
	if (!is.null(estimate)) {
		S[ncol(input)-1] <- l / estimate
		if (S[ncol(input)-1] > 1)
			S[ncol(input)-1] = 1
		M[ncol(input)] <- round(m[ncol(input)] / estimate,0)
		if (M[ncol(input)] > M[ncol(input) - 1])
			M[ncol(input)] = M[ncol(input) - 1]
	}

	absolutes <- matrix(c(m, r, z, M), ncol = ncol(input), byrow = TRUE)
	rownames(absolutes) <- c("detected", "here plus downstream", "not here but downstream", "estimated")
	colnames(absolutes) <- colnames(input)
	names(p) <- colnames(input)
	survival <- matrix(S, nrow = length(S))
	
	the.rows <- c()
	for(i in 2:ncol(input)){
		maxcharA <- max(nchar(colnames(input)[-ncol(input)]))
		maxcharB <- max(nchar(colnames(input)[-1]))
		if(nchar(colnames(input)[i - 1] != maxcharA))
			centeringA <- paste(rep(" ", maxcharA - nchar(colnames(input)[i - 1])), collapse = "")
		else
			centeringA <- ""
		if(nchar(colnames(input)[i] != maxcharB))
			centeringB <- paste(rep(" ", maxcharB - nchar(colnames(input)[i])), collapse = "")
		else
			centeringB <- ""
		the.rows[i - 1] <- paste(centeringA, colnames(input)[i - 1], " -> ", colnames(input)[i], centeringB, " =", sep = "")
	}
	rownames(survival) <- the.rows
	colnames(survival) <- ""
	if (is.null(estimate))
		return(list(absolutes = absolutes, efficiency = p, survival = survival, lambda = l))
	else
		return(list(absolutes = absolutes, efficiency = p, survival = survival, lambda = l, estimate = estimate))
}

dualArray <- function(input){
	r <- z <- p <- m <- M <- rep(NA, ncol(input))
	for(i in 1:(ncol(input))){
		# number of tags detected at i and elsewhere (r)
		tr <- input[input[, i] == 1, -i]
		if (is.data.frame(tr))
			r[i] = sum(apply(tr, 1, function(f) any(f == 1)))
		if (is.vector(tr))
			r[i] = sum(tr)
		# number of tags NOT detected at i but detected elsewhere (z)
		tz <- input[input[, i] == 0, -i]
		if (is.data.frame(tz))
			z[i] = sum(apply(tz, 1, function(f) any(f == 1)))
		if (is.vector(tz))
			z[i] = sum(tz)
		# probability of detection (p)
		p[i] <- r[i] / (r[i] + z[i])
		# number of detected tags at i (m)
		m[i] = sum(input[, i])
		# number of fish estimated alive at i (M)
		M[i] = m[i] / p[i]
		if (M[i] > nrow(input))
			M[i] = nrow(input)
	}
	combined.p <- 1 - prod(1 - p)
	absolutes <- matrix(c(apply(input, 2, sum), r[1]), nrow = 3)
	colnames(absolutes) <- ""
	rownames(absolutes) <- c("detected at original:", "detected at replicate:", "detected at both:")
	names(p) <- c("original", "replicate")
	return(list(absolutes = absolutes, single.efficiency = p, combined.efficiency = combined.p))
}
