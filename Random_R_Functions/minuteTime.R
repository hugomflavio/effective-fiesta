minuteTime <- function(x, format = c("h", "m", "s"), seconds = TRUE) {
	format <- match.arg(format)
	.converter <- function(x) {
	 	if(!is.na(x)){
	 		if(x < 0){
			 	x <- abs(x)
			 	neg = TRUE
			} else neg = FALSE
			if(format == "h") 
				x = x
			if(format == "m") 
				x = x/60
			if(format == "s") 
				x = x/3600
			m = x %% 1
		  h = x - m
		  m = 60 * m
		  s = m %% 1
		  m = m - s
		  s = round(60 * s, 0)
		  if (h < 10) h <- paste(0, h, sep = "")
		  if (!seconds & s>30) m = m+1
		  if (m < 10) m <- paste(0,m,sep="")
		  if (s < 10) s <- paste(0,s,sep="")
		  if (seconds) 
		  	x <- paste(h, m, s, sep = ":")
		  else 
		  	x <- paste(h, m, sep = ":")
		  if (neg) x <- paste("-", x)
		}
	  return(x)
	}
	if (length(x) < 1) stop("Input appears to be empty.")
	if (!is.numeric(x)) stop("Input is not numeric.")
	if (length(x) == 1) output <- .converter(x)
	if (length(x) > 1) output <- unlist(lapply(x, .converter))
	return(output)
}
