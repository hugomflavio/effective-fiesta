expand_line <- function(x1, x2, y1, drop = 0, y2 = y1){
		if (length(drop) == 1)
			y1d <- y2d <- min(c(y1, y2)) - drop
		if (length(drop) == 2) {
			y1d <- y1 - drop
			y2d <- y2 - drop
		}
		output <- data.frame(x = c(x1, x1, x2, x2), y = c(y1d, y1, y2, y2d))
	return(output)
}
