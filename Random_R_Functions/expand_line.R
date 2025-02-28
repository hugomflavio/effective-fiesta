expand_line <- function(x1, x2, y1, drop = 0, y2 = y1){
	if (length(drop) == 1) {
		drop[2] <- drop[1]
	}
	if (length(drop) == 2) {
		y1d <- y1 - drop[1]
		y2d <- y2 - drop[2]
	}
	output <- data.frame(x = c(x1, x1, x2, x2), y = c(y1d, y1, y2, y2d))
	return(output)
}


geom_signif <- function(p, x1, x2, y1, drop = 0, y2 = y1, 
						label_raise = 0, label = "*", colour = "black",
						text_colour = "black") {
	
	the_line <- expand_line(x1 = x1, x2 = x2, 
							y1 = y1, y2 = y2,
							drop = drop)

	p <- p + ggplot2::geom_line(data = the_line, 
								ggplot2::aes(x = x, y = y), colour = colour)
	p <- p + ggplot2::annotate("text",
							   x = mean(the_line$x), 
							   y = mean(c(y1, y2)) + label_raise, 
							   label = label, colour = text_colour,
							   hjust = 0.5, vjust = 0)
	return(p)
}
