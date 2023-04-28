expand.line <- function(x1,x2,y1,height=0,y2=y1){
	message("New improved expand_line function already exists (expand_line.R).")
		y3 <- max(c(y1,y2)) + height 
		output <- data.frame(x = c(x1,x1,x2,x2), y = c(y1,y3,y3,y2))
	return(output)
}
