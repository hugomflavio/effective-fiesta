create.year.stats <- function(input,firstcol,lastcol) {
	years <- sort(unique(input$year))
	rows <- seq(firstcol,lastcol, by=1)
	output <- matrix(nrow=length(rows),ncol=length(years)+1)

	for( i in 1:length(years) ){
		for(j in 1:length(rows) ){
		output[j,i] <- sum(input[input$year == years[i], rows[j]])
		}
	}
	for( j in 1:length(rows) ){
		output[j,ncol(output)] <- sum(output[j,1:ncol(output)-1])
	}
	output <- as.data.frame(output)
	colnames(output) <- c(years,"Overall")
	rownames(output) <- colnames(input)[rows]
	return(output)
}

