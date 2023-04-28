facet.line <- function(input,facet.name="facet"){
	expand.line <- function(x1,x2,y1,height=0,y2=y1){
		y3 <- max(c(y1,y2)) + height 
		output <- data.frame(x = c(x1,x1,x2,x2), y = c(y1,y3,y3,y2))
	return(output)
	}
	if(!inherits(input,"data.frame")){
		stop("Input must be a dataframe containing values to pass to 'expand.line'.")
	}
	output <- data.frame(x=vector(),y=vector(),facet=character())
	for(i in 1:ncol(input) ){
		if(nrow(input)==4) temp <- expand.line(input[1,i],input[2,i],input[3,i],input[4,i])
		if(nrow(input)==5) temp <- expand.line(input[1,i],input[2,i],input[3,i],input[4,i],input[5,i])
		temp$facet <- rep(colnames(input)[i],4)
		output <- rbind(output,temp)
	}
	colnames(output)[3] <- facet.name
	return(as.data.frame(output))
}
