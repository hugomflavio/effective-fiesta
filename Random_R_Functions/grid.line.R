grid.line <- function(input,facet.head="facet"){
	.expand.line <- function(x1,x2,y1,y2=y1,height=0){
			y3 <- max(c(y1,y2)) + height 
			output <- data.frame(x = c(x1,x1,x2,x2), y = c(y1,y3,y3,y2))
		return(output)
	}

	if( !inherits(input,"list") ){
		cat("E: Input must be a list containing the input for expand.line and named after the levels which guide facet_grid().\n")
	} else {
		recipient <- data.frame(x=numeric(),y=numeric(),facet=character())
		for(i in names(input)){
			to.add <- .expand.line(input[[i]][1],input[[i]][2],input[[i]][3],input[[i]][4],input[[i]][5])
			to.add$facet <- rep(names(input[i]),4)
			recipient <- rbind(recipient,to.add)
			rm(to.add)
		}
		colnames(recipient)[3] <- facet.head
		recipient[,3] <- as.factor(recipient[,3])
		return(recipient)
	}
}

