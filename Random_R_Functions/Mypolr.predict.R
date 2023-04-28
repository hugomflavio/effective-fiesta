Mypolr.predict <- function(model,newdata,trim=NA){
	require(effects)
	probs <- suppressMessages(Effect(names(newdata), model, xlevels=newdata))
	input <- as.data.frame(probs)
	recipient <- data.frame()
	for( i in probs$y.levels ){
		if(!grepl("[^0-9.]",i)){ i.match <- paste("X",i,sep="") }else{ i.match <- i }
		add <- cbind(input[,names(newdata)],rep(i,length.out=nrow(input)),input[,c(gsub(" ",".",paste(c("prob","L.prob","U.prob"),i.match,sep=".")))])
		colnames(add) <- c(names(newdata),probs$response,"fit","CIlow","CIhigh")
		recipient <- rbind(recipient,add)
		rm(add)
	}
	rm(i,i.match)
	if( exists("trim") && inherits(trim,"list") ){
		fX <- names(trim[[1]])
		cX <- colnames(trim[[1]][[1]])
		for( i in fX ){
			for( k in cX ){
				trimmed <- recipient[,match(names(trim),colnames(recipient))] == i & (recipient[,k]<trim[[1]][[i]][1,k] | recipient[,k]>trim[[1]][[i]][2,k])
			}
			cat("M: Trimming ",sum(trimmed)," rows in the '",i,"' group.\n",sep="")
			recipient <- recipient[!trimmed,]
			rm(trimmed)
		}
	}
	return(recipient)
}

