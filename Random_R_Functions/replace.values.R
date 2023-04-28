replace.values <- function(receiver,sender,reccol,sendcol)
{	if ( !reccol %in% colnames(receiver) ) {
		cat("Function Note: Receiver column '",as.character(reccol),"' unexistant, new column created \n", sep="")
		receiver[,as.character(reccol)] <- NA
	}
	temp <- match(sender$Code,receiver$Code)
	temp2 <- sender$Code %in% receiver$Code
	receiver[temp[!is.na(temp)],as.character(reccol)] <- sender[temp2,as.character(sendcol)]
	return(receiver)	
}

