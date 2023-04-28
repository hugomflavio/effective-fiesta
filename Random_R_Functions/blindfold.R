blindfold <- function(x,columns="all",blind.var=FALSE, file=NULL){
	.write.list <- function (z, file, blind.var, ...) # adapted from Changyou Sun, see https://www.rdocumentation.org/packages/erer/versions/2.5/topics/write.list
	{
	    if (!inherits(z, "list")) 
	        stop("\nNeed an 'list' object.\n")
	    if (is.null(names(z))) {
            add.name <- paste("result.", 1:length(z), sep = "")
        }
        else {
            add.name <- names(z)
        }
		options(warn = -1)
	    for (k in 1:length(z)) {
	        dat <- as.data.frame(z[[k]])
            h2 <- as.data.frame(cbind(Variable = add.name[k], 
            						 row.name = rownames(dat), 
            						 dat))
	        ap <- ifelse(k == 1, FALSE, TRUE)
	        write.table(x = h2, file = file, sep = ",", append = ap, 
	            row.names = FALSE, ...)
	    }
	    options(warn = 0)
	}
	output <- x
	if( columns=="all" ){
		columns <- colnames(x)
	} 
	else {
		if( any(is.na(match(columns,colnames(x)))) ) stop("Undefined columns selected")
	}
	match.list <- list()
	for( i in columns ){
		if( inherits(x[,i],"factor") ){
			blind.match <- data.frame(Original=levels(x[,i]),Blindfold=sample(LETTERS,length(levels(x[,i])),replace=FALSE))
			output[,i] <- factor(blind.match[as.numeric(x[,i]),2],levels=levels(blind.match[,2]))
			match.list[[i]] <- blind.match
		}
		if( inherits(x[,i],"logical") ){
			blind.match <- data.frame(Original=unique(x[,i]),Blindfold=sample(LETTERS,length(unique(x[,i])),replace=FALSE))
			output[,i] <- factor(blind.match[match(x[,i],blind.match[,1]),2],levels=levels(blind.match[,2]))
			match.list[[i]] <- blind.match
		}
		if( inherits(x[,i],"numeric") | inherits(x[,i],"integer") ){
			match.list[[i]] <- "Originals kept"
		}
	}
	if( blind.var ){
		colnames(output)[match(columns,colnames(output))] <- paste("Var",seq_len(length(columns)),sep="")
		names(match.list)[match(columns,names(match.list))] <- paste("Var",seq_len(length(columns)),"_",columns,sep="")
	}
	if( is.character(file) ){
		.write.list(match.list,blind.var,file=file)
		return(output)
	}
	else {
		return(list(output=output,match.list=match.list))
	}
}