multi.matchl <- function(...,type=c("Names","Simple","Detailed")){
	type <- match.arg(type)
	matchl <- function(original,match){
		!is.na(match(original,match))
	}
	objects <- list(...)
	names(objects) <- as.character(substitute(list(...))[-1])
	output <- list()
	for( i in seq_len(length(objects)) ){
		recipient <- data.frame(Names=objects[i])
		for( j in seq_len(length(objects)) ){
			if(i!=j){
				recipient[,(ncol(recipient)+1)] <- matchl(objects[[i]],objects[[j]])
				colnames(recipient)[ncol(recipient)] <- names(objects)[j]
			}
		}
		output[[i]] <- recipient 
		names(output)[i] <- names(objects)[i]
	}
	if( type == "Detailed" )	return(output)
	if( type == "Simple" ){
		new.output <- output
		for( i in seq_len(length(output)) ){
			if(ncol(output[[i]])==2){
				new.output[[i]] <- output[[i]][,2]
			} else {
				new.output[[i]] <- apply(output[[i]][,-1],1,all)
			}
		}
		return(new.output)
	}
	if( type == "Names" ){
		if(ncol(output[[i]])==2){
			new.output <- objects[[1]][output[[1]][,2]]
		} else {
			new.output <- objects[[1]][apply(output[[1]][,-1],1, all)]
		}
		return(new.output)
	}
}


