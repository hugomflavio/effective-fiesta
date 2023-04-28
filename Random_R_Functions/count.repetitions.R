count.repetitions <- function(provider,target) {
	output <- vector(mode="integer", length=length(target))
	names(output) <- target
	for( i in 1:length(target) ){
		output[i] <- sum(provider == as.character(target[i]))
	}
	return(output)
}

