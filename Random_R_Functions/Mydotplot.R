Mydotplot <- function(input){
	n <- sqrt(ncol(input))
	if(n%%1==0){
		grid.x = grid.y = n
	} else {
		if(n %% 1 < 0.5){
			grid.y = n - (n %% 1)
			grid.x = grid.y + 1
		} else {
			grid.x = grid.y = (n - (n %% 1))+1
		}
	}

	par(mfrow=c(grid.y,grid.x))

	for(i in colnames(input)){
		if( is.character(input[,i]) ){
			x <- as.factor(input[,i])
		} else {
			x <- input[,i]
		}
		plot( x = as.numeric(x), y = seq_len(nrow(input)), 
			main=i, yaxt = "n", ylab="", xlab=paste("(",class(input[,i]),")",sep=""), 
			pch=16, cex=0.8, xaxt="n")
		if(is.factor(x)){
			axis(1, at = seq_len(length(levels(x))), labels = levels(x))
		} else {
			axis(1)
		}
	}

	par(mfrow=c(1,1))
}