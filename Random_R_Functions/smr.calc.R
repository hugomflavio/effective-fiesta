smr.calc <- function(data){
	data <- as.vector(data)
	the.Mclust <- Mclust(data, G=1:4, verbose=F)
	cl <- the.Mclust$classification
	cl2 <- as.data.frame(table(cl)) 
	valid <- cl2$Freq>=0.1*length(data)
	the.cl <- min(as.numeric(cl2$cl[valid]))
	mlnd <- the.Mclust$parameters$mean[the.cl]
	left.distr <- data[the.Mclust$classification==the.cl]
	CVmlnd <- sd(left.distr)/mlnd*100
	return(list(mlnd=mlnd,CVmlnd=CVmlnd))
}
