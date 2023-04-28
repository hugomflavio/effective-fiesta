dfapply <- function(df, columns, FUN){
	FUN <- match.fun(FUN)
	for(i in columns){
		df[, i] <- forceAndCall(1, FUN, df[, i])
	}
	return(df)
}