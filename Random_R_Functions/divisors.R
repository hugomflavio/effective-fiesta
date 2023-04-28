divisors <- function(x){
	y <- seq_len(x)
	y[ x%%y == 0 ]
}
