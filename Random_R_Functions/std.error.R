std.error <- function(x, na.rm = TRUE){
    a <- length(x)

    if(na.rm) 
        x <- x[!is.na(x)]

    if(a != length(x)) 
        message("M: Omitted ", a - length(x), " missing values.")

    output <- sd(x) / sqrt(length(x))
 
    return(output)
}
