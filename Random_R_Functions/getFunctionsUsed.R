#' Crawl through R files to extract the functions used
#' 
#' @param files a (list of) file(s) to crawl through
#' @param exclude.base Logical: Should functions that belong to the base package be excluded from the output?
#' 
#' @return The functions used in each file
#' 
getFunctionsUsed <- function(files, exclude.base = FALSE) {
  functions.used <- list()
  
  for (file in files) {

    x <- readLines(file)
    
    a <- gsub("[^\\: & ^a-z & ^A-Z & ^\\. & ^\\_ & \\( & 0-9 & \\[ & \\]]", "", x)
    a <- gsub("#[^#]*", "", a)
    a <- gsub('\"[^\"]*\"', "", a)
    a <- gsub("\'[^\']*\'", "", a)
    a <- gsub("\\t", "", a)
    
    b <- a[grepl("\\(", a)]
    
    y <- strsplit(b, "\\(")

    z <- unlist(lapply(y, function(j) j[-length(j)]))
    
    aux <- stringr::str_extract(z, '[^\\ & ^\\[ & ^! & \\" & ^, & ^:]*$')
    aux <- unique(aux[aux != ""])
    
    if (exclude.base)
      aux <- aux[is.na(match(aux, lsf.str("package:base")))]
    
    functions.used[[length(functions.used) + 1]] <- sort(aux)
    names(functions.used)[[length(functions.used)]] <- file
  }

  return(functions.used)
}
 
