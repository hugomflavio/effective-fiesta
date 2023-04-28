#' Extract day, sunrise and sunset from sunrise-and-sunset.com saved pages.
#'
#' @param file The name of the input file
#' @param tz The timezone of the extracted data
#' 
getSunTimes <- function(file, tz){
	input <- readChar(file, nchar = file.info(file)$size)
	extract <- unlist(stringr::str_extract_all(pattern = '<time datetime="[^>]*', string = input))
	extract <- gsub(pattern = "<time datetime=", replacement = "", x = extract)
	extract <- gsub(pattern = "\"", replacement = "", x = extract)
	extract <- gsub(pattern = "T", replacement = " ", x = extract)
	output <- matrix(data = extract, ncol = 4, byrow = TRUE)
	output <- as.data.frame(output[,1:3], stringsAsFactors = FALSE)
	colnames(output) <- c("Day","Sunrise","Sunset")
	rownames(output) <- 1:nrow(output)
	output$Day <- as.POSIXct(output$Day, tz = tz)
	output$Sunrise <- as.POSIXct(output$Sunrise, tz = tz)
	output$Sunset <- as.POSIXct(output$Sunset, tz = tz)
	return(output)
}
