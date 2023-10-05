if (!exists("datapack")) {
	datapack <- read.serialConnection(myArduino)
}

while(datapack == "") {
	Sys.sleep(1)
	datapack <- read.serialConnection(myArduino)
}

values <- strsplit(datapack, "\n")[[1]]
values <- values[values != ""]
if (length(values) > 0) {
	aux <- lapply(values, function(i) {
		x <- strsplit(i, "\t")[[1]]
		x <- as.numeric(x)
		output <- data.frame(Timestamp = NA, T = x[1], Millis = x[2])
		return(output)
	})

	recipient <- do.call(rbind, aux)
	recipient$Timestamp <- Sys.time()
	recipient$Timestamp <- recipient$Timestamp - (recipient$Millis - recipient$Millis[nrow(recipient)])/1000

	recipient$T <- round(coef(calibration)[2]*recipient$T + coef(calibration)[1], 2)
	recipient <- recipient[recipient$T > 0, ]

	if (nrow(recipient) > 0) {
		if (exists("output")) {
			output <- rbind(output, recipient)
		} else {
			output <- recipient
		}
		message(output$T[nrow(output)])
	}
}

datapack <- ""

