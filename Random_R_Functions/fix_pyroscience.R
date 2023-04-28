setwd('/home/hflavio/Dropbox/Respirometry/18C_exposure/')


compile_run <- function(folder) {
	files <- list.files(paste0(folder, '/ChannelData/'))

	O2.file.link <- grepl('Oxygen.txt', files)

	if (all(!O2.file.link)) {
		stop('No oxygen files found')
	}

	O2.files <- files[O2.file.link]

	aux <- lapply(O2.files, function(i) {
		x <- load.pyroscience.o2.file(paste0(folder, '/ChannelData/', i), date.format = '%d-%m-%Y')
		x <- x[, c('Date.Time', 'Sample.CompT', 'Pressure.CompP', 'Oxygen.Main')]
		ch <- stringr::str_extract(i,'(?<=Ch.)[0-9]')
		colnames(x)[2:4] <- paste0(c('Temp.', 'Pressure.', 'Ox.'),  ch)
		return(x)
	})


	very.start <- min(as.POSIXct(sapply(aux, function(i) {
		as.character(min(i$Date.Time))
	})))

	very.end <- max(as.POSIXct(sapply(aux, function(i) {
		as.character(max(i$Date.Time))
	})))

	recipient <- data.frame(Date.Time = seq(from = very.start, to = very.end, by = 1))

	for (i in aux) {
		recipient <- merge(recipient, i, by = 'Date.Time', all = TRUE)
	}

	return(recipient)
}


x <- compile_run('2022-05-22_144725_18C_E293-296_SMR_CH5-8')

tail(x)