if (!exists("backup_data")) backup_data <- TRUE

if (backup_data) {
	if (!exists(last_line)) last_line <- 0
	write.csv(output[(last_line - 1):nrow(output), ], "temperature_log.csv", append = file.exists("temperature_log.csv"))
	if (nrow(output) > 17280) {
		output <- output[(nrow(output)-17280):nrow(output), ]
	}
	last_line <- nrow(output)
	backup_data <- FALSE
}

if (format(Sys.time(), "%H:%M") == "08:00") {
	process_interval <- 60 * 60 * 1
}

if (format(Sys.time(), "%H:%M") == "20:00") {
	process_interval <- Inf
}

if (difftime(Sys.time(), last_processed, units = "sec") > process_interval)) {
	send_update <- TRUE
}

if (send_update)  {
	message(paste0(Sys.time(),": Running process_info"))
	last_processed <- Sys.time()

	x_limits <- as.character(c((Sys.time() - 60*60*24), Sys.time()))
	x_limits <- as.POSIXct(x_limits, tz = "America/Toronto")

	p <- ggplot(output, aes(x = Timestamp, y = T))
	p <- p + geom_line()
	p <- p + scale_x_datetime(limits = x_limits, expand = c(0, 0))
	rm(p)
	ggsave("last_24h.png", width = 4, height = 6)

	if (file.exists("last_24h.png")) {
		slackr_upload("last_24h.png", initial_comment = "```Here is the trace of the last 24 hours in CR114A:```", channel = "sturgeon-bot")
		file.remove("last_24h.png")
	}
}

send_update <- FALSE