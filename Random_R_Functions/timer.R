timer <- function(t, eternal = FALSE) {
	loadf("minuteTime.R")

	go.again <- TRUE
	counter <- 1

	if (eternal)
		message("Looping forever. To stop, cancel function.")

	while( go.again) {

		if (!eternal)
			go.again <- FALSE

		for (i in 0:(t-1)) {
			if (eternal)
				message(paste0(minuteTime(t - i, format = 's'), ' (round ', counter, ')\r'), appendLF = FALSE)
			else
				message(paste0(minuteTime(t - i, format = 's'), '\r'), appendLF = FALSE)
			Sys.sleep(1)
		}
		
		callr::r_bg(function(beep) beep(), args = list(beep = beep))

		message("TIMER UP!\r", appendLF = FALSE)
			Sys.sleep(0.2)
		message("         \r", appendLF = FALSE)
			Sys.sleep(0.2)
		message("TIMER UP!\r", appendLF = FALSE)
			Sys.sleep(0.2)
		message("         \r", appendLF = FALSE)
			Sys.sleep(0.2)
		message("TIMER UP!\r", appendLF = FALSE)
			Sys.sleep(0.2)
		message("         \r", appendLF = FALSE)
			Sys.sleep(0.2)

		if (eternal) {
			message("Restarting timer.         \r", appendLF = FALSE)
			Sys.sleep(0.2)
			message("Restarting timer..        \r", appendLF = FALSE)
			Sys.sleep(0.2)
			message("Restarting timer...       \r", appendLF = FALSE)
			Sys.sleep(0.2)
			message("Restarting timer....      \r", appendLF = FALSE)
			Sys.sleep(0.2)
			message("Restarting timer.....     \r", appendLF = FALSE)
			Sys.sleep(0.2)
			message("                          \r", appendLF = FALSE)
		} else {
			message("TIMER UP!")
		}

		counter <- counter + 1
	}
}
