library("serial")
library("slackr")
library("ggplot2")
library("readODS")

source("slack_secrets.R")

loop_interval <- 5 
last_processed <- Sys.time()
process_interval <- 60

calibration_data <- read_ods("calibration.ods")
calibration <- lm(Pyro ~ Arduino, data = calibration_data)

# listPorts() # not working for Linux. use below instead
# sList <- dir("/dev/", pattern = "tty[0SU.'ACM''USB''XRUSB''AMA''rfcomm''AP']")
# sList

myArduino <- serialConnection(
	port = "ttyUSB0", # then change here
	mode = "9800,n,8,1",
	buffering = "none",
	newline = TRUE,
	eof = "",
	translation = "cr",
	handshake = "none",
	buffersize = 4096)

if (!isOpen(myArduino)) {
	open(myArduino)
	Sys.sleep(10)
}
 
while (TRUE) {
	fail_check <- tryCatch(source("modifiers.R"), error = function(e) TRUE)

	fail_check <- tryCatch(source("read_serial.R"),	error = function(e) TRUE)

	fail_check <- tryCatch(source("process_info.R"), error = function(e) TRUE)

	for(i in 1:loop_interval) {
		message("\r.", appendLF=FALSE)
		Sys.sleep(0.5)
		message("\r ", appendLF=FALSE)
		Sys.sleep(0.5)
	}	
}