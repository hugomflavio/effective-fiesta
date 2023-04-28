ika_land <- function(gia = 0, hop = 0, spe = 0, swo = 0, car = 0, arc = 0, sli = 0, mor = 0, cat = 0, ram = 0, gyr = 0, bal = 0, coo = 0, doc = 0){
	output <- c(0,0,0,0,0)
	names(output) <- c("Wood", "Wine", "Crystal", "Sulfur", "Citizens")
	output["Wood"] <- 
		gia * 130 + 
		hop * 40 + 
		spe * 30 + 
		swo * 30 + 
		car * 50 + 
		arc * 30 + 
		sli * 20 + 
		mor * 300 + 
		cat * 260 + 
		ram * 220 + 
		gyr * 25 + 
		bal * 40 + 
		coo * 50 + 
		doc * 50
	output["Wine"] <- coo * 150
	output["Crystal"] <- doc * 450
	output["Sulfur"] <- 
		gia * 180 + 
		hop * 30 + 
		swo * 30 + 
		car * 150 + 
		arc * 25 + 
		mor * 1250 + 
		cat * 300 + 
		gyr * 100 + 
		bal * 250
	output["Citizens"] <- 
		gia * 2 + 
		hop * 1 + 
		spe * 1 + 
		swo * 1 + 
		car * 1 + 
		arc * 1 + 
		sli * 1 + 
		mor * 5 + 
		cat * 5 + 
		ram * 5 + 
		gyr * 3 + 
		bal * 5 + 
		coo * 1 + 
		doc * 1
	return(output)
}