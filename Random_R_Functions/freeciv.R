unit_stat <- function(base = 1, vet = 0, terrain = c("flat", "rough", "swamp", "forest", "tundra", "jungle", "hills", "mountains", "ocean"),
					 river = FALSE, city = c("no", "small", "large"), walls = FALSE, great.wall = FALSE,
					 fortified = FALSE, fortress = FALSE, hp = 10, fp = 1) {

	terrain <- match.arg(terrain)
	city <- match.arg(city)

	
	if (vet > 0)
		vet.bonus <- 1.50 + (0.25 * (vet - 1))
	else	
		vet.bonus <- 1


	if (terrain == "flat")
		terrain.bonus <- 1

	if (terrain %in% c("rough", "swamp", "forest", "tundra"))
		terrain.bonus <- 1.25

	if (terrain == "jungle")
		terrain.bonus <- 1.4

	if (terrain == "hills")
		terrain.bonus <- 1.5

	if (terrain == "mountains")
		terrain.bonus <- 2

	if (terrain == "ocean")
		terrain.bonus <- 1.1

	if (city == "no") {
		if (great.wall)
			warning("Unit is not in a city, ignoring 'great.wall'.", call. = FALSE, immediate. = TRUE)
		if (walls)
			warning("Unit is not in a city, ignoring 'walls'.", call. = FALSE, immediate. = TRUE)
		city.bonus <- 1
	}	else {
		if (!fortified)
			warning("Unit is in a city, if it can fortify, remember to set fortified to TRUE.", call. = FALSE, immediate. = TRUE)
		if (fortress)
			warning("Unit is in a city, ignoring 'fortress'.", call. = FALSE, immediate. = TRUE)
		fortress <- FALSE
	}

	if (city == "small") {
		if (walls) {
			if (great.wall)
				city.bonus <- 2.9
			else
				city.bonus <- 2.5
		}
		else {
			if (great.wall)
				city.bonus <- 1.9
			else
				city.bonus <- 1.5
		}
	}

	if (city == "large") {
		if (walls) {
			if (great.wall)
				city.bonus <- 3.4
			else
				city.bonus <- 3
		}
		else {
			if (great.wall)
				city.bonus <- 2.4
			else
				city.bonus <- 2
		}
	}

	x <- base * vet.bonus * (1 + 0.5 * fortified) * (1 + 1 * fortress) * city.bonus * terrain.bonus * (1 + 0.25 * river)
	
	output <- c(base = base,
				vet = vet.bonus,
				fortified = (1 + 0.5 * fortified),
				fortress = (1 + 1 * fortress),
				city = city.bonus,
				terrain = terrain.bonus,
				river = (1 + 0.25 * river),
				strength = x,
				hp = hp,
				fp = fp)
	
	class(output) <- c("numeric", "unit_stat")

	return(output)
}


buy_unit <- function(p) {
	cost <- floor(2 * p + (p^2 / 20))
	rate <- round(cost/p, 2)

	return(c(cost = cost, rate = rate))
}


battle <- function(attacker, defender, trials = 500000) {
	if (!("unit_stat" %in% class(attacker)) & length(attacker) != 3)
		stop("Please either use the function unit_stat() to input the attacker stats, or provide the strength, hp and firepower for the attacker in a single vector of size 3.")

	if (!"unit_stat" %in% class(defender))
		stop("Please use the function unit_stat() to input the defender stats")

	if (!("unit_stat" %in% class(attacker)))
		names(attacker) <- c("strength", "hp", "fp")

	attacker_n_to_lose <- as.integer((attacker["hp"] + defender["fp"] - 1) / defender["fp"])

	defender_n_to_lose <- as.integer((defender["hp"] + attacker["fp"] - 1) / attacker["fp"])

	attacker_p_victory <- attacker["strength"] / (attacker["strength"] + defender["strength"])

	defender_p_victory <- 1 - attacker_p_victory


	max_rounds <- attacker_n_to_lose + defender_n_to_lose - 1

	
	binom_save <- attacker_p_victory ^ (defender_n_to_lose - 1)

	accum_prob <- binom_save

	attacker_rounds_lost <- 1

	while (attacker_rounds_lost < attacker_n_to_lose) {
		n <- attacker_rounds_lost + defender_n_to_lose - 1;
		
		binom_save <- binom_save * n;
		
		binom_save <- binom_save / attacker_rounds_lost;
		
		binom_save <- binom_save * defender_p_victory;
		
		accum_prob <- accum_prob + binom_save;

		attacker_rounds_lost <- attacker_rounds_lost + 1
	}
	
	accum_prob <- accum_prob * attacker_p_victory;

	message("Game calculated win prob: ", round(accum_prob * 100, 2), "%")

	trial_results <- rbinom(trials, max_rounds, attacker_p_victory)


	# defender stats

	defender_hp_left <- defender["hp"] - (trial_results * attacker["fp"])
	defender_hp_left[defender_hp_left < 0] <- 0

	defender_outcome <- as.data.frame(table(defender_hp_left)/length(defender_hp_left)*100)

	defender_outcome$defender_hp_left <- as.numeric(as.character(defender_outcome$defender_hp_left))

	colnames(defender_outcome)[2] <- "Prob"

	defender_outcome$cumProb <- rev(cumsum(rev(defender_outcome$Prob)))

	defender_outcome <- round(defender_outcome, 4)


	# attacker stats

	attacker_hp_left <- attacker["hp"] - ((max_rounds - trial_results) * defender["fp"])
	attacker_hp_left[attacker_hp_left < 0] <- 0

	attacker_outcome <- as.data.frame(table(attacker_hp_left)/length(attacker_hp_left)*100)

	attacker_outcome$attacker_hp_left <- as.numeric(as.character(attacker_outcome$attacker_hp_left))

	colnames(attacker_outcome)[2] <- "Prob"

	attacker_outcome$cumProb <- rev(cumsum(rev(attacker_outcome$Prob)))

	attacker_outcome <- round(attacker_outcome, 4)




	return(list(attacker = attacker, defender = defender, attacker_outcome = attacker_outcome, defender_outcome = defender_outcome))
}


continue_battle <- function(previous_battle, attacker, trials = 500000, promoted = FALSE) {
	if (!("unit_stat" %in% class(attacker)) & length(attacker) != 3)
		stop("Please either use the function unit_stat() to input the attacker stats, or provide the strength, hp and firepower for the attacker in a single vector of size 3.")

	if (!("unit_stat" %in% class(attacker)))
		names(attacker) <- c("strength", "hp", "fp")

	output <- previous_battle[c("attacker", "defender", "defender_outcome")]

	if(is.list(output$attacker)) {
		output$attacker[[length(output$attacker) + 1]] <- attacker
	} else {
		output$attacker <- list(output$attacker, attacker)
	}
	
	if (promoted) {
		if (output$defender["vet"] == 1) {
			attributes(output$defender)$Promotions <- paste0(attributes(output$defender)$Promotions, "Round ", length(output$attacker) - 1, ": (1 -> 1.5); ")
			output$defender["vet"] <- 1.5
		} else {
			attributes(output$defender)$Promotions <- paste0(attributes(output$defender)$Promotions, "Round ", length(output$attacker) - 1, ": (", output$defender["vet"], " -> ", output$defender["vet"] + 0.25, ");")
			output$defender["vet"] <- output$defender["vet"] + 0.25
		}

		output$defender["strength"] <- prod(output$defender[1:7])
	}

	aux <- lapply(1:nrow(output$defender_outcome), function(i) {
		if (output$defender_outcome$defender_hp_left[i] == 0) {
			return(data.frame(defender_hp_left = 0, Prob = 100 * (output$defender_outcome$Prob[i]/100)))
		} else {
			temp.defender <- output$defender
			temp.defender["hp"] <- output$defender_outcome$defender_hp_left[i]
			x <- suppressMessages(battle(attacker = attacker,
										 defender = temp.defender,
										 trials = trials))

			x$defender_outcome$Prob <- x$defender_outcome$Prob * (output$defender_outcome$Prob[i]/100)	
			return(x$defender_outcome[,-3])
		}
	})

	aux.db <- do.call(rbind, aux)

	defender_outcome <- as.data.frame(aggregate(aux.db$Prob, list(aux.db$defender_hp_left), sum))

	colnames(defender_outcome) <- c("defender_hp_left", "Prob")

	defender_outcome$cumProb <- rev(cumsum(rev(defender_outcome$Prob)))

	defender_outcome <- round(defender_outcome, 5)

	output$defender_outcome <- defender_outcome

	return(output)
}


tech_discount <- function(base_cost, players_that_know, all_players_alive, techleakagerate = 100) {
	if (!is.numeric(base_cost))
		base_cost <- tech_cost(base_cost)
	new.price <- floor(base_cost * (1 - (techleakagerate/100) * (players_that_know/all_players_alive)))
	bulbs.saved <- base_cost - new.price
	return(c(Updated_cost = new.price, Bulbs_saved = bulbs.saved))
}


tech_cost <- function(tech, server = "LT76Team") {
	tech_list <- readLines(paste0("https://raw.githubusercontent.com/longturn/games/master/", server, "/data/", server, "/techs.ruleset"))
	tech_line <- grep(paste0(tech, "\\]"), tech_list)
	tech_end <- tech_line + grep("^$", tech_list[tech_line:(tech_line+50)])[1] - 1
	tech_cost_line <- tech_line + grep("cost", tech_list[tech_line:tech_end]) - 1
	tech_cost <- tech_list[tech_cost_line]
	tech_cost <- as.numeric(stringr::str_extract(tech_cost, "[0-9]+"))
	return(tech_cost*3)
}

tech_cost(tech = "democracy")

# tech_cost <- function(start_cost, base_tech_cost, n_req) {
# 	cost1 <- start_cost + (base_tech_cost * (1 + n_req) * sqrt(1 + n_req)/2)
# 	cost2 <- 0.5 * (n_req * 2)^1.5 * start_cost
# 	return(c(cost1, cost2))
# }
