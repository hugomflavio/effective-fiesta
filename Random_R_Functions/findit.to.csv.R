findit.to.csv <- function(file, outname, append=FALSE) {
	raw <- readChar(file, file.info(file)$size)
	data <- strsplit(raw,'<span class="document-counter">')
	library(stringr)

	output <- data.frame(Title=character()
			,Author=character()
			,Year=vector()
			,stringsAsFactors=FALSE
			)

	for( i in 2:length(data[[1]]) ){
		my.title <- str_extract(data[[1]][[i]],'/catalog/[0-9]+">[^><]+.*[a-zA-Z]+.*[^><]+</a>')
		temp <- sub('/catalog/[0-9]+">',"",my.title)
		filtered.title <- sub('</a>',"",temp); rm(my.title,temp)

		my.author <- str_extract(data[[1]][[i]],'"Find other material by [^><]+"')
		temp <- sub('"Find other material by ',"",my.author)
		filtered.author <- sub('"',"",temp); rm(my.author,temp)

		my.year <- str_extract(data[[1]][[i]],'rft.date=[1-2]{1}[0-9]{3}')
		filtered.year <- sub('rft.date=',"",my.year); rm(my.year)

		my.row <- c(filtered.title,filtered.author,filtered.year)
		rm(filtered.title,filtered.author,filtered.year)

		output[i-1,] <- my.row; rm(my.row)
	}

	if( append ){
		write.table(output,outname,row.names=F, sep=",", append=T, col.names=F)
	} else {
		write.table(output,outname,row.names=F, sep=",")
	}
}