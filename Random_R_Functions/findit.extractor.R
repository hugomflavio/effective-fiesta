findit.extractor <- function(file,out="output.csv"){
	raw <- readChar(paste(file),file.info(file)$size)
	data <- strsplit(raw,'<span class="document-counter">')
	library(stringr)

	output <- data.frame(Title=character()
		,Author=character()
		,Year=vector()
		,stringsAsFactors=FALSE
		)

	for( i in 2:length(data[[1]]) ){
		title <- str_extract(data[[1]][[i]],'href="/en/catalog/[^<]+</a>')
		temp <- sub('href="/en/catalog/[^>]+>',"",title)
		temp <- sub('</a>',"",temp)
		filtered.title <- gsub("\n","",temp); rm(title,temp)

		author <- str_extract(data[[1]][[i]],'"Find other material by [^><]+"')
		temp <- sub('Find other material by ',"",author)
		filtered.author <- gsub('"',"",temp); rm(author,temp)

		year <- str_extract(data[[1]][[i]],'rft.date=[1-2]{1}[0-9]{3}')
		filtered.year <- sub('rft.date=',"",year)

		my.row <- c(filtered.title,filtered.author,filtered.year)
		output[i-1,] <- my.row
	}
	write.csv(output,paste(out),row.names=F)
}
