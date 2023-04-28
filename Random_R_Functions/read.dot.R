read.dot <- function (input) {
    lines <- readLines(input)
    nodes <- lines[grepl("->", lines) | grepl("--", lines)]
    nodes <- gsub("->","--",gsub("\\[label=","--",gsub(";","",gsub("\\]","",gsub(" ","",nodes)))))
    nodePairs <- strsplit(nodes,"--")
   	gatesPresent <- FALSE
    if( any(unlist(lapply(nodePairs, function(x) length(x)==3 ))) ){
    	gatesPresent <- TRUE
    	recipientA <- vector()
    	recipientB <- vector()
    	for( i in 1:length(nodePairs) ){
    		recipientA <- c(recipientA,nodePairs[[i]][1:2])
    		recipientB <- c(recipientB,nodePairs[[i]][3])
    	}
    	nodes <- unique(recipientA)
    	gates <- unique(recipientB)
    	gateways <- matrix(0, nrow=length(nodes), ncol=length(gates), dimnames = list(nodes, gates))
    	for( node in nodes ){
    		for( i in 1:length(nodePairs) ){
    			if( any(grepl(node,nodePairs[[i]])) ) gateways[node,i] <- 1
    		}
    	}
    	nodePairs <- lapply(nodePairs, function(x) x <- x[1:2])
    } else {
    	nodes <- unique(unlist(nodePairs))
    }
    graph <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
   	for( node in nodes ){
    	for( i in 1:length(nodePairs) ){
    		if( any(grepl(node,nodePairs[[i]])) ){
    			destination <- nodePairs[[i]][!grepl(node,nodePairs[[i]])]
    			graph[node,destination] <- 1
    			graph[destination,node] <- 1
    		}
    	}
    }
    for( i in 1:(length(nodes)-1) ){
	    for( node in nodes ){
	    	for( destination in nodes){
	    		if( graph[node,destination]==i ){
		    		candidates <- rownames(graph)!=destination & rownames(graph)!=node & graph[,destination]==1
		    		if( any(candidates) ){
		    			to.change <- names(candidates)[candidates]
		    			for( j in to.change ){
		    				if( graph[node,j]==0 ) graph[node,j] <- graph[node,j] + 1 + i
		    			}
		    		}
	    		}
	    	}
	    }
	}
	if( gatesPresent ){
		return(list(graph=graph,gateways=gateways))
	} else {
		return(graph)
	}
}