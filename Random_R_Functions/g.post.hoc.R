g.post.hoc <- function(x,sequential=F) {
  # Create the output lists
 my.P.value.list <- list() 
 my.G.value.list <- list()
  # Each variable (column) will be analysed separately
 for( k in 1:ncol(x) ){
  # Create the matrixes wich will be later indexed on the lists
  my.P.values <- matrix(ncol=nrow(x)-1,nrow=nrow(x)-1)
  colnames(my.P.values) <- rownames(x)[2:nrow(x)]
  rownames(my.P.values) <- rownames(x)[1:(nrow(x)-1)]
  my.G.values <- matrix(ncol=nrow(x)-1,nrow=nrow(x)-1)
  colnames(my.G.values) <- rownames(x)[2:nrow(x)]
  rownames(my.G.values) <- rownames(x)[1:(nrow(x)-1)]
  # Run g.test on row-by-row contingency tables (2x2)
  for( i in 2:nrow(x) ){
   for ( j in 1:(nrow(x)-1) ){
    if( j < i ){
     row.totals <- apply(x[c(j,i),],1,sum)
  # Sequential = T, must be used when failing a test implies impossibility to perform the following test. See more below.
       if ( sequential ){ 
      negatives <- row.totals - x[c(j,i),1:k]
     } else { 
      negatives <- row.totals - x[c(j,i),k]
     }
     to.test <- data.frame(positives=x[c(j,i),k],negatives=negatives)
  # Transfer values to the interim matrixes
       my.P.values[j,i-1] <- g.test(to.test, print=F)[[2]]
     my.G.values[j,i-1] <- g.test(to.test, print=F)[[1]]
    } else {
     my.P.values[j,i-1] <- NA
     my.G.values[j,i-1] <- NA
    }
   }  
  }
  # Store interim matrixes on the output lists
    my.P.value.list[[k]] <- my.P.values
  names(my.P.value.list)[k] <- colnames(x)[k]
  my.G.value.list[[k]] <- my.G.values
  names(my.G.value.list)[k] <- colnames(x)[k]
  rm(my.G.values)
 }
  # Bind output lists into single object and return it
   return(list(P.values=my.P.value.list,G=my.G.value.list))
}

  # About sequential: If each column represents death or failure at a given point, and implies
  # an incapacity for that individual to be tested on the second variable, then the total number
  # of subjects on Variable B must be adjusted.
  # For example, if during a migration a fish fails to pass checkpoint A, it is not qualifiable for
  # analysis in checkpoint B.
  # Hence, when doing the analysis for checkpoint B, the total of individuals to be considered 
  # equals the original row totals minus the fish that perished on checkpoing A.
  # Note: When doing such an analysis, the last variable is considered to be the number of organisms that
  # successfully completed all checkpoints. Thus, the last varible is matched against the original row total
  # (Because all deaths + all survivals = all individuals)
  # If failing on variable A does not imply impossibility to test variable B, then "sequential" must be turned off.