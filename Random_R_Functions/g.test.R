g.test <- function(data, totals, print = TRUE, method = c("direct", "total")) {
  method <- match.arg(method)
  if (method == "Total") {
    if (length(data) != length(totals))
      stop("Data's and totals' length do not match. Include the total for each group.")
    x <- list(data = data, remaining = totals - data)
  } else {
    x <- data
  }
  # Step 1: Calculate the sum of x*ln(x) for all observed frequencies:
  # HF. 2018-02-27 - Added na.rm=T to avoid crashing when a count is 0.
  freq.sum <- sum(x * log(x), na.rm = TRUE)
  # Step 2: Calculate x*ln(x) for the grant total:
  n <- sum(x)
  total.sum <- sum(n * log(n))
  # Step 3: Calculate x*ln(x) for all row and column totals:
  row.totals <- apply(x,1,sum)
  col.totals <- apply(x,2,sum)
  # HF. 2018-02-27 - Added if() statement to prevent function crash
  if (any(row.totals==0) | any(col.totals==0))
    stop("Error: One of the row or column sums equals 0.")
  row.sum <- sum(row.totals*log(row.totals))
  col.sum <- sum(col.totals*log(col.totals))
  edge.sum <- row.sum + col.sum
  # Step 4: Calculate Step 1 + Step 2 - Step 3:
  half.g <- freq.sum + total.sum - edge.sum
  # Step 5: Double half.g to get G:
  G <- half.g * 2
  # Step 6: For contingency tables smaller or equal to 2x2, apply Williams correction:
  small <- nrow(x) <= 2 & ncol(x) <= 2
  if (small) {
    a <- (n / row.totals[[1]]) + (n / row.totals[[2]]) - 1
    b <- (n / col.totals[[1]]) + (n / col.totals[[2]]) - 1
    correction <- 1 + ((a * b) / (6 * n))
    G <- G / correction
  }
  # Finally, prepare data for display:
  # degrees of freedom is (#rows-1)*(#columns-1)
  degf <- (nrow(x) - 1) * (ncol(x) - 1)
  # If conditions are right, p.value is from chi^2 distribution (area to right of test statistic)
  p.G = 1 - pchisq(G,degf)
  # return a list with expected counts, test statistic, and p-value
  if (p.G > 0.05)
    signif <- ""
  if (p.G <= 0.05)
    signif <- "*"
  if (p.G <= 0.01)
    signif <- "**"
  if (p.G <= 0.001)
    signif <- "***"

  if (p.G < 0.0001)
    values <- paste0("G = ", round(G, 4), ", df = ", round(degf, 0), ", p-value < 0.0001 *** \n")
  else
    values <- paste0("G = ", round(G, 4), ", df = ", round(degf, 0), ", p-value = ", round(p.G, 4), " ", signif, "\n")

  if (print) {
    message("G-Test for Contingency Tables\n")
    message(paste("Selected method:", method, "\n"))
    message("Data:")
    print(x)
    message("\nProportions:")
    print(x/row.totals)
    message("\n")
    if (small)
      message("Applied William's correction.\n")
    else
      message("No correction necessary.\n")
    message(values)
  }
  return(invisible(list(G = G, p.value = p.G, data = x, percent = x/row.totals)))
}
