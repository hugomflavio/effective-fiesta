plot_collinearity <- function(input) {
  continuous <- sapply(input, class) %in% c("integer", "numeric")
  factors <- sapply(input, class) %in% c("factor", "character", "logical")
  skipped1 <- FALSE
  skipped2 <- FALSE
  if (sum(continuous) > 1) {
    panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
    {
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y)
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste0(prefix, txt)
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex.cor * r)
    }
    pairs(input[, continuous], pch = 20, lower.panel = panel.cor)
  } else { 
  	if (sum(continuous) == 1) {
      skipped1 <- TRUE
      message("M: Only one continuous variable found, skipping continuous colinearity comparisons.")
    }
    if (sum(continuous) == 0) {
      skipped1 <- TRUE
      message("M: No continuous variable found, skipping continuous colinearity comparisons.")
    }
  }
  if (sum(continuous) > 0) {
    if (!skipped1) {
      dev.new()
    }
    aux <- reshape2::melt(input, 
      id.vars = colnames(input)[factors], 
      meas.vars = colnames(input)[continuous])
    colnames(aux)[colnames(aux) == "variable"] <- "continuous"
    x <- suppressWarnings(reshape2::melt(aux, 
      id.vars = c("continuous", "value"), 
      meas.vars = colnames(input)[factors]))
    colnames(x)[ncol(x)] <- "x"

    p <- ggplot(data = x, aes(x = x, y = value))
    p <- p + geom_boxplot(outlier.shape = NA)
    p <- p + geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.2)
    p <- p + facet_grid(continuous ~ variable, scales = "free")
    p + labs(x = "")
    print(p)
  } else {
    if(sum(continuous) == 0) {
      skipped2 <- TRUE
      message("M: No continuous variable found, skipping factor colinearity comparisons.")
    }
  }
  if (skipped1 & skipped2) {
    message("M: No plots to draw.")
  }
}
