plot_vars <- function(input) {
  if (is.numeric(input[, 1])) {
    j_height <- 0
  } else {
    j_height <- 0.2
  }

  plots <- lapply(2:ncol(input), function(i) {
    if (is.numeric(input[, i])) {
      j_width <- (max(input[, i], na.rm = TRUE) - min(input[, i], na.rm = TRUE))*0.01
    } else {
      j_width <- 0.2
    }
    p <- ggplot(data = input, aes(y = .data[[colnames(input)[1]]], x = .data[[colnames(input)[i]]]))
    p <- p + geom_point(
      position = position_jitter(
        width = j_width, height = j_height), 
      alpha = 0.2)
    p
  })

  patchwork::wrap_plots(plots)
}
