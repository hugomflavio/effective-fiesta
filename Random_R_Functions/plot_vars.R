#' Plot all variables of a data.frame
#' 
#' Requires both ggplot2 and patchwork to work
#'
#' @param input a data.frame
#' @param y optional, a column to use as y axis. If none is provided, the row
#'          index is used as the y axis.
#'
#' @return a grid of ggplots
#'
plot_vars <- function(input, y) {
  if (any(!c("ggplot2", "patchwork") %in% rownames(installed.packages()))) {
    stop("This function requires packages ggplot2 and patchwork.")
  }
  if (!is.data.frame(input)) {
    warning("Input must be a data.frame. Attempting to convert.",
            immediate. = TRUE, call. = FALSE)
  }
  pd <- as.data.frame(input)
  if (missing("y")) {
    pd$new_index_col_123 <- 1:nrow(pd)
    y <- "row index"
  } else {
    if (!y %in% colnames(pd)) {
      stop("y is not a column name of input")
    }
    if (ncol(pd) < 2) {
      stop("y provided but input only has one column. No x to plot.")
    }
    pd$new_index_col_123 <- pd[, y]
    pd[, y] <- NULL
  }
  p_list <- lapply(colnames(pd)[-ncol(pd)], function(column) {
    p <- ggplot2::ggplot(data = pd)
    p <- p + ggplot2::aes(x = .data[[column]], y = new_index_col_123)
    if (class(pd[, column]) %in% c("logical", "factor", "character")) {
      jitter_w <- 0.2
    } else {
      jitter_w <- 0
    }
    if (class(pd$new_index_col_123) %in% c("logical", "factor", "character")) {
      jitter_h <- 0.2
    } else {
      jitter_h <- 0
    }
    p <- p + ggplot2::geom_point(position = position_jitter(width = jitter_w,
                                                            height = jitter_h),
                                 alpha = 0.2)
    p <- p + ggplot2::labs(title = column,
                           x = paste0("(", class(pd[, column]), ")"),
                           y = y)
    p + ggplot2::theme_bw()
  })
  
  return(patchwork::wrap_plots(p_list))
}
