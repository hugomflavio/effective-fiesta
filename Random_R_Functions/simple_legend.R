#' Simple ggplot legend positioning
#' 
#' @param h Horizontal position
#' @param v Vertical position
#' @param mar margin between the legend content and the box. Four values: top, left, down, right.
#' 
#' @export
#'
simple_legend <- function(h = 0.95, v = 0.95, mar = c(1, 4, 1, 2), borderwidth = 0){
	if (h > 0.5)
		jh <- "right"
	else
		jh <- "left"
	if (v > 0.5)
		jv <- "top"
	else
		jv <- "bottom"
	return(theme(legend.title = element_blank(),
  legend.position = c(h, v),
	legend.justification = c(jh, jv),
	legend.box.just = jh,
	legend.margin = margin(mar[1], mar[2], mar[3], mar[4]), # top, left, down, right
  legend.background = element_rect(fill = "white", linetype = "solid", colour = "black", linewidth = borderwidth),
	))
}
