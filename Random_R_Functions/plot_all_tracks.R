# Note: patchwork must be loaded beforehand, or else plot_all_tracks won't work
library('patchwork')

plot_all_tracks <- function(input, water, tag, plot_stations = TRUE) {
	tracks <- 1:nrow(input$tracks[[tag]]))
	plots <- lapply(tracks, function(track) {
		output <- plotTracks(input = input, base.raster = water, tag = tag, track = track) 
		if (plot_stations)
			output <- output + addStations(input = input)
		return(output)
	})

	plot_grid <- actel:::nearsq(length(plots))

	# build plot code line
	plot_code <- parse(text = paste(c(paste0('plots[[', tracks, ']]'), 'plot_layout(nrow = plot_grid[1], ncol = plot_grid[2])'), collapse = ' + '))

	# plot it
	eval(plot_code)
}

# example use:
# plot_all_tracks(input = rsp.data, water = water, tag = ..., plot_stations = TRUE)
