#' convert coordinates to another system
#' 
#' @param data an input dataframe containing coordinate columns
#' @param coords a vector of size 2 with the x and y column names
#' @param from the EPSG code that the coordinates are in
#' @param to the EPSG code we want them converted to
#' 
#' @return the data object with updated coordinate columns
#' 
#' @export
#' 
conv_coords <- function(data, coords, from, to) {
  aux <- sf::st_as_sf(data, coords = coords, crs = from)
  aux <- sf::st_transform(aux, to)
  aux <- as(aux, "Spatial")
  aux <- as.data.frame(aux)
  data[, coords[1]] <- aux$coords.x1
  data[, coords[2]] <- aux$coords.x2
  return(data)
}
 
