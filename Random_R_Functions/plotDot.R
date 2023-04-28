plotDot <- function(dot, spatial, coord.x, coord.y, expand = 1, file) {
# requires:
# DiagrammeR
# DiagrammeRsvg
# rsvg

  if (!missing(file) && !grepl("png$|pdf$|svg$", file))
      stop("Could not recognise file extension. Please chose a png, pdf or svg file name.", call = FALSE)

  if (is.character(dot))
    dot <- readDot(string = dot)

  unique.arrays <- unique(unlist(dot[, c(1,3)]))

  if (!missing(spatial)) {
  
    if (missing(coord.x) | missing(coord.y))
      stop("spatial was provided, but coord.x and/or coord.y are missing.", call. = FALSE)

    if (!any(colnames(spatial) == coord.x))
      stop("Could not find column '", coord.x, "' in spatial.", call. = FALSE)

    if (!any(colnames(spatial) == coord.y))
      stop("Could not find column '", coord.y, "' in spatial.", call. = FALSE)
    
    if (any(is.na(match(unique.arrays, unique(spatial$Array)))))
      stop("spatial was provided, but not all arrays specified in the dot exist in spatial", call. = FALSE)

    if (any(is.na(match(unique(spatial$Array), unique.arrays))))
      warning("Not all arrays present in spatial were listed in dot", immediate. = TRUE, call. = FALSE)

    xspatial <- spatial[!is.na(match(spatial$Array, unique.arrays)), ]

    dot.coords <- aggregate(xspatial[, c(coord.x, coord.y)], list(xspatial$Array), mean)
    colnames(dot.coords) <- c("Array", "x", "y")

    dot.coords$x <- dot.coords$x - min(dot.coords$x)
    dot.coords$y <- dot.coords$y - min(dot.coords$y)
    
    if (max(dot.coords$x)/max(dot.coords$y) >= 1) {
      dot.coords$x <- dot.coords$x / max(dot.coords$x) * (max(dot.coords$x) / max(dot.coords$y) * 10) * expand
      dot.coords$y <- dot.coords$y / max(dot.coords$y) * 10 * expand
    } else {
      dot.coords$x <- dot.coords$x / max(dot.coords$x) * 10 * expand
      dot.coords$y <- dot.coords$y / max(dot.coords$y) * (max(dot.coords$y) / max(dot.coords$x) * 10) * expand
    }

    dot.coords <- dot.coords[match(dot.coords$Array, unique.arrays), ]
  }

  if (missing(spatial)) {
    diagram_nodes <- data.frame(
      id = 1:length(unique(unlist(dot[, c(1, 3)]))),
      label = unique(unlist(dot[, c(1, 3)])),
      fillcolor = '#56B4E9',
      stringsAsFactors = FALSE)
  } else {
    diagram_nodes <- data.frame(
      id = 1:length(unique.arrays),
      label = unique.arrays,
      x = round(dot.coords$x, 0),
      y = round(dot.coords$y, 0),
      fillcolor = '#56B4E9',
      stringsAsFactors = FALSE)
  }
  # prepare edge data frame
  if (nrow(dot) == 1) {
    if (dot[1, 1] == dot[1, 3]) {
      diagram_edges <- NULL
      complete <- FALSE
    } else {
      diagram_edges <- as.data.frame(t(apply(dot[, c(1, 3), drop = FALSE], 2, function(x) match(x, diagram_nodes$label))))
      complete <- TRUE
    }
  } else {
    diagram_edges <- as.data.frame(apply(dot[, c(1, 3), drop = FALSE], 2, function(x) match(x, diagram_nodes$label)))
    complete <- TRUE
  }
  if (complete) {
    colnames(diagram_edges) <- c("from", "to")

    diagram_edges$rel <- "requires"
    diagram_edges$dir <- "none"
    diagram_edges$arrowhead <- "none"
    diagram_edges$arrowtail <- "none"

    diagram_edges$dir[dot$to != "--"] <- "both"
    diagram_edges$arrowhead[dot$to == "->"] <- "normal"
    diagram_edges$arrowtail[dot$to == "->"] <- "tee"
    diagram_edges$arrowhead[dot$to == "<-"] <- "tee"
    diagram_edges$arrowtail[dot$to == "<-"] <- "normal"
  }

  if (is.null(diagram_edges))
    x <- DiagrammeR::create_graph(diagram_nodes)
  else
    x <- DiagrammeR::create_graph(diagram_nodes, diagram_edges)

  if (missing(file)) {
    return(DiagrammeR::render_graph(x))
  } else {
    DiagrammeR::export_graph(x, file = file)
  }
}
