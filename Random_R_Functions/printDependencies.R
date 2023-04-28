#' Print dependency diagrams
#' 
#' @param dot a dot data frame
#' @param src a data frame listing the source of each function
#' @param orphans a vector of functions which do not depend and are not a dependency or any function
#' @param highlight the name of a function to be highlighted
#' @param save if "no", the plot is printed; if one of "svg", "pdf" or "png", the plot is saved in the corresponding format
#' @param file The name of the output file (do not include the extension!)
#' @param show cat the dot script used to draw the graphic
#' 
#' @return the dependency plot
#' 
printDependencies <- function(dot, src = NULL, orphans = NULL, highlight = NULL, save = c("no", "svg", "pdf", "png"), file = "printDependencies_output", show = FALSE) {
  save <- match.arg(save)
  diagram_nodes <- data.frame(
    id = 1:(length(unique(unlist(dot[, c(1, 3)]))) + length(orphans)),
    label = c(unique(unlist(dot[, c(1, 3)])), orphans),
    stringsAsFactors = FALSE)
  
  if (!is.null(src)) {
    src$fillcolor <- as.factor(src$src)
    levels(src$fillcolor) <- viridis::viridis(length(levels(src$fillcolor)))
    link <- match(diagram_nodes$label, src$fun)
    diagram_nodes$fillcolor <- src$fillcolor[link]
  } else {
    diagram_nodes$fillcolor <- rep("#56B4E9", nrow(diagram_nodes))
  }

  # node string
  node_list <- split(diagram_nodes, diagram_nodes$fillcolor)
  node_aux <- paste0(unlist(lapply(node_list, function(n) {
    s <- paste0("node [fillcolor = '", n$fillcolor[1], "']\n")
    s <- paste0(s, paste0(n$id, " [label = '", n$label, "']", collapse = "\n"), "\n")
  })), collapse = "\n")
  node_list <- do.call(rbind.data.frame, node_list)
  node_fragment <- paste0(node_aux, "\n")
  
  # prepare edge data frame
  diagram_edges <- as.data.frame(apply(dot[, c(1, 3)], 2, function(x) match(x, diagram_nodes$label)))
  colnames(diagram_edges) <- c("from", "to")
  diagram_edges$extra <- ""
  
  if (!is.null(highlight)) {
    highlight <- node_list$id[which(node_list$label == highlight)]
    if (any(is.na(highlight)))
      stop("Some of the values to highlight are not present in the input.")
    diagram_edges$extra[diagram_edges$from == highlight] <- '[color="red",penwidth=2]'
    diagram_edges$extra[diagram_edges$to == highlight] <- '[color="blue",penwidth=2]'
  }

  # edge string
  edge_fragment <- paste0(apply(diagram_edges, 1, function(x) paste0(paste0(x[1:2], collapse = "->"), x[3])), collapse = "\n")

  x <- paste0("digraph {
  rankdir = LR
  node [shape = box,
        style = filled,
        fontname = helvetica,
        fontcolor = white,
        color = grey]

  ", node_fragment, "

  edge [color = grey]
  ", edge_fragment, "
  }")

  if (show)
    cat(x)
  plot <- DiagrammeR::grViz(x)
  plot_string <- DiagrammeRsvg::export_svg(plot)
  plot_raw <- charToRaw(plot_string)
  if (save == "svg")
    rsvg::rsvg_svg(svg = plot_raw, file = paste0(file, ".svg"))
  if (save == "pdf")
    rsvg::rsvg_pdf(svg = plot_raw, file = paste0(file, ".pdf"))
  if (save == "png")
    rsvg::rsvg_png(svg = plot_raw, file = paste0(file, ".png"))
  if (save == "no")
    return(plot)
}

#' Crawl through R source files to extract function information
#' 
#' @param files a (list of) file(s) to crawl through
#' 
#' @return The dependencies for each listed function
#' 
getDependencyList <- function(files) {
  recipient <- list()
  for (file in files) {
  x <- readLines(file)
  x <- gsub("#[^\n]*", "", x) # remove comment bits to avoid accidentally grabbing commented function calls

  breaks <- c(which(grepl(" <- function\\(", x)), length(x) + 1)
    for (i in 1:(length(breaks)-1)) {
      a <- gsub("[^\\: & ^a-z & ^A-Z & ^\\. & ^\\_ & \\( & 0-9 & \\[ & \\]]", "", x[breaks[i]:(breaks[i + 1] - 1)])
      a <- gsub('\"[^\"]*', "", a)
      a <- gsub("\'[^\']*", "", a)
      a <- gsub("\\t", "", a)
      b <- a[grepl("\\(", a)]
      y <- strsplit(b, "\\(")
      z <- unlist(lapply(y, function(j) j[-length(j)]))
      aux <- stringr::str_extract(z, '[^\\ & ^\\[ & ^! & \\" & ^, & ^:]*$')
      aux <- unique(aux[aux != ""])
      aux <- aux[is.na(match(aux, lsf.str("package:base")))]
      recipient[[length(recipient) + 1]] <- aux
      names(recipient)[[length(recipient)]] <- stringr::str_extract(x[breaks[i]], "^[^\\ ]*")
      attributes(recipient[[length(recipient)]])$source <- file
    }
  }
  recipient <- recipient[names(recipient) != ""]
  dependency.list <- recipient
  return(dependency.list)
}

#' Convert the dependency list into a dot string
#' 
#' @param input the dependency list
#' 
#' @return a dot string
#' 
dottifyDependencies <- function(input) {
  aux <- names(input)
  recipient <- lapply(input, function(x) {
      x <- x[!is.na(match(x, aux))]  
  })

  output <- unlist(lapply(names(recipient), function(i) {
    if (length(recipient[[i]]) > 0)
      paste(paste0(recipient[[i]], " -> ", i, "\n"), collapse = "")
    else
      paste(i, "\n")
    }))
  output <- paste(output, collapse = "")
  return(output)
}

findOrphans <- function(input) {
  aux <- names(input)
  recipient <- lapply(input, function(x) {
      x <- x[!is.na(match(x, aux))]  
  })
  if (any(link <- unlist(lapply(recipient, length)) == 0)) {
    independent <- names(recipient)[link]
    dependencies <- unique(unlist(recipient))
    if (any(link <- !independent %in% dependencies))
      return(independent[link])
  }
  message("No orphans were found.")
  return(NULL)
}
