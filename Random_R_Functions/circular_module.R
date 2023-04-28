#' Time analysis
#' 
#' This module activates functions used to explore and plot time-of-day data.
#' 
#' 
NULL

#' Draw a section on the outside of the circle
#' 
#' @param from value where the section should start
#' @param to value where the section should end
#' @param units units of the from and to variables, defaults to "hours"
#' @param template variable to feed into the circular package base functions
#' @param limits two values controling the vertical start and end points of the section
#' @param fill The colour of the section
#' @param border The colour of the section's border
#' 
circularSection <- function(from,to,units="hours",template="clock24",limits=c(1,0), fill="white", border="black"){
  if( inherits(from,"character") ){
    hour.from <- circular(decimalTime(from),units=units,template=template)
    hour.to <- circular(decimalTime(to),units=units,template=template)
  } else {
    hour.from <- circular(from,units=units,template=template)
    hour.to <- circular(to,units=units,template=template)
  }
  if(hour.to < hour.from) hour.to <- hour.to+24
  zero <- attr(hour.from,"circularp")$zero # extracted from the circular data
  xmin <- as.numeric(conversion.circular(hour.from,units="radians"))*-1
  xmax <- as.numeric(conversion.circular(hour.to,units="radians"))*-1
  xx <- c(limits[1]*cos(seq(xmin, xmax, length=1000)+zero),rev(limits[2]*cos(seq(xmin, xmax, length=1000)+zero)))
  yy <- c(limits[1]*sin(seq(xmin, xmax, length=1000)+zero),rev(limits[2]*sin(seq(xmin, xmax, length=1000)+zero)))
  polygon(xx, yy, col=fill, border=border)
}

#' Wrapper for the converter function
#' 
#' @param x time value(s) to be converted
#' 
#' @return Numerical time(s)
#'
decimalTime <- function(x) {
  if(length(x)<1) stop("Input appears to be empty.")
  if(length(x)==1) output <- converter(x)
  if(length(x)>1) output <- unlist(lapply(x,converter))
  return(output)
}

#' Convert HH:MM:SS time to HH.HHH time
#' 
#' @param x time string to be converted
#' 
#' @return Numerical time
#' 
#' @keywords internal
#' 
converter <- function(x) {
  x = as.numeric(unlist(strsplit(x, ":")))
  if(length(x)==2) x = x[1]+x[2]/60
  if(length(x)==3) x = x[1]+x[2]/60+x[3]/3600
  return(x)
}

#' Edited rose diagram function
#' 
#' Adapted from the rose.diag function of the circular package
#' 
#' @param x a vector, matrix or data.frame. The object is coerced to class circular.
#' @param pch point character to use. See help on par.
#' @param cex point character size. See help on par.
#' @param axes logical: if TRUE axes are plotted according to properties of x.
#' @param shrink parameter that controls the size of the plotted circle. Default is 1. Larger values shrink the circle, while smaller values enlarge the circle.
#' @param bins number of arcs to partition the circle with.
#' @param upper logical: if TRUE, the rose diagram cells are "upper"-closed intervals.
#' @param ticks logical: if TRUE ticks are plotted according to the value of bins.
#' @param tcl length of the ticks.
#' @param tcl.text the position of the axis labels.
#' @param radii.scale make possible to choose sector radius form: square-root of relative frequency (sqrt, default) or conventional linear scale (linear).
#' @param border the color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders.
#' @param col the color for filling the rose diagram. The default, NULL, is to leave rose diagram unfilled. color of the points. The values are recycled if needed.
#' @param tol proportion of white space at the margins of plot.
#' @param uin desired values for the units per inch parameter. If of length 1, the desired units per inch on the x axis.
#' @param xlim,ylim the ranges to be encompassed by the x and y axes. Useful for centering the plo
#' @param prop numerical constant determining the radii of the sectors. By default, prop = 1. 
#' @param digits number of digits used to print axis values.
#' @param plot.info an object from plot.circular that contains information on the zero, the rotation and next.points.
#' @param units the units used in the plot. If NULL the units of the first component of 'x' is used.
#' @param template the template of the plot. Ignored if plot.info is provided.
#' @param zero the zero of the plot. Ignored if plot.info or template are provided.
#' @param rotation the rotation of the plot. Ignored if plot.info or template are provided.
#' @param main,sub,xlab,ylab title, subtitle, x label and y label of the plot.
#' @param add add the rose diag to an existing plot.
#' @param control.circle parameters passed to plot.default in order to draw the circle. The function circle.control is used to set the parameters.
#' @param rings logical: if TRUE, inner rings are displayed for visual reference
#' @param rings.lty line type of the rings, See help on par.
#' @param ring.text logical: if notes should be displayed.
#' @param ring.text.pos The position of the rings' text. Ignored if ring.text is set to FALSE.
#' @param ring.text.cex The size of the ring's text. Ignored if ring.text is set to FALSE.
#' 
#' @return A list with the zero, rotation and next.points values, to be parsed to an overlaying graphic.
#' 
myRoseDiag <- function (x, pch = 16, cex = 1, axes = TRUE, shrink = 1, bins = 24, 
  upper = TRUE, ticks = TRUE, tcl = 0.025, tcl.text = 0.125, 
  radii.scale = c("sqrt", "linear"), border = NULL, col = c("lightblue", "#c0ff3e80", "#ffc0cb80", "#F0E4424D", "#0072B24D", "#D55E004D"), 
  tol = 0.04, uin = NULL, xlim = c(-1, 1), ylim = c(-1, 1), 
  prop = 1, digits = 2, plot.info = NULL, units = NULL, template = NULL, 
  zero = NULL, rotation = NULL, main = NULL, sub = NULL, xlab = "", 
  ylab = "", add = TRUE, control.circle = circle.control(), rings= c("none", "absolute", "relative"), 
  rings.lty = 2, ring.text = FALSE, ring.text.pos = -0.04, ring.text.cex = 1) {
  rings <- match.arg(rings)
  radii.scale <- match.arg(radii.scale)
  
  if (is.list(x)) {
    max.length <- max(unlist(lapply(x, length)))
    for (i in 1:length(x)) {
      if (length(x[[i]]) < max.length)
        x[[i]] <- c(x[[i]], circular(rep(NA, max.length - length(x[[i]]))))
    }
  }
  
  xx <- as.data.frame(x)
  nseries <- ncol(xx)
  
  if (length(col) != nseries)
    col <- rep(col, length.out = nseries)
  xcircularp <- attr(as.circular(xx[, 1]), "circularp")
  modulo <- xcircularp$modulo
  if (is.null(units)) 
    units <- xcircularp$units
  if (is.null(plot.info)) {
    if (is.null(template)) 
      template <- xcircularp$template
    if (template == "geographics" | template == "clock24") {
      zero <- pi/2
      rotation <- "clock"
    } else {
      if (template == "clock12") {
        zero <- pi/2
        rotation <- "clock"
        modulo <- "pi"
      }
    }
    if (is.null(zero)) 
      zero <- xcircularp$zero
    if (is.null(rotation)) 
      rotation <- xcircularp$rotation
    next.points <- 0
  } else {
    zero <- plot.info$zero
    rotation <- plot.info$rotation
    next.points <- plot.info$next.points
  }
  if (!add) {
    CirclePlotRad(xlim = xlim, ylim = ylim, uin = uin, shrink = shrink, 
      tol = tol, main = main, sub = sub, xlab = xlab, ylab = ylab, 
      control.circle = control.circle)
  }
  if (is.null(bins)) {
    bins <- NROW(x)
  } else {
    bins <- round(bins)
    if (bins <= 0) 
      stop("bins must be non negative")
  }
  if (is.null(border)) {
    border <- seq(nseries)
  } else {
    if (length(border) != nseries) {
      border <- rep(border, length.out = nseries)
    }
  }
  pch <- rep(pch, nseries, length.out = nseries)
  if (axes) {
    axis.circular(units = units, template = template, zero = zero, 
      rotation = rotation, digits = digits, cex = cex, 
      tcl = tcl, tcl.text = tcl.text)
  }
  if (!is.logical(ticks)) 
    stop("ticks must be logical")
  if (ticks) {
    at <- circular((0:bins)/bins * 2 * pi, zero = zero, rotation = rotation)
    ticks.circular(at, tcl = tcl)
  }
  for (iseries in 1:nseries) {
    x <- xx[, iseries]
    x <- c(na.omit(x))
    n <- length(x)
    if (n) {
      x <- conversion.circular(x, units = "radians", modulo = modulo)
      attr(x, "circularp") <- attr(x, "class") <- NULL
      if (template == "clock12") 
        x <- 2 * x
      x <- x%%(2 * pi)
      RosediagRad(x, zero = zero, rotation, bins, upper, 
        radii.scale, prop, border, col = col[iseries])
    }
  }
  return(invisible(list(zero = zero, rotation = rotation, next.points = 0, x = x, bins = bins,
    radii.scale = radii.scale, prop = prop, col = col)))
}

#' Copied as-is from the circular package, so myRoseDiag can have access to it.
#' 
#' Not used directly.
#' 
#' @keywords internal
#' 
CirclePlotRad <- function(xlim = c(-1, 1), ylim = c(-1, 1), uin = NULL, shrink = 1, tol = 0.04, 
  main = "", sub = "", xlab = "", ylab = "", control.circle = circle.control()) {
   xlim <- shrink * xlim
   ylim <- shrink * ylim
   midx <- 0.5 * (xlim[2] + xlim[1])
   xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[1])
   midy <- 0.5 * (ylim[2] + ylim[1])
   ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[1])
   oldpin <- par("pin")
   xuin <- oxuin <- oldpin[1]/diff(xlim)
   yuin <- oyuin <- oldpin[2]/diff(ylim)
   if (is.null(uin)) {
    if (yuin > xuin)
     yuin <- xuin
    else
     xuin <- yuin
   } else {
    if (length(uin) == 1)
     uin <- uin * c(1, 1)
    if (any(c(xuin, yuin) < uin))
     stop("uin is too large to fit plot in")
    xuin <- uin[1]; yuin <- uin[2]
   }
   xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
   ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
   n <- control.circle$n
   x <- cos(seq(0, 2 * pi, length = n))
   y <- sin(seq(0, 2 * pi, length = n))
   axes <- FALSE
   log <- ""
   xaxs <- "i"
   yaxs <- "i"
   ann <-  par("ann")
   frame.plot <- axes
   panel.first <- NULL 
   panel.last <- NULL
   asp <- NA
   plot.default(x = x, y = y, type = control.circle$type, xlim = xlim, ylim = ylim, log = "", main = main, 
  sub = sub, xlab = xlab, ylab = ylab, ann = ann, axes = axes, frame.plot = frame.plot, 
  panel.first = panel.first, panel.last = panel.last, asp = asp, col = control.circle$col, 
  bg = control.circle$bg, pch = control.circle$pch, cex = control.circle$cex, lty = control.circle$lty, 
  lwd = control.circle$lwd)
}

#' Copied as-is from the circular package, so myRoseDiag can have access to it.
#' 
#' Not used directly.
#' 
#' @keywords internal
#' 
RosediagRad <- function(x, zero, rotation, bins, upper, radii.scale, prop, border, col, ...) {
  #### x musts be in modulo 2pi
  n <- length(x)
  freq <- rep(0, bins)
  arc <- (2 * pi)/bins
  if (!is.logical(upper))
     stop("upper must be logical")
  if (upper == TRUE)
     x[x == 0] <- 2*pi
  
  x[x >= 2*pi] <- 2*pi-4*.Machine$double.eps
   # for (i in 1:bins) {
   #  freq[i] <- sum(x < i * arc & x >= (i - 1) * arc)
   # }
  breaks <- seq(0,2*pi,length.out=(bins+1))
  freq <- hist.default(x, breaks=breaks, plot=FALSE, right=upper)$counts   
  rel.freq <- freq/n
  if (rotation == "clock")
     rel.freq <- rev(rel.freq)
  
  if (radii.scale == "sqrt") {
     radius <- sqrt(rel.freq)*prop
  } else {
     radius <- rel.freq*prop
  }
  sector <- seq(0, 2 * pi - (2 * pi)/bins, length = bins)
  mids <- seq(arc/2, 2 * pi - pi/bins, length = bins)
  for (i in 1:bins) {
     if (rel.freq[i] != 0) {
      xx <- c(0, radius[i]*cos(seq(sector[i], sector[i]+(2*pi)/bins, length=1000/bins)+zero), 0)
      yy <- c(0, radius[i]*sin(seq(sector[i], sector[i]+(2*pi)/bins, length=1000/bins)+zero), 0)
      polygon(xx, yy, border=border, col=col)
     }
  }
}

#' Draw rings at absolute points
#' 
#' Adapted from RosediagRad to draw rings inside the circular plot. Called if rings = TRUE in myRoseDiag
#' 
#' @inheritParams myRoseDiag
#' 
#' @keywords internal
#'  
ringsAbs <- function(plot.params, border, rings.lty, 
  ring.text, ring.text.pos, ring.text.cex){
  n <- length(plot.params$x)
  freq <- rep(plot.params$x,plot.params$bins)
  arc <- (2 * pi)/plot.params$bins
  breaks <- seq(0,2*pi,length.out=(plot.params$bins+1))
  freq <- hist.default(plot.params$x, breaks=breaks, plot=FALSE)$counts   
  my.max <- range(freq)[2]
  my.breaks <- my.max/divisors(my.max)
    while(length(my.breaks)<3){
    my.max <- my.max+1
    my.breaks <- divisors(my.max)
    } 
  clean.breaks <- my.breaks[-c(1,length(my.breaks))]
  nlines <- my.max/clean.breaks
  breaker <- clean.breaks[which.min(abs(nlines-4))]
  line.values <- seq(from=breaker,to=my.max,by=breaker)
  rel.values <- line.values/n
  if (plot.params$radii.scale == "sqrt") {
     radius <- sqrt(rel.values)*plot.params$prop
  } else {
     radius <- rel.values*plot.params$prop
  }
  for (i in 1:length(radius)) {
    xx <- c(radius[i]*cos(seq(0, 2*pi, length=1000)+plot.params$zero))
    yy <- c(radius[i]*sin(seq(0, 2*pi, length=1000)+plot.params$zero))
    polygon(xx, yy, border=border, lty=rings.lty)
  }
  if(ring.text){
    text(x=rep(0,length(radius)),y=(radius*-1)+ring.text.pos,line.values,cex=ring.text.cex)
  }
}

#' Draw rings at relative points
#' 
#' Adapted from RosediagRad to draw rings inside the circular plot. Called if rings = TRUE in myRoseDiag
#' 
#' @inheritParams myRoseDiag
#' 
#' @keywords internal
#'  
ringsRel <- function(plot.params, border, rings.lty, 
  ring.text, ring.text.pos, ring.text.cex){
  range <- seq(from=0,to=1/plot.params$prop,length.out=5)
  radius <- c(0.25,0.50,0.75,1)
  line.values <- paste(round(range*100,2),"%",sep="")
  if (plot.params$radii.scale == "sqrt")
    radius <- sqrt(radius)
  for (i in 1:3) {
    xx <- c(radius[i]*cos(seq(0, 2*pi, length=1000)+plot.params$zero))
    yy <- c(radius[i]*sin(seq(0, 2*pi, length=1000)+plot.params$zero))
    polygon(xx, yy, border = border, lty = rings.lty)
  }

  if(ring.text)
    text(x=rep(0,4),y=(radius*-1)+ring.text.pos,line.values[-1],cex=ring.text.cex)
}

#' Draw mean value in the axis margin
#' 
#' Computes and draws the mean value for a given dataset, may also plot standard error of the mean
#' or standard deviation ranges.
#' 
#' @param x the input dataset
#' @param mean.col colour of the mean dash.
#' @param mean.length vertical length of the mean dash.
#' @param mean.lwd width of the mean dash.
#' @param box.range One of "none", "std.error" or "sd", controls the statistic used to draw the range boxes.
#' @param fill Fill colour for the range box.
#' @param border Border colour for the range box.
#' @param box.size Vertical size of the range box.
#' @param edge.length Vertical size of the edge whiskers in the range box.
#' @param edge.lwd Width of the edge whiskers in the range box.
#' 
roseMean <- function(input, col = c("cornflowerblue", "chartreuse3", "deeppink"),
  mean.length = c(0.0125, -0.0125), mean.lwd = 4,
  box.range = c("none", "std.error", "sd"), fill="white", border="black",
  box.size = c(1.015, 0.985), edge.length = c(0.025, -0.025), edge.lwd = 2){
  box.range <- match.arg(box.range)
  col <- scales::alpha(col,1)
  if(is.matrix(input) | is.data.frame(input)){
    plotdata <- list()
    for(i in 1:ncol(input)) plotdata[[i]] <- input[,i]
    names(plotdata) <- colnames(input)
  } else {
  	if (is.list(input)){
  	  if (any(!unlist(lapply(input, function(x) inherits(x, "circular")))))
  	  	stop("Input is a list but not all elements in the list are circular objects.\n")
  	  plotdata <- input
  	} else {
      plotdata <- list(input)
    }
  }
  if (!exists("plotdata"))
  	stop("Input must be a list of circular objects, a data.frame, a matrix or a vector.\n")
  for (i in 1:length(plotdata)) {
	b <- mean.circular(plotdata[[i]], na.rm = T)
	if(box.range != "none"){
	  if(box.range == "std.error")
	    c <- std.error(plotdata[[i]])
	  if(box.range == "sd")
	    c <- sd(plotdata[[i]])
	  zero <- attr(plotdata[[i]],"circularp")$zero # extracted from the circular data
	  left <- as.numeric(conversion.circular((b-c),units="radians"))*-1
	  right <- as.numeric(conversion.circular((b+c),units="radians"))*-1
	    xx <- c(box.size[1]*cos(seq(left, right, length=1000)+zero),rev(box.size[2]*cos(seq(left, right, length=1000)+zero)))
	    yy <- c(box.size[1]*sin(seq(left, right, length=1000)+zero),rev(box.size[2]*sin(seq(left, right, length=1000)+zero)))
	    polygon(xx, yy, col=fill, border=border)
	  lines.circular(c(b+c,b+c), edge.length, lwd = edge.lwd, col = border)
	  lines.circular(c(b-c,b-c), edge.length, lwd = edge.lwd, col = border)
	}
	lines.circular(c(b,b), mean.length, lwd = mean.lwd, col = col[i], lend = 1)
  }
}

#' Extract n characters from the right side of a string
#' 
#' @param x input string
#' @param n number of characters to extract
#' 
right <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#' Calculate the standard error of the mean
#' 
#' @param x input data
#' @param na.rm logical: if TRUE, missing values are removed.
#' 
#' @return SDM
#' 
std.error <- function(x,na.rm=T){
 a <- length(x)
 if(na.rm) x <- x[!is.na(x)]
 output <- sd(x)/sqrt(length(x))
 if(a!=length(x)) cat("M: Ommited",a-length(x),"missing values.\n")
 return(output)
}


divisors <- function(x){
    y <- seq_len(x)
    y[ x%%y == 0 ]
}
