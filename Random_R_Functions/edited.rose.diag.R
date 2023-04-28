myRoseDiag <- function (x, pch = 16, cex = 1, axes = TRUE, shrink = 1, bins = NULL, 
    upper = TRUE, ticks = TRUE, tcl = 0.025, tcl.text = 0.125, 
    radii.scale = c("sqrt", "linear"), border = NULL, col = NULL, 
    tol = 0.04, uin = NULL, xlim = c(-1, 1), ylim = c(-1, 1), 
    prop = 1, digits = 2, plot.info = NULL, units = NULL, template = NULL, 
    zero = NULL, rotation = NULL, main = NULL, sub = NULL, xlab = "", 
    ylab = "", add = FALSE, control.circle = circle.control(), stairs= FALSE, 
    annotated=FALSE, stairs.lty = 2, note.pos=-0.04, note.cex = 1) 
{
    radii.scale <- match.arg(radii.scale)
    if (is.matrix(x) | is.data.frame(x)) {
        nseries <- ncol(x)
    }
    else {
        nseries <- 1
    }
    xx <- as.data.frame(x)
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
        }
        else if (template == "clock12") {
            zero <- pi/2
            rotation <- "clock"
            modulo <- "pi"
        }
        else {
            if (is.null(zero)) 
                zero <- xcircularp$zero
            if (is.null(rotation)) 
                rotation <- xcircularp$rotation
        }
        next.points <- 0
    }
    else {
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
    }
    else {
        bins <- round(bins)
        if (bins <= 0) 
            stop("bins must be non negative")
    }
    if (is.null(border)) {
        border <- seq(nseries)
    } else {
        if (length(border) != nseries) {
            border <- rep(border, nseries)[1:nseries]
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
        x <- na.omit(x)
        n <- length(x)
        if (n) {
            x <- conversion.circular(x, units = "radians", modulo = modulo)
            attr(x, "circularp") <- attr(x, "class") <- NULL
            if (template == "clock12") 
                x <- 2 * x
            x <- x%%(2 * pi)
            RosediagRad(x, zero = zero, rotation, bins, upper, 
                radii.scale, prop, border[iseries], col)
            if(stairs=="absolute")
            abs.stairs(x,zero,bins,radii.scale,prop,border,stairs.lty,
              annotated=annotated,note.pos,note.cex)
            if(stairs=="relative")
            rel.stairs(zero,radii.scale,prop,stairs.lty,annotated,note.pos,note.cex)
        }
    }
    return(invisible(list(zero = zero, rotation = rotation, next.points = 0)))
}

CirclePlotRad <- function(xlim=c(-1,1), ylim=c(-1,1), uin=NULL, shrink=1, tol=0.04, main=NULL, sub=NULL, xlab=NULL, ylab=NULL, control.circle=circle.control()) {
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
   plot.default(x=x, y=y, type=control.circle$type, xlim=xlim, ylim=ylim, log="", main=main, sub=sub, xlab=xlab, ylab=ylab, ann=ann, axes=axes, frame.plot=frame.plot, panel.first=panel.first, panel.last=panel.last, asp=asp, col=control.circle$col, bg=control.circle$bg, pch=control.circle$pch, cex=control.circle$cex, lty=control.circle$lty, lwd=control.circle$lwd)
}

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
     #    freq[i] <- sum(x < i * arc & x >= (i - 1) * arc)
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

abs.stairs <- function(x,zero,bins,radii.scale,prop,border,stairs.lty,annotated,note.pos,note.cex)
{
    n <- length(x)
    freq <- rep(x,bins)
    arc <- (2 * pi)/bins
    breaks <- seq(0,2*pi,length.out=(bins+1))
    freq <- hist.default(x, breaks=breaks, plot=FALSE)$counts   
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
    if (radii.scale == "sqrt") {
       radius <- sqrt(rel.values)*prop
    } else {
       radius <- rel.values*prop
    }
    for (i in 1:length(radius)) {
      xx <- c(radius[i]*cos(seq(0, 2*pi, length=1000)+zero))
      yy <- c(radius[i]*sin(seq(0, 2*pi, length=1000)+zero))
      polygon(xx, yy, border=border, lty=stairs.lty)
    }
    if(annotated){
      text(x=rep(0,length(radius)),y=(radius*-1)+note.pos,line.values,cex=note.cex)
    }
}

rel.stairs <- function(zero,radii.scale,prop,stairs.lty,annotated,note.pos,note.cex)
{
    range <- seq(from=0,to=1/prop,length.out=5)
    radius <- c(0.25,0.50,0.75,1)
    line.values <- paste(round(range*100,2),"%",sep="")
    if (radii.scale == "sqrt")
      radius <- sqrt(radius)
    for (i in 1:3) {
      xx <- c(radius[i]*cos(seq(0, 2*pi, length=1000)+zero))
      yy <- c(radius[i]*sin(seq(0, 2*pi, length=1000)+zero))
      polygon(xx, yy, lty=stairs.lty)
    }

    if(annotated)
      text(x=rep(0,4),y=(radius*-1)+note.pos,line.values[-1],cex=note.cex)
    
}

rose.mean <- function(x,mean.col,mean.length=c(0.0125,-0.0125),mean.lwd=4,
       box.range=c("none","std.error","sd"), fill="white", border="black",
       box.size=c(1.015,0.985),edge.length=c(0.025,-0.025),edge.lwd=2)
{
  if(!missing(box.range) & length(box.range)>1) stop("Please select one 'range' method.")
  box.range <- match.arg(box.range)
  b <- mean.circular(x)
  if(box.range!="none"){
    if(box.range=="std.error")
      c <- std.error(x)
    if(box.range=="sd")
      c <- sd(x)
    zero <- attr(x,"circularp")$zero # extracted from the circular data
    left <- as.numeric(conversion.circular((b-c),units="radians"))*-1
    right <- as.numeric(conversion.circular((b+c),units="radians"))*-1
        xx <- c(box.size[1]*cos(seq(left, right, length=1000)+zero),rev(box.size[2]*cos(seq(left, right, length=1000)+zero)))
        yy <- c(box.size[1]*sin(seq(left, right, length=1000)+zero),rev(box.size[2]*sin(seq(left, right, length=1000)+zero)))
        polygon(xx, yy, col=fill, border=border)
    lines.circular(c(b+c,b+c),edge.length,lwd=edge.lwd, col=border)
    lines.circular(c(b-c,b-c),edge.length,lwd=edge.lwd, col=border)
  }
  lines.circular(c(b,b),mean.length,lwd=mean.lwd, col=mean.col,lend=1)
}
