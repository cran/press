
## make the plot with contours and scattered points for alpha & tau
decoy.ramachandran.plot1 <- function(x.scatter, y.scatter,show,
    x.grid, y.grid , z.grid, plot.title,levels,col)
{
    xlim = range(x.grid, finite = TRUE)
    ylim = range(y.grid, finite = TRUE)
    zlim = range(z.grid, finite = TRUE)
    color.palette = cm.colors
    asp = NA
    xaxs = "i"
    yaxs = "i"
    las = 1
    axes = TRUE
    frame.plot = axes
    
    if (missing(z.grid)) {
        stop("no 'z.grid' matrix specified")
    }
    else if (is.list(x.grid)) {
        y.grid <- x.grid$y
        x.grid <- x.grid$x
    }
    if (any(diff(x.grid) <= 0) || any(diff(y.grid) <= 0))
        stop("increasing 'x.grid' and 'y.grid' values expected")

    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)

    if (!is.matrix(z.grid) || nrow(z.grid) <= 1 || ncol(z.grid) <= 1)
        stop("no proper 'z.grid' matrix specified")
    if (!is.double(z.grid))
        storage.mode(z.grid) <- "double"
    .Internal(filledcontour(as.double(x.grid), as.double(y.grid), z.grid, as.double(levels),
        col = col))

    if (!(missing(x.scatter)) && !(missing(y.scatter))) {
          plot.xy(xy.coords(x.scatter,y.scatter,NULL,NULL,NULL,NULL),
                xlim=xlim, ylim=ylim, xlab="", ylab="", asp=asp,
          type="p", pch=24, cex=0.4,col="black")
     text(40,230,paste(show[1],"% in allowed\n(90%)region",sep=""),col='#B3E8FF')
     text(40,180,paste(show[2],"% in favoured\n(75%)region",sep=""),col='#7FD9FF')
     text(40,130,paste(show[3],"% in most favoured\n(50%)region",sep=""),col='dodgerblue3')
    }
    title(main=plot.title, xlab=expression(alpha), ylab=expression(tau))
    axis(1, at=c(0,30,60,90,120,150,180))
    axis(2, at=c(0,60,120,180,240,300,360))
    box()
    legend("topleft",c("50%","75%", "90%"), col = c('dodgerblue3','#7FD9FF','#B3E8FF'),
    text.col = c('dodgerblue3','#7FD9FF','#B3E8FF'),lty = c(1,1,  1),  pch = c(19, 19,19),cex=1.5,
    merge = TRUE)

}





## make the plot with contours and scattered points  for beta vs tau
decoy.ramachandran.plot2 <- function(x.scatter, y.scatter,show,
    x.grid, y.grid , z.grid, plot.title,levels,col )
{
    xlim = range(x.grid, finite = TRUE)
    ylim = range(y.grid, finite = TRUE)
    zlim = range(z.grid, finite = TRUE)
    color.palette = cm.colors
    asp = NA
    xaxs = "i"
    yaxs = "i"
    las = 1
    axes = TRUE
    frame.plot = axes
    if (missing(z.grid)) {
        stop("no 'z.grid' matrix specified")
    }
    else if (is.list(x.grid)) {
        y.grid <- x.grid$y
        x.grid <- x.grid$x
    }
    if (any(diff(x.grid) <= 0) || any(diff(y.grid) <= 0))
        stop("increasing 'x.grid' and 'y.grid' values expected")

    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)

    if (!is.matrix(z.grid) || nrow(z.grid) <= 1 || ncol(z.grid) <= 1)
        stop("no proper 'z.grid' matrix specified")
    if (!is.double(z.grid))
        storage.mode(z.grid) <- "double"
    .Internal(filledcontour(as.double(x.grid), as.double(y.grid), z.grid, as.double(levels),
        col = col))

    if (!(missing(x.scatter)) && !(missing(y.scatter))) {
          plot.xy(xy.coords(x.scatter,y.scatter,NULL,NULL,NULL,NULL),
                xlim=xlim, ylim=ylim, xlab="", ylab="", asp=asp,
          type="p", pch=24, cex=0.4,col="black")
     text(40,230,paste(show[1],"% in allowed\n(90%)region",sep=""),col="#C0FFC0")
     text(40,180,paste(show[2],"% in favoured\n(75%)region",sep=""),col="#80FF80")
     text(40,130,paste(show[3],"% in most favoured\n(50%)region",sep=""),col="chartreuse3")

    }

     title(main=plot.title, xlab=expression(beta), ylab=expression(tau))
     axis(1, at=c(0,30,60,90,120,150,180))
     axis(2, at=c(0,60,120,180,240,300,360))
     box()
    legend("topleft",c("50%","75%", "90%"), col = c("chartreuse3","#80FF80","#C0FFC0"),
    text.col = c("chartreuse3","#80FF80","#C0FFC0"),lty = c(1,1,  1),  pch = c(19, 19,19),cex=1.5,
    merge = TRUE)

}

