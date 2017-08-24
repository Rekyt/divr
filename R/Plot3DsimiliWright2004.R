#' LES Plot3D
#'
#' Produce the same 3D Plot as found in Wrigth 2004 paper.
#'
#' @param yourdata a datasheet containing the variables to plot.
#' @param Xaxis name of the variable to plot in X
#' @param xlab label for x axis
#' @param logxyz character containing the axes to log (log10)
#' @param Rotate parameter to moove the plot around a vertical axe
#' @param Flip parameter to moove the plot around a horizontal axe
#'
#' @examples
#'
#' LESPlot3D(iris, "Sepal.Length", "Sepal.Width", "Petal.Length",
#'           "Sepal.Length", "Sepal.Width", "Petal.Length")
#' @export

LESPlot3D <- function(yourdata, Xaxis, Yaxis, Zaxis, xlab = "X", ylab = "Y",
                      zlab = "Z", logxyz = "", Rotate = 110, Flip = 25) {


  # log
  if (grepl("x", logxyz)) {
    yourdata[[Xaxis]] <- log10(yourdata[[Xaxis]])
    Xrange <- log10(pretty(10^yourdata[[Xaxis]],n=4))
    Xrange <- Xrange[which(Xrange > range(yourdata[[Xaxis]],na.rm = T)[1] &
                             Xrange < range(yourdata[[Xaxis]],na.rm = T)[2])]
  } else {
    Xrange <- pretty(yourdata[[Xaxis]],n=4)
    Xrange <- Xrange[which(Xrange > range(yourdata[[Xaxis]],na.rm = T)[1] &
                             Xrange < range(yourdata[[Xaxis]],na.rm = T)[2])]
  }

  if (grepl("y", logxyz)) {
    yourdata[[Yaxis]] <- log10(yourdata[[Yaxis]])
    Yrange <- log10(pretty(10^yourdata[[Yaxis]], n = 4))
    Yrange <- Yrange[which(Yrange > range(yourdata[[Yaxis]], na.rm = T)[1] &
                             Yrange < range(yourdata[[Yaxis]],na.rm = T)[2])]
  } else {
    Yrange <- pretty(yourdata[[Yaxis]], n = 4)
    Yrange <- Yrange[which(Yrange > range(yourdata[[Yaxis]], na.rm = T)[1] &
                             Yrange < range(yourdata[[Yaxis]], na.rm = T)[2])]
  }
  if (grepl("z", logxyz)) {
    yourdata[[Zaxis]] <- log10(yourdata[[Zaxis]])
    Zrange <- log10(pretty(10^yourdata[[Zaxis]], n = 4))
    Zrange <- Zrange[which(Zrange > range(yourdata[[Zaxis]],na.rm = T)[1] &
                             Zrange < range(yourdata[[Zaxis]],na.rm = T)[2])]
  } else {
    Zrange <- pretty(yourdata[[Zaxis]], n = 4)
    Zrange <- Zrange[which(Zrange > range(yourdata[[Zaxis]], na.rm = T)[1] &
                             Zrange < range(yourdata[[Zaxis]], na.rm = T)[2])]
  }

  # limites elargies des plots
  Xmin <- min(yourdata[[Xaxis]],na.rm = T) -
    (20/100)*(max(yourdata[[Xaxis]],na.rm = T) -
                min(yourdata[[Xaxis]],na.rm = T))
  Xmax <- max(yourdata[[Xaxis]],na.rm = T) +
    (20/100)*(max(yourdata[[Xaxis]],na.rm = T) -
                min(yourdata[[Xaxis]],na.rm = T))
  Ymin <- min(yourdata[[Yaxis]],na.rm = T) -
    (20/100)*(max(yourdata[[Yaxis]],na.rm = T) -
                min(yourdata[[Yaxis]],na.rm = T))
  Ymax <- max(yourdata[[Yaxis]],na.rm = T) +
    (20/100)*(max(yourdata[[Yaxis]],na.rm = T) -
                min(yourdata[[Yaxis]],na.rm = T))
  Zmin <- min(yourdata[[Zaxis]],na.rm = T) -
    (20/100)*(max(yourdata[[Zaxis]],na.rm = T) -
                min(yourdata[[Zaxis]],na.rm = T))
  Zmax <- max(yourdata[[Zaxis]],na.rm = T) +
    (20/100)*(max(yourdata[[Zaxis]],na.rm = T) -
                min(yourdata[[Zaxis]],na.rm = T))


  # Ombres

  panelfirst <- function(pmat) {

    #plan XY
    XY <- trans3D(yourdata[[Xaxis]],yourdata[[Yaxis]],
                  z = rep(Zmin, nrow(yourdata)), pmat = pmat)
    scatter2D(XY$x, XY$y, col = "grey", pch = 16,
              cex = 1, add = TRUE, colkey = FALSE)

    coord1 <- trans3D(Xrange,rep(Ymin,length(Xrange)),
                      z = rep(Zmin, length(Xrange)), pmat = pmat)
    coord2 <- trans3D(Xrange,rep(Ymax,length(Xrange)),
                      z = rep(Zmin, length(Xrange)), pmat = pmat)

    segments2D(x0 = coord1$x,y0 =  coord1$y,x1 = coord2$x,y1 = coord2$y,
               lwd = 1.5, add = T, col = "grey", colkey = FALSE)
    coord1 <- trans3D(rep(Xmin, length(Yrange)), Yrange,
                      z = rep(Zmin, length(Yrange)), pmat = pmat)
    coord2 <- trans3D(rep(Xmax, length(Yrange)), Yrange,
                      z = rep(Zmin, length(Yrange)), pmat = pmat)
    segments2D(x0 = coord1$x, y0 = coord1$y, x1 = coord2$x, y1 = coord2$y,
               lwd = 1.5, add = T, col="grey", colkey = FALSE)

    CoordLab <- trans3D(x = rep(Xmax + (10/100)*(max(yourdata[[Xaxis]], na.rm = T) -
                                                   min(yourdata[[Xaxis]], na.rm = T)), length(Yrange)),
                        y = Yrange,
                        z = rep(Zmin, length(Yrange)), pmat = pmat)
    if (grepl("y", logxyz) ==T){
      text2D(x = CoordLab$x, y = CoordLab$y, labels = 10^Yrange,add = T,colkey = FALSE)
    }else{text2D(x = CoordLab$x, y = CoordLab$y, labels = Yrange,add = T,colkey = FALSE)}


    #plan ZY
    XY <- trans3D(x = rep(Xmin, nrow(yourdata)), y = yourdata[[Yaxis]],
                  z = yourdata[[Zaxis]], pmat = pmat)
    scatter2D(XY$x, XY$y, col="grey", pch = 16,
              cex = 1, add = TRUE, colkey = FALSE)

    coord1 <- trans3D(x = rep(Xmin, length(Yrange)),Yrange,z = rep(Zmin, length(Yrange)), pmat = pmat)
    coord2 <- trans3D(x = rep(Xmin,length(Yrange)),Yrange, z = rep(Zmax, length(Yrange)), pmat = pmat)
    segments2D(x0 = coord1$x,y0 =  coord1$y,x1 = coord2$x,y1 =  coord2$y,lwd=1.5,add=T,col="grey", colkey = FALSE)
    coord1 <- trans3D(x = rep(Xmin, length(Zrange)),rep(Ymin,length(Zrange)),Zrange, pmat = pmat)
    coord2 <- trans3D(x = rep(Xmin, length(Zrange)),rep(Ymax,length(Zrange)),Zrange, pmat = pmat)
    segments2D(x0 = coord1$x,y0 =  coord1$y,x1 = coord2$x,y1 =  coord2$y,lwd=1.5,add=T,col="grey", colkey = FALSE)

    CoordLab <- trans3D(x = rep(Xmax+(10/100)*(max(yourdata[[Xaxis]],na.rm = T)-min(yourdata[[Xaxis]],na.rm = T)),length(Zrange)),
                        y = rep(Ymin, length(Zrange)),
                        z = Zrange, pmat = pmat)
    if (grepl("z",logxyz)) {
      text2D(x = CoordLab$x, y = CoordLab$y, labels = 10^Zrange,add = T,colkey = FALSE)
    }else{text2D(x = CoordLab$x, y = CoordLab$y, labels = Zrange,add = T,colkey = FALSE)}

    #plan XZ
    XY <- trans3D(x = yourdata[[Xaxis]], y = rep(Ymin,nrow(yourdata)),
                  z = yourdata[[Zaxis]], pmat = pmat)
    scatter2D(XY$x, XY$y, col="grey", pch = 16,
              cex = 1, add = TRUE, colkey = FALSE)

    coord1 <- trans3D(x = Xrange, y = rep(Ymin,length(Xrange)), z = rep(Zmin, length(Xrange)), pmat = pmat)
    coord2 <- trans3D(x = Xrange, y = rep(Ymin,length(Xrange)), z = rep(Zmax, length(Xrange)), pmat = pmat)
    segments2D(x0 = coord1$x,y0 =  coord1$y,x1 = coord2$x,y1 =  coord2$y,lwd=1.5,add=T,col="grey", colkey = FALSE)
    coord1 <- trans3D(x = rep(Xmin, length(Zrange)),rep(Ymin,length(Zrange)), z = Zrange, pmat = pmat)
    coord2 <- trans3D(x = rep(Xmax, length(Zrange)),rep(Ymin,length(Zrange)), z = Zrange, pmat = pmat)
    segments2D(x0 = coord1$x, y0 =  coord1$y, x1 = coord2$x, y1 =  coord2$y, lwd=1.5, add=T, col="grey", colkey = FALSE)

    CoordLab <- trans3D(x = Xrange,
                        y = rep(Ymax+(10/100)*(max(yourdata[[Yaxis]],na.rm = T)-min(yourdata[[Yaxis]],na.rm = T)),length(Xrange)),
                        z = rep(Zmin, length(Xrange)), pmat = pmat)
    if (grepl("x",logxyz)){
      text2D(x = CoordLab$x, y = CoordLab$y, labels = 10^Xrange,add = T,colkey = FALSE)
    } else {text2D(x = CoordLab$x, y = CoordLab$y, labels = Xrange,add = T,colkey = FALSE)}


  }



  # Plot 3D

  scatter3D(yourdata[[Xaxis]], yourdata[[Yaxis]], yourdata[[Zaxis]],
            panel.first = panelfirst, col = "black", pch = 16, theta = Rotate,
            phi = Flip, xlab = xlab, ylab = ylab, zlab = zlab, cex.lab = 0.8,
            labels = rep("",length(Xrange) + length(Yrange) + length(Zrange)),
            xlim = c(Xmin,Xmax), ylim = c(Ymin,Ymax), zlim = c(Zmin,Zmax))
}
