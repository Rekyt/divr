LESPlot3D<-function(yourdata,Xaxis,Yaxis,Zaxis,
                    xlab="X",ylab="Y",zlab="Z",
                    Xticks=NULL,Yticks=NULL,Zticks=NULL,
                    logxyz="",Rotate=110,Flip=25){


  # log
  if(grepl("x",logxyz)==T){
    yourdata[[Xaxis]]<-log10(yourdata[[Xaxis]])
    if(is.null(Xticks)){
      Xticks<-log10(pretty(10^yourdata[[Xaxis]],n=4))
      Xticks<-Xticks[which(Xticks>range(yourdata[[Xaxis]],na.rm = T)[1] & Xticks<range(yourdata[[Xaxis]],na.rm = T)[2])]
    }else{Xticks<-log10(Xticks)}
    Mx<-nchar(10^Xticks[3])
  }else{
    if(is.null(Xticks)){
      Xticks<-pretty(yourdata[[Xaxis]],n=4)
      Xticks<-Xticks[which(Xticks>range(yourdata[[Xaxis]],na.rm = T)[1] & Xticks<range(yourdata[[Xaxis]],na.rm = T)[2])]
    }
      Mx<-nchar(10^Xticks[3])

  }

  if(grepl("y",logxyz)==T){
    yourdata[[Yaxis]]<-log10(yourdata[[Yaxis]])
    if(is.null(Yticks)){
      Yticks<-log10(pretty(10^yourdata[[Yaxis]],n=4))
      Yticks<-Yticks[which(Yticks>range(yourdata[[Yaxis]],na.rm = T)[1] & Yticks<range(yourdata[[Yaxis]],na.rm = T)[2])]
    }
      My<-nchar(10^Yticks[3])

  }else{
    if(is.null(Yticks)){
      Yticks<-pretty(yourdata[[Yaxis]],n=4)
      Yticks<-Yticks[which(Yticks>range(yourdata[[Yaxis]],na.rm = T)[1] & Yticks<range(yourdata[[Yaxis]],na.rm = T)[2])]
    }
      My<-nchar(10^Yticks[3])

  }

  if(grepl("z",logxyz)==T){
    yourdata[[Zaxis]]<-log10(yourdata[[Zaxis]])
    if(is.null(Zticks)){
      Zticks<-log10(pretty(10^yourdata[[Zaxis]],n=4))
      Zticks<-Zticks[which(Zticks>range(yourdata[[Zaxis]],na.rm = T)[1] & Zticks<range(yourdata[[Zaxis]],na.rm = T)[2])]
    }else{Zticks<-log10(Zticks)}
      Mz<-nchar(10^Zticks[3])

  }else{
    if(is.null(Zticks)){
      Zticks<-pretty(yourdata[[Zaxis]],n=4)
      Zticks<-Zticks[which(Zticks>range(yourdata[[Zaxis]],na.rm = T)[1] & Zticks<range(yourdata[[Zaxis]],na.rm = T)[2])]
    }
    Mz<-nchar(10^Zticks[3])

  }

  # limites elargies des plots
  Xmin<-min(yourdata[[Xaxis]],na.rm = T)-(20/100)*(max(yourdata[[Xaxis]],na.rm = T)-min(yourdata[[Xaxis]],na.rm = T))
  Xmax<-max(yourdata[[Xaxis]],na.rm = T)+(20/100)*(max(yourdata[[Xaxis]],na.rm = T)-min(yourdata[[Xaxis]],na.rm = T))
  Ymin<-min(yourdata[[Yaxis]],na.rm = T)-(20/100)*(max(yourdata[[Yaxis]],na.rm = T)-min(yourdata[[Yaxis]],na.rm = T))
  Ymax<-max(yourdata[[Yaxis]],na.rm = T)+(20/100)*(max(yourdata[[Yaxis]],na.rm = T)-min(yourdata[[Yaxis]],na.rm = T))
  Zmin<-min(yourdata[[Zaxis]],na.rm = T)-(20/100)*(max(yourdata[[Zaxis]],na.rm = T)-min(yourdata[[Zaxis]],na.rm = T))
  Zmax<-max(yourdata[[Zaxis]],na.rm = T)+(20/100)*(max(yourdata[[Zaxis]],na.rm = T)-min(yourdata[[Zaxis]],na.rm = T))


#### Ombres ####

  panelfirst <- function(pmat) {

#### plan XY
    XY <- trans3D(yourdata[[Xaxis]],yourdata[[Yaxis]],
                  z = rep(Zmin, nrow(yourdata)), pmat = pmat)
    scatter2D(XY$x, XY$y, col="grey", pch = 16,
              cex = 1, add = TRUE, colkey = FALSE)

    coord1 <- trans3D(Xticks,rep(Ymin,length(Xticks)),z = rep(Zmin, length(Xticks)), pmat = pmat)
    coord2 <- trans3D(Xticks,rep(Ymax,length(Xticks)),z = rep(Zmin, length(Xticks)), pmat = pmat)
    segments2D(x0 = coord1$x,y0 =  coord1$y,x1 = coord2$x,y1 =  coord2$y,lwd=1.5,add=T,col="grey", colkey = FALSE)
    coord1 <- trans3D(rep(Xmin,length(Yticks)),Yticks,z = rep(Zmin, length(Yticks)), pmat = pmat)
    coord2 <- trans3D(rep(Xmax,length(Yticks)),Yticks,z = rep(Zmin, length(Yticks)), pmat = pmat)
    segments2D(x0 = coord1$x,y0 =  coord1$y,x1 = coord2$x,y1 =  coord2$y,lwd=1.5,add=T,col="grey", colkey = FALSE)

    CoordLab <- trans3D(x = rep(Xmax+((My/4*(5/100))+(5/100))*(max(yourdata[[Xaxis]],na.rm = T)-min(yourdata[[Xaxis]],na.rm = T)),length(Yticks)),
                        y = Yticks,
                        z = rep(Zmin, length(Yticks)), pmat = pmat)
    CoordLabel <- trans3D(x = Xmax+((My/4*(20/100))+(10/100))*(max(yourdata[[Xaxis]],na.rm = T)-min(yourdata[[Xaxis]],na.rm = T)),
                          y = mean(Yticks),
                          z = Zmin, pmat = pmat)
    angle<-atan((CoordLab$y[1]-CoordLab$y[length(CoordLab$y)])/(CoordLab$x[1]-CoordLab$x[length(CoordLab$x)]))*180/pi
    text2D(x = CoordLabel$x, y = CoordLabel$y, labels = ylab,add = T,colkey = FALSE,srt=angle)

    if(grepl("y",logxyz)==T){
      text2D(x = CoordLab$x, y = CoordLab$y, labels = 10^Yticks,add = T,colkey = FALSE)
    }else{text2D(x = CoordLab$x, y = CoordLab$y, labels = Yticks,add = T,colkey = FALSE)}


#### plan ZY
    XY <- trans3D(x = rep(Xmin, nrow(yourdata)), y = yourdata[[Yaxis]],
                  z = yourdata[[Zaxis]], pmat = pmat)
    scatter2D(XY$x, XY$y, col="grey", pch = 16,
              cex = 1, add = TRUE, colkey = FALSE)

    coord1 <- trans3D(x = rep(Xmin, length(Yticks)),Yticks,z = rep(Zmin, length(Yticks)), pmat = pmat)
    coord2 <- trans3D(x = rep(Xmin,length(Yticks)),Yticks, z = rep(Zmax, length(Yticks)), pmat = pmat)
    segments2D(x0 = coord1$x,y0 =  coord1$y,x1 = coord2$x,y1 =  coord2$y,lwd=1.5,add=T,col="grey", colkey = FALSE)
    coord1 <- trans3D(x = rep(Xmin, length(Zticks)),rep(Ymin,length(Zticks)),Zticks, pmat = pmat)
    coord2 <- trans3D(x = rep(Xmin, length(Zticks)),rep(Ymax,length(Zticks)),Zticks, pmat = pmat)
    segments2D(x0 = coord1$x,y0 =  coord1$y,x1 = coord2$x,y1 =  coord2$y,lwd=1.5,add=T,col="grey", colkey = FALSE)

    CoordLab <- trans3D(x = rep(Xmax+((Mz/4*(5/100))+(5/100))*(max(yourdata[[Xaxis]],na.rm = T)-min(yourdata[[Xaxis]],na.rm = T)),length(Zticks)),
                        y = rep(Ymin, length(Zticks)),
                        z = Zticks, pmat = pmat)
    CoordLabel <- trans3D(x = Xmax+((Mz/4*(20/100))+(10/100))*(max(yourdata[[Xaxis]],na.rm = T)-min(yourdata[[Xaxis]],na.rm = T)),
                          y = Ymin,
                          z = mean(range(Zticks)), pmat = pmat)
    angle<-atan((CoordLab$y[1]-CoordLab$y[length(CoordLab$y)])/(CoordLab$x[1]-CoordLab$x[length(CoordLab$x)]))*180/pi+180
    text2D(x = CoordLabel$x, y = CoordLabel$y, labels = zlab,add = T,colkey = FALSE,srt=angle)

    if(grepl("z",logxyz)==T){text2D(x = CoordLab$x, y = CoordLab$y, labels = 10^Zticks,add = T,colkey = FALSE)
    }else{text2D(x = CoordLab$x, y = CoordLab$y, labels = Zticks,add = T,colkey = FALSE)}

#### plan XZ
    XY <- trans3D(x = yourdata[[Xaxis]], y = rep(Ymin,nrow(yourdata)),
                  z = yourdata[[Zaxis]], pmat = pmat)
    scatter2D(XY$x, XY$y, col="grey", pch = 16,
              cex = 1, add = TRUE, colkey = FALSE)

    coord1 <- trans3D(x = Xticks, y = rep(Ymin,length(Xticks)), z = rep(Zmin, length(Xticks)), pmat = pmat)
    coord2 <- trans3D(x = Xticks, y = rep(Ymin,length(Xticks)), z = rep(Zmax, length(Xticks)), pmat = pmat)
    segments2D(x0 = coord1$x,y0 =  coord1$y,x1 = coord2$x,y1 =  coord2$y,lwd=1.5,add=T,col="grey", colkey = FALSE)
    coord1 <- trans3D(x = rep(Xmin, length(Zticks)),rep(Ymin,length(Zticks)), z = Zticks, pmat = pmat)
    coord2 <- trans3D(x = rep(Xmax, length(Zticks)),rep(Ymin,length(Zticks)), z = Zticks, pmat = pmat)
    segments2D(x0 = coord1$x, y0 =  coord1$y, x1 = coord2$x, y1 =  coord2$y, lwd=1.5, add=T, col="grey", colkey = FALSE)

    M<-nchar(Xticks[1])
    CoordLab <- trans3D(x = Xticks,
                        y = rep(Ymax+((Mx/4*(5/100))+(5/100))*(max(yourdata[[Yaxis]],na.rm = T)-min(yourdata[[Yaxis]],na.rm = T)),length(Xticks)),
                        z = rep(Zmin, length(Xticks)), pmat = pmat)
    CoordLabel <- trans3D(x = mean(range(Xticks)),
                          y = Ymax+((Mx/4*(20/100))+(10/100))*(max(yourdata[[Yaxis]],na.rm = T)-min(yourdata[[Yaxis]],na.rm = T)),
                          z = Zmin, pmat = pmat)
    angle<-atan((CoordLab$y[1]-CoordLab$y[length(CoordLab$y)])/(CoordLab$x[1]-CoordLab$x[length(CoordLab$x)]))*180/pi
    text2D(x = CoordLabel$x, y = CoordLabel$y, labels = xlab,add = T,colkey = FALSE,srt=angle)

    if(grepl("x",logxyz)==T){text2D(x = CoordLab$x, y = CoordLab$y, labels = 10^Xticks,add = T,colkey = FALSE)
    }else{text2D(x = CoordLab$x, y = CoordLab$y, labels = Xticks,add = T,colkey = FALSE)}


  }



#### Plot 3D

  scatter3D(yourdata[[Xaxis]], yourdata[[Yaxis]], yourdata[[Zaxis]],
            panel.first = panelfirst ,
            col= "black", pch=16,
            theta=Rotate, phi = Flip,
            xlab=xlab, ylab=ylab, zlab=zlab,
            cex.lab=0.8,axes=F,
            xlim=c(Xmin,Xmax),ylim=c(Ymin,Ymax),zlim=c(Zmin,Zmax))


}
