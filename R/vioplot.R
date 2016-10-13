#' Violon plots
#'
#' Produce a violon plot taking into acount weightings
#'
#' @param x a numeric vector of data
#' @param weights a numeric vector with the same length as x
#' @param xlim limit of x in plot
#' @param ylim limit of y in plot
#' @param xlab label for x axis
#' @param label general plot label
#' @param wex the maximum value for the density curve
#' @param h the kernel density smoothing parameter
#' @param add if TRUE adds the plot to the current plot window
#' @param nblim minimum length of x for computing the kernel density
#' @param at ?
#' @param pch points aspect
#' @param lwd ?
#' @param col ?
#' @param col.points ?
#' @param lty ?
#' @param cex ?
#' @param las ?
#' @param boxplot logical value for adding a boxplot or not
#' @param bg background color
#' @param side a character value describind if the distribution should be drawn
#'        in the "above", "below", "right", "left" or "both" side(s)
#' @param fill the color of the distribution area
#' @param pts logical value for adding x data on the curve distrivution
#' @param boxplot.fill the color of the boxplot
#' @param horizontal logical value
#'
#' @examples
#'
#' data("ChickWeight")
#' vioplot(x = chickwts$weight, horizontal = FALSE, side="both", fill="grey70",
#'         boxplot.fill="grey30", label = "Weights", xlab = "none")
#'
#' @seealso
#'
#' For group comparison see vioplotGp
#'
#' @export

vioplot<-function(x, weights = NULL, xlim = NA, ylim = NA, xlab = NA,
                  label = "none", wex = .75, h = NA, add = F, nblim = 10, at = 1,
                  pch = 19, lwd = 1, col = 1, col.points = 1, lty = 1, cex = 1,
                  las = 0, boxplot = TRUE, bg = 1, side = "above", fill = NA,
                  pts = TRUE, boxplot.fill = "white", horizontal = TRUE){
  # weights
  w<-weights
  n<-length(x)
  if (is.null(weights)) w<-rep(1/n,n) else if (length(weights)!=n) stop("Weight length not correct")
  w<-w/sum(w)
  if (length(unique(w))==1) wcex<-rep(0.4,n) else wcex<-(w-min(w))/(max(w)-min(w))

  # h
  if (is.na(h)) distri<-density(x, weights = w) else distri<-density(x, weights=w, bw=h)
  h=distri$bw

  # side
  if (!side%in%c("above","below","right","left","both")) stop('side argument should be "above","below", "right", "left" or "both"')

  # data descriptors
  nb<-length(x)
  gamme<-range(x)
  quart<-quantile(x)

  # point projection function
  fyy<-function(x){
    p<-max(which(distri$x<x))
    yinf<-distri$y[p]
    ysup<-distri$y[p+1]
    xinf<-distri$x[p]
    xsup<-distri$x[p+1]
    yp<-yinf+(ysup-yinf)*(x-xinf)/(xsup-xinf)

    yp<-yp
    return(yp)
  }
  xx<-c(gamme[1],distri$x[distri$x>gamme[1]&distri$x<gamme[2]],gamme[2])
  yy<-c(fyy(gamme[1]),distri$y[distri$x>gamme[1]&distri$x<gamme[2]],fyy(gamme[2]))


  # plot window and par
  par0<-par()[c("col","lty","lwd","cex", "pch")]
  par(col=col,lty=lty,pch=pch, lwd=lwd, cex=cex)

  if(!add){
    if(horizontal){
      plot.new()
      if (any(is.na(xlim))) xlimi<-c(min(x),max(x)) else xlimi=xlim
      if (any(is.na(ylim))) ylimi<-c(min(at)-1,max(at)+1) else ylimi=ylim
      plot.window(xlim=xlimi, ylim=ylimi)
      box(which ="plot",col=1, lwd=1)
      axis(1, las=las)
      if (any(label!="none")){
        axis(2, at=at,labels=label, las=las)
      }
      if(all(is.na(xlab))){
        title(xlab=deparse(substitute(x)))
      }else if (any(xlab!="none")){
        title(xlab=xlab)
      }
    }else{
      plot.new()
      if (any(is.na(xlim))) ylimi<-c(min(x),max(x)) else ylimi=xlim
      if (any(is.na(ylim))) xlimi<-c(min(at)-1,max(at)+1) else xlimi=ylim
      plot.window(xlim=xlimi, ylim=ylimi)
      box(which ="plot",col=1, lwd=1)
      axis(2, las=las)
      if (any(label!="none")){
        axis(1, at=at,labels=label, las=las)
      }
      if(all(is.na(xlab))){
        title(ylab=deparse(substitute(x)))
      }else if (any(xlab!="none")){
        title(ylab=xlab)
      }
    }
  }

  # density if nb>nblim
  if(nb>=nblim){
    if (side=="above"|side=="right"|side=="both"){
      med<-fyy(quart[3])*(wex/max(yy))+at
      yyd<-mapply(fyy,x)*(wex/max(yy))+at
      if (horizontal){
        if (!is.na(fill)) polygon(c(xx,rev(xx)),c(yy*(wex/max(yy))+at,rep(at,length(xx))), col = fill, border = NA )
        #if(boxplot) segments(quart[3],at+.05,quart[3],med,col = col)
        lines(xx,yy*(wex/max(yy))+at)
        if (pts) points(x,yyd, col=col.points, bg=bg, cex=2*wcex+.2)
      }else{
        if (!is.na(fill)) polygon(c(yy*(wex/max(yy))+at,rep(at,length(xx))), c(xx,rev(xx)),col = fill, border = NA )
        #if(boxplot) segments(at+.05,quart[3],med,quart[3],col = col)
        lines(yy*(wex/max(yy))+at,xx)
        if (pts) points(yyd,x,bg=bg, col=col.points, cex=2*wcex+.2)
      }
    }
    if (side=="below"|side=="left"|side=="both"){
      med<-at-fyy(quart[3])*(wex/max(yy))
      yyd<-at-mapply(fyy,x)*(wex/max(yy))
      if (horizontal){
        if (!is.na(fill)) polygon(c(xx,rev(xx)),c(at-yy*(wex/max(yy)),rep(at,length(xx))), col = fill, border = NA )
        #if(boxplot) segments(quart[3],at-.05,quart[3],med,col = col)
        lines(xx,at-yy*(wex/max(yy)))
        if (pts) points(x,yyd, bg=bg, col=col.points, cex=2*wcex+.2)
      }else{
        if (!is.na(fill)) polygon(c(at-yy*(wex/max(yy)),rep(at,length(xx))), c(xx,rev(xx)),col = fill, border = NA )
        #if(boxplot) segments(at-.05,quart[3],med,quart[3],col = col)
        lines(at-yy*(wex/max(yy)),xx)
        if (pts) points(yyd,x,bg=bg, col=col.points, cex=2*wcex+.2)
      }
    }
  }

  #boxplot if boxplot
  if(boxplot){
    if (horizontal){
      segments(xx[1],at-.05,xx[1],at+.05,col = col)
      segments(xx[length(xx)],at-.05,xx[length(xx)],at+.05,col = col)
      segments(xx[1],at,quart[2],at,col = col)
      segments(quart[4],at,xx[length(xx)],at,col = col)
      rect(quart[2],at-.05,quart[4],at+.05, border=col, col=boxplot.fill)
      points(quart[3],at, bg=bg)
    }else{
      segments(at-.05,xx[1],at+.05,xx[1],col = col)
      segments(at-.05,xx[length(xx)],at+.05,xx[length(xx)],col = col)
      segments(at,xx[1],at,quart[2],col = col)
      segments(at,quart[4],at,xx[length(xx)],col = col)
      rect(at-.05,quart[2],at+.05, quart[4],border=col, col=boxplot.fill)
      points(at, quart[3],bg=bg)
    }
  }

  #par0 and return
  par(par0)
  return(h)
}
