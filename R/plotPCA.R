#' Plot method for \code{pca} objects
#'
#' Custom function to plot \code{pca} objects from \code{ade4} package
#'
#' @param pca a \code{pca} object from the \code{dudi.pca} function in \code{ade4}
#' @param axes a length 2 vector specifying the components to plot
#' @param xlim,ylim numeric vectors of length 2, giving the x and y coordinates ranges
#' @param ind logical value to project individuals in the correlation circle using standardized axes
#' @param cex.p individuals points size if \code{ind} is TRUE
#' @param las text orientation on axes
#' @param var.labels character string or mathplot code used for variable names
#' @param var.pos variable name position in 1, 2, 3, 4 using a named vector
#' @param ... extra parameters to custom points in \code{ind} is TRUE
#'
#' @import graphics plotrix
#'
#' @export


plot.pca <- function(pca, axes = c(1, 2), xlim=c(-1.2,1.2), ylim=c(-1.2,1.2),
                     ind = FALSE, cex.p = 1, las = 1,
                     var.labels = NULL, var.pos = NULL, ...){

  par0 <- par()$las
  par(las = las)
  eigen<-round(pca$eig/sum(pca$eig)*100,1)

  Axis1<-axes[1]
  Axis2<-axes[2]
  signx<-ifelse(sign(sum(sign(pca$co[,Axis1])))<0,-1,1)
  signy<-ifelse(sign(sum(sign(pca$co[,Axis2])))<0,-1,1)

  plot.new()
  plot.window(ylim=ylim, xlim=xlim, asp = T)
  title(xlab = paste0("Component ", Axis1," (",eigen[Axis1],"%)"),
        ylab = paste0("Component ", Axis2, " (",eigen[Axis2],"%)"))
  axis(1); axis(2)

  mult <- 0.9 / max(abs(range(pca$li)))
  plotrix::draw.circle(0, 0, 1, lty=2, border ="grey75")
  segments(c(0,-1),c(-1,0),c(0,1), c(1,0), lty=2, col="grey70")
  if(ind) points(signx*pca$li[,Axis1]*mult, signy*pca$li[,Axis2]*mult, cex = cex.p, ...)
  arrows(0,0,signx*pca$co[,Axis1],signy*pca$co[,Axis2], lwd=1, length = .08)

  if(!is.null(var.labels)) row.names(pca$co) <- var.labels
  pos <- NULL
  for (j in 1:nrow(pca$co)){
    a<-pca$co[j,c(Axis1,Axis2)]
    if(which.max(abs(a)) == 1) b <- ifelse(signx*a[1] < 0, 2, 4)
    if(which.max(abs(a)) == 2) b <- ifelse(signy*a[2] < 0, 1, 3)
    pos<-c(pos,b)
  }
  if(!is.null(var.pos) & !is.null(names(var.pos))){
    if(any(names(var.pos) %in% var.labels)) pos[match(names(var.pos), var.labels)] <- var.pos
  }
  dep<-cbind(c(0,0),c(0,.01),c(0,-.01),
             c(.01,0),c(.01,.01),c(.01,-.01),
             c(-.01,0),c(-.01,.01),c(-.01,-.01))
  for(z in 1:9){
    text(signx*pca$co[,Axis1]+dep[1,z],signy*pca$co[,Axis2]+dep[2,z],parse(text = row.names(pca$co)), pos=pos, col="white", font = 2)
  }
  text(signx*pca$co[,Axis1],signy*pca$co[,Axis2],parse(text = row.names(pca$co)), pos=pos, cex = .8)
  par(las = par0)
}

