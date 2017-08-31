#' Phylogenetic correlation test using Pearson's product moment
#'
#' Test for association between paired samples, using the Pearson's
#' product moment correlation coefficient and taking into account
#' the dataset phylogenetic structure
#'
#' @param 'x, y' numeric vectors of data values. x and y must have the same length
#' @param tree An object of class phylo representing the phylogeny (with branch lengths) to consider
#'
#' @examples
#'
#' @import phytools stats
#'
#' @export

phylo.cor.test<-function(x,y,tree){

  if (length(x)!=length(y)) stop("'x' and 'y' must have the same length")
  X<-cbind(x,y)
  obj<-phytools::phyl.vcv(X,ape::vcv(tree),1)
  r.xy<-cov2cor(obj$R)["x","y"]
  t.xy<-r.xy*sqrt((Ntip(tree)-2)/(1-r.xy^2))
  P.xy<-2*pt(abs(t.xy),df=Ntip(tree)-2,lower.tail=F)
  list(r=r.xy, r.squared=r.xy^2,t=t.xy, p.value = P.xy)
}



