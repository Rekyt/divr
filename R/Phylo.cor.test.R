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
#' @import stats
#'
#' @export

phylo.cor.test<-function (x,y,tree) {

  if (length(x) != length(y)) stop("'x' and 'y' must have the same length")
  if (!inherits(tree, "phylo")) stop("tree should be an object of class 'phylo'")
  if (length(x) > ape::Ntip(tree)) stop("length of 'y' and 'x' cannot be greater than number of taxa in your tree")

  if (is.null(names(x)) | is.null(names(y))) {
    if (length(x) < ape::Ntip(tree)) stop("'y' and 'x' have no names. Their length must be equal to the number of taxa in your tree")
    warning("'y' and 'x' have no names. Function will assume that the order matches tree$tip.label")
    X <- cbind(x, y)
    rownames(X) <- tree$tip.label
  } else {
    if (!all(names(x) %in% names(y))) stop("not all 'x' names match with 'y' names")
    X <- merge(x, y, by = "row.names")
    rownames(X) <- X$Row.names
    X <- X[, -1]
    if (!all(rownames(X) %in% tree$tip.label)) stop("Not all 'x' or 'y' names match with tree$tip.label")
    sup<-which(!tree$tip.label %in% rownames(X))
    if (length(sup) > 0) tree<-ape::drop.tip(tree, sup)
  }

  C <- ape::vcv.phylo(tree)[rownames(X), rownames(X)]
  obj <- phytools::phyl.vcv(as.matrix(X), C, 1)
  r.xy <- cov2cor(obj$R)["x","y"]
  t.xy <- r.xy * sqrt((ape::Ntip(tree) - 2) / (1 - r.xy^2))
  P.xy <- 2 * pt(abs(t.xy), df = ape::Ntip(tree) - 2, lower.tail = F)
  result <- list(r = r.xy, r.squared = r.xy^2, t = t.xy, df = ape::Ntip(tree) - 2, p.value = P.xy)
  class(result) <- "phycor"
  return(result)
}

print.phycor <- function (x, ...) {
  cat("\n Phylogenetic Pearson's product-moment correlation \n",
  "\n",
  paste0("t = ", round(x$t, 3), ", df = ", x$df ,", p-value = ", round(x$p.value, 6)),"\n",
  "alternative hypothesis: true correlation is not equal to 0 \n",
  paste0("cor = ", round(x$r, 6)),
  "\n")
}



