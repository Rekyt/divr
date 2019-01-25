#' Plot a correlation diagram
#'
#' Summary diagram of the correlations between variables
#'
#' @param dat a matrix or data.frame with variables in column.
#' @param labels custom label vector in the same order than column names.
#' @param phylo NULL or an object of class \code{phylo} representing the phylogeny (with branch lengths) to consider
#' @param order logical to order variables according to a clustering method.
#' @param method method used for the correlation test. One of "pearson" (default), "kendall", or "spearman".
#' @param p.critic Critical p.value under which a correlation line should be drawn
#' @param pval.adjust method to adjust p-values in "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr".
#' @param col 2 length vector of colour names for positive and negative correlation.
#'
#' @examples
#' data("mtcars")
#' cor_nwk(mtcars)
#'
#' @import graphics
#' @importFrom corrplot corrMatOrder
#' @importFrom utils combn
#' @importFrom stats p.adjust
#'
#' @author Jules Segrestin
#'
#' @export


cor_nwk <- function(dat, labels = NA, phylo = NULL, method = "pearson", p.critic = 0.05, pval.adjust = NULL,
                    order = TRUE, col = c(1, "grey80")){

  # ajuste le jeu de donnees selon les arguments
  if(all(!is.na(labels))) colnames(dat) <- labels
  if (order) dat <- dat[, corrMatOrder(cor(dat, method = method))]
  if (!is.null(pval.adjust)){
    if (! pval.adjust %in% c("holm", "hochberg", "hommel",
                             "bonferroni", "BH", "BY", "fdr", "none")){
      stop("pval.adjust incorrect")
    }
  }

  # plot the names of variables
  angles <- seq(0, 2*pi, length.out = ncol(dat)+1)[-1]
  pts <- data.frame(var = colnames(dat), x = sin(angles), y = cos(angles))
  plot.new()
  plot.window(xlab = "", ylab = "", asp = T,
              xlim = c (-1.3, 1.3), ylim = c(-1.3, 1.3))

  pos <- NULL
  for (i in 1:length(pts$var)){
    a <- which.max(abs(pts[i, -1]))
    if(a == 1) b <- ifelse(pts[i, -1][a] > 0 , 4, 2)
    if(a == 2) b <- ifelse(pts[i, -1][a] > 0 , 3, 1)
    pos <- c(pos, b)
  }
  text(pts$x * 1.05, pts$y * 1.05, parse(text = colnames(dat)), pos = pos)

  # calcul du r.critic

  allcomb <- combn(colnames(dat), 2)
  df <- nrow(dat) - 2
  r <- seq(0, .99, .01)
  t <- r * sqrt(df/(1-r^2))
  p <- 2  * pmin(pt(t, df = df, lower.tail = FALSE),
                pt(t, df = df, lower.tail = TRUE))
  if (!is.null(pval.adjust)) p <- sapply(p, function(x) p.adjust(x, pval.adjust, n = ncol(allcomb)))
  r.critic <- approx(p, r, p.critic)$y

  wd <- seq(r.critic, 1, length.out = 4)

  # calcul du test de correlation
  val <- NULL
  pval <- NULL
  for (i in 1:ncol(allcomb)){

    x <- dat[, allcomb[1, i]]; names(x) <- rownames(dat)
    y <- dat[, allcomb[2, i]]; names(y) <- rownames(dat)

    if(is.null(phylo)){
      test <- cor.test(x, y, method = method)
      val <- c(val, test$estimate)
      pval <- c(pval, test$p.value)
    } else {
      test <- phylo.cor.test(x, y, tree = phylo, method = "pcov")
      val <- c(val, test$r)
      pval <- c(pval, test$p.value)
    }
  }

  #Correction des pval
  if (!is.null(pval.adjust)) pval <- p.adjust(pval, pval.adjust)

  # Segments
  for (i in 1:length(pval)){
    if(pval[i] < p.critic){
      lwd <- ifelse(abs(val[i]) < wd[1], 1, max(which(wd < abs(val[i]))))
      coli <- ifelse(val[i] > 0, col[1], col[2])
      loc <- pts[pts$var %in% allcomb[, i], -1]
      segments(loc[1,1], loc[1,2], loc[2,1], loc[2,2], lwd = lwd*2-1, col = coli)
    }
  }

  points(pts$x, pts$y, pch = 16)
}
