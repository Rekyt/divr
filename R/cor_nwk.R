#' Plot a correlation diagram
#'
#' Summary diagram of the correlations between variables
#'
#' @param dat a matrix or data.frame with variables in column.
#' @param labels custom label vector in the same order than column names.
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


cor_nwk <- function(dat, labels = NA, method = "pearson", p.critic = 0.05, pval.adjust = NULL,
                    order = TRUE, col = c(1, "grey80")){

  if(!is.na(labels)) colnames(dat) <- labels
  value <- cor(dat, method = method)
  if (order) dat <- dat[, corrMatOrder(value)]
  value <- cor(dat, method = method)

  if (!is.null(pval.adjust)){
    if (! pval.adjust %in% c("holm", "hochberg", "hommel",
                             "bonferroni", "BH", "BY", "fdr", "none")){
      stop("pval.adjust incorrect")
    }
  }

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
  text(pts$x * 1.1, pts$y * 1.1, colnames(dat), pos = pos)

  allcomb <- combn(colnames(dat), 2)
  ordre <- order(abs(apply(allcomb, MARGIN = 2, function(x) value[x[1], x[2]])))
  allcomb <- allcomb[, ordre]

  df <- nrow(dat) - 2
  p.seuil <- p.critic
  critical.t <- qt(p.seuil/2, df, lower.tail = F)
  critical.r <- sqrt((critical.t^2) / ( (critical.t^2) + df))

  wd <- seq(critical.r, 1, length.out = 6)

  for (i in 1:ncol(allcomb)){
    test <- cor.test(dat[, allcomb[1, i]], dat[, allcomb[2, i]], method = method)
    val <- test$estimate
    pval <- test$p.value
    if (!is.null(pval.adjust)) pval <- p.adjust(pval, pval.adjust, n = ncol(allcomb))

    lwd <- which(wd < abs(val))
    if(length(lwd) == 0 & pval < p.critic) lwd <- 1
    coli <- ifelse(val > 0, col[1], col[2])
    loc <- pts[pts$var %in% allcomb[, i], -1]
    if(length(lwd) > 0 & pval < 0.05) segments(loc[1,1], loc[1,2], loc[2,1], loc[2,2], lwd = max(lwd), col = coli)
    #if(pval < 0.1 & pval > 0.05) segments(loc[1,1], loc[1,2], loc[2,1], loc[2,2], lty = 3, col = coli)
  }

  points(pts$x, pts$y, pch = 16)
}
