#' Make density plot with quantiles
#'
#' Plot the density of a numeric vector and show the different quantiles colored
#' in areas
#'
#' @param ex_vector a numeric vector from which the density should be computed
#'
#' @param probs a numeric vector giving the quantile (default: \code{NULL} but
#'  from 0 to 1 by step of 0.1)
#'
#' @examples
#'
#' vec = rnorm(1000)
#'
#' gg = make_quantile_density(vec)
#'
#' gg
#'
#' @export
make_quantile_density = function(ex_vector, probs = NULL) {

  if (!requireNamespace("ggplot2", quietly = T)) {
    stop("'ggplot2' is needed to make 'make_quantile_density()' work =(")
  } else {
    ex_vector = na.omit(ex_vector)

    if (is.null(probs)) {
      quant = quantile(ex_vector, probs = seq(0, 1, 0.1))
    } else {
      quant = quantile(ex_vector, probs = probs)
    }

    # Computes density
    dens_df = with(density(ex_vector), data.frame(x, y))

    # Take only "real" parts of the density
    dens_df = subset(dens_df, x >= quant[[1]], x <= quant[[length(quant)]])

    # Cut the areas based on quantiles
    dens_df$quant = cut(dens_df$x, breaks = quant)

    qname = names(quant)

    # Generate nice labels '10%-20%', etc.
    qlabels = paste(qname[-length(qname)], qname[-1], sep = "-")

    p_dens_col = ggplot2::ggplot(dens_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_area(ggplot2::aes(fill = quant)) +
      ggplot2::geom_line() +
      ggplot2::scale_fill_brewer(palette = "PuOr", labels = qlabels)

    return(p_dens_col)
  }
}
