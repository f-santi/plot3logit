
#' It computes the confidence region in the ternary space
#'
#' Given the parameters of the confidence ellipse in the
#' space of covariate coefficiets, it returns the confidence
#' region of the effect.
#'
#' @param mu centre of the ellipse.
#' @param Sig covariance matrix of the ellipse.
#' @param conf confidence level.
#' @param npoints number of points of the border.
#'
#' @return
#' `data.frame` with three columns (named `p1`, `p2`, and `p3`)
#' with ternary coordinates of the 
#'
#' @keywords internal
confregion <- function(mu, Sig, conf = 0.95, npoints = 100) {
  # Compute the ellipse
  out <- ellipse::ellipse(x = Sig, centre = mu, level = conf)
  out %<>% as.data.frame
  
  # Compute the ternary coordinates
  out$p1 <- exp(out$x) / (1 + exp(out$x) + exp(out$y))
  out$p2 <- exp(out$y) / (1 + exp(out$x) + exp(out$y))
  out$p3 <- 1 / (1 + exp(out$x) + exp(out$y))
  
  # Return the confidence ellipse
  return(out[ , c('p1', 'p2', 'p3'), drop = FALSE])
}










