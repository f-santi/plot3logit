
#' Covariance matrix of covariate change
#'
#' Given the covariance matrix of paramters, it return the covariance
#' matrix associated to the chanve in covariate values in the space
#' of covariantes.
#'
#' @param vcovB variance-covariance matrix of \eqn{vec(B)}.
#' @param vdelta numeric vector of covariate change.
#'
#' @return
#' Squared numeric matrix of order two.
#'
#' @keywords internal
vcovB2vcovDeltaB <- function(vcovB, vdelta) {
  if (!is.vector(vdelta)) { vdelta %<>% as.vector}
  x <- kronecker(diag(2), vdelta)
  return(crossprod(x, vcovB) %*% x)
}



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
#' with ternary coordinates of the points of the ellipse.
#'
#' @examples
#' Ternary::TernaryPlot()
#' depo <- plot3logit:::confregion(1:2, 0.3 * diag(2:1))
#' Ternary::TernaryPoints(depo)
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










