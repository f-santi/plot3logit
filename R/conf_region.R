
#' Covariance matrix of covariate change
#'
#' Given the covariance matrix of parameters, it return the covariance
#' matrix associated to the change in covariate values in the space
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
#' @param conf confidence level of the region.
#' @param npoints number of points of the border.
#'
#' @return
#' `data.frame` with three columns (named `p1`, `p2`, and `p3`)
#' with ternary coordinates of the points of the ellipse.
#'
#' @examples
#' Ternary::TernaryPlot()
#' Ternary::TernaryPolygon(
#'   coordinates = plot3logit:::confregion(1:2, 0.3 * diag(2:1)),
#'   col = grDevices::rgb(0, 0, 0.5, 0.2),
#'   border = NA
#' )
#' Ternary::TernaryPolygon(
#'   coordinates = plot3logit:::confregion(1:2, 0.1 * diag(2:1)),
#'   col = grDevices::rgb(0, 0, 0.5, 0.2),
#'   border = NA
#' )
#'
#' @keywords internal
confregion <- function(mu, Sig, conf = 0.95, npoints = 100) {
  # Compute the ellipse
  ellipse::ellipse(x = Sig, centre = mu, level = conf) %>%
    # Compute the ternary coordinates
    XB2P_cat3logit %>%
    # Prepare the output
    data.frame %>%
    set_colnames(c('p1', 'p2', 'p3')) %>%
    return
}





#' It adds the confidence regions to a "field3logit" object
#'
#' Given the confidence level, it computes the confidence regions
#' the effects for each arrow of the `field3logit` object.
#'
#' @inheritParams confregion
#' @param x an object of class `field3logit`.
#'
#' @return
#' Object of class `field3logit` with updated confidence regions.
#'
#' @examples
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#' plot3logit:::add_confregions_field3logit(field0)
#'
#' @keywords internal
add_confregions_field3logit <- function(x, conf = 0.95, npoints = 100) {
  # Check the input
  if (x$ordinal) {
  	warning('Confidence regions are not available for ordinal models.')
  	x$conf <- NA
  	return(x)
  }
  
  # Compute the covariance matrix of the ellipse
  SigMa <- vcovB2vcovDeltaB(x$vcovB, x$vdelta)
  
  # Compute the confidence regions
  x$effects %<>%
    lapply(function(w) lapply(w, function(y) {
    	  P2XB(y$to, x) %>%
        confregion(SigMa) %>%
        set_colnames(x$lab) -> y$confregion
        
      return(y)
    }))
  
  # Update other object attributes
  x$conf <- conf
  
  # Return the updated object
  return(x)
}





#' Computes the confidence regions of covariate effects
#'
#' Given the confidence level, it computes the confidence regions of the effects
#' for each arrow of the `field3logit` or `multifield3logit` object given in
#' input. If the `field3logit` or `multifield3logit` object already contains the
#' confidence regions, they will be updated if the value of `conf` is different.
#'
#' @param x an object of class `field3logit` or `multifield3logit`.
#' @param conf confidence level of the regions.
#' @param npoints number of points of the borders of the regions.
#'
#' @return
#' Object of class `field3logit` or `multifield3logit` with updated
#' confidence regions.
#'
#' @examples
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#' field0
#' add_confregions(field0)
#'
#' @export
add_confregions <- function(x, conf = 0.95, npoints = 100) {
  # Check the class of input
  depo <- inherits(x, c('field3logit','multifield3logit'), which = TRUE)
  
  # Compute the confidence regions
  if (all(depo == 0)) {
  	stop('Only objects of class "field3logit" and "multifield3logit" are allowed')
  } else if (depo[2] == 0) {
  	x %<>% add_confregions_field3logit(conf, npoints)
  } else {
  	x %<>%
  	  lapply(add_confregions_field3logit, conf = conf, npoints = npoints) %>%
  	  structure(class = c('multifield3logit', 'field3logit'))
  }

  # Return the updated object
  return(x)
}









