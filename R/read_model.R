
#' Builds up matrix of coefficients from fitted models
#'
#' [read_model()] reads arguments `model`, `ordinal`, `type` and `alpha` passed
#' by function [field3logit()] and properly sets the matrix of coefficients
#' and the other needed model-specific functions.
#'
#' @inheritParams field3logit
#' @param model see [field3logit()].
#' @param type class of the model. Currently, forced to `"logit"`
#'   by [field3logit()] when [read_model()] is called.
#' @param alpha see [field3logit()].
#'
#' @return
#' `read_model` returns a named `list` with the following components:
#'
#' \item{B}{matrix of coefficients.}
#' \item{vcovB}{variance-covariance matrix of coefficients.}
#' \item{alpha}{see [field3logit()].}
#' \item{model}{argument `type`.}
#' \item{ordinal}{`logical` variable indicating wheter the model is
#'   ordinal or not.}
#' \item{linkfun}{link function.}
#' \item{linkinv}{inverse of the link function.}
#' \item{DeltaB2pc}{function that computes the points the curve of
#'   the field should pass through (see [DeltaB2pc()]).}
#'
#' @seealso [`plot3logit-package`], [field3logit()].
#'
#' @keywords internal
read_model <- function(model, type, alpha = NULL, vcov = NULL) {
  # Initialise the output object
  out <- list(B = NULL, vcovB = vcov, alpha = alpha, model = type,
    ordinal = !is.null(alpha), readfrom = NULL, lab = NULL,
    linkfun = NULL, linkinv = NULL, DeltaB2pc = NULL)
  
  # Read model
  out %<>% modifyList(extract3logit(model))
  
  # Add link functions
  if ((out$model == 'logit') & !out$ordinal) {
  	out$linkinv <- linkinv_cat3logit
  	out$linkfun <- linkfun_cat3logit
  	out$DeltaB2pc <- DeltaB2pc_cat3logit
  } else if ((out$model == 'logit') & out$ordinal) {
  	out$linkinv <- function(x) linkinv_ord3logit(x, out$alpha)
  	out$linkfun <- function(x) linkfun_ord3logit(x, out$alpha)
  	out$DeltaB2pc <- function(x, ...) DeltaB2pc_ord3logit(x, out$alpha, ...)
  }
  
  # Check dimensionality of matrix coefficient  
  if (out$ordinal) {
  	if (ncol(out$B) != 1) {
  	  warning('Dimensionality mismatch. Only the first column is considered.')
  	  out$B %<>% extract(, 1)
  	}
  } else {
  	if (ncol(out$B) < 2) {
  	  stop('Dimensionality mismatch. Only one column is available.')
  	} else if (ncol(out$B) > 2) {
  	  warning('Dimensionality mismatch. Only the first two columns are considered.')
  	}
  	out$B %<>% extract(, 1:2)
  }
  
  # Residual setting
  if (is.null(out$lab)) { out$lab <- c('p1', 'p2', 'p3') }
  if (out$ordinal) {
  	colnames(out$B) <- 'Coef.'
  } else {
  	colnames(out$B) <- out$lab[-1]
  }
  
  # Output
  out
}


