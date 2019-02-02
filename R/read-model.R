
#' Builds up matrix of coefficients from fitted models
#'
#' It reads arguments `model`, `ordinal`, `type` and `alpha` passed
#' by function [`field3logit`] and properly sets the matrix of
#' coefficient and the other needed model-specific functions.
#'
#' @param model see [`field3logit`].
#' @param type class of the model. Currently, forced to `"logit"`
#'   by [`field3logit`] when [`read_model`] is called.
#' @param alpha see [`field3logit`].
#'
#' @return
#' A named `list` with the following components:
#'
#' \item{B}{matrix of coefficients.}
#' \item{model}{argument `type`.}
#' \item{ordinal}{`logical` variable indicating wheter the model is
#'   ordinal or not.}
#' \item{P2XB}{link function.}
#' \item{XB2P}{inverse of the link function.}
#' \item{DeltaB2pc}{function that computes the points the curve of
#'   the field should pass through (see [`DeltaB2pc`]).}
#'
#' @seealso [`plot3logit-package`], [`field3logit`].
#'
#' @keywords internal
read_model <- function(model, type, alpha) {
  out<- list(B = NULL, model = type, ordinal = !is.null(alpha),
    readfrom = NULL,
    P2XB = NULL, XB2P = NULL, DeltaB2pc = NULL)
  
  # Read matrix
  if (all(inherits(model, c('multinom', 'nnet'), TRUE))) {
  	out$B <- t(stats::coef(model))
  	out$model <- 'logit'
  	out$ordinal <- FALSE
  	out$readfrom <- 'nnet::multinom'
  	out$lab <- model$lab
  } else if (inherits(model, 'polr')) {
  	out$B <- as.matrix(stats::coef(model))
  	alpha <- cumsum(model$zeta)
  	out$model <- ifelse(model$method == 'logistic', 'logit', model$method)
  	out$ordinal <- TRUE
  	out$readfrom <- 'MASS::polr'
  	out$lab <- model$lev
  } else if (inherits(model, 'mlogit')) {
  	depo <- stats::coef(model)
  	depo %<>%
  	  names %>%
  	  strsplit(':') %>%
  	  Reduce(rbind, .) %>%
  	  as.data.frame %>%
  	  set_colnames(c('lev', 'variable')) %>%
  	  cbind(depo) %>%
  	  reshape2::dcast(variable ~ lev, value.var = 'depo', fill = NA)  	  
    depo %>%
      { as.matrix(.[ , -1]) } %>%
      set_rownames(depo$variable) -> out$B
    out$model <- 'logit'
  	out$ordinal <- FALSE
  	out$readfrom <- 'mlogit::mlogit'
  	out$lab <- names(model$freq)
  } else {
  	out$B <- as.matrix(model)
  	if ((nrow(out$B) == 2) & (ncol(out$B) == 1)) { out$B %<>% t }
  	out$readfrom <- 'matrix'
  	out$lab <- attr(model, 'labs')
  }
  
  # Add link functions
  if ((out$model == 'logit') & !out$ordinal) {
  	out$XB2P <- XB2P_cat3logit
  	out$P2XB <- P2XB_cat3logit
  	out$DeltaB2pc <- DeltaB2pc_cat3logit
  } else if ((out$model == 'logit') & out$ordinal) {
  	out$XB2P <- function(x) XB2P_ord3logit(x, alpha)
  	out$P2XB <- function(x) P2XB_ord3logit(x, alpha)
  	out$DeltaB2pc <- function(x, ...) DeltaB2pc_ord3logit(x, alpha, ...)
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
  
  out
}



#' It computes the vector of covariate change
#'
#' Given the argument `delta` passed to [`field3logit`] either as
#' a `numeric` vector, a `character` or an `expression` (see
#' [`field3logit`]), `get_vdelta` returns the `numeric` vector of
#' covariate change \eqn{\Delta\in\textbf{R}^k}.
#'
#' @param model object returned by [`read_model`].
#' @param delta see [`field3logit`].
#'
#' @return
#' `numeric` vector of covariate change \eqn{\Delta\in\textbf{R}^k}.
#'
#' @examples
#' library(magrittr)
#'
#' # Example 1
#' matrix(c(0.11, 0.07, -0.1, 0.09), 2) %>%
#'   plot3logit:::read_model('logit', NULL) %>%
#'   plot3logit:::get_vdelta(c(0, 1), .)
#'
#' # Example 2
#' library(nnet)
#' data(cross_1year)
#' mod0 <- multinom(employment_sit ~ ., cross_1year)
#' plot3logit:::read_model(mod0, 'logit', NULL) %>%
#'   plot3logit:::get_vdelta('genderFemale', .)
#'
#' # Example 3
#' plot3logit:::read_model(mod0, 'logit', NULL) %>%
#'   plot3logit:::get_vdelta('-0.5 * genderFemale + hsscore', .)
#'
#' @keywords internal
get_vdelta <- function(delta, model) {
  if (!is.numeric(delta) & (length(delta) > 1)) { delta %<>% extract(1) }

  if (is.character(delta)) { delta %<>% parse(text = .) }
  
  if (is.expression(delta)) {
  	depoE <- new.env()
  	n <- nrow(model$B)
  	mapply(function(x, y) assign(x, versor(y, n), envir = depoE),
  	  rownames(model$B), 1 : n)
  	delta <- eval(delta, depoE)
  }
  delta
}


