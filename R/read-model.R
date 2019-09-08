
#' Builds up matrix of coefficients from fitted models
#'
#' `read_model` reads arguments `model`, `ordinal`, `type` and `alpha` passed
#' by function [`field3logit`] and properly sets the matrix of coefficients
#' and the other needed model-specific functions.
#'
#' @param model see [`field3logit`].
#' @param type class of the model. Currently, forced to `"logit"`
#'   by [`field3logit`] when [`read_model`] is called.
#' @param alpha see [`field3logit`].
#'
#' @return
#' `read_model` returns a named `list` with the following components:
#'
#' \item{B}{matrix of coefficients.}
#' \item{vcovB}{variance-covariance matrix of coefficients.}
#' \item{alpha}{see [`field3logit`].}
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
  out <- list(B = NULL, vcovB = NULL, alpha = alpha, model = type,
    ordinal = !is.null(alpha), readfrom = NULL,
    P2XB = NULL, XB2P = NULL, DeltaB2pc = NULL)
  
  # Read models
  if (all(inherits(model, c('multinom', 'nnet'), TRUE))) {
  	out %<>% modifyList(read_from_multinom(model))
  } else if (inherits(model, 'polr')) {
  	out %<>% modifyList(read_from_polr(model))
  } else if (inherits(model, 'mlogit')) {
  	out %<>% modifyList(read_from_mlogit(model))
  } else {
  	out %<>% modifyList(read_from_matrix(model))
  }
  
  # Add link functions
  if ((out$model == 'logit') & !out$ordinal) {
  	out$XB2P <- XB2P_cat3logit
  	out$P2XB <- P2XB_cat3logit
  	out$DeltaB2pc <- DeltaB2pc_cat3logit
  } else if ((out$model == 'logit') & out$ordinal) {
  	out$XB2P <- function(x) XB2P_ord3logit(x, out$alpha)
  	out$P2XB <- function(x) P2XB_ord3logit(x, out$alpha)
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
  
  out
}





#' @rdname read_model
#' @keywords internal
read_from_multinom <- function(model, ...) {
  list(
    B = t(stats::coef(model)),
  	vcovB = stats::vcov(model),
    alpha = NULL,
    model = 'logit',
    ordinal = FALSE,
    readfrom = 'nnet::multinom',
    lab = model$lab
  )
}



#' @rdname read_model
#' @keywords internal
read_from_polr <- function(model, ...) {
  list(
  	B = as.matrix(stats::coef(model)),
  	# vcovB = NULL,
  	alpha = cumsum(model$zeta),
  	model = ifelse(model$method == 'logistic', 'logit', model$method),
  	ordinal = TRUE,
  	readfrom = 'MASS::polr',
  	lab = model$lev
  )
}



#' @rdname read_model
#' @keywords internal
read_from_mlogit <- function(model, ...) {
  depoB <- stats::coef(model)
  depoB %<>%
    names %>%
    strsplit(':') %>%
    Reduce(rbind, .) %>%
    as.data.frame %>%
    set_colnames(c('lev', 'variable')) %>%
    cbind(depoB) %>%
    reshape2::dcast(variable ~ lev, value.var = 'depoB', fill = NA)
  depoB %<>%
    { as.matrix(.[ , -1]) } %>%
    set_rownames(depoB$variable)
  
  #depoP <- matrix()
  
  list(
    B = depoB,
  	# vcovB = NULL, #depoP %*% stats::vcov(model) %*% depoP,
    alpha = NULL,
    model = 'logit',
    ordinal = FALSE,
    readfrom = 'mlogit::mlogit',
    lab = names(model$freq)
  )
}



#' @rdname read_model
#' @keywords internal
read_from_matrix <- function(model, ...) {
  depo <- as.matrix(model)
  if ((nrow(depo) == 2) & (ncol(depo) == 1)) { depo %<>% t }
  
  list(
    B = depo,
    # vcovB
    # alpha
    # model
    # ordinal
  	readfrom = 'matrix',
  	lab = attr(model, 'labs')
  )
}

