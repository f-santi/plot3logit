
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
#' \item{P2XB}{link function.}
#' \item{XB2P}{inverse of the link function.}
#' \item{DeltaB2pc}{function that computes the points the curve of
#'   the field should pass through (see [DeltaB2pc()]).}
#'
#' @seealso [`plot3logit-package`], [field3logit()].
#'
#' @keywords internal
read_model <- function(model, type, alpha = NULL, vcov = NULL) {
  # Initialise the output object
  out <- list(B = NULL, vcovB = vcov, alpha = alpha, model = type,
    ordinal = !is.null(alpha), readfrom = NULL,
    P2XB = NULL, XB2P = NULL, DeltaB2pc = NULL)
  
  # Read models
  if (all(inherits(model, c('multinom', 'nnet'), TRUE))) {
  	out %<>% modifyList(read_from_multinom(model))
  } else if (inherits(model, 'polr')) {
  	out %<>% modifyList(read_from_polr(model))
  } else if (inherits(model, 'mlogit')) {
  	out %<>% modifyList(read_from_mlogit(model))
  } else if (inherits(model, 'vgam')) {
  	out %<>% modifyList(read_from_vgam(model))
  } else {
  	out %<>% modifyList(read_from_matrix(model, alpha, vcov))
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
  if (out$ordinal) {
  	colnames(out$B) <- 'Coef.'
  } else {
  	colnames(out$B) <- out$lab[-1]
  }
  
  # Output
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
  	vcovB = NULL,
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
  # Check the version of "mlogit"
  if(utils::packageVersion('mlogit') <= '1.0.3.1') {
  	message('A new version of "mlogit" is available')
  	depoN <- c('lev', 'variable')
  	
  	depo <- length(stats::coef(model))
  	indxP <- c(
  	  seq(from = 1, to = depo, by = 2),
  	  seq(from = 2, to = depo, by = 2)
  	)
  } else {
  	depoN <- c('variable', 'lev')
  	indxP <- seq_along(stats::coef(model))
  }
  
  # Read the matrix of coefficients
  model %>%
    stats::coef() %>%
    as.matrix %>%
    set_colnames('coef') %>%
    as_tibble(rownames = 'name') %>%
    tidyr::separate('name', depoN, ':') %>%
    pivot_wider(
      id_cols = 'variable',
      names_from = 'lev',
      values_from = 'coef'
    ) %>%
    tbl2matrix('variable') -> depoB
  
  # Read the covariance matrix of coefficients
  model %>%
    stats::vcov() %>%
    extract(indxP, indxP) -> depoV
   
  # Prepare the output
  list(
    B = depoB,
  	vcovB = depoV,
    alpha = NULL,
    model = 'logit',
    ordinal = FALSE,
    readfrom = 'mlogit::mlogit',
    lab = names(model$freq)
  )
}



#' @rdname read_model
#' @keywords internal
read_from_vgam <- function(model, ...) {
  # Check the object
  if (!inherits(model@family, 'vglmff')) {
  	stop('Object of class "VGAM" should be of family "vglmff"')
  }
  if (!identical(model@family@vfamily, c('multinomial', 'VGAMcategorical'))) {
  	stop('Object of class "VGAM" (family "vglmff") does not refer to a multinomial categorical regression')
  }
  if (!identical(model@misc$link, 'multilogitlink')) {
  	stop('The multinomial regression is not a multilogit')
  }
  
  # Set the parameters
  depoL <- list(
    ref = model@misc$ynames[model@misc$refLevel],
    oth = model@misc$ynames[-model@misc$refLevel]
  )
  depo <- length(stats::coef(model))
  indxP <- c(
    seq(from = 1, to = depo, by = 2),
  	seq(from = 2, to = depo, by = 2)
  )
  
  # Read the matrix of coefficients
  model %>%
    stats::coef(matrix = TRUE) %>%
    as.matrix %>%
    set_colnames(depoL$oth) -> depoB
  
  # Read the covariance matrix of coefficients
  model %>%
    stats::vcov() %>%
    extract(indxP, indxP) -> depoV
   
  # Prepare the output
  list(
    B = depoB,
  	vcovB = depoV,
    alpha = NULL,
    model = 'logit',
    ordinal = FALSE,
    readfrom = 'VGAM::vgam',
    lab = c(depoL$ref, depoL$oth)
  )
}



#' @rdname read_model
#' @keywords internal
read_from_matrix <- function(model, alpha, vcov, ...) {
  list(
    B = as.matrix(model),
    vcovB = vcov,
    alpha = alpha,
    model = 'logit',
    ordinal = !is.null(alpha),
  	readfrom = 'matrix',
  	lab = attr(model, 'levels')
  )
}

