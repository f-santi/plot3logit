
#' Extract information from fitted models
#'
#' [extract3logit()] reads argument `x` properly sets the matrix of
#' coefficients and the other needed x-specific functions.
#'
#' @param x see [field3logit()].
#' @param ... other arguments.
#'
#' @return
#' An object of class `model3logit`.
#'
#' @seealso [`plot3logit-package`], [field3logit()].
#'
#' @export
extract3logit <- function(x, ...) {
  UseMethod('extract3logit')
}



#' @rdname extract3logit
#' @export
extract3logit.model3logit <- function(x, ...) return(x)



#' @rdname extract3logit
#' @export
extract3logit.multinom <- function(x, ...) {
  list(
    B = t(stats::coef(x)),
  	vcovB = stats::vcov(x),
    alpha = NULL,
    model = 'logit',
    ordinal = FALSE,
    readfrom = 'nnet::multinom',
    lab = x$lab
  ) %>%
    structure(class = 'model3logit')
}



#' @rdname extract3logit
#' @export
extract3logit.polr <- function(x, ...) {
  list(
  	B = as.matrix(stats::coef(x)),
  	vcovB = NULL,
  	alpha = cumsum(x$zeta),
  	x = ifelse(x$method == 'logistic', 'logit', x$method),
  	ordinal = TRUE,
  	readfrom = 'MASS::polr',
  	lab = x$lev
  ) %>%
    structure(class = 'model3logit')
}



#' @rdname extract3logit
#' @export
extract3logit.mlogit <- function(x, ...) {
  # Check the version of "mlogit"
  if(utils::packageVersion('mlogit') <= '1.0.3.1') {
  	message('A new version of "mlogit" is available')
  	depoN <- c('lev', 'variable')
  	
  	depo <- length(stats::coef(x))
  	indxP <- c(
  	  seq(from = 1, to = depo, by = 2),
  	  seq(from = 2, to = depo, by = 2)
  	)
  } else {
  	depoN <- c('variable', 'lev')
  	indxP <- seq_along(stats::coef(x))
  }
  
  # Read the matrix of coefficients
  x %>%
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
  x %>%
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
    lab = names(x$freq)
  ) %>%
    structure(class = 'model3logit')
}



#' @rdname extract3logit
#' @export
extract3logit.vgam <- function(x, ...) {
  # Check the object
  if (!inherits(x@family, 'vglmff')) {
  	stop('Object of class "VGAM" should be of family "vglmff"')
  }
  if (!identical(x@family@vfamily, c('multinomial', 'VGAMcategorical'))) {
  	stop('Object of class "VGAM" (family "vglmff") does not refer to a multinomial categorical regression')
  }
  if (!identical(x@misc$link, 'multilogitlink')) {
  	stop('The multinomial regression is not a multilogit')
  }
  
  # Set the parameters
  depoL <- list(
    ref = x@misc$ynames[x@misc$refLevel],
    oth = x@misc$ynames[-x@misc$refLevel]
  )
  depo <- length(stats::coef(x))
  indxP <- c(
    seq(from = 1, to = depo, by = 2),
  	seq(from = 2, to = depo, by = 2)
  )
  
  # Read the matrix of coefficients
  x %>%
    stats::coef(matrix = TRUE) %>%
    as.matrix %>%
    set_colnames(depoL$oth) -> depoB
  
  # Read the covariance matrix of coefficients
  x %>%
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
  ) %>%
    structure(class = 'model3logit')
}



#' @rdname extract3logit
#' @export
extract3logit.list <- function(x, ...) {
  list(
    B = x$B,
    vcovB = x$vcov,
    alpha = x$alpha,
    model = 'logit',
    ordinal = !is.null(x$alpha),
  	readfrom = 'list',
  	lab = x$levels
  ) %>%
    structure(class = 'model3logit')
}

