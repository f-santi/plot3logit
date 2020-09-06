
#' Extract information from fitted models
#'
#' [extract3logit()] extracts all information which is relevant for
#' computing the vector field(s) from the object passed to argument `x`. See
#' **Details** for information on how new `S3` methods of generic 
#' [extract3logit()] should be implemented.
#'
#' When a specific method is not available for a fitted model, it is possible to
#' pass a list to argument `x`. In that case, the list should consists of the
#' following components:
#' 
#' * `levels`: the character vector with the three possible values of the
#'   dependent variable. The first value will be interpreted as the reference
#'   level.
#' * `B`: the matrix of regression coefficients. It should be a matrix (or any
#'   coercible object) with two columns if the model is cardinal, with only one
#'   column if the model is ordinal. The row names of `B` are used as names
#'   of covariates, hence they must be unique and valid variable names.
#' * `alpha`: if the the model is ordinal, it should be a numeric vector of
#'   length two, otherwise this component should be either set to `NULL` or not
#'   set at all.
#' * `vcovB`: the covariance matrix
#'
#' If a new `S3` method for generic [extract3logit()] has to be implemented, the
#' following components may be set:
#' 
#' * `readfrom`: character with information about the function that returned the
#'   estimates in the form `package::function` (for example `nnet::multinom`,
#'   `MASS::polr`, ...)
#'
#' **In any case**, once the list has been created, the new method should invoke
#' the default method [extract3logit.default()] and return its ouput. By so
#' doing, automatic checks and initialisations are run before the `model3logit`
#' object is returned.
#'
#' @param x an object of any of the classes listed below. If a list is
#'   passed, it should be structured as described in section **Details**.
#' @param ... other arguments passed to other methods.
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
extract3logit.default <- function(x, ...) {

  if (all('list' == class(x))) {
    depo <- 'list'
  } else {
    depo <- paste0('class [', paste(class(x), collapse = ', '), ']')
  }

  # Structure the list and set default values
  list(
    B        = NULL,
    vcovB    = NULL,
    alpha    = NULL,
    model    = 'logit',
    ordinal  = NULL,
    readfrom = depo,
    levels   = NULL,
    ool      = 1:3
  ) -> depoD

  depoD %>%
    # Update the list
    modifyList(x, keep.null = TRUE) %>%
    # Select components
    `[`(names(depoD)) %>%
    # Set the class
    structure(class = 'model3logit') -> out
  
  # Check and complete the attributes
  if (is.null(out[['B']])) { stop('The matrix of coefficients is not available') }
  out[['B']] %<>% as.matrix
  out[['ordinal']] <- !is.null(out[['alpha']])
  
  if (out[['ordinal']]) {
    if (ncol(out[['B']]) != 1) {
      stop('Dimension mismatch on the matrix of coefficients')
    }
    out[['alpha']] %<>% as.numeric
  } else {
    if (ncol(out[['B']]) != 2) {
      stop('Dimension mismatch on the matrix of coefficients')
    }
  }
  
  if (out[['model']] != 'logit') {
    stop('Current implementation of "plot3logit" works only with logit links')
  }
  
  # Return the object
  return(out)
}



#' @export
print.model3logit <- function(x, ...) {
 
  type  <- ifelse(x$ordinal, 'ordinal', 'categorical')
  vcovB <- ifelse(is.null(x$vcovB), 'not available', 'available')
  
  cat('\n Object of class "model3logit"\n')
  cat('-------------------------------------------\n')
  cat('Model has been read from :', x$readfrom, '\n')
  cat('Type of model            :', type, '\n')
  cat('Possible outcomes        :', paste(x$levels, collapse = '; '), '\n')
  cat('Reference level          :', x$levels[1], '\n')
  cat('Matrix of coefficients   :', paste(dim(x$B), collapse = ' x '), '\n')
  cat('Covariance matrix        :', vcovB, '\n\n')

  invisible(x)
}



#' @rdname extract3logit
#' @export
extract3logit.clm <- function(x, ...) {
  # Checks
  if (length(x[['alpha']]) != 2) {
  	stop('Only models with trinomial dependent variable are allowed')
  }
  if (x[['link']] != 'logit') {
  	stop('Only logit link function is allowed')
  }
  
  # Prepare the output
  list(
  	B = as.matrix(x[['beta']]),
  	vcovB = NULL,
  	alpha = cumsum(x[['alpha']]),
  	x = 'logit',
  	ordinal = TRUE,
  	readfrom = 'ordinal::clm',
  	levels = x[['y.levels']],
  	ool = 1:3
  ) %>%
    extract3logit.default(...)
}



#' @rdname extract3logit
#' @export
extract3logit.clm2 <- function(x, ...) {
  # Checks
  if (length(x[['Alpha']]) != 2) {
  	stop('Only models with trinomial dependent variable are allowed')
  }
  if (x[['link']] != 'logistic') {
  	stop('Only logit link function is allowed')
  }
  
  # Prepare the output
  list(
  	B = as.matrix(x[['beta']]),
  	vcovB = NULL,
  	alpha = cumsum(x[['Alpha']]),
  	x = 'logit',
  	ordinal = TRUE,
  	readfrom = 'ordinal::clm2',
  	levels = x[['lev']],
  	ool = 1:3
  ) %>%
    extract3logit.default(...)
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
    levels = names(x$freq),
    ool = 1:3
  ) %>%
    extract3logit.default(...)
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
    levels = x$lab,
    ool = 1:3
  ) %>%
    extract3logit.default(...)
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
  	levels = x$lev,
  	ool = 1:3
  ) %>%
    extract3logit.default(...)
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
    levels = c(depoL$ref, depoL$oth),
    ool = order(c(x@misc$refLevel, (1:3)[-x@misc$refLevel]))
  ) %>%
    extract3logit.default(...)
}



#' @rdname extract3logit
#' @export
extract3logit.vglm <- function(x, ...) {
  out <- extract3logit.vgam(x, ...)
  out[['readfrom']] <- 'VGAM::vglm'
  return(out)
}

