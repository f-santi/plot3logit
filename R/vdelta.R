
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


