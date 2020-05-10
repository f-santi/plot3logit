
#' List of deprecated and defunct functions
#'
#' The following functions are deprecated and will no longer be updated.
#' They may be removed in a future version of the package.
#'
#' @section Deprecated functions:
#'
#' * [plot3logit()] (since version 2.0.0). Instead of [plot3logit()], generate a
#'   `field3logit` object through [field3logit()] and then plot it through the
#'   method [plot()] (standard graphics based on package
#'   [`Ternary`][Ternary::Ternary-package]), through [autoplot()], or through
#'   [gg3logit()] plus some `stat_*3logit` stats (graphics based on package
#'   [`ggtern`][ggtern::ggtern-package]).
#'
#' @docType data
#' @name deprecated-functions
#'
NULL





#' Computation and representation of the vector field
#'
#' @description
#'
#' \lifecycle{deprecated}
#'
#' This function is deprecated and may be soon removed from the package.
#'
#' [plot3logit()] method draws the ternary plot using standard graphics
#' methods provided by package `Ternary`. Use the method [plot()] of
#' `field3logit` objects instead.
#'
#' @inheritParams field3logit
#'
#' @return
#' `S3` object of class `field3logit` structured as a named `list`.
#'
#' @seealso
#' [field3logit()].
#'
#' @name plot3logit-deprecated
#'
#' @export
plot3logit <- function(model, delta, label = '<empty>', p0 = NULL,
  alpha = NULL, ncurves = 8, narrows = Inf, edge = 0.01, ...) {

  lifecycle::deprecate_warn()
  
  depo <- field3logit(model = model, delta = delta, p0 = p0,
    alpha = alpha, ncurves = ncurves, narrows = narrows,
    edge = edge, label = label)
  
  graphics::plot(depo, ...)
  
  invisible(depo)
}





