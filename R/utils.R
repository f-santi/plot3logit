
#' Computes convex combinations of two vectors
#'
#' Given two vectors and one or multiple coefficients in \eqn{[0,1]},
#' convex combinations of vectors are computed.
#'
#' @param w `numeric` in \eqn{[0,1]} (multiple values are allowed).
#' @param x,y `numeric` vectors of equal length.
#' @param simplify if `TRUE` a matrix is returned; if `FALSE` the
#'   output will be a `list`.
#'
#' @examples
#' plot3logit:::convex_comb(c(0, 0.3, 0.5, 1), c(1, 0, 0), c(0, 1, 0))
#' plot3logit:::convex_comb(c(0, 0.3, 0.5, 1), c(1, 0, 0), c(0, 1, 0), FALSE)
#'
#' @keywords internal
convex_comb<- function(w, x, y, simplify = TRUE) {
  out<- sapply(w, function(w) { y + w * (x - y) }, simplify = simplify)
  if (simplify) { out %<>% t }
  out
}



#' Computes the versor
#'
#' Given an `n`-dimensional space, the `k`-th versor is returned.
#'
#' @param n dimension of the space.
#' @param k order of the versor.
#' @param simplify if `TRUE`, a `numeric` is returned, otherwise
#'   `versor` returns a column matrix.
#'
#' @examples
#' plot3logit:::versor(1, 3)
#' plot3logit:::versor(3, 6)
#' plot3logit:::versor(3, 6, FALSE)
#'
#' @keywords internal
versor<- function(k, n, simplify = TRUE) {
  out <- rep(0, n)
  out[k] <- 1
  if (!simplify) { out %<>% matrix }
  out
}



#' Convert a tibble to a matrix
#'
#' Given an `n`-dimensional space, the `k`-th versor is returned.
#'
#' @param n dimension of the space.
#' @param k order of the versor.
#' @param simplify if `TRUE`, a `numeric` is returned, otherwise
#'   `versor` returns a column matrix.
#'
#' @examples
#' plot3logit:::versor(1, 3)
#' plot3logit:::versor(3, 6)
#' plot3logit:::versor(3, 6, FALSE)
#'
#' @keywords internal
tbl2matrix <- function(x, .rownames = NULL) {
  if (!is.null(.rownames)) {
  	x %>% pull(.rownames) -> depo
  	x %<>% select(-!!(.rownames))
  } else { depo <- NULL }
  
  x %>%
    as.matrix %>%
    set_rownames(depo) %>%
    return
}

