
#' Set the labels of a `field3logit` or a `multifield3logit` object
#'
#' It enables the labels of an existing `field3logit` or a `multifield3logit`
#' object to be set.
#'
#' @param x a `field3logit` or a `multifield3logit` object.
#' @param value a character with the new label (or labels in case of a
#'   `multifield3logit` object).
#'
#' @name labels
#'
#' @export
`labels<-` <- function(x, value) {
  UseMethod('labels<-')
}




