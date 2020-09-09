
#' Set the labels of a `field3logit` or a `multifield3logit` object
#'
#' It sets the labels of an existing `field3logit` or a `multifield3logit`
#' object.
#'
#' @param object a `field3logit` or a `multifield3logit` object.
#' @param value a character with the new label (or labels in case of a
#'   `multifield3logit` object).
#'
#' @name labels
#'
#' @export
`labels<-` <- function(object, value) {
  UseMethod('labels<-')
}




