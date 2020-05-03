#' Create a new gg3logit
#'
#' `gg3logit` initializes a [`ggplot`][ggplot2::ggplot] object through
#' [`ggtern`][ggtern::ggtern]. If a fortified `field3logit` or a
#' `multifield3logit` object is passed as argument `data` to `gg3logit`,
#' the labels of the ternary plot are automatically. The same happens if a
#' `field3logit` or a `multifield3logit` object is passed; in that case,
#' `gg3logit` preliminarly invoke the `fortify` method.
#'
#' @param x a `field3logit` or a `multifield3logit` object. If not
#'   specified, must be supplied in each layer added to the plot.
#'
#' @family `gg` functions
#'
#' @examples
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale', conf = 0.95)
#'
#' autoplot(field0)
#'
#' @export
autoplot <- function(x, conf = TRUE) {
  
  if (!inherits(x, 'field3logit')) {
  	stop('Only objects of class "field3logit" and "multifield3logit" are allowed')
  }
  
  ggdepo <- gg3logit(x) + stat_field3logit()
  
  if (conf) {
  	ggdepo <- ggdepo + stat_conf3logit()
  }

  return(ggdepo)
}
