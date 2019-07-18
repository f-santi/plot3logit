
#' Compute the linear predictors implied by trinomial probability distributions
#'
#' Given the probability distributions \eqn{P\in[0,\,1]^{n\times 3}} of the
#' dependent variable, it computes the the values of linear predictors
#' \eqn{XB\in\textbf{R}^{n\times 2}} according to notation used in Santi,
#' Dickson and Espa (2019).
#'
#' @param P object of class `matrix` (or other coercible classes) such that
#'   \eqn{P\in[0,\,1]^{n\times 3}}.
#' @param alpha `numeric` vector of length two where constants \eqn{\alpha^{(1)}}
#'   and \eqn{\alpha^{(2)}} are stored (only for ordinal models), as
#'   defined in Equation (7) of Santi, Dickson and Espa (2019).
#'
#' @return
#' Numeric matrix \eqn{\textbf{R}^{n\times 2}} of linear predictors.
#'
#' @references Santi F., M. M. Dickson, G. Espa (2019)
#' "A graphical tool for interpreting regression coefficients of trinomial logit
#' models", *The American Statistician*, **73**(2), pp. 200-207.
#' \doi{10.1080/00031305.2018.1442368}
#'
#' @seealso [`XB2P`].
#'
#' @name P2XB
NULL



#' @rdname P2XB
#' @keywords internal
P2XB_cat3logit<- function(P) {
  if (is.vector(P)) { P %<>% matrix(1) }
  
  P %>%
    as.matrix %>%
    { .[ , 2:3, drop = FALSE] / .[1] } %>%
    log %>%
    return
}



#' @rdname P2XB
#' @keywords internal
P2XB_ord3logit<- function(P, alpha) {
  if (is.vector(P)) { P %<>% matrix(1) }
  
  P %>%
    as.matrix %>%
    extract(, 1) %>%
    { . / (1 - .) } %>%
    log %>%
    { alpha[1] - . } %>%
    return
}


