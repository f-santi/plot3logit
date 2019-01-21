
#' Computes trinomial probability distributions implied by linear predictors
#'
#' `XB2P_cat3logit` and `XB2P_ord3logit` compute the matrix
#' \eqn{P\in[0,\,1]^{n\times 3}} of probability distributions of the
#' dependent variable for categorical and ordinal models respectively.
#' Functions `X2P_cat3logit` and `X2P_ord3logit` perform the same computation,
#' except that the design matrix \eqn{X\in\textbf{R}^{n\times k}} and
#' the coefficient matrix \eqn{B\in\textbf{R}^{k\times 2}} are taken as
#' separate input arguments.
#'
#' @inheritParams P2XB
#' @param XB object of class `matrix` (or other coercible classes) such that
#'   \eqn{XB\in\textbf{R}^{n\times 2}} for *categorical* models or
#'   \eqn{XB\in\textbf{R}^n} for *ordinal* models.
#' @param X object of class `matrix` (or other coercible classes) such that
#'   \eqn{X\in\textbf{R}^{n\times k}}.
#' @param B object of class `matrix` (or other coercible classes) such that
#'   \eqn{B\in\textbf{R}^{k\times 2}} for *categorical* models or
#'   \eqn{B\in\textbf{R}^k} for *ordinal* models.
#'
#' @return
#' Numeric matrix \eqn{[0,\,1]^{n\times 3}} of probability distributions.
#'
#' @inherit P2XB references
#'
#' @seealso [`P2XB`].
#'
#' @name XB2P
NULL



#' @rdname XB2P
#' @keywords internal
XB2P_cat3logit<- function(XB) {
  if (is.vector(XB)) { XB %<>% matrix(1) }
  
  XB %>%
    as.matrix %>%
    exp %>%
    cbind(1, .) %>%
    apply(1, function(x) x / sum(x)) %>%
    t %>%
    return
}



#' @rdname XB2P
#' @keywords internal
X2P_cat3logit <- function(X, B) { XB2P_cat3logit(X %*% B) }



#' @rdname XB2P
#' @keywords internal
XB2P_ord3logit<- function(XB, alpha) {
  outer(-XB, alpha, '+') %>%
    exp %>%
    { . / (1 + .) } %>%
    cbind(0, ., 1) %>%
    apply(1, diff) %>%
    t %>%
    return
}



#' @rdname XB2P
#' @keywords internal
X2P_ord3logit <- function(X, B, alpha) { XB2P_ord3logit(X %*% B, alpha) }

