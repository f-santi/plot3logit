
#' Ternary Plots for Trinomial Regression Models
#'
#' An implementation of the ternary plot for interpreting regression
#' coefficients of trinomial regression models, as proposed in
#' \insertCite{santi2019;textual}{plot3logit}.
#'
#' The package permits the covariate effects of trinomial regression models to
#' be represented graphically by means of a ternary plot. The aim of the
#' plots is helping the interpretation of regression coefficients in terms of
#' the effects that a change in regressors' values has on the probability
#' distribution of the dependent variable. Such changes may involve either a
#' single regressor, or a group of them (composite changes), and the package
#' permits both cases to be represented in a user-friendly way. Methodological
#' details are illustrated and discussed in
#' \insertCite{santi2019;textual}{plot3logit}.
#'
#' The package can read the results of **both categorical and ordinal trinomial
#' logit** regression fitted by various functions (see the next section) and
#' creates a `field3logit` object which may be represented by means of functions
#' [gg3logit()] and [stat_field3logit()].
#'
#' The `plot3logit` package inherits graphical classes and methods from the
#' package [`ggtern`][ggtern::ggtern-package]
#' \insertCite{hamilton2018}{plot3logit} which, in turn, is based on the
#' [`ggplot2`][ggplot2::ggplot2-package] package
#' \insertCite{wickham2016}{plot3logit}.
#'
#' Graphical representation based on **standard graphics** is made available
#' through the package [`Ternary`][Ternary::TernaryPlot]
#' \insertCite{smith2017}{plot3logit} by function [TernaryField()]
#' and in particular by the method [`plot`][plot.field3logit] of
#' `field3logit` class.
#'
#' Since version 2.0.0, `plot3logit` permits one to draw also the confidence
#' regions associated to the covariates effects. See the vignette of the
#' package (type `vignette('plot3logit-overview')`) and the help of function
#' [stat_conf3logit()] for some examples.
#'
#' @section Compatibility:
#' Function [field3logit()] can read trinomial regression estimates from the
#' output of the following functions:
#' * [`multinom`][nnet::multinom] of package `nnet` (logit regression);
#' * [`polr`][MASS::polr] of package `MASS` (ordinal logit regression);
#' * [`mlogit`][mlogit::mlogit] of package `mlogit` (logit regression).
#'
#' Moreover, explicit matrix of regression coefficients can be passed to
#' [field3logit()]. See examples and function [field3logit()] for further
#' details.
#'
#' @examples
#' \dontrun{
#' data(cross_1year)
#'
#' # Read from "nnet::multinom"
#' library(nnet)
#' mod0 <- multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#' gg3logit(field0) + stat_field3logit()
#'
#' # Read from "MASS::polr"
#' library(MASS)
#' mydata <- cross_1year
#' mydata$finalgrade <- factor(mydata$finalgrade,
#'   c('Low', 'Average', 'High'), ordered = TRUE)
#' mod1 <- polr(finalgrade ~ gender + irregularity, data = mydata)
#' field1 <- field3logit(mod1, 'genderFemale')
#' gg3logit(field1) + stat_field3logit()
#'
#' # Read from "mlogit::mlogit"
#' library(mlogit)
#' mydata <- mlogit.data(cross_1year, choice = 'employment_sit', shape = 'wide')
#' mod2 <- mlogit(employment_sit ~ 0 | gender + finalgrade, data = mydata)
#' field2 <- field3logit(mod2, 'genderFemale')
#' gg3logit(field2) + stat_field3logit()
#'
#' # Read from matrix
#' M <- matrix(c(-2.05, 0.46, -2.46, 0.37), nrow = 2)
#' rownames(M) <- c('(Intercept)', 'genderFemale')
#' attr(M, 'labs') <- c('Employed', 'Unemployed', 'Trainee')
#' field3 <- field3logit(M, c(0, 1))
#' gg3logit(field3) + stat_field3logit()
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso [field3logit()], [gg3logit()], [TernaryField()].
#'
#' @encoding UTF-8
#'
#' @docType package
#' @name plot3logit-package
#' @aliases plot3logit-package
#'
#' @import magrittr Ternary ggtern
#'
#' @importFrom dplyr bind_rows mutate pull select
#' @importFrom ggplot2 fortify unit arrow
#' @importFrom graphics points arrows plot
#' @importFrom Rdpack reprompt
#' @importFrom stats uniroot
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils modifyList tail
#'
NULL

utils::globalVariables('.')

