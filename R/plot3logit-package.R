
#' Ternary Plots for Trinomial Regression Models
#'
#' An implementation of the ternary plot for interpreting regression
#' coefficients of trinomial regression models, as proposed in Santi, Dickson
#' and Espa (2018).
#'
#' The package permits the covariate effects of trinomial regression models to
#' be represented graphically by means of a ternary plot. The aim of the
#' plots is helping the interpretation of regression coefficients in terms of
#' the effects that a change in regressors' values has on the probability
#' distribution of the dependent variable. Such changes may involve either a
#' single regressor, or a group of them (composite changes), and the package
#' permits both cases to be represented in a user-friendly way. Theoretical
#' and methodological details are illustrated and discussed in Santi, Dickson
#' and Espa (2018).
#'
#' The package can read the results of **both categorical and ordinal trinomial
#' logit** regression fitted by various functions (see the next section) and
#' creates a `field3logit` object which may be represented by means of functions
#' [`gg3logit`] and [`stat_3logit`].
#'
#' The `plot3logit` package inherits graphical classes and methods from the
#' package [`ggtern`][ggtern::ggtern-package] (Hamilton 2018) which, in turn, is
#' based on the [`ggplot2`][ggplot2::ggplot2-package] package (Wickham 2016).
#'
#' Graphical representation based on **standard graphics** is made available
#' through the package [`Ternary`][Ternary::TernaryPlot] (Smith 2017) by
#' functions [`plot3logit`] and [`TernaryField`], and in particular by the
#' [`plot`][plot.field3logit] method of `field3logit` objects.
#'
#' @section Compatibility:
#' Function [`field3logit`] of package `plot3logit` can read trinomial
#' regression estimates from the output of the following functions:
#' * [`multinom`][nnet::multinom] of package `nnet` (logit regression);
#' * [`polr`][MASS::polr] of package `MASS` (ordinal logit regression);
#' * [`mlogit`][mlogit::mlogit] of package `mlogit` (logit regression).
#'
#' Moreover, explicit matrix of regression coefficients can be passed to
#' [`field3logit`]. See examples and function [`field3logit`] for further
#' details.
#'
#' @examples
#' data(cross_1year)
#'
#' # Read from "nnet::multinom"
#' library(nnet)
#' mod0 <- multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#' gg3logit(field0) + stat_3logit()
#'
#' # Read from "MASS::polr"
#' library(MASS)
#' mydata <- cross_1year
#' mydata$finalgrade <- factor(mydata$finalgrade,
#'   c('Low', 'Average', 'High'), ordered = TRUE)
#' mod1 <- polr(finalgrade ~ gender + irregularity, data = mydata)
#' field1 <- field3logit(mod1, 'genderFemale')
#' gg3logit(field1) + stat_3logit()
#'
#' # Read from "mlogit::mlogit"
#' library(mlogit)
#' mydata <- mlogit.data(cross_1year, choice = 'employment_sit', shape = 'wide')
#' mod2 <- mlogit(employment_sit ~ 0 | gender + finalgrade, data = mydata)
#' field2 <- field3logit(mod2, 'genderFemale')
#' gg3logit(field2) + stat_3logit()
#'
#' # Read from matrix
#' M <- matrix(c(-2.05, 0.46, -2.46, 0.37), nrow = 2,
#'   dimnames = list(c('(Intercept)', 'genderFemale'),
#'     c('Unemployed', 'Trainee')))
#' field3 <- field3logit(M, c(0, 1))
#' gg3logit(field3) + stat_3logit()
#'
#' @references
#'
#' Hamilton N. E., M. Ferry (2018) "`ggtern`: Ternary Diagrams Using ggplot2",
#' *Journal of Statistical Software, Code Snippets*, **87**(3), 1-17.
#' \doi{10.18637/jss.v087.c03}
#'
#' Santi F., M. M. Dickson, G. Espa (2018) "A graphical tool for interpreting
#' regression coefficients of trinomial logit models", *The American
#' Statistician*. \doi{10.1080/00031305.2018.1442368}
#'
#' Smith M. R. (2017). "`Ternary`: An R Package for Creating Ternary Plots",
#' *Zenodo*. \doi{10.5281/zenodo.1068996}
#'
#' Wickham, H. (2016) *`ggplot2`: Elegant Graphics for Data Analysis.*
#' Springer-Verlag, New York.
#'
#' @seealso [`field3logit`], [`gg3logit`], [`TernaryField`].
#'
#' @encoding UTF-8
#'
#' @docType package
#' @name plot3logit-package
#' @aliases plot3logit-package
#'
#' @import magrittr Ternary ggtern
#'
#' @importFrom ggplot2 fortify unit arrow
#' @importFrom graphics points arrows
#' @importFrom stats uniroot
#' @importFrom utils modifyList tail
#'
NULL

utils::globalVariables('.')

