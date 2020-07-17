
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ternary Plots for Trinomial Regression Models

## Presentation

The package permits the covariate effects of trinomial regression models
to be represented graphically by means of a ternary plot. The aim of the
plots is helping the interpretation of regression coefficients in terms
of the effects that a change in regressors’ values has on the
probability distribution of the dependent variable. Such changes may
involve either a single regressor, or a group of them (composite
changes), and the package permits both cases to be handled in a
user-friendly way. Theoretical and methodological details are
illustrated and discussed in Santi, Dickson, and Espa (2019).

The package can read the results of **both categorical and ordinal
trinomial logit** regression fitted by various functions (see the next
section) and creates a `field3logit` object which may be represented by
means of functions `gg3logit` and `stat_field3logit`.

The `plot3logit` package inherits graphical classes and methods from the
package `ggtern` (Hamilton and Ferry 2018) which, in turn, is based on
the package `ggplot2` (Wickham 2017).

Graphical representation based on **standard graphics** is made
available through the package `Ternary` (Smith 2017) by functions
`plot3logit` and `TernaryField`, and by the `plot` method of
`field3logit` objects.

See the help of `field3logit` for representing composite effects and
`multifield3logit` for drawing multiple fields and the presentation
vignette `plot3logit-overview` by typing:

``` r
vignette('plot3logit-overview', package = 'plot3logit')
```

## Compatibility

Function `field3logit` of package `plot3logit` can read trinomial
regression estimates from the output of the following functions:

  - `multinom` of package `nnet` (logit regression);
  - `polr` of package `MASS` (ordinal logit regression);
  - `mlogit` of package `mlogit` (logit regression);
  - `vgam` of package `VGAM` (logit regression).

Moreover, explicit matrix of regression coefficients can be passed to
`field3logit`. See the help of the package (type `?
'plot3logit-package'`) for furhter details.

## An example

Fit a trilogit model by means of package `nnet` where the student’s
employment situation is analysed with respect to all variables in the
dataset `cross_1year`:

``` r
data(cross_1year)
library(nnet)
mod0 <- multinom(employment_sit ~ ., data = cross_1year)
```

The gender effect is analysed by means of a ternary plot which is
generated in two steps, however, package `plot3logit` should be loaded:

``` r
library(plot3logit)
```

Firstly, the vector field is computed:

``` r
field0 <- field3logit(mod0, 'genderFemale')
```

Secondly, the field is represented on a ternary plot, using either
`gg`-graphics:

``` r
gg3logit(field0) + stat_field3logit()
```

or standard graphics:

``` r
plot(field0)
```

## References

<div id="refs" class="references">

<div id="ref-hamilton2018">

Hamilton, N. E., and M. Ferry. 2018. “ggtern: Ternary Diagrams Using
ggplot2.” *Journal of Statistical Software, Code Snippets* 87 (3): 1–17.
doi:[10.18637/jss.v087.c03](https://doi.org/10.18637/jss.v087.c03).

</div>

<div id="ref-santi2019">

Santi, F., M. M. Dickson, and G. Espa. 2019. “A Graphical Tool for
Interpreting Regression Coefficients of Trinomial Logit Models.” *The
American Statistician* 73 (2): 200–207.
doi:[10.1080/00031305.2018.1442368](https://doi.org/10.1080/00031305.2018.1442368).

</div>

<div id="ref-smith2017">

Smith, M. R. 2017. “Ternary: An R Package for Creating Ternary Plots.”
*Zenodo*.

</div>

<div id="ref-wickham2016">

Wickham, H. 2017. *ggplot2: Elegant Graphics for Data Analysis*. New
York: Springer-Verlag.

</div>

</div>
