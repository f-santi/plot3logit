
<!-- README.md is generated from README.Rmd. Please edit that file -->
Ternary Plots for Trinomial Regression Models
=============================================

Presentation
------------

The package permits the covariate effects of trinomial regression models to be represented graphically by means of a ternary plot. The aim of the plots is helping the interpretation of regression coefficients in terms of the effects that a change in regressors' values has on the probability distribution of the dependent variable. Such changes may involve either a single regressor, or a group of them (composite changes), and the package permits both cases to be represented in a user-friendly way. Theoretical and methodological details are illustrated and discussed in Santi, Dickson and Espa (2018).

The package can read the results of **both categorical and ordinal trinomial logit** regression fitted by various functions (see the next section) and creates a `field3logit` object which may be represented by means of functions `gg3logit` and `stat_3logit`.

The `plot3logit` package inherits graphical classes and methods from the package `ggtern` (Hamilton 2018) which, in turn, is based on the `ggplot2` package (Wickham 2016).

Graphical representation based on **standard graphics** is made available through the package `Ternary` (Smith 2017) by functions `plot3logit` and `TernaryField`, and in particular by the `plot` method of `field3logit` objects.

See the help of `field3logit` for representing composite effects and `multifield3logit` for drawing multiple fields.

Compatibility
-------------

Function `field3logit` of package `plot3logit` can read trinomial regression estimates from the output of the following functions:

-   `multinom` of package `nnet` (logit regression);
-   `polr` of package `MASS` (ordinal logit regression);
-   `mlogit` of package `mlogit` (logit regression).

Moreover, explicit matrix of regression coefficients can be passed to `field3logit`. See the help of the package (type `? 'plot3logit-package'`) for furhter details.

An example
----------

Fit a trilogit model by means of package `nnet` where the student's employment situation is analysed with respect to all variables in the dataset `cross_1year`:

``` r
data(cross_1year)
library(nnet)
mod0 <- multinom(employment_sit ~ ., data = cross_1year)
```

The gender effect is analysed by means of a ternary plot which is generated in two steps, however, package `plot3logit` should be loaded:

``` r
library(plot3logit)
```

Firstly, the vector field is computed:

``` r
field0 <- field3logit(mod0, 'genderFemale')
```

Secondly, the field is represented on a ternary plot, using either `gg`-graphics:

``` r
gg3logit(field0) + stat_3logit()
```

or standard graphics:

``` r
plot(field0)
```

References
----------

Hamilton N. E., M. Ferry (2018) "ggtern: Ternary Diagrams Using ggplot2", *Journal of Statistical Software, Code Snippets*, **87**(3), 1-17. <https://doi.org/10.18637/jss.v087.c03>

Santi F., M. M. Dickson, G. Espa (2018) "A graphical tool for interpreting regression coefficients of trinomial logit models", *The American Statistician*. <https://doi.org/10.1080/00031305.2018.1442368>

Smith M. R. (2017). "Ternary: An R Package for Creating Ternary Plots", *Zenodo*. <https://doi.org/10.5281/zenodo.1068996>

Wickham, H. (2016) *ggplot2: Elegant Graphics for Data Analysis.* Springer-Verlag, New York.
