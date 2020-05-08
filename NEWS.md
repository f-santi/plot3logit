

# plot3logit 2.0.0


## Major changes

* Added functions for computing the confidence regions of the covariate effects
  of categorical trilogit models and function for representing confidence
  regions both in standard an gg graphics.
* Changed dependencies of the package both in section "Depends" (updated the
  version of ggtern) and in section "Imports" (added `dplyr`, `forcats`,
  `grDevices`, `lifecycle`, `purrr`, `Rdpack`, `tibble`, `tidyr`, `tidyselect`
  and removed `reshape2`).
* Added the `plot` function for `multifield3logit` in standard graphics.
* Updated the functions for reading the output of `mlogit` according to the
  upcoming new version of the package. The package `plot3logit` handles the
  output of both the current and the upcoming version of `mlogit`.
* Completely redefined the output of methods `as.data.frame` and `fortify` for
  objects of class `field3logit` and `multifield3logit`.
* Function `plot3logit` is now deprecated and not updated.
* Partially redefined function `stat_3logit` and added function
  `stat_field3logit` which works like the previous version of `stat_3logit`.
  


## Minor changes

* Added methods `coef` and `vcov` for objects of class `field3logit` and
  `multifield3logit`.



## Typos and other marginal changes

* Added zero element (NULL) in the sum of `field3logit` and `multifield3logit`.
* Added attribute `ordinal` to S3 class `field3logit` and updated the S3 method
 `print`.





# plot3logit 1.0.2


## Major changes

* Vignette `plot3logit-overview` has been added to the package.
* Updated dependencies by including package "ellipse" amongst imported packages.


## Minor changes

* The level `Long` of variable `irregulariry` of dataset `cross_1year` has
  been changed to `High`.


## Typos and other marginal changes

* Updated references to Santi, Dickson and Espa (2019).





# plot3logit 1.0.1


## Major changes

* Bug solved for `as.data.frame.field3logit` when a zero-coefficient model is
  passed to field3logit. The previous version was not able to return a
  data.frame correctly built. Functions `stat_3logit`, and `TernaryField` have
  been adapted so as to draw points instead of arrows.


## Minor changes

* Interface of `fortify` methods has been redefined by including argument
  `data`, so that now all `fortify` methods of the package are compliant with
  the interface of the generic.

* `field3logit` now can read categories labels of the dependent variable from
  attribute `labs` when argument `model` is a `matrix`. Example in the help of
  package presentation has been updated.


## Typos and other marginal changes

* `CITATION` file has been amended: the title of the package and other typos
  have been corrected.

* Revision of the file `README.md`.





# plot3logit 1.0.0

First release of the package `plot3logit` on CRAN!
