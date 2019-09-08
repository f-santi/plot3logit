
# plot3logit 1.0.2


## Major changes

* Vignette "plot3logit-overview" has been added to the package.
* Updated dependencies by including package "ellipse" amongst imported packages


## Minor changes

* The level "Long" of variable "irregulariry" of dataset "cross_1year" has
  been changed to "High".


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
