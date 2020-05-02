
Stat3Logit <- ggplot2::ggproto('StatIdentity', Stat,
  compute_group = function(data, scales) return(data)
)


#' Create a new gg3logit
#'
#' `gg3logit` initializes a [`ggplot`][ggplot2::ggplot] object through
#' [`ggtern`][ggtern::ggtern]. If a fortified `field3logit` or a
#' `multifield3logit` object is passed as argument `data` to `gg3logit`,
#' the labels of the ternary plot are automatically. The same happens if a
#' `field3logit` or a `multifield3logit` object is passed; in that case,
#' `gg3logit` preliminarly invoke the `fortify` method.
#'
#' @param data a `field3logit` or a `multifield3logit` object. If not
#'   specified, must be supplied in each layer added to the plot.
#' @param mapping list of aesthetic mappings to use for plot. If not
#'   specified, must be supplied in each layer added to the plot. **Note
#'   that** mappings `x`, `y` and `z` are **not** required: they will be
#'   overwritten if specified (see examples).
#' @param ... additional arguments passed through to [`ggtern`][ggtern::ggtern].
#'
#' @family `gg` functions
#'
#' @examples
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#'
#' gg3logit(field0) + stat_3logit()
#'
#' @export
gg3logit <- function (data = NULL, mapping = aes(), ...) {

  if (!is.null(data)) {
    if (inherits(data, 'field3logit')) { data %<>% fortify }
  	depo <- colnames(data$arrow[[1]])
    mapping %<>% modifyList(ggplot2::aes_(
      x    = as.symbol(depo[1]),
      y    = as.symbol(depo[2]),
      z    = as.symbol(depo[3]),
      xend = as.symbol(depo[4]),
      yend = as.symbol(depo[5]),
      zend = as.symbol(depo[6])
    ))
  }

  ggtern(data = data, mapping = mapping, ...) +
    limit_tern(breaks = (0:5) / 5, labels = (0:5) / 5) +
    theme_showarrows()
}





#' Add a field to a `gg3logit` plot
#'
#' `stat_3logit` add a field to a [`gg3logit`] plot.
#'
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::stat_identity
#' @inheritParams gg3logit
#' @param data a `field3logit` or a `multifield3logit` object.
#' @param mapping list of aesthetic mappings to use for plot. **Note
#'   that** mappings `x`, `y` and `z` are **not** required: they will be
#'   overwritten if specified (see examples).
#' @param arrow. specification for arrow heads, as created by
#'   function [`arrow`][grid::arrow] of package [`grid`][grid::grid-package].
#'
#' @family `gg` functions
#'
#' @examples
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#'
#' gg3logit(field0) + stat_3logit()
#' gg3logit() + stat_3logit(data = field0)
#'
#' @export
stat_3logit <- function(mapping = aes(), data = NULL, geom = 'segment',
  position = 'identity', show.legend = NA, inherit.aes = TRUE,
  arrow. = arrow(length = unit(0.2, 'cm')), ...) {

  params <- list(arrow = arrow., ...)
  
  if (!is.null(data)) {
  	if (inherits(data, 'field3logit')) { data %<>% fortify }
  	depo <- colnames(data$arrow[[1]])
    mapping %<>% modifyList(ggplot2::aes_(
      x    = as.symbol(depo[1]),
      y    = as.symbol(depo[2]),
      z    = as.symbol(depo[3]),
      xend = as.symbol(depo[4]),
      yend = as.symbol(depo[5]),
      zend = as.symbol(depo[6])
    ))
  } else {
  	mapping %<>% utils::modifyList(list(
  	  x = NULL, y = NULL, z = NULL,
  	  xend = NULL, yend = NULL, zend = NULL
  	))
  }

  data %<>%
    select(-'region') %>%
    unnest(cols = 'arrow')

  if (!is.null(data$idarrow) & all(data$idarrow == 'X')) {
  	geom <- 'point'
  	params['arrow'] <- NULL
  }
  
  ggplot2::layer(
    stat = Stat3Logit, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = params
  )
}





#' Add a field to a `gg3logit` plot
#'
#' `stat_3logit` add a field to a [`gg3logit`] plot.
#'
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::stat_identity
#' @inheritParams gg3logit
#' @param data a `field3logit` or a `multifield3logit` object.
#' @param mapping list of aesthetic mappings to use for plot. **Note
#'   that** mappings `x`, `y` and `z` are **not** required: they will be
#'   overwritten if specified (see examples).
#' @param arrow. specification for arrow heads, as created by
#'   function [`arrow`][grid::arrow] of package [`grid`][grid::grid-package].
#'
#' @family `gg` functions
#'
#' @examples
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#'
#' gg3logit(field0) + stat_3logit()
#' gg3logit() + stat_3logit(data = field0)
#'
#' @export
stat_conf3logit <- function(mapping = aes(), data = NULL, geom = 'polygon',
  position = 'identity', show.legend = NA, inherit.aes = TRUE, ...) {

  if (!is.null(data)) {
  	if (inherits(data, 'field3logit')) { data %<>% fortify }
  	depo <- colnames(data$arrow[[1]])
  	
    mapping %<>% modifyList(ggplot2::aes_(
      x     = as.symbol(depo[1]),
      y     = as.symbol(depo[2]),
      z     = as.symbol(depo[3]),
      group = as.symbol('idarrow')
    ))
  } else {
  	mapping %<>% utils::modifyList(list(
  	  x = NULL, y = NULL, z = NULL, group = NULL
  	))
  }

  data %<>%
    select(-'arrow') %>%
    unnest(cols = 'region')

  ggplot2::layer(
    stat = Stat3Logit, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = list(...)
  )
}


