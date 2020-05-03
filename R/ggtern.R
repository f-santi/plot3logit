
Stat3Logit <- ggplot2::ggproto('StatIdentity', Stat,
  compute_group = function(data, scales) {
  	data %>%
  	  dplyr::filter(type == 'arrow') %>%
  	  return
  }
)

Conf3Logit <- ggplot2::ggproto('StatConfidenceTern', Stat,
  compute_group = function(data, scales) {
  	data %>%
  	  dplyr::filter(type == 'region') %>%
  	  return
  }
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
#' gg3logit(field0) + stat_field3logit()
#'
#' @export
gg3logit <- function (data = NULL, mapping = aes(), ...) {

  if (!is.null(data)) {
    if (inherits(data, 'field3logit')) {
    	  data %<>% fortify
      
      mapping %<>%
        modifyList(ggplot2::aes_(
          x     = as.symbol(colnames(data)[5]),
          y     = as.symbol(colnames(data)[6]),
          z     = as.symbol(colnames(data)[7]),
          xend  = as.symbol(colnames(data)[8]),
          yend  = as.symbol(colnames(data)[9]),
          zend  = as.symbol(colnames(data)[10]),
          group = as.symbol('group'),
          type  = as.symbol('type')
        ))
    }
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
#' field0 <- field3logit(mod0, 'genderFemale', conf = 0.95)
#'
#' gg3logit(field0) + stat_field3logit()
#' gg3logit(field0) + stat_field3logit() + stat_conf3logit()
#'
#' @export
stat_3logit <- function(mapping = aes(), data = NULL, geom = 'segment',
  position = 'identity', show.legend = NA, inherit.aes = TRUE,
  arrow. = arrow(length = unit(0.2, 'cm')), ...) {

  .Deprecated('stat_field3logit')
  
  stat_field3logit(
    mapping = mapping, data = data, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, arrow. = arrow., ...
  )
}



#' @rdname stat_3logit
#' @export
stat_field3logit <- function(mapping = aes(), data = NULL, geom = 'segment',
  position = 'identity', show.legend = NA, inherit.aes = TRUE,
  arrow. = arrow(length = unit(0.2, 'cm')), ...) {

  params <- list(arrow = arrow., ...)
  
  if (!is.null(data)) {
    if (inherits(data, 'field3logit')) {
      data %<>% fortify
      
      mapping %<>%
        modifyList(ggplot2::aes_(
          x     = as.symbol(colnames(data)[5]),
          y     = as.symbol(colnames(data)[6]),
          z     = as.symbol(colnames(data)[7]),
          xend  = as.symbol(colnames(data)[8]),
          yend  = as.symbol(colnames(data)[9]),
          zend  = as.symbol(colnames(data)[10]),
          group = as.symbol('group'),
          type  = as.symbol('type')
        ))
    }
  } else {
  	mapping %<>% utils::modifyList(list(
  	  x = NULL, y = NULL, z = NULL,
  	  xend = NULL, yend = NULL, zend = NULL,
  	  group = NULL, type = NULL
  	))
  }

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



#' @rdname stat_3logit
#' @export
stat_conf3logit <- function(mapping = aes(), data = NULL, geom = 'polygon',
  position = 'identity', show.legend = NA, inherit.aes = TRUE, ...) {

  list(fill = 'blue', alpha = 0.2) %>%
    modifyList(list(...)) -> params
    
  if (!is.null(data)) {
  	if (inherits(data, 'field3logit')) { data %<>% fortify }
  	mapping %<>%
      modifyList(ggplot2::aes_(
          x     = as.symbol(colnames(data)[5]),
          y     = as.symbol(colnames(data)[6]),
          z     = as.symbol(colnames(data)[7]),
          xend  = as.symbol(colnames(data)[8]),
          yend  = as.symbol(colnames(data)[9]),
          zend  = as.symbol(colnames(data)[10]),
          group = as.symbol('group'),
          type  = as.symbol('type')
        ))
  } else {
  	mapping %<>% utils::modifyList(list(
  	  x = NULL, y = NULL, z = NULL,
  	  xend = NULL, yend = NULL, zend = NULL,
  	  group = NULL, type = NULL
  	))
  }
  
  if (!is.null(mapping$fill))  { params$fill  <- NULL }
  if (!is.null(mapping$alpha)) { params$alpha <- NULL }

  ggplot2::layer(
    stat = Conf3Logit, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = params
  )
}


