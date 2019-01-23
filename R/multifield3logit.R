
#' Multiple trilogit fields
#'
#' Methods of `S3` class `multifield3logit` handle multiple `fields3logit`
#' objects simultaneously and permit new `multifield3logit` object to be
#' easily created by means of the sum operator `+`. The [`fortify`] method
#' of `multifield3logit` permits multiple `field3logit` objects to be easily
#' handled by [`gg3logit`] and `ggtern`-based functions and methods. See
#' Examples.
#'
#' @param x,y object of class `field3logit` or `multifield3logit`.
#' @inheritParams field3logit
#' @param ... other arguments passed to or from other methods.
#' @param maxitems maximum number of items to be enumerated when an object of
#'   class `multifield3logit` is printed.
#'
#' @return
#' `S3` object of class `multifield3logit` structured as a named `list`.
#'
#' @family `gg` functions
#'
#' @seealso
#' [`field3logit`].
#'
#' @examples
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ ., data = cross_1year)
#' mod0
#'
#' field_Sdur <- field3logit(mod0, 'durationShort',
#'   label = 'Short duration')
#' field_Hfgr <- field3logit(mod0, 'finalgradeHigh',
#'   label = 'High final grade')
#'
#' gg3logit(field_Sdur + field_Hfgr) +
#'   stat_3logit() +
#'   facet_wrap(~ label)
#'
#' refpoint <- list(c(0.7, 0.15, 0.15))
#'
#' field_Sdur <- field3logit(mod0, 'durationShort',
#'   label = 'Short duration', p0 = refpoint, narrows = 1)
#' field_Ldur <- field3logit(mod0, 'durationLong',
#'   label = 'Long duration', p0 = refpoint, narrows = 1)
#' field_Hfgr <- field3logit(mod0, 'finalgradeHigh',
#'   label = 'High final grade', p0 = refpoint, narrows = 1)
#' field_Lfgr <- field3logit(mod0, 'finalgradeLow',
#'   label = 'Low final grade', p0 = refpoint, narrows = 1)
#'
#' mfields <- field_Sdur + field_Ldur  + field_Lfgr + field_Hfgr
#' mfields
#'
#' gg3logit(mfields) +
#'   stat_3logit(aes(colour = label)) +
#'   theme_zoom_L(0.45)
#'
#' @export
multifield3logit <- function(x, ...) {
  depo <- inherits(x, c('field3logit','multifield3logit'), which = TRUE)
  if (all(depo == 0)) {
  	stop('Only objects of class "field3logit" and "multifield3logit" are allowed')
  } else if (depo[2] == 0) {
  	x %<>%
  	  list %>%
  	  set_names(x$label) %>%
  	  structure(class = c('multifield3logit', 'field3logit'))
  }
  return(x)
}



#' @rdname multifield3logit
#' @export
`+.field3logit` <- function(x, y) {
  c(multifield3logit(x), multifield3logit(y)) %>%
    structure(class = c('multifield3logit', 'field3logit')) %>%
    return
}



#' @rdname multifield3logit
#' @export
print.multifield3logit <- function(x, maxitems = 10, ...) {
  cat(' Object of class "multifield3logit"\n')
  cat('------------------------------------\n')
  cat('Number of fields         :', length(x), '\n')
  cat('Labels\n')
  
  x %>%
    extract(1:min(maxitems, length(x))) %>%
    names %>%
    nchar %>%
    max -> depoL
  for (j in 1:min(length(x), maxitems)) {
  	cat('  ', j, '. ', names(x)[j],
  	  paste0(rep(' ', depoL - nchar(names(x)[j])), collapse = ''),
  	  '  (dX: ', paste(x[[j]]$delta, collapse = ', '), ')\n', sep = '')
  }
  if (length(x) > maxitems) {
  	cat('  [output truncated - see argument "maxlabels"]\n')
  }
  cat('\n')
  
  invisible(x)
}



#' @rdname multifield3logit
#' @export
fortify.multifield3logit <- function(model, data, ...) {
  lapply(model, fortify) %>%
    Reduce(rbind, .) %>%
    return
}




