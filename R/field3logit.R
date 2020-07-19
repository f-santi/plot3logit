
prepare_arrow <- function(comp, from, to) {
  tibble(comp = comp, from = from, to = to) %>%
    pivot_longer(cols = c('from', 'to'), names_to = 'role') %>%
    dplyr::arrange('role') %>%
    mutate(comp = paste0(.$comp, ifelse(.$role == 'from', '', '_to'))) %>%
    select(-'role') %>%
    pivot_wider(names_from = 'comp') %>%
    return
}


prepare_block <- function(label, idarrow, comp, from, to, confregion) {
  # Prepare arrows
  tibble(type = 'arrow', comp = comp, from = from, to = to) %>%
    pivot_longer(cols = c('from', 'to'), names_to = 'role') -> depo
  
  # Prepare confidence regions
  if (!is.null(confregion)) {
  	confregion %>%
      pivot_longer(cols = 1:3, names_to = 'comp') %>%
      mutate(role = 'from', type = 'region') %>%
      select('type', 'comp', 'role', 'value') %>%
      bind_rows(depo) -> depo
  }
  
  # Complete output
  depo %>%
    mutate(label = label, idarrow = idarrow, group = NA) %>%
    select('label', 'idarrow', 'group', tidyselect::everything()) %>%
    return
}



field3logit_list <- function(model, delta, label, p0, alpha, vcov, nstreams,
  narrows, edge, conf, npoints) {
    	
  delta %>%
  lapply(function(w) {
  	if (is.list(w)) {
  	  list(
  	    model = model, label = label, p0 = p0, alpha = alpha, 
  	    vcov = vcov, nstreams = nstreams, narrows = narrows,
  	    edge = edge, conf = conf, npoints
  	  ) %>%
  	  modifyList(w) %>%
  	  do.call('field3logit', .) -> out
  	} else {
  	  field3logit(
  	    model, w, paste(w, collapse = '|'), p0, alpha, vcov,
  	    nstreams, narrows, edge, conf, npoints
  	  ) -> out
  	}
  	out
  }) %>%
  Reduce(`+`, .) %>%
  return
}



#' `field3logit` simplification function and test
#'
#' Given an object of class `field3logit`, [simplify_field3logit()] returns
#' it in the simplified form. Function [is_simplified_field3logit()] tests
#' whether an object of class `field3logit` is in the simplified form.
#'
#' @param x an object of class `field3logit`.
#'
#' @keywords internal
simplify_field3logit<- function(x) {
  if (!is_simplified_field3logit(x)) {
    x %<>%
      { Map(function(a, b) { names(a) %<>% paste0(b, .); a }, ., names(.)) } %>%
      Reduce(c,.)
  }
  
  class(x) <- c('field3logit', 'simplified')
  return(x)
}



#' @rdname simplify_field3logit
#' @keywords internal
is_simplified_field3logit <- function(x) {
  all(inherits(x, c('field3logit', 'simplified'), TRUE))
}



#' Computation of the vector field
#'
#' @description
#'
#' [field3logit()] computes the vector field associated to a change in
#' regressior values (which may involve more than one regressor) of a trinomial
#' logit model either fitted by some multinomial regression function or
#' explicitly specified.
#'
#' The method [plot()] draws the ternary plot using standard graphics methods
#' provided by package `Ternary`. See function [gg3logit()] for plotting
#' through the package [`ggtern`][ggtern::ggtern_package] based on the
#' grammar of graphics.
#' 
#' Methods [as.data.frame()], [as_tibble()], [fortify()] and [tidy()] permits
#' the graphical information of a `field3logit` object to be exported in a
#' standardised format (either a `data.frame` or a `tibble`).
#'
#' @details
#'
#' Argument `delta` could be passed in one of the following formats:
#' * explicitly, as a `numeric` vector corresponding to the change
#'   \eqn{\Delta x\in\bm{R}^k} in regressors values \eqn{x\in\bm{R}^k};
#' * implicitly, as a `character` of the name of the covariate to be considered.
#'   In this case, vector \eqn{\Delta x\in\bm{R}^k} is computed for a unit
#'   change of the specified covariate;
#' * as a mathematical expression (passed as an `expression` or a `character`
#'   object) involving one or more than one covariates. This allows one to
#'   analyse the effects of composite covariate changes through an easy-to-write
#'   and easy-to-read code without having to cope with explicit numerical
#'   specification of vector \eqn{\Delta x\in\bm{R}^k}.
#'
#' See examples for comparing all three methods.
#'
#' **It is also possible to pass a `list` to argument `delta`.** In such a case,
#' the function `field3logit` is run once for every component of `delta`,
#' and the set of generated `field3logit` objects is combined into a single
#' object of class `multifield3logit`. The compoments of the list passed to
#' `delta` must be named lists whose elements are used as arguments of each call
#' of function `field3logit`, whereas the arguments specified in the parent call
#' of `field3logit` are used as default values. It follows that arguments shared
#' by all fields can be specified once in the parent call of `field3logit`, and
#' only arguments which changes from field to field (such as `delta` and
#' `label`) should be set in the lists making up the list passed to `delta`. See
#' the last example in section Examples and the help of [multifield3logit()].
#'
#' @inheritParams effect
#' @param model either a fitted trinomial model or a matrix of regressor
#'   coefficients. See section *Compatibility* and examples of
#'   [`plot3logit-package`].
#' @param delta the change in the values of covariates to be represented.
#'   This could be either a `numeric` vector, the name of a covariate
#'   (passed either as a `character` or an `expression`), or a mathematical
#'   expression involving one or more than one covariates (passed either as
#'   a `character` or an `expression`). If a list is passed to `delta`,
#'   multiple fields are computed according to parameters passed as
#'   components of a 2-level list. See details and examples.
#' @param p0 `list` of starting points (ternary coordinates) of the curves
#'   of the field. If not specified, `field3logit` automatically compute
#'   `nstreams` candidate points so that arrows are evenly distributed over
#'   the ternary plot area. See Examples.
#' @param alpha `numeric` vector of length two where constants \eqn{\alpha^{(1)}}
#'   and \eqn{\alpha^{(2)}} are stored (only for ordinal models), as
#'   defined in Equation (7) of \insertCite{santi2019;textual}{plot3logit}.
#' @param nstreams number of stream lines of the field to be computed. In case
#'   of ordinal models, this parameter is ineffective, as only one curve
#'   can be drawn. The parameter is ineffective also in case that argument
#'   `p0` is set.
#' @param narrows maximum number of arrows to be drawn per curve.
#' @param edge minimum distance between each arrow (or point) and
#'   the edge of the ternary plot.
#' @param x,object object of class `field3logit`.
#' @param add `logical` argument which specifies whether the field
#'   should be added to an existing plot (`add = TRUE`) or a new
#'   ternary plot should be drawn (`add = FALSE`).
#' @param ... other arguments passed to or from other methods.
#' @param wide it allows to choose whether `as.data.frame`, `as_tibble`,
#'   `fortify` and `tidy` should return a `data.frame` or a `tibble` in wide
#'   (default) or long form.
#' @param label label to be used for identifying the field when multiple
#'   fields are plotted. See [multifield3logit()].
#' @param data not used. Argument included only for interface compatibility with
#'   the generic `fortify`.
#' @param conf confidence level of confidence regions to be computed **for each
#'   arrow** of the field.
#' @param npoints number of points of the border to be computed **for each
#'   confidence region**.
#' @param vcov (**only if** the model is read from a matrix, otherwise it will
#'   be ignored) variance-covariance matrix of parameter estimates. The elements
#'   of the variance-covariance matrix should be ordered according to the matrix 
#'   of parameter estimates where the categories of the dependent variable are
#'   the slow index, whereas the covariates are the fast index.
#' @param value value to be assigned.
#'
#' @return
#' `S3` object of class `field3logit` structured as a named `list` or an object
#' of class `multifield3logit` if `delta` is a `list`.
#'
#' @seealso
#' [multifield3logit()], [gg3logit()], [autoplot()].
#'
#' @inherit cross_1year references
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' data(cross_1year)
#'
#' \dontrun{
#' # Fitting the model
#' mod0 <- nnet::multinom(employment_sit ~ finalgrade + irregularity + hsscore,
#'   cross_1year)
#' mod0
#' 
#' # Assessing the effect of "finalgradeHigh" (explicit notation)
#' field0 <- field3logit(mod0, c(0, 0, 1, 0, 0, 0))
#' gg3logit(field0) + stat_field3logit()
#'
#' # Assessing the effect of "finalgradeHigh" (implicit notation)
#' field0 <- field3logit(mod0, 'finalgradeHigh')
#' gg3logit(field0) + stat_field3logit()
#' 
#' # Assessing the combined effect of "finalgradeHigh" and
#' # a decrease of "hsscore" by 10
#' field0 <- field3logit(mod0, 'finalgradeHigh - 10 * hsscore')
#' gg3logit(field0) + stat_field3logit()
#' }
#'
#' # Fitting the model
#' mod1 <- nnet::multinom(employment_sit ~ ., data = cross_1year)
#'
#' # List passed to argument "delta" for generating "multifield3logit" objects
#' refpoint <- list(c(0.7, 0.15, 0.15))
#' depo <- list(
#'   list(delta = 'durationShort',  label = 'Short duration'),
#'   list(delta = 'durationLong',   label = 'Long duration'),
#'   list(delta = 'finalgradeHigh', label = 'High final grade'),
#'   list(delta = 'finalgradeLow',  label = 'Low final grade')
#' )
#' mfields <- field3logit(mod1, delta = depo, p0 = refpoint, narrows = 1)
#' mfields
#'
#' @export
field3logit <- function(model, delta, label = '<empty>', p0 = NULL,
  alpha = NULL, vcov = NULL, nstreams = 8, narrows = Inf, edge = 0.01,
  conf = NA, npoints = 100) {

  # Check argument "delta"
  if (is.list(delta)) {
  	field3logit_list(
  	  model, delta, label, p0, alpha, vcov,
  	  nstreams, narrows, edge, conf, npoints
    ) -> out
  } else {
    # Read input
    modB <- read_model(model, 'logit', alpha, vcov)
    vdelta <- get_vdelta(delta, modB)
    DeltaB <- as.numeric(crossprod(vdelta, modB$B))

    # Compute the starting points of the curves
    if (is.null(p0)) {
      modB$DeltaB2pc(DeltaB, nstreams, edge) %>%
        pc2p0(DeltaB, edge, modB[c('XB2P','P2XB')]) -> p0
    } else {
      p0 <- list(pp = p0)
      p0$status <- ifelse(all(DeltaB == 0), 'p', 'p0')
    }

    # Compute the arrows
    if (p0$status == 'p') {
    	  lapply(p0$pp, function(x) {
  	    list(A1 = list(from = x, to = rep(NA, 3)))
  	  }) -> out
    } else {
      out <- lapply(p0$pp, gen_path, DeltaB = DeltaB,
        edge = edge, nmax = narrows, flink = modB[c('XB2P','P2XB')])
    }

    # Create field3logit object
    names(out) %<>% paste0('C', 1:length(out), .)
    out <- list(B = modB$B, alpha = modB$alpha, delta = delta,
      vdelta = vdelta, lab = modB$lab, readfrom = modB$readfrom,
      effects = out, label = label, vcovB = modB$vcovB,
      ordinal = modB$ordinal, conf = conf
    )
    class(out) <- 'field3logit'
  
    # Compute the confidence regions
    if (!is.null(out$vcov) & !is.na(conf)) {
  	  out %<>% add_confregions(conf, npoints)
    }
  }
  
  # Output
  out
}



#' @rdname field3logit
#' @export
print.field3logit <- function(x, ...) {
  x$effects %>%
    lapply(length) %>%
    unlist %>%
    sum -> na
 
  type  <- ifelse(x$ordinal, 'ordinal', 'categorical')
  vcovB <- ifelse(is.null(x$vcovB), 'not available', 'available')
  conf  <- ifelse(is.na(x$conf), 'not available', paste0(100 * x$conf, '%'))
  
  cat(' Object of class "field3logit"\n')
  cat('-------------------------------\n')
  cat('Label                    :', x$label, '\n')
  cat('Possible outcomes        :', paste(x$lab, collapse = '; '), '\n')
  cat('Type of model            :', type, '\n')
  cat('Effect                   :', x$delta, '\n')
  
  if (!is.numeric(x$delta)) {
    cat('Explicit effect          :', x$vdelta, '\n')
  }
  
  cat('Model has been read from :', x$readfrom, '\n')
  cat('Number of stream lines   :', length(x$effects), '\n')
  cat('Number of arrows         :', na, '\n')
  cat('Covariance matrix        :', vcovB, '\n')
  cat('Confidence regions       :', conf, '\n')

  invisible(x)
}



#' @rdname field3logit
#' @export
plot.field3logit <- function(x, ..., add = FALSE, length = 0.05) {

  if (!add)  {
  	TernaryPlot(atip = x$lab[1], btip = x$lab[2], ctip = x$lab[3],
  	  alab = x$lab[1], blab = x$lab[2], clab = x$lab[3],
  	  grid.minor.lines = 1)
  }
  
  TernaryField(x, ..., length = length)
  
  invisible(x)
}



#' @rdname field3logit
#' @export
as_tibble.field3logit <- function(x, ..., wide = TRUE) {
  depoLab <- list(label = x$label, lab = x$lab)
  
  x %<>%
    use_series('effects') %>%
    simplify_field3logit %>%
    purrr::imap(~ prepare_block(
      label = depoLab$label,
      idarrow = .y,
      comp = depoLab$lab,
      from = .x$from,
      to = .x$to,
      confregion = .x$confregion
    )) %>%
    purrr::reduce(bind_rows) %>%
    mutate(comp = factor(.$comp, depoLab$lab))
  
  if (wide) {
    x %<>%
      dplyr::arrange(.$role) %>%
      mutate(comp = paste0(.$comp, ifelse(.$role == 'from', '', '_end'))) %>%
      select(-'role') %>%
      pivot_wider(
        names_from = 'comp',
        values_from = 'value',
        values_fill = list(NA),
        values_fn = list(value = list)
      ) %>%
      tidyr::unnest(setdiff(colnames(.), c('label', 'idarrow', 'obj')))
  }
  
  x %>%
    mutate(group = forcats::fct_anon(factor(paste0(.$label, .$idarrow)), 'H')) %>%
    dplyr::mutate_if(is.character, factor) %>%
    return
}



#' @rdname field3logit
#' @export
as.data.frame.field3logit <- function(x, ..., wide = TRUE) {
  as_tibble(x, ..., wide = wide) %>%
    as.data.frame %>%
    return
}



#' @rdname field3logit
#' @export
fortify.field3logit <- function(model, data, ..., wide = TRUE) {
  as_tibble.field3logit(model, ..., wide = wide) %>%
    return
}



#' @rdname field3logit
#' @export
tidy.field3logit <- function(x, ..., wide = TRUE) {
  as_tibble.field3logit(x, ..., wide = wide) %>%
    return
}



#' @rdname field3logit
#' @export
coef.field3logit <- function(object, ...) {
  return(object$B)
}



#' @rdname field3logit
#' @export
vcov.field3logit <- function(object, ...) {
  return(object$vcovB)
}



#' @rdname field3logit
#' @export
labels.field3logit <- function(object, ...) {
  return(object$label)
}



#' @rdname field3logit
#' @export
`labels<-.field3logit` <- function(x, value) {
  x$label <- as.character(value)[1]
  return(x)
}



