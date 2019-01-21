
#' `field3logit` simplification function and test
#'
#' Given an object of class `field3logit`, `simplify_field3logit` returns
#' it in the simplified form. Function `is_simplified_field3logit` tests
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
#' `field3logit` computes the vector field associated to a change in regressior
#' values (which may involve more than one regressor) of a trinomial logit
#' model either fitted by some multinomial regression function or explicitly
#' specified.
#'
#' `plot3logit` and `plot` method draw the ternary plot using standard graphics
#' methods provided by package `Ternary`. See function [`gg3logit`] for plotting
#' through the package [`ggtern`][ggtern::ggtern-package] based on the grammar
#' of graphics.
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
#' @param model either a fitted trinomial model or a matrix of regressor
#'   coefficients. See section *Compatibility* and examples of
#'   [`plot3logit-package`].
#' @param delta the change in the values of covariates to be represented.
#'   This could be either a `numeric` vector, the name of a covariate
#'   (passed either as a `character` or an `expression`), or a mathematical
#'   expression involving one or more than one covariates (passed either as
#'   a `character` or an `expression`). See details and examples.
#' @param p0 `list` of starting points (ternary coordinates) of the curves
#'   of the field. If not specified, `field3logit` automatically compute
#'   `ncurves` candidate points so that arrows are evenly distributed over
#'   the ternary plot area. See Examples.
#' @param alpha `numeric` vector of length two where constants \eqn{\alpha^{(1)}}
#'   and \eqn{\alpha^{(2)}} are stored (only for ordinal models), as
#'   defined in Equation (7) of Santi, Dickson and Espa (2018).
#' @param ncurves number of curves of the field to be computed. In case
#'   of ordinal models, this parameter is ineffective, as only one curve
#'   can be drawn. The parameter is ineffective also in case that argument
#'   `p0` is set.
#' @param narrows maximum number of arrows to be drawn per curve.
#' @param edge minimum distance between each arrow (or point) and
#'   the edge of the ternary plot.
#' @param x object of class `field3logit`.
#' @param add `logical` argument which specifies whether the field
#'   should be added to an existing plot (`add = TRUE`) or a new
#'   ternary plot should be drawn (`add = FALSE`).
#' @param ... other arguments passed to or from other methods.
#' @param wide it allows to choose whether `as.data.frme` should return a
#'   `data.frame` object in wide (default) or long form.
#' @inheritParams effect
#' @param label label to be used for identifying the field when multiple
#'   fields are plotted. See [`multifield3logit`].
#'
#' @return
#' `S3` object of class `field3logit` structured as a named `list`.
#'
#' @seealso
#' [`multifield3logit`], [`gg3logit`].
#'
#' @inherit cross_1year references
#'
#' @examples
#' data(cross_1year)
#'
#' # Model fit
#' mod0 <- nnet::multinom(employment_sit ~ finalgrade + irregularity + hsscore,
#'   cross_1year)
#' mod0
#' 
#' # Assessing the effect of "finalgradeHigh" (explicit notation)
#' field0 <- field3logit(mod0, c(0, 0, 1, 0, 0, 0))
#' gg3logit(field0) + stat_3logit()
#'
#' # Assessing the effect of "finalgradeHigh" (implicit notation)
#' field0 <- field3logit(mod0, 'finalgradeHigh')
#' gg3logit(field0) + stat_3logit()
#' 
#' # Assessing the combined effect of "finalgradeHigh" and
#' # a decrease of "hsscore" by 10
#' field0 <- field3logit(mod0, 'finalgradeHigh - 10 * hsscore')
#' gg3logit(field0) + stat_3logit()
#'
#' @export
field3logit <- function(model, delta, label = '<empty>', p0 = NULL,
  alpha = NULL, ncurves = 8, narrows = Inf, edge = 0.01) {

  # Read input
  modB <- read_model(model, 'logit', alpha)
  vdelta <- get_vdelta(delta, modB)
  DeltaB <- as.numeric(crossprod(vdelta, modB$B))

  # Compute the starting points of the curves
  if (is.null(p0)) {
    modB$DeltaB2pc(DeltaB, ncurves, edge) %>%
      pc2p0(DeltaB, edge, modB[c('XB2P','P2XB')]) -> p0
  } else {
    p0 <- list(pp = p0)
    p0$status <- ifelse(all(DeltaB == 0),'p','p0')
  }

  # Compute the arrows
  if (p0$status == 'p') {
    out <- p0$pp
  } else {
    out <- lapply(p0$pp, gen_path, DeltaB = DeltaB,
      edge = edge, nmax = narrows, flink = modB[c('XB2P','P2XB')])
  }

  # Output
  names(out) %<>% paste0('C', 1:length(out), .)
  out <- list(B = modB$B, alpha = modB$alpha, delta = delta,
    vdelta = vdelta, lab = modB$lab, readfrom = modB$readfrom,
    effects = out, label = label)
  class(out) <- 'field3logit'
  out
}



#' @rdname field3logit
#' @export
plot3logit <- function(model, delta, label = '<empty>', p0 = NULL,
  alpha = NULL, ncurves = 8, narrows = Inf, edge = 0.01, ...) {

  depo <- field3logit(model = model, delta = delta, p0 = p0,
    alpha = alpha, ncurves = ncurves, narrows = narrows,
    edge = edge, label = label)
  
  graphics::plot(depo, ...)
  
  invisible(depo)
}



#' @rdname field3logit
#' @export
print.field3logit <- function(x, ...) {
  x$effects %>%
    lapply(length) %>%
    unlist %>%
    sum -> na
  
  cat(' Object of class "field3logit"\n')
  cat('-------------------------------\n')
  cat('Label                    :', x$label, '\n')
  cat('Possible outcomes        :', paste(x$lab, collapse = '; '), '\n')
  cat('Effect                   :', x$delta, '\n')
  
  if (!is.numeric(x$delta)) {
    cat('Explicit effect          :', x$vdelta, '\n')
  }
  
  cat('Model has been read from :', x$readfrom, '\n')
  cat('Number of curves         :', length(x$effects), '\n')
  cat('Number of arrows         :', na, '\n')

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
as.data.frame.field3logit <- function(x, ..., wide = TRUE) {

  depoLab <- list(label = x$label, lab = x$lab)
  
  x %<>%
    use_series('effects') %>%
    simplify_field3logit

  x %>%
    names %>%
    strsplit('A') %>%
    Reduce(rbind, ., NULL) %>%
    as.data.frame %>%
    { .[rep(1 : nrow(.), each = 2), ] } %>%
    cbind(depoLab$label, .) %>%
    set_colnames(c('label', 'curve', 'arrow')) %>%
    set_rownames(NULL) -> depo
    
  levels(depo$arrow) %<>% paste0('A', .)

  x %<>%
    Reduce(function(x, y) { rbind(x, y$from, y$to) }, . , NULL) %>%
    data.frame %>%
    set_colnames(depoLab$lab) %>%
    cbind(depo, role = rep(c('from', 'to'), nrow(depo) / 2), .)

  if (wide) {
    merge(x[x$role == 'from', -4], x[x$role == 'to', -4],
  	  by = c('label', 'curve', 'arrow'), suffixes = c('', '_end')
  	) -> x
  }
  
  return(x)
}


#' @rdname field3logit
#' @export
fortify.field3logit <- function(model, ...) {
  return(as.data.frame.field3logit(model, ...))
}




