
handle_block_delta <- function(block, covnames, pattern = '<<(.+?)>>') {
  if (stringr::str_detect(block$delta, pattern)) {
  	# Find matches
    cand <- stringr::str_match(block$delta, pattern)[1, 2]
 
    # List of matching covatiates
  	grep(paste0('^', cand), covnames, value = TRUE) %>%
  	  # Generate new blocks
  	  lapply(function(x) {
  	  	newblock <- block
  	  	newblock$delta <- stringr::str_replace(newblock$delta, pattern, x)
  	  	newblock$label2 %<>% c(x)
  	  	return(newblock)
  	  }) %>%
  	  # Recursion
  	  lapply(handle_block_delta, covnames = covnames, pattern = pattern) -> block
  	  
  	  if (is.list(block[[1]][[1]])) {
  	    block %<>% unlist(recursive = FALSE)
  	  }
  } else { block %<>% list }
  
  return(block)
}


pre_process_delta <- function(delta, model) {
  # Check and structure
  if (is.numeric(delta)) {
  	delta <- list(list(delta = delta))
  } else if (is.character(delta)) {
  	delta <- lapply(delta, function(x) { list(delta = x) })
  } else {
  	delta <- as.list(delta)
  }
  
  # Elaborate
  lapply(delta, handle_block_delta, covnames = rownames(model$B)) %>%
    # Reduce last level
    unlist(recursive = FALSE) %>%
    # Prepare labels
    lapply(function(x) {
    	  if (!is.null(x$label2)) {
    	    x$label %<>% paste0(' (', paste(x$label2, collapse = '; '), ')')
    	    x$label2 <- NULL
    	  }
    	  x
    }) %>%
    # Output
    return()
}



#' It computes the vector of covariate change
#'
#' Given the argument `delta` passed to [`field3logit`] either as
#' a `numeric` vector, a `character` or an `expression` (see
#' [`field3logit`]), `get_vdelta` returns the `numeric` vector of
#' covariate change \eqn{\Delta\in\textbf{R}^k}.
#'
#' @param model object returned by [`read_model`].
#' @param delta see [`field3logit`].
#'
#' @return
#' `numeric` vector of covariate change \eqn{\Delta\in\textbf{R}^k}.
#'
#' @examples
#' library(magrittr)
#'
#' # Example 1
#' matrix(c(0.11, 0.07, -0.1, 0.09), 2) %>%
#'   plot3logit:::read_model('logit') %>%
#'   plot3logit:::get_vdelta(c(0, 1), .)
#'
#' # Example 2
#' library(nnet)
#' data(cross_1year)
#' mod0 <- multinom(employment_sit ~ ., cross_1year)
#' plot3logit:::read_model(mod0, 'logit') %>%
#'   plot3logit:::get_vdelta('genderFemale', .)
#'
#' # Example 3
#' plot3logit:::read_model(mod0, 'logit') %>%
#'   plot3logit:::get_vdelta('-0.5 * genderFemale + hsscore', .)
#'
#' @keywords internal
get_vdelta <- function(delta, model) {
  if (!is.numeric(delta) & (length(delta) > 1)) { delta %<>% extract(1) }

  if (is.character(delta)) { delta %<>% parse(text = .) }
  
  if (is.expression(delta)) {
  	depoE <- new.env()
  	n <- nrow(model$B)
  	
  	mapply(
  	  FUN = function(x, y) assign(x, versor(y, n), envir = depoE),
  	  rownames(model$B),
  	  1:n
  	)
  	delta <- eval(delta, depoE)
  }
  delta
}


