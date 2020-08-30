
# Generate a random "field3logit" object from matrix
test_rfield3logit<- function(delta = NULL, vcov = FALSE) {
  if (is.null(delta)) {
    k <- sample(2:9, 1)
    delta <- plot3logit:::versor(sample(2:k, 1), k)	
  } else {
  	k <- length(delta)
  }
  
  if (is.logical(vcov)) {
  	if (vcov) {
  	  matrix(rnorm(4 * k^2), 2 * k, 2 * k) %>%
  	    crossprod -> vcov
  	} else { vcov <- NULL }
  }
  
  M <- matrix(rnorm(2 * k), k, 2)
  rownames(M) <- paste0('X', 0:(k - 1))
  attr(M, 'levels') <- c('Class A', 'Class B', 'Class C')
  
  field3logit(M, delta, vcov = vcov)
}

