
confregion <- function(mu, Sig, conf = 0.95, npoints = 100) {
  # Compute the ellipses
  out <- ellipse::ellipse(x = Sig, centre = mu, level = conf)
  out %<>% as.data.frame
  
  # Compute the ternary coordinates
  out$p1 <- exp(x) / (1 + exp(x) + exp(y))
  out$p2 <- exp(y) / (1 + exp(x) + exp(y))
  out$p3 <- 1 / (1 + exp(x) + exp(y))
  
  # Return the confidence ellipse
  return(out[ , c('p1', 'p2', 'p3')])
}
