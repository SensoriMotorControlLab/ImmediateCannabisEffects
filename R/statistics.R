

# Hautus correction:
# add 0.5 to both the number of hits and the number of false alarms, 
# and add 1 to both the number of signal trials and the number of noise trials;
# dubbed the loglinear approach (Hautus, 1995)

dprime <- function(hits, misses, fas, crs, hautus=FALSE) {
  
  if (hautus) {
    hit_rate    <- (hits + 0.5) / (hits + misses + 1)
    fa_rate     <- (fas  + 0.5) / (fas  + crs    + 1)
    specificity <- (crs  + 0.5) / (crs  + misses + 1)
  } else {
    hit_rate <- hits / (hits + misses)
    fa_rate  <- fas  / (fas  + crs)
    specificity <- (crs) / (crs  + misses)
  }
  
  Zhr    <- qnorm(hit_rate)
  Zfr    <- qnorm(fa_rate)
  dprime <- Zhr - Zfr
  beta   <- exp(-Zhr * Zhr/2 + Zfr * Zfr/2)
  c      <- -(Zhr + Zfr)/2
  
  return( list( 'dprime'      = dprime,
                'beta'        = beta,
                'c'           = c,         
                'sensitivity' = hit_rate,
                'specificity' = specificity ) )
  
}

cowan.k <- function(hits, misses, fas, crs, N) {
  
  hit_rate <- hits / (hits + misses)
  cr_rate  <- crs  / (crs  + fas)
  
  K <- (hit_rate + cr_rate - 1) * N
  
  return(K)
  
}