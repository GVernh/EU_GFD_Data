###------------------------
hyperVolCont  		<- function(X) {
  # Hypervolume as described by Blonder et al. 2014
  if (length(dim(X)) == 2) {
    n = dim(X)[1]
    p = dim(X)[2]
    library("hypervolume")
    bws <- estimate_bandwidth(X)
    hv <- hypervolume(X, repsperpoint = 100*p, bandwidth = bws, quantile = 0.1, verbose = FALSE)
    return(hv@Volume)
  } else {
    return(NA)
  }
}