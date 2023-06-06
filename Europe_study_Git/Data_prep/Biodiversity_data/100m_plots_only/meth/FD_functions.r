# Franziska Schrodt 2013
# some considerations on how to estimate functional biodiversity

# open issue: should we uniformize each trait?
# advantage: outlier robust
# calculate covariance or mutual information matrix
# then apply det, log det, trace, each of them possibly normalized
# consider the "small n, large p" problem

## DEFINE SOME FUNCTIONS WE NEED

###------------------------
p2uniform <- function(series) {
  # Stefan Harmeling, Miguel Mahecha, Oct. 2012
  # transform distribution of a data vector to a uniform distributions
  # this is possibly a nonlinear transformation ... but rescales outliers
  if (!is.na(series[1])) {
    N      <- length(series)
    idx    <- order(series)
    y      <- array(data = NA, N)
    y[idx] <- 1:N
    y      <- y/N
    return(y)
  } else {
    return(series)
  }
}



