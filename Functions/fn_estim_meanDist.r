###------------------------
meanDist 		<- function(X) {
  # Estimate the mean center to the centroid
  if (length(dim(X)) == 2) {
    n = dim(X)[1]
    p = dim(X)[2]
    dm <- as.matrix(dist(X, upper = TRUE))
    return(sum(dm)/(n^2-n))
  } else {
    return(NA)
  }
}