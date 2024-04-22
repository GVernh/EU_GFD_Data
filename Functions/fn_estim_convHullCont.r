###------------------------
convHullCont  		<- function(X) {
  require("geometry")
  require("ape")
  # Convex hull content (sometimes called it Functional Richness)
  if (length(dim(X)) == 2) {
    n = dim(X)[1]
    p = dim(X)[2]
	dm <-as.matrix(dist(X, upper = TRUE))
	pcoa_out <- pcoa(dm, correction = "none", rn = NULL)
	convhull <- convhulln(pcoa_out$vectors[, 1:min(p, 3)], "FA")$vol
    return(convhull)
  } else {
    return(NA)
  }
}

