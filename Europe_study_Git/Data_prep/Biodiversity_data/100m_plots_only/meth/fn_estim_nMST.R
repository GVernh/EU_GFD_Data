###------------------------
nMST  		<- function(X) {
  # Use the minimum spanning tree and estimate its branch length
  # For details see Villéger et al 2008, Ecology; they call this metric Functional eveness 
  if (length(dim(X)) == 2) {
    n = dim(X)[1]
    p = dim(X)[2]
    library("igraph")
    dm   <- as.matrix(dist(X, upper = TRUE))
	g    <- graph.adjacency(dm, mode = "max", weighted = TRUE)
    gmst <- minimum.spanning.tree(g)
    we   <- get.edge.attribute(gmst, "weight")
	# total length of MST
    swe  <- sum(we)
	# number of links
    nwe  <- length(we)
    we   <- we/swe
    we[we > (1/nwe)] <- 1/nwe
	nMST <- (sum(we)-1/nwe)/(1-1/nwe)
    return(nMST)
  } else {
    return(NA)
  }
}