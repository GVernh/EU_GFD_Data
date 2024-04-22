###------------------------
entMSTpalencia  		<- function(X, alpha = 1) {
 
 n = dim(X)[1]
 d = dim(X)[2]
 if (length(dim(X)) == 2) {
    
	require("igraph")
	# make gamma
	nsamp = 10000
	p     = d*(1-alpha)
	L_p_gamma = array(NA, niter)
	XX 	 <- array(runif(nsamp * p), c(nsamp, p))
	dm   <- as.matrix(dist(X, upper = TRUE))
	g    <- graph.adjacency(dm, mode = "max", weighted = TRUE)
	gmst <- minimum.spanning.tree(g)
	we   <- get.edge.attribute(gmst, "weight")
	L_p_gamma  <- sum(we^p)
	gamma <- mean(L_p_gamma)/(n^(1-p/d))
	
    dm   <- as.matrix(dist(X, upper = TRUE))
	g    <- graph.adjacency(dm, mode = "max", weighted = TRUE)
    gmst <- minimum.spanning.tree(g)
    we   <- get.edge.attribute(gmst, "weight")
	L_n  <- sum(we^p)
	H	 <- 1/(1-alpha)*log(L_n/(gamma*n^(1-p/d)))
    return(H)
  } else {
    return(NA)
  }
}