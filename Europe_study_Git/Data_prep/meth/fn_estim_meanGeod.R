###------------------------
meanGeod  		<- function(X, k = 3) {
  # Mean geodesic distance
  if (length(dim(X)) == 2) {
    n = dim(X)[1]
    p = dim(X)[2]
    library("igraph")
    dm  <- as.matrix(dist(X,upper=TRUE))
    dm2 <- apply(X = dm, MARGIN = 2, FUN = function(x) {x[order(x)[(k+2):length(x)]]=0;x})
    g   <- graph.adjacency(dm2, mode = "max", weighted = TRUE)
    # Ensuee that subgraphs are connected
    while (!is.connected(g)) {
      clu <-clusters(g)
      mf  <- matrix(rep(FALSE, n*n), ncol = n)
      for (i in 1:clu$no) {
        mf <- (mf | outer(clu$membership==i,clu$membership==i,FUN="&"))
      }
      m <- dm
      m[mf] <- 0
      m[m == 0] <- 10*max(m)
      ind <- arrayInd(which.min(m), .dim=c(n,n))
      g[ind[1], ind[2]] <- dm[ind[1], ind[2]]
      dm2[ind[1], ind[2]] <- dm[ind[1], ind[2]]
    }
    sp <- shortest.paths(g)
    return(sum(sp)/(n^2-n))
  } else {
    return(NA)
  }
}