###------------------------
eDep    	<- function(X) {
  # effective dependence D_e as described by Pe{\~n}a & Rodriguez 2003:
  # but adjusted to be computable for "small n, large p" cases
  if (length(dim(X)) == 2) {
    n = dim(X)[1]
    p = dim(X)[2]
    if (n < p) {
      COR	<- unclass(as.matrix(cor.shrink(X)))		
    } else {
      COR	<- cor(X)
    }
    D_e <- 1-abs(det(COR))^(1/(p-1))
    return(D_e)
  } else {
    return(NA)
  }
}