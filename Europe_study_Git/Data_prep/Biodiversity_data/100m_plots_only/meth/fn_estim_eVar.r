###------------------------
eVar  		<- function(X) {
  # effective variance V_e and SD_e as described by Pe{\~n}a & Rodriguez 2003,
  # but adjusted to be computable for "small n, large p" cases
  if (length(dim(X)) == 2) {
    n = dim(X)[1]
    p = dim(X)[2]
    if (n < p) {
      COV	<- unclass(as.matrix(cov.shrink(X)))		
    } else {
      COV	<- cov(X)
    }
    V_e <- det(COV)^(1/p)
    return(V_e)
  } else {
    return(NA)
  }
}