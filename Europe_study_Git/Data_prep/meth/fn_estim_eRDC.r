
###------------------------
eRDC    	<- function(X) {
  # effective RDC analogue to the effective dependence
  if (length(dim(X)) >= 2) {
    n = dim(X)[1] 
    p = dim(X)[2]
    rdc_mat <- array(data = NA, c(p, p))
	  
    for (i in 1:p) {
      for (j in i:p) {
        return_rdc     <- try(rdc(X[, i], X[, j]))
		if (class(return_rdc) == "try-error") {return_rdc <- NA}		
        rdc_mat[i, j]  <- return_rdc[1]
      }
    }
    # and make full matrices
    rdc_mat[lower.tri(rdc_mat)] <- rotate180.matrix(t(rotate180.matrix(rdc_mat)))[lower.tri(rdc_mat)]
    # and summarize in different ways
    RDC_e <- 1-abs(det(abs(rdc_mat)))^(1/(p-1))
    RDC_avg <- mean(rdc_mat[upper.tri(rdc_mat)])
    return(RDC_e)
  } else {
    return(NA) 
  }
}


rdc<-function(x,y,k=5,s=1) {
  x<-cbind(apply(as.matrix(x),2,function(u) ecdf(u) (u)),1)
  y<-cbind(apply(as.matrix(y),2,function(u) ecdf(u) (u)),1)
  wx<-matrix(rnorm(ncol(x)*k,0,s),ncol(x),k)
  wy<-matrix(rnorm(ncol(y)*k,0,s),ncol(y),k)
  max(cancor(cbind(cos(x%*%wx),sin(x%*%wx)),cbind(cos(y%*%wy),sin(y%*%wy)))$cor)
}

# supportive functions provided by enrico t. via some R exchange
# Flip matrix (upside-down)
flip.matrix <- function(x) {
  mirror.matrix(rotate180.matrix(x))
}

# Mirror matrix (left-right)
mirror.matrix <- function(x) {
  xx <- as.data.frame(x);
  xx <- rev(xx);
  xx <- as.matrix(xx);
  xx;
}

# Rotate matrix 90 clockworks
rotate90.matrix <- function(x) {
  t(mirror.matrix(x))
}

# Rotate matrix 180 clockworks
rotate180.matrix <- function(x) {
  xx <- rev(x);
  dim(xx) <- dim(x);
  xx;
}

#rotate a matrix 90 deg clockwise
rot90 <- function(a){
  n <- dim(a)[1]
  #stopifnot(n==dim(a)[2])
  t(a[n:1,])
}


