makeDepTraits <- function(nobs, ntraits, nInnerDim, type = 'lin', deg = c(0.3,0.5,0.7,0.9), fclass = c(1:7)) {
  
  #nobs<-100
  #ntraits<-10
  #nInnerDim<-4
  #type<-'lin'
  #deg<-0.9
  #fclass<-c(1:7)
  #pUseInner<-0.5
  
  # First make inner variables
  tr_inner <- scale(matrix(rnorm(nobs*nInnerDim),ncol=nInnerDim))
  
  ndeg     <- length(deg)
  
  #Create empty trait matrices
  traits <-list()
  for (i in 1:ndeg) {
    traits[[i]] <- array(NA,c(nobs,ntraits))
  }
  
  if (type=="lin") {
    
    for (i in 1:ntraits) {
      # Determine weights of the inner dimensions choose exp distribution
      w              <- rexp(nInnerDim)
      #w[w<(-log(1-pUseInner))] <- 0
      # Create traits without noise
      tr         <- tr_inner%*%w
      # Add noise
      for (j in 1:ndeg) {
      traits[[j]][,i] <- scale(deg[j] * scale(tr) + (1 - deg[j]) * rnorm(nobs))
    }
    }
    
  } else {
    # Initialize a list of nonlinear functions
    flist <- list()
    
    flist[[1]] <- function(x) {abs(x)}         # Knee
    flist[[2]] <- function(x) {x*x}            # Parabolic
    flist[[3]] <- function(x) {(x*x)/(1+x*x)}  # sigmoid 1
    flist[[4]] <- function(x) {x/sqrt(1+2*x*x)}# sigmoid 2
    flist[[5]] <- function(x) {spread<-diff(range(x))*0.1;sin(spread*pi*x)} # periodic
    flist[[6]] <- function(x) {p2uniform(x)} # 
    flist[[7]] <- function(x) {exp(-.5*x^2)*cos((2*pi*x)/5)}
    for (i in 1:ntraits) {
      # Determine weights of the inner dimensions choose exp distribution
      w              <- rexp(nInnerDim)
      # Determine nonlinear function to use for each Inner Dim
      fc             <- sample(fclass,size=nInnerDim,replace=TRUE)
      tr             <- rep(0,nobs)
      for (j in 1:nInnerDim) {
        tr <- tr + w[j] * scale(flist[[fc[j]]](tr_inner[,j]))
      }
      for (j in 1:ndeg) {
        traits[[j]][,i]  <- scale(deg[j] * scale(tr) + (1 - deg[j]) * rnorm(nobs))
      }
    }
  }
  list(Traits=traits,Inner=tr_inner)
  
  
}