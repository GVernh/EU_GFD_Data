getTraitMatrix<-function(location, # location index
                         nobs=200, # Number of observations
                         perspecies=FALSE # Is the number of obs interpreted as per species or total?
) {
  
  cn    <- colnames(Spp_Matrix[,2:dim(Spp_Matrix)[2]])[Spp_Matrix[location,2:dim(Spp_Matrix)[2]]>0]
  nspec <- length(cn)
  
  m     <- matrix(,ncol=ntraits,nrow=0)
  i     <- 1
  if (perspecies == TRUE) {
    for (i in 1:length(cn)) {
      inds  <- which(traits$Spp==cn[i])
      ichos<- sample(x=inds, size=nobs, replace=TRUE)
      m    <- rbind(m,traits[ichos,4:dim(traits)[2]])
    }
  } else {
    # Calculate how many observations are drawn per species
    nobsperspec <- rep(floor(nobs / nspec),nspec)
    nmore       <- nobs %% nspec
    if (nmore > 0) {
      imore       <- sample(nspec, size=nmore,replace=FALSE)
      nobsperspec[imore] <- nobsperspec[imore] + 1
    }
    for (i in 1:length(cn)) { 
      inds  <- which(traits$Spp==cn[i])
      if (length(inds)>0) {
        ichos<- sample(x=inds, size=nobsperspec[i], replace=TRUE)
        m    <- rbind(m,traits[ichos,4:dim(traits)[2]])
      } else {
        warning(paste0("In location ",location," Species ", cn[i]," does not exist in trait table! Species omitted"))
      }
    }
  }
  return(m)
}

# Function that calculates a list of FD indices in a given location, nrep determines number of bootstrapping rounds
getIndices <- function(location, FD_estimators, nrep=10) {
  results        <- matrix(0,nrow=1,ncol=length(FD_estimators)+1)
  n              <- matrix(0,nrow=1,ncol=length(FD_estimators)+1)
  results[1,1]   <- location
  n[1]           <- 1
  for (irep in 1:nrep) {
    m              <- getTraitMatrix(location,200,FALSE)
    for (i in 1:length(FD_estimators)) {
      expr <- parse(text=paste0("results[1,i+1]<-results[1,i+1] + ",FD_estimators[i],"(m); n[i+1]<-n[i+1]+1")) 
      try(eval(expr))
    }
  }
  results[,-1] <- results[,-1]/n[-1]
  print(location)
  return(results)
}