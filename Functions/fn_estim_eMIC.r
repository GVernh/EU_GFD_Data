
###------------------------
MIC_e    	<- function(X) {
  # effective MIC analogue to the effective dependence
  if (length(dim(X)) >= 2) {
    n = dim(X)[1] 
    p = dim(X)[2]
    mic_mat <- array(data = NA, c(p, p))
    mas_mat <- array(data = NA, c(p, p))
    mev_mat <- array(data = NA, c(p, p))
    mcn_mat <- array(data = NA, c(p, p))
    nonlin_mat <- array(data = NA, c(p, p))
    # get everythig we can out of the MIC functionality
	
    for (i in 1:p) {
      for (j in i:p) {
        return_mic     <- try(MIC(X[, i], X[, j]))
		if (class(return_mic) == "try-error") {return_mic <- array(data = NA, c(5))}		
        mic_mat[i, j]  <- return_mic[1]  
        mas_mat[i, j]  <- return_mic[2]
        mev_mat[i, j]  <- return_mic[3]   
        mcn_mat[i, j]  <- return_mic[4]  
        nonlin_mat[i, j] <- return_mic[5]
      }
    }
    # and make full matrices
    mic_mat[lower.tri(mic_mat)] <- rotate180.matrix(t(rotate180.matrix(mic_mat)))[lower.tri(mic_mat)]
    mas_mat[lower.tri(mic_mat)] <- rotate180.matrix(t(rotate180.matrix(mas_mat)))[lower.tri(mic_mat)]
    mev_mat[lower.tri(mic_mat)] <- rotate180.matrix(t(rotate180.matrix(mev_mat)))[lower.tri(mic_mat)]
    mcn_mat[lower.tri(mic_mat)] <- rotate180.matrix(t(rotate180.matrix(mcn_mat)))[lower.tri(mic_mat)]
    nonlin_mat[lower.tri(mic_mat)] <- rotate180.matrix(t(rotate180.matrix(nonlin_mat)))[lower.tri(mic_mat)]
    # and summarize in different ways
    MIC_e <- 1-abs(det(mic_mat))^(1/(p-1))
    MIC_avg <- mean(mic_mat[upper.tri(mic_mat)])
    NLIN_avg <- mean(nonlin_mat[upper.tri(nonlin_mat)])
    return(c(MIC_e, MIC_avg, NLIN_avg))
  } else {
    return(NA) 
  }
}

MIC    	<- function(x, y) {
  # maximum information content (MIC) as described by Reshaf et al. 2012
  # put everything into a dataframe
  xy     <- data.frame(x, y)
  result <- .jevalArray(rMINE(t(as.matrix(xy)), "matrix", 0))
  r      <- result[[1]]
  return_vec    <- array(data = NA, c(1, 5))
  return_vec[1] <- r$getMIC()
  return_vec[2] <- r$getMIC() - r$getPearson()^2
  return_vec[3] <- r$getMAS()
  return_vec[4] <- r$getMEV()
  return_vec[5] <- r$getMCN()
  colnames(return_vec) <- c('MIC', 'Nonlin', 'MAS', 'MEV', 'MCN')
  return(return_vec)
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

# THE CODE BELOW IS FROM: http://www.exploredata.net/ 
# As noted elsehwere, the original code is difficult to handle 
# Miguel Mahecha and Fabian Gans adapted it to extract the MIC metrics to the workspace

library("rJava")
.jinit(classpath="MINE.jar")

###########################################
# MINE - runs MINE on a comma-separated
#  values (CSV) file. All parameters are as
#  in the Java version. The parameters
#  var1.id and var2.id are for use when
#  the analysis style specified requires
#  more information. For instance, since
#  style="master.variable" requires an
#  additional variable id (the id of the
#  master variable, you will need to
#  specify var1.id in this case.
#
#  As in the Java version, if you do not
#  specify a value for style, and you
#  specify a value for var1.id and not
#  var2.id, then style="master.variable"
#  will be assumed. If you do not specify a
#  value for style, and you specify values
#  for both var1.id and var2.id, then
#  style="one.pair" will be assumed.
#
# EXAMPLES:
#  MINE("Spellman.csv","all.pairs",0)
#  MINE("Spellman.csv",0)
#   will both run MINE on "Spellman.csv"
#   and have it analyze each variable only
#   against the 0-th variable.
#
#  MINE("Spellman.csv","one.pair",0,5)
#  MINE("Spellman.csv",0,5)
#   will both run MINE on "Spellman.csv"
#   and have it analyze only the 0-th
#   variable against the 5-th variable.
###########################################
MINE <- function (
  input.filename,
  style=c("master.variable", "all.pairs", "adjacent.pairs", "pairs.between", "one.pair"),
  var1.id=NA,
  var2.id=NA,
  required.common.vals.fraction=0,
  max.num.boxes.exponent=0.6,
  notify.wait=100,
  num.clumps.factor=15,
  debug.level=0,
  gc.wait=Inf,
  job.id
) {
  
  printHeader()
  
  params <- getParams(input.filename, style, var1.id, var2.id, required.common.vals.fraction, max.num.boxes.exponent, notify.wait, num.clumps.factor, debug.level, gc.wait, job.id)
  
  # run the analysis
  cat("reading in dataset...\n")
  flush.console()
  
  dataset <- .jnew("data/Dataset",
                   params$inputfile,
                   params$analysisParams$mineParams$debug)
  
  cat("done.\n")
  flush.console()
  
  doAnalysis(dataset, params)
}

###########################################
# rMINE - runs MINE on an R matrix.
#  all parameters are as in MINE, except
#  that the name of the results file will
#  begin with output.prefix rather than
#  the name of the input file (since there
#  is no input file).
#
#  MINE assumes that each row of the
#  supplied matrix is a variable, and each
#  column is a record.
#
# EXAMPLE:
#  rMINE(matrix(1:10,2),"matrix",0)
#   will run MINE on matrix(1:10,2),
#   assuming that each of the two rows
#   in the matrix is a variable.
###########################################
rMINE <- function (
  data,
  output.prefix,
  style=c("master.variable", "all.pairs", "adjacent.pairs", "pairs.between", "one.pair"),
  var1.id=NA,
  var2.id=NA,
  required.common.vals.fraction=0,
  max.num.boxes.exponent=0.6,
  notify.wait=100,
  num.clumps.factor=15,
  debug.level=0,
  gc.wait=Inf,
  job.id
) {
  printHeader()
  
  if(missing(output.prefix))
    stop("you must specify output.prefix so that I'll know what to name the output file!")
  
  params <- getParams(output.prefix, style, var1.id, var2.id, required.common.vals.fraction, max.num.boxes.exponent, notify.wait, num.clumps.factor, debug.level, gc.wait, job.id)
  
  # run the analysis
  cat("reading in dataset...\n")
  flush.console()
  
  data <- .jarray(data, dispatch=TRUE)
  
  dataset <- .jnew("data/Dataset", data, params$analysisParams$mineParams$debug)
  cat("done.\n")
  flush.console()
  
  r<-doAnalysis(dataset, params)
  return(r)
}

printHeader <- function () {
  # print header
  cat(J("main/Analyze")$header())
  cat("\n\n")
  flush.console()
}

getParams <- function(
  input.filename,
  style=c("master.variable", "all.pairs", "adjacent.pairs", "pairs.between", "one.pair"),
  var1.id=NA,
  var2.id=NA,
  required.common.vals.fraction=0,
  max.num.boxes.exponent=0.6,
  notify.wait=100,
  num.clumps.factor=15,
  debug.level=0,
  gc.wait=Inf,
  job.id
) {
  if (gc.wait==Inf) gc.wait <- J("java.lang.Integer")$MAX_VALUE
  else gc.wait <- as.integer(gc.wait)
  
  # create parameters object	
  if(missing(job.id)) {
    args <- .jarray(c(
      input.filename,
      style,
      var1.id,
      var2.id,
      paste("cv=", required.common.vals.fraction, sep = ""),
      paste("exp=", max.num.boxes.exponent, sep = ""),
      paste("notify=", notify.wait, sep = ""),
      paste("c=", num.clumps.factor, sep = ""),
      paste("d=", debug.level, sep = ""),
      paste("gc=", gc.wait, sep = "")
    ))
  } else {
    args <- .jarray(c(
      input.filename,
      style,
      var1.id,
      var2.id,
      paste("cv=", required.common.vals.fraction, sep = ""),
      paste("exp=", max.num.boxes.exponent, sep = ""),
      paste("notify=", notify.wait, sep = ""),
      paste("c=", num.clumps.factor, sep = ""),
      paste("d=", debug.level, sep = ""),
      paste("gc=", gc.wait, sep = ""),
      paste("id=", job.id, sep = "")
    ))
  }
  
  params <- .jnew("main/JobParameters", args)
  flush.console()
  
  #confirm parameters for user
  cat(params$toString())
  cat("\n")
  flush.console()
  
  params
}

doAnalysis <- function (dataset, params) {
  toAnalyze <- .jnew("analysis/VarPairQueue", dataset)
  
  params$analysisStyle$addVarPairsTo(toAnalyze, dataset$numVariables())
  
  a <- .jnew("analysis/Analysis", dataset, toAnalyze)
  
  cat("Analyzing...\n")
  flush.console()
  
  while(! a$varPairQueue()$isEmpty()) {
    # print a status update
    statusUpdate <- paste(a$numResults() + 1, " calculating: ", a$varPairQueue()$peek()$var1$name(), " vs ", a$varPairQueue()$peek()$var2$name(), "...\n", sep="")
    cat(statusUpdate)
    flush.console()
    
    # create a file containing the status update (for use when running on a cluster)
    #write(statusUpdate, file=params$statusFileName())
    
    # analyze some more pairs
    a$analyzePairs(J("analysis.results/BriefResult")$class,
                   params$analysisParams,
                   params$notifyWait)
  }
  
  cat(paste(a$numResults(), " variable pairs analyzed.\n", "Sorting results in descending order...\n", sep=""))
  flush.console()
  
  results <- a$getSortedResults()
  
  
  cat("done. printing results\n")
  flush.console()
  
  #print the results
  #repeat {
  #	if(J("main/Analyze")$printResults(results, params)) {
  #		break
  #	}
  #	else {
  #		n <- readline("writing results to output file failed. Perhaps it is locked in some way. Enter 1 to try again, 0 otherwise: ")
  #		if(n == 0) break
  #	}			
  #}
  
  cat("Analysis finished. See file \"")
  cat(params$resultsFileName())
  cat("\" for output\n")
  results
}
