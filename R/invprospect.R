#--------------------------------------------------------------------------------------------------#
##'
##' @name invprospect
##' @title Invert the PROSPECT family (PROSPECT-4,PROSPECT-5,PROSPECT-5B) of leaf radiative transfer models
##'
##' @details {
##' Function to invert the PROSPECT family (PROSPECT-4,PROSPECT-5,PROSPECT-5B) of leaf radiative 
##' transfer models (RTMs) on measured reflectance and transmittance data.  This function is used for single spectra 
##' inversions.
##' }
##'
##' @param refl observed leaf reflectance data
##' @param tran observed leaf transmittance data
##' @param model version of PROSPECT model to invert.  Options: 4,5,5B.  Default = 4
##' @param data.type Invert a single observation ("single") or multiple observations in a dataframe ("multiple")
##' @param method algorithm for finding the minimum of cost function between observed and modeled spectra 
##' (i.e. optimize the model parameters). Current options: DEoptim
##' @param param.min Minimum parameter values for optimiation search.  E.g. For PROSPECT-4 c(0.2,2,0.00001,0.00001)
##' @param param.max Maximum parameter values for optimiation search. E.g. For PROSPECT-4 c(5,200,0.1,0.1)
##' @param method.control Optional list for controlling optimization. Relevant options: VTR, NP,
##' F, CR, trace, itermax, reltol, steptol, strategy. Defaults: VTR=0.0001, NP=10*(parameter length),
##' F=0.7, CR=0.9, trace=5, itermax=2000, reltol=0.001, steptol=50, strategy=2. For more info
##' see ?DEopitm.control 
##' @param cpus the number of cpus to use in parallel inversion of PROSPECT (optional)
##' @param cluster.type The type of cluster to run the parallel inversion. Options: 'SOCK','MPI'.  
##' Default: 'SOCK'.  Also see ?snowfall
##' @return output optimum set of leaf parameters (N,Cab,Car,Cbrown,Cw,Cm), rmse of the inversion, and 
##' associated modeled reflectance and transmittance
##'
##' @export
##'
##' @author Shawn P. Serbin
invprospect <- function(refl,tran,model,data.type=c("single","multiple"),method="DEoptim",param.min=NULL,param.max=NULL,
                        method.control=list(VTR=0.0001,NP=NULL,F=0.7,CR=0.9,trace=5,itermax=2000,reltol=0.001,steptol=50,strategy=2),
                        cpus,cluster.type){
  sep <- .Platform$file.sep
  
  # check inputs
  model <- match.arg(as.character(model),c("4","5","5B","5b"),several.ok=FALSE)
  data.type <- match.arg(tolower(data.type),c("single","multiple"),several.ok=FALSE)
  method <- match.arg(tolower(method),c("deoptim"),several.ok=FALSE)
  cluster.type <- match.arg(toupper(cluster.type), c("SOCK","MPI"),several.ok=FALSE)

  
  if (cpus>1){
    ### Determine if it is possible to run optimization in parallel
    if(! require(snow) | ! require(snowfall) | ! require(doSNOW)) {
      warning("Required packages for parallel optimization are not availible;
Please install snow/snowfall and doSNOW to run PROSPECT inversion in parallel;
Running inversion in serial")
      parallel <- FALSE
    } else {
      parallel <- TRUE
      print("Running inversion in parallel")
      paste(" ")
      paste(" ")
    }
  } # End CPU check

  ### Define optim threshold if not already defined
  #if (method=="DEoptim" & missing(strategy)) strategy <- 2
  #if (missing(threshold)) threshold <- 0.015
  
  if (missing(cpus)) cpus <- 2
  if (missing(cluster.type)) cluster.type <- "SOCK"
  
  ### Set ranges for leaf parameters during optimization
  if (model=="4"){
    param.min.default <- c(0.2,2,0.00001,0.00001)
    param.max.default <- c(5,200,0.5,0.5)
  }
  if (model=="5"){
    param.min.default <- c(0.2,2,1,0.00001,0.00001)
    param.max.default <- c(5,200,200,0.5,0.5)
  }
  
  # Set defualt parameter ranges if not set in function call
  if (is.null(param.min)) {
    param.min <- param.min.default
  }
  if (is.null(param.max)) {
    param.max <- param.max.default
  }
  
  # set the optimum number of NPs if not set in function call
  if (is.null(method.control$NP)) {
    NP <- 10*length(param.min)
  } else {
    NP <- method.control$NP
  }
  
  print(" ")
  print(" ")
  print("-------------------------------------------------------")
  print(paste(" Inverting PROSPECT-",model," model",sep=""))
  print("-------------------------------------------------------")
  print(" ")
  print(" ")
  
  # Set the function to be evaluated.  PROSPECT-4/5/5B
  fn <- paste("merit.p",model,sep="")

  ### Get optimum solution
  if (parallel) {
    sfInit(parallel=TRUE, cpus=cpus, type=cluster.type)
    cl <- sfGetCluster()
    clusterExport(cl,list("refl","tran"))
    clusterEvalQ(cl,library(Rprospect,DEoptim))  # Have to define which libs to copy to nodes
    registerDoSNOW(cl)
    DEctrl <- list(VTR=method.control$VTR,NP=NP,F=method.control$F,CR=method.control$CR,trace=method.control$trace,
                   itermax=method.control$itermax, reltol=method.control$reltol,steptol=method.control$steptol,
                   strategy=method.control$strategy,parallelType=2)
    print("------------------------------------------------------------------------------")
    print(" Optimization iterations:")
    t1 <- Sys.time()
    inv <- do.call(DEoptim,args=list(fn,lower=param.min, upper=param.max, DEctrl))
    t2 <- Sys.time()
    ellapsed <- t2-t1
    print(" ")
    print("---- Processing time:")
    print(ellapsed)
    sfStop() # close open cluster

  } else {
    DEctrl <- list(VTR=method.control$VTR,NP=NP,F=method.control$F,CR=method.control$CR,trace=method.control$trace,
                   itermax=method.control$itermax, reltol=method.control$reltol,steptol=method.control$steptol,
                   strategy=method.control$strategy)
    t1 <- Sys.time()
    inv <- do.call(DEoptim,args=list(fn,lower=param.min, upper=param.max, DEctrl))
    t2 <- Sys.time()
    ellapsed <- t2-t1
    print(" ")
    print("---- Processing time:")
    print(ellapsed)
  } # End parallel if/else
  
  
  ### Provide inversion statistics
  print(" ")
  rmse <- inv$optim$bestval
  print(" Inversion info:")
  print(paste("RMSE: ", round(rmse,3)," Iterations: ",inv$optim$iter," Function evaluations: ",
              inv$optim$nfeval, sep=""))
  print(" ")
  print("--------------------------------------------------------------------------------------")
  print(" Optimum parameters:")
  if (model==4){
    print(paste("N: ",round(inv$optim$bestmem[1],3)," Cab: ",round(inv$optim$bestmem[2],3),
                " Cw: ",round(inv$optim$bestmem[3],3)," Cm: ",round(inv$optim$bestmem[4],3),sep=""))
    ### Output 
    mod.spec <- prospect4(inv$optim$bestmem[1],inv$optim$bestmem[2],inv$optim$bestmem[3],
                          inv$optim$bestmem[4])
    parms <- data.frame(N=inv$optim$bestmem[1], Cab=inv$optim$bestmem[2],Cw=inv$optim$bestmem[3],
                        Cm=inv$optim$bestmem[4],Inv.RMSE=rmse,row.names="PROSPECT.Parameters")
  } else if (model==5){
    print("not yet implemented")
  }
  print(" ")
  output <- list(Parameters=parms,PROSPECT.Spectra=mod.spec,DEoptim.obj=inv)
  
  invisible(output)
} # End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################