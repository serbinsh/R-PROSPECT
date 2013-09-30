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
##' @param method algorithm for finding the minimum of cost function between observed and modeled spectra 
##' (i.e. optimize the model parameters). Current options: DEoptim
##' @param strategy DEoptim strategy (see DEoptim)
##' @param threshold minimum threshold of the difference between observed and modeled spectra during 
##' optimization. Default = 0.0001 
##' @param cpus the number of cpus to use in parallel inversion of PROSPECT (optional)
##' @param type the type of cluster to run the parallel inversion. Options: 'SOCK','MPI'.  
##' Default: 'SOCK'.  Also see ?snowfall
##' @return output optimum set of leaf parameters (N,Cab,Car,Cbrown,Cw,Cm), rmse of the inversion, and 
##' associated modeled reflectance and transmittance
##'
##' @export
##'
##' @author Shawn P. Serbin
invprospect <- function(refl,tran,model,method,strategy,threshold,cpus,type){
  
  if (cpus>1){
    ### Determine if it is possible to run optimization in parallel
    if(! require(snowfall) | ! require(doSNOW)) {
      warning("Required packages for parallel optimization are not availible")
      warning("Please install snow/snowfall and doSNOW to run PROSPECT inversion in parallel")
      warning("Running inversion in serial")
      parallel <- FALSE
    } else {
      parallel <- TRUE
    }
  } # End CPU check

  ### Define optim threshold if not already defined
  if (method=="DEoptim" & missing(strategy)) strategy <- 2
  if (missing(threshold)) threshold <- 0.015
  if (missing(cpus)) cpus <- 2
  if (missing(type)) type <- "SOCK"
  
  ### Set ranges for leaf parameters during optimization
  if (model==4){
    parm_min <- c(0.2,2,0.0001,0.0001)
    parm_max <- c(5,200,1,1)    
  } else if (model==5){
    parm_min <- c(0.2,2,1,0.0001,0.0001)
    parm_max <- c(5,200,200,1,1)
  } else if (model=="5B"){
    stop("not implemented yet")
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
    sfInit(parallel=TRUE, cpus=cpus, type=type)
    cl <- sfGetCluster()
    clusterExport(cl,list("refl","tran"))
    clusterEvalQ(cl,library(Rprospect,DEoptim))  # Have to define which libs to copy to nodes
    registerDoSNOW(cl)
    DEctrl <- list(VTR=threshold,NP=40,F=0.7,CR=0.9,trace=5,itermax=5000, reltol=0.001,
                   steptol=50,strategy=strategy,parallelType=2)
    print("------------------------------------------------------------------------------")
    print(" Optimization iterations:")
    t1 <- Sys.time()
    #inv <- DEoptim(fn, lower=parm_min, upper=parm_max, DEctrl)
    inv <- do.call(DEoptim,args=list(fn,lower=parm_min, upper=parm_max, DEctrl))
    t2 <- Sys.time()
    ellapsed <- t2-t1
    print(" ")
    print("---- Processing time:")
    print(ellapsed)
    sfStop() # close open cluster

  } else {
    DEctrl <- list(VTR=threshold,NP=40,F=0.8,CR=0.9,trace=5,itermax=5000, reltol=0.001,
                   steptol=50,strategy=strategy)
    t1 <- Sys.time()
    #inv <- DEoptim(fn, lower=parm_min, upper=parm_max, DEctrl)
    inv <- do.call(DEoptim,args=list(fn,lower=parm_min, upper=parm_max, DEctrl))
    t2 <- Sys.time()
    ellapsed <- t2-t1
    print(" ")
    print("---- Processing time:")
    print(ellapsed)
  } # End parallel if/else
  
  
  ### Provide inversion statistics
  print(" ")
  print("------------------------------------------------------------------------------")
  print(" Optimum parameters:")
  print(paste("N: ",round(inv$optim$bestmem[1],3)," Cab: ",round(inv$optim$bestmem[2],3),
              " Cw: ",round(inv$optim$bestmem[3],3)," Cm: ",round(inv$optim$bestmem[4],3),sep="")  )
  rmse <- inv$optim$bestval
  print(" Inversion info:")
  print(paste("RMSE: ", round(rmse,3)," Iterations: ",inv$optim$iter," Function evaluations: ",
              inv$optim$nfeval, sep=""))
  print(" ")
  
  ### Output 
  mod.spec <- prospect4(inv$optim$bestmem[1],inv$optim$bestmem[2],inv$optim$bestmem[3],
                        inv$optim$bestmem[4])
  parms <- data.frame(N=inv$optim$bestmem[1], Cab=inv$optim$bestmem[2],Cw=inv$optim$bestmem[3],
                      Cm=inv$optim$bestmem[4],Inv.RMSE=rmse,row.names="PROSPECT.Parameters")
  output <- list(Parameters=parms,PROSPECT.Spectra=mod.spec,DEoptim.obj=inv)
    
} # End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################