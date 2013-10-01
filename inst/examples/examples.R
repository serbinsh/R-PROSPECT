####################################################################################################
# R-PROSPECT Examples
#
# Last Updated: 09/30/2013 by SPS
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load required library
library(Rprospect)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Output directory.  Needs to be set
output.dir <- '/Users/serbin/Data/'
#--------------------------------------------------------------------------------------------------#


#################### Some examples for testing #####################

## Run PROSPECT-4
LRT <- prospect4(2,65,0.004,0.002)
plot(LRT[,1],LRT[,2],type="l",xlab="Wavelength (nm)",
     ylab="Reflectance/Transmittance (%)",main="PROSPECT - 4")

## Run PROSPECT-5
LRT <- prospect5(2,65,30,0.006,0.003)
plot(LRT[,1],LRT[,2],type="l",xlab="Wavelength (nm)",
     ylab="Reflectance/Transmittance (%)",main="PROSPECT - 5")

## Run PROSPECT-5B
LRT <- prospect5B(2,65,30,0.05,0.006,0.003)
plot(LRT[,1],LRT[,2],type="l",xlab="Wavelength (nm)",
     ylab="Reflectance/Transmittance (%)",main="PROSPECT - 5B")


## Example PROSPECT Inversion

## Load test data
poplar <- read.csv(file=paste(path.package("Rprospect"),"/extdata/poplar_leaf.csv",sep=""),
                     header=T)   # test poplar dataset
clover <- read.csv(file=paste(path.package("Rprospect"),"/extdata/clover_leaf.csv",sep=""),
                     header=T)   # test clover dataset
beech <- read.csv(file=paste(path.package("Rprospect"),"/extdata/beech_leaf.csv",sep=""),
                     header=T)   # test beech dataset
corn <- read.csv(file=paste(path.package("Rprospect"),"/extdata/corn_leaf.csv",sep=""),
                   header=T)   # test beech dataset
acsa3.bottom <- read.csv(file=paste(path.package("Rprospect"),"/extdata/acsa3_bottom_leaf.csv",sep=""),
                         header=T)   # test beech dataset
acsa3.top <- read.csv(file=paste(path.package("Rprospect"),"/extdata/acsa3_top_leaf.csv",sep=""),
                         header=T)   # test beech dataset


# Choose spec sample to invert
spectra = acsa3.top  # chosen spectra to invert

# Plot R & T Spectra
plot(spectra[,1],spectra[,2],type="l",lwd=2.5,ylim=c(0,1))
lines(spectra[,1],1-spectra[,3],lwd=2.5,col="dark grey")
box(lwd=2.2)

## ---- Test PROSPECT inversion ----
waves <- spectra[,1]
refl <- spectra[,2]
tran <- spectra[,3]

## PROSPECT-4
inv = invprospect(refl,tran,model=4,data.type="single",method="DEoptim",method.control=list(VTR=0.1,NP=50,trace=10,strategy=2),
                  cpus=2,cluster.type='SOCK')
plot.prospect.inv(inv,outdir=output.dir,file='test_prospect.inv4')


## PROSPECT-5
inv = invprospect(refl,tran,model=5,"DEoptim",strategy=2,threshold=0.01,cpus=4)
plot.prospect.inv(inv,outdir=output.dir,file='test_prospect.inv5')






# UPDATED
inv = invprospect(refl,tran,model=4,method="DEoptim",strategy=2,threshold=0.01,cpus=4)
plot.prospect.inv(inv,outdir=output.dir,file='test_prospect.inv4')

#--------------------------------------------------------------------------------------------------#

### end of file