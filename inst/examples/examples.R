#--------------------------------------------------------------------------------------------------#
# Examples
#--------------------------------------------------------------------------------------------------#


library(Rprospect)
#################### Some examples for testing #####################

## Run PROSPECT-4
LRT <- prospect4(2,65,0.004,0.002)
plot(LRT[,1],LRT[,2],type="l",xlab="Wavelength (nm)",
     ylab="Reflectance/Transmittance (%)")

## Run PROSPECT-5
LRT <- prospect5(2,65,30,0.006,0.003)
plot(LRT[,1],LRT[,2],type="l",xlab="Wavelength (nm)",
     ylab="Reflectance/Transmittance (%)")

## Run PROSPECT-5B
LRT <- prospect5B(2,65,30,0.05,0.006,0.003)
plot(LRT[,1],LRT[,2],type="l",xlab="Wavelength (nm)",
     ylab="Reflectance/Transmittance (%)")


## Example PROSPECT Inversion

## Load test data
data(poplar)  # test poplar dataset
data(clover)  # test clover dataset
data(beech)   # test beech dataset

spectra = clover  # chosen spectra to invert
plot(spectra[,1],spectra[,2],type="l",lwd=2.5,ylim=c(0,1))
lines(spectra[,1],1-spectra[,3],lwd=2.5,col="dark grey")
box(lwd=2.2)

## Test PROSPECT inversion
waves <- spectra[,1]
refl <- spectra[,2]
tran <- spectra[,3]

## PROSPECT-4
inv = invprospect(refl,tran,model=4,method="DEoptim",strategy=2,threshold=0.01,cpus=4)
plot.prospect.inv(inv,outdir='/Users/serbin/Data/Dropbox/FERST_Code/Test_Rprospect/',file='test_prospect.inv4')

## PROSPECT-5
inv = invprospect(refl,tran,model=5,"DEoptim",strategy=2,threshold=0.01,cpus=4)
plot.prospect.inv(inv,outdir='/Users/serbin/Data/Dropbox/FERST_Code/Test_Rprospect/',file='test_prospect.inv5')


