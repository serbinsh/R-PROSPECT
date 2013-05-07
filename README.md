R-PROSPECT
==========

R functions for running PROSPECT family of leaf radiative transfer models



Some Example Code For Using the Library:

# Run PROSPECT-4
LRT <- prospect4(2,65,0.004,0.002)
plot(LRT[,1],LRT[,2],type="l",xlab="Wavelength (nm)",
     ylab="Reflectance/Transmittance (%)",main="PROSPECT - 4")

# Run PROSPECT-5
LRT <- prospect5(2,65,30,0.006,0.003)
plot(LRT[,1],LRT[,2],type="l",xlab="Wavelength (nm)",
     ylab="Reflectance/Transmittance (%)",main="PROSPECT - 5")

# Run PROSPECT-5B
LRT <- prospect5B(2,65,30,0.05,0.006,0.003)
plot(LRT[,1],LRT[,2],type="l",xlab="Wavelength (nm)",
     ylab="Reflectance/Transmittance (%)",main="PROSPECT - 5B")
     

# Invert PROSPECT-4.  Using parallel computing
inv = invprospect(refl,tran,model=4,method="DEoptim",strategy=2,threshold=0.01,cpus=4)
plot.prospect.inv(inv,outdir=output.dir,file='test_prospect.inv4')
