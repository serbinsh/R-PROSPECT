#
# Install package depends
#
# on unix/linx need to install gsl-bin and libgsl0-dev prior to R gsl package
#
list.of.packages <- c('gsl','DEoptim','testthat','foreach','snowfall','doSNOW') 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos="http://cran.us.r-project.org")
}

