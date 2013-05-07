#
# Install package depends
#
#
list.of.packages <- c('gsl','DEoptim','testthat','snowfall','doSNOW') 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos="http://cran.us.r-project.org")
}

