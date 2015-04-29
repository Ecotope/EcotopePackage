# install.packages("devtools")
library("devtools")
# devtools::install_github("klutometis/roxygen")
library(roxygen2)


setwd("E:/R/")
# create("EcotopePackage")
# setwd("./EcotopePackage")
# document()
# install("EcotopePackage")

install_github('Ecotope/EcotopePackage')
library(EcotopePackage)

 devtools::document()



? cdx
? name_fix
? var_range_fix

? continuous_uniform_time
? omit_NA_cols
? omit_zero_cols
? omit_NA_or_zero_cols
? rbind.all.columns
? single_pt_avg_impute
? stata_time_to_R_time
