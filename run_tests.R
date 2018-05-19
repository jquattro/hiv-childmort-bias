rm(list = ls())
if (!require(magrittr)){
  install.packages("magrittr",dependencies = TRUE,repos='http://cran.us.r-project.org')  
}

require(magrittr)


if (!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')  
}

require(tidyverse)

if(!require(RUnit)){
  install.packages("RUnit")
}

require(RUnit)

if (!require(yaml)){
  install.packages("yaml",dependencies = TRUE,repos='http://cran.us.r-project.org')  
}

require(yaml)

base.path <- file.path(getwd(),".")

source(file.path(base.path,"simulation_functions.R"))

# 
testsuite_simulation <- defineTestSuite("Simulation", dirs=c("./unit.test"), testFileRegexp = "test_simulation", testFuncRegexp = "^test_")

test.result_simulation <- runTestSuite(testsuite_simulation)

 
printTextProtocol(test.result_simulation)

rm(list = ls())
