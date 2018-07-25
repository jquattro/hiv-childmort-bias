# When we provide a vector of ages to baby.death.hiv, we should get a vector of probabilities of death, one for each age.
# HIV children mortality by age: "0"=0.376,  "1"=0.2019,	"2"=0.1184,	"3"=0.07061,	"4"=0.0588, "5"=0.0588,"6"=0.0588. 0 for children above age 6
test_baby.death.hiv_01 <- function(){
  
  # Define input
  
  ages <- c("2","3","5","0","10")
  
  # Define what we expect to get with the input
  
  target <-  c(0.1184,0.07061,0.0588,0.376,0)  
  
  # Get the result from the function
  
  test <- baby.death.hiv(ages)
  
  # Check that they are the same (function from RUnit)
  
  checkEquals(target,test)
}


#' When we provide a year and a vector of hiv dates to phiv.death, we should get
#' we should get the probability of death for HIV positive adults not in ART.
test_phiv.death_01 <- function(){
  
  # Define inputs
  
  year <- 1993
  
  hiv_dates <- 1993-c(0:18) # people infected 18 year age and later.
  
  # Define target. One result for each hiv_date
  
  target <- c(0.01,0.02020202,0.041237113,0.053763441,0.079545455,0.098765432,0.123287671,0.15625,0.185185185,0.204545455,0.228571429,	0.296296296,0.263157895,0.357142857,0.333333333,0.333333333,0.5,0.5,1)
  
  # Compute function output for each hiv_date
  
  test <- c()
  
  
  for(hiv_date in hiv_dates){
    test <- c(test,phiv.death(hiv_date,year))
    
  }
      
  # Check that we get the same result
  
  checkEquals(target,test)
  
}