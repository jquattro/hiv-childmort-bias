### FERTILITY ###

#' Annual probability of birth as a function of calendar year and mother’s age.
#' HIV negative
#' 
#' The birth probability was set to NULL for women younger than 15 years and older than 49 years.
#' Pr(birth)_current year,age =ASFR_nearest year,age *(TFR_current year/TFR_nearest year)
#' current_year is the current year in the simulation, nearest year is the year nearest to the current
#' year for which ASFR are available, age is age of the mother in current year.
#' 
#' @param age (numeric) mother's age in current year
#' @param yr (numeric) current year in simulation
#' @param asfr_s (data.frame) estimates of age-specific fertility rates (ASFR) 
#' from the United Nations Population Division's World Fertility Data (2013) 
#' @param tfr_s (data.frame) Interpolated estimates of the total fertility rate (TFR) 
#' from the United Nations Population Division’s World Population Prospects (2012)
#' @return (numeric) annual probability of birth.
prob.birth <- function(age,yr,asfr_s,tfr_s){
  
  # If yr is earlier than any of our data, get the first year available.
  
  if(yr<min(tfr_s[,"year"])){yr=min(tfr_s[,"year"])}
  
  # Get total fertility rate for that year
  tfr1 = tfr_s[,"tfr"][tfr_s[,"year"]==yr]
  
  # Get total fertility rate with nearest year (rounds down in case of tie)
  
  row = which.min(abs(asfr_s[,"refyr"]-yr))
  
  tfr2 = asfr_s[row,"tfr"]
  
  # Compute probability of birth according to the mother's age.
  # pr= asfr_nearest_yr * ( tfr1 / tfr2 )
  
  if(age>14 & age<20){ # aged 15-19
    pb=asfr_s[row,"asfr15"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>19 & age<25){ # aged 20-14
    pb=asfr_s[row,"asfr20"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>24 & age<30){ # aged 25-29
    pb=asfr_s[row,"asfr25"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>29 & age<35){ # aged 30-34
    pb=asfr_s[row,"asfr30"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>34 & age<40){ # aged 35-39
    pb=asfr_s[row,"asfr35"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>39 & age<45){ # aged 40-44 
    pb=asfr_s[row,"asfr40"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>44 & age<50){ # aged 45-49
    pb=asfr_s[row,"asfr45"]*(tfr1/tfr2)/1000
    return(pb)
  }
}

#' Annual probability of birth as a function of calendar year and mother’s age.
#' HIV negative
#' 
#' Uses prob.birth to compute annual probability of birth for each years, ages combination
#' The birth probability was set to zero for women younger than 15 years and older than 49 years.
#' 
#' @param ages (numeric) mother's ages
#' @param yr (numeric) years in simulation
#' @param asfr_s (data.frame) estimates of age-specific fertility rates (ASFR) 
#' from the United Nations Population Division's World Fertility Data (2013) 
#' @param tfr_s (data.frame) Interpolated estimates of the total fertility rate (TFR) 
#' from the United Nations Population Division’s World Population Prospects (2012)
#' @return (matrix) annual probability of birth for each age-year
prob.birth.years.ages <- function(ages,years,asfr_s,tfr_s){
  
  # Create an empty matrix
  prob.birth.all <- matrix(NA,length(years),length(ages) )
  row.names(prob.birth.all) <- years
  colnames(prob.birth.all) <- ages
  
  # Fill the matrix
  for (y in years) { # For each year
    for (a in ages) { # For eacj age
      res <- prob.birth(a,y,asfr_s,tfr_s) # Compute annual probability of birth
      
      # set probability to zero when the result is NULL (women younger than 15 years and older than 49 years).
      if (length(res)>0 ){prob.birth.all[as.character(y),as.character(a)] <- res
      } else {
        prob.birth.all[as.character(y),as.character(a)] <- 0
      }
    }
  }
  
  prob.birth.all
  
}

#' Reduction in prob of giving birth due to HIV 
#'
#' Among women aged 15-19 years not on ART, those who were HIV-positive experienced higher ASFRs compared to 
#' HIV-negative women, with the ratio dependent on the percent of 15-19 year old women who were 
#' sexually active; also, among those aged 19, HIV-positive women experienced lower fertility rates 
#' relative to HIV-negative women. We use ratios estimated by Chen and Walker (2010).
#' 
#' Among women over age 19, ART erases half of the fertility decrease caused by HIV/AIDS. 
#' In other words, for women on ART, the ASFR ratios in Table S1 increase by half the difference 
#' from one (one indicating equal ASFRs between HIV-positive and HIV-negative women). 
#' ASFR for 15-19 year olds is not affected by ART.
#' 
#' @param age (numeric) mother's age
#' @param sexactive15 (numeric) percent of females aged 15-19 who are sexually active
#' @param art (integer) either: 0 (not on ART) or 1 (on ART)
#' @return (numeric) reduction in prob of giving birth due to HIV 
fert_hiv <- function(age,sexactive15,art){
  
  if(art==0){ # Not on ART
    
    if(age>14 & age<20){ # aged 15-19
      fert_hiv=2.528-.031*sexactive15
      return(fert_hiv)
    }
    if(age>19 & age<25){ # aged 20-24
      fert_hiv=.765
      return(fert_hiv)
    }
    if(age>24 & age<30){ # aged 25-29
      fert_hiv=.706
      return(fert_hiv)
    }
    if(age>29 & age<35){ # aged 30-34
      fert_hiv=.647
      return(fert_hiv)
    }
    if(age>34 & age<40){ # aged 35-39
      fert_hiv=.588
      return(fert_hiv)
    }
    if(age>39 & age<45){ # aged 40-44
      fert_hiv=.529
      return(fert_hiv)
    }
    if(age>44 & age<50){ # aged 45-49
      fert_hiv=.470
      return(fert_hiv)
    }
  }else{ #On ART
    if(age>14 & age<20){ # aged 15-19
      fert_hiv=2.528-.031*sexactive15
      return(fert_hiv)
    }
    if(age>19 & age<25){ # aged 20-24
      fert_hiv=.8825
      return(fert_hiv)
    }
    if(age>24 & age<30){ # aged 25-29
      fert_hiv=.853
      return(fert_hiv)
    }
    if(age>29 & age<35){ # aged 30-34
      fert_hiv=.824
      return(fert_hiv)
    }
    if(age>34 & age<40){ # aged 35-39
      fert_hiv=.794
      return(fert_hiv)
    }
    if(age>39 & age<45){ # aged 40-44
      fert_hiv=.765
      return(fert_hiv)
    }
    if(age>44 & age<50){ # aged 45-49
      fert_hiv=.735
      return(fert_hiv)
    }
  }
}


#' Reduction in prob of giving birth due to HIV for each art, age combination
#'
#' Uses fert_hiv to compute the reduciton in probability of giving birth due to HIV
#' for each art, ages combination.
#' 
#' @param ages (numeric) vector of mother's ages
#' @param sexactive15 (numeric) percent of females aged 15-19 who are sexually active
#' @param art (integer) vector of values that should be either: 0 (not on ART) or 1 (on ART)
#' @return (matrix) reduction in prob of giving birth due to HIV 
prob.birth.hiv <- function(ages,sexactive15,arts){
  
  # Create an empty matrix
  prob.birth.hiv <- matrix(NA,length(ages),length(arts))
  row.names(prob.birth.hiv) <- ages
  colnames(prob.birth.hiv) <- arts
  
  # Fill the matrix
  for (a in ages) {# For each age
    for (b in arts) {# For each art
      # Compute probability reduction
      res <- fert_hiv(a,sexactive15,b)
      # If the result is not NULL, store the result in the matrix, otherwise, set the value to 0
      if (length(res)>0 ){
        prob.birth.hiv[as.character(a),as.character(b)] <- res
      }else {
        prob.birth.hiv[as.character(a),as.character(b)] <- 0
      }
    }
  }
  prob.birth.hiv
}

  
