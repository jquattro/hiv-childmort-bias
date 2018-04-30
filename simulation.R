##### FERTILITY #####

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
#' @param ages (numeric) vector mother's ages
#' @param yr (numeric) vector of years in simulation
#' @param asfr_s (data.frame) estimates of age-specific fertility rates (ASFR) 
#' from the United Nations Population Division's World Fertility Data (2013) 
#' @param tfr_s (data.frame) Interpolated estimates of the total fertility rate (TFR) 
#' from the United Nations Population Division’s World Population Prospects (2012)
#' @return (matrix) annual probability of birth for each age-year
prob.birth.ages.years <- function(ages,years,asfr_s,tfr_s){
  
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


##### MORTALITY #####

#' Annual probability of death for HIV-negative children for 
#' each age-year combination
#' 
#' Uses the time series for 5q0 and 1q0 estimates from the UN Inter-agency 
#' Group for Child Mortality Estimation (IGME) (2012) for one country.
#' 
#' @param ages (numeric) vector of child's ages
#' @param yr (numeric) vector of years in simulation
#' @param u5m_c (data.frame) child mortatily UN Inter-agency Group for Child Mortality Estimation (UN IGME) (2012) for one country
#' @return (matrix) annual probability of death for  HIV-negative children for 
#' each age and year combination
baby.death.nohiv <- function(ages,years,u5m_c){

  # Create empty matrix: years x ages
  baby.death.nohiv <- matrix(NA,length(years),length(ages) )
  row.names(baby.death.nohiv) <- years
  colnames(baby.death.nohiv) <- ages
  
  # Fill the matrix
  for (y in years) { # For each year
    for (a in ages) { # For each age
      # if the year is previous to our earliest year of u5m_c, get the first year available
      if(y<min( u5m_c$year)){
        yr=min(u5m_c$year)
      }else{
        yr=y
      }
      
      # ages less than 0 and grater than 4 have 0 probablity
      if(a<0){baby.death.nohiv[as.character(y),as.character(a)]=0}
      if(a>4){baby.death.nohiv[as.character(y),as.character(a)]=0}
      
      # Age 0: use 1q0 ratio
      if(a==0){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_0[u5m_c$year==yr]}
      
      # Age 1: use 1q1 ratio
      if(a==1){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_1[u5m_c$year==yr]}   
      
      # Age 2: use 1q2 ratio
      if(a==2){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_2[u5m_c$year==yr]}   
      
      # Age 3: use 1q3 ratio
      if(a==3){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_3[u5m_c$year==yr]}   
      
      # Age 4: use 1q4 ratio
      if(a==4){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_4[u5m_c$year==yr]}   
    }
  }
  
  baby.death.nohiv  
}



#' Annual probability of death, HIV negative women
#' 
#' Time series for the probability of dying between ages 15 and 60 (45q15) 
#' were taken from the Institute for Health Metrics and Evaluation (2010) 
#' for selected countries. To obtain age-specific annual probabilities of death, 
#' the 45q15 for an input “model country” and year in the simulation were matched
#' to the UN model life table (UN Population Division 2012) with the closest 45q15.
#' 
#' @param age (numeric) woman's age
#' @param year (numeric) year in simulation
#' @param mort_s (data.frame) mortality series for the model country, must contain columns year and q45_15 
#' from the Institute for Health Metrics and Evaluation.
#' @param adultmort (data.frame) UN model life table (UN Population Division). models mortality for each 45q15
#' @param matmort (data.frame) Maternal mortality
#' @param am_cntry (character) model country for adult mortality
#' @param u5m_c (data.frame) child mortatily UN Inter-agency Group for Child Mortality 
#' Estimation (UN IGME) (2012) for one country.
#' @return (numeric) annual probability of death, HIV negative women.
phivneg.death <- function(age,year,mort_s,adultmort,am_cntry,matmort,u5m_c){
  
  # Get 45q15 from the Institute for Health Metrics and Evaluation for the model country
  #  during the simulation year. If the simulation year is earlier than 1970, get 1970 value.
  if(year<1970){q45_15 = mort_s[,"q45_15"][mort_s[,"year"]==1970]}
  if(year>=1970){q45_15 = mort_s[,"q45_15"][mort_s[,"year"]==year]}
  
  # Get E.0 from the UN model life table with the closest 45q15 (rounds down in case of tie) to that 
  # in Institute for Health Metrics series.
  # E.0 indicates which distribution we should use
  min45 = which.min(abs(adultmort[,"q45_15"]-q45_15))
  e0 = adultmort[min45,"E.0."]
  
  # Compute probability
  
  # For ages 0 to 4, compute as in baby.death.nohiv
  
  if(age<0){return(0)}
  if(year<min( u5m_c$year)){yr=min(u5m_c$year)
  }else{yr=year}
  if(age==0){return(u5m_c$q1_0[u5m_c$year==yr])}
  if(age==1){return(u5m_c$q1_1[u5m_c$year==yr])} 
  if(age==2){return(u5m_c$q1_2[u5m_c$year==yr])} 
  if(age==3){return(u5m_c$q1_3[u5m_c$year==yr])} 
  if(age==4){return(u5m_c$q1_4[u5m_c$year==yr])}
  
  # Compute probability women older than 4
  
  # Ages 5 and 6 interpolate linearly between pr at 4 and 7 
  
  if(age==5){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    q1_4=u5m_c$q1_4[u5m_c$year==yr]
    pd1yr = q1_4-((q1_4-q1_7)/3)
    return(pd1yr)
  }
  if(age==6){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    q1_4=u5m_c$q1_4[u5m_c$year==yr]
    pd1yr = q1_4-(2*((q1_4-q1_7)/3))
    return(pd1yr)
  }
  if(age==7){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    pd1yr = q1_7
    return(pd1yr)
  }
  
  # Ages 8 and 9 interpolate linearly between 7 and 10?
  
  if(age==8){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    pd5yr = adultmort$q.x.n.[adultmort$age==10 & adultmort$E.0.==e0]
    q1_10 = 1-(1-pd5yr)^(1/5)
    pd1yr = q1_7-((q1_7-q1_10)/3)
    return(pd1yr)
  }
  if(age==9){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    pd5yr = adultmort$q.x.n.[adultmort$age==10 & adultmort$E.0.==e0]
    q1_10 = 1-(1-pd5yr)^(1/5)
    pd1yr = q1_7-((q1_7-q1_10)/2)
    return(pd1yr)  }
  
  
  if(age>9 & age<15){
    pd5yr = adultmort$q.x.n.[adultmort$age==10 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    return(pd1yr)
  }
  if(age>14 & age<20){
    pd5yr = adultmort$q.x.n.[adultmort$age==15 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==1]
    return(pd1yr)
  }
  if(age>19 & age<25){
    pd5yr = adultmort$q.x.n.[adultmort$age==20 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==2]
    return(pd1yr)
  }
  if(age>24 & age<30){
    pd5yr = adultmort$q.x.n.[adultmort$age==25 & adultmort$E.0.==e0 ]
    pd1yr = 1-(1-pd5yr)^(1/5)
    pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==3]
    return(pd1yr)
  }
  if(age>29 & age<35){
    pd5yr = adultmort$q.x.n.[adultmort$age==30 & adultmort$E.0.==e0 ]
    pd1yr = 1-(1-pd5yr)^(1/5)
    pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==4]
    return(pd1yr)
  }
  if(age>34 & age<40){
    pd5yr = adultmort$q.x.n.[adultmort$age==35 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==5]
    return(pd1yr)
  }
  if(age>39 & age<45){
    pd5yr = adultmort$q.x.n.[adultmort$age==40 & adultmort$E.0.==e0 ]
    pd1yr = 1-(1-pd5yr)^(1/5)
    pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==6]
    return(pd1yr)
  }
  if(age>44 & age<50){
    pd5yr = adultmort$q.x.n.[adultmort$age==45 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==7]
    return(pd1yr)  
  }
  
  # For ages older than 49, return 0
  
  if(age>49){return(0)}
}

#' Annual probability of death, HIV negative women for each age year combination
#' 
#' Uses phivneg.death to compute annual probability of death for each age-year combination
#' 
#' @param ages (numeric) vector of ages
#' @param years (numeric) vector of years in simulation
#' @param mort_s (data.frame) mortality series for the model country, must contain columns year and q45_15 
#' from the Institute for Health Metrics and Evaluation.
#' @param adultmort (data.frame) UN model life table (UN Population Division). models mortality for each 45q15
#' @param matmort (data.frame) Maternal mortality
#' @param am_cntry (character) model country for adult mortality
#' @param u5m_c (data.frame) child mortatily UN Inter-agency Group for Child Mortality 
#' Estimation (UN IGME) (2012) for one country.
#' @return (matrix) annual probability of death, HIV negative women (years x ages)
phivneg.death.ages.years <- function(ages,years,mort_s,adultmort,am_cntry,matmort,u5m_c){
  
  prob.death.all <- matrix(NA,length(years),length(ages) )
  row.names(prob.death.all) <- years
  colnames(prob.death.all) <- ages
  #head(prob.death.all)
  for (y in years) {
    for (a in ages) {
      res <- phivneg.death(a,y,mort_s,adultmort,am_cntry,matmort,u5m_c)
      if (length(res)>0 ){prob.death.all[as.character(y),as.character(a)] <- res}
      else {
        prob.death.all[as.character(y),as.character(a)] <- 0
      }
    }
  }
  
  prob.death.all
  
}