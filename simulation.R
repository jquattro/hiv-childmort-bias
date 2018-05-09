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
#' @param matmort (data.frame) Maternal mortality per country and year
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
  
  # Ages 8 and 9 interpolate linearly between 7 and 10
  
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
    pd1yr = q1_7-(2*(q1_7-q1_10)/3)
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


#' Probability of death HIV positive adults not in ART
#' 
#' The annual probability of death for HIV-positive women who were not on ART 
#' was based on cumulative mortality reported in Walker, Hill, and Zhao (2012)
#'
#' @param hiv_date (integer) year of HIV infection
#' @param year (integer) current year in simulation 
#' @return (numeric) Probability of death HIV positive adults not in ART
phiv.death <- function(hiv_date, year){
  
  # cumulative mortality reported in Walker, Hill, and Zhao (2012)
  
  aidsmort <- c(0.01,0.02020202,0.041237113,0.053763441,0.079545455,0.098765432,0.123287671,0.15625,0.185185185,0.204545455,0.228571429,	0.296296296,0.263157895,0.357142857,0.333333333,0.333333333,0.5,0.5)
  
  
  if(year<hiv_date){
    return("error: year<hiv_date")
    }
  
  duration <- year - hiv_date
  
  #Assume no one survives more than 18 yrs with HIV
  if(duration>17){
    pd <- 1
  }else{
    pd <- aidsmort[duration+1]
  }
  return(pd)
}

# Weibull distribution transition probability
weib.tp <- function(t,scale,shape){
  tp = 1 - exp(-((1/scale)*t^shape - (1/scale)*(t-1)^shape))
  return(tp)
}

#' Probability of death for HIV pos adults on ART
#' 
#' HIV-positive women on ART faced an annual probability of death that 
#' was a function of CD4 count at ART initiation, presence or absence of 
#' symptoms at baseline, and time since initiation. The function was taken 
#' from the “medium” scenario published by Hallett et al. (2008). 
#' Women were assigned to “symptomatic” or “non-symptomatic” with probability 0.5, 
#' based on Braitstein et al. (2006).
#' 
#' @param year (integer) Current simulation year
#' @param cd4 (numeric) CD4 count at ART initiation
#' @param  art_date (integer) year of ART initiation
#' @return (numeric) probability of death for HIV positive adults.
art.surv.vec <- function(year,cd4,art_date){
  
  # Shape of the Weibull distribution
  shape=1.6
  
  # pacients have 50% probability of having high viral load ("symptomatic")
  # ART-LINC ART-CC Lancet 2006 50% patients have high viral load
  
  hvl <- ifelse(runif(length(cd4))<0.5,1,0)
  
  # If a patient doesnot have high viral load, she has a low viral load. ("non-symptomatic")
  lvl <- 1 - hvl
  
  # Time since ART initiation
  t = year-art_date
  
  # Annual probability of death is a function of CD4 count at ART initiation, 
  # presence or ausence of symptoms at baseline and time since initiation
  
  # Probabilities for "symptomatic" 
  
  # CD4 < 50
  
  # Computations are similar for other intervals of CD4
  
  # Proababiliy during the first year after ART
  
  hvl.cd450.y1 <- .109 
  
  # select only cases with the right amount of CD4 and one year since ART initiation 
  
  hvl.cd450.y1.select <- hvl*ifelse(cd4<50 & t==1,1,0) 
  
  # Use Weibull distribution to compute probability after the first year
  
  hvl.cd450.yg1 <- weib.tp(t,13.7,shape)
  
  # select only cases with the right amount of CD4 and more than one year since ART initiation 
  
  hvl.cd450.yg1.select <- hvl*ifelse(cd4<50 & t>1,1,0)
  
  # CD4 in  [50,100)
  
  hvl.cd4100.y1 <- .67
  hvl.cd4100.y1.select <- hvl*ifelse(cd4>=50 & cd4<100 & t==1,1,0)
  hvl.cd4100.yg1 <- weib.tp(t,16,shape)
  hvl.cd4100.yg1.select <- hvl*ifelse(cd4>=50 & cd4<100 & t>1,1,0)
  
  # CD4 in  [100,200)
  
  hvl.cd4200.y1 <- .46
  hvl.cd4200.y1.select <- hvl*ifelse(cd4>=100 & cd4<200 & t==1,1,0)
  hvl.cd4200.yg1 <- weib.tp(t,16.9,shape)
  hvl.cd4200.yg1.select <- hvl*ifelse(cd4>=100 & cd4<200 & t>1,1,0)
  
  # CD4 in  [200,350)
  
  hvl.cd4350.y1 <- .17
  hvl.cd4350.y1.select <- hvl*ifelse(cd4>=200 & cd4<350 & t==1,1,0)
  hvl.cd4350.yg1 <- weib.tp(t,23.3,shape)
  hvl.cd4350.yg1.select <- hvl*ifelse(cd4>=200 & cd4<350 & t>1,1,0)
  
  # CD4 >= 350
  
  hvl.cd4g350.y1 <- .17
  hvl.cd4g350.y1.select <- hvl*ifelse(cd4>=350 & t==1,1,0)
  hvl.cd4g350.yg1 <- weib.tp(t,33.3,shape)
  hvl.cd4g350.yg1.select <- hvl*ifelse(cd4>=350 & t>1,1,0)
  
  # Probabilities for "non-symptomatic" 
  
  # Computations are similar as in "symptomatic"
  
  # CD4 < 50
  
  lvl.cd450.y1 <- .109
  lvl.cd450.y1.select <- lvl*ifelse(cd4<50 & t==1,1,0)
  lvl.cd450.yg1 <- weib.tp(t,24.4,shape)
  lvl.cd450.yg1.select <- lvl*ifelse(cd4<50 & t>1,1,0)
  
  # CD4 in  [50,100)
  
  lvl.cd4100.y1 <- .67
  lvl.cd4100.y1.select <- lvl*ifelse(cd4>=50 & cd4<0 & t==1,1,0)
  lvl.cd4100.yg1 <- weib.tp(t,28.4,shape)
  lvl.cd4100.yg1.select <- lvl*ifelse(cd4>=50 & cd4<100 & t>1,1,0)
  
  # CD4 in  [100,200)
  
  lvl.cd4200.y1 <- .46
  lvl.cd4200.y1.select <- lvl*ifelse(cd4>=100 & cd4<200 & t==1,1,0)
  lvl.cd4200.yg1 <- weib.tp(t,30.1,shape)
  lvl.cd4200.yg1.select <- lvl*ifelse(cd4>=100 & cd4<200 & t>1,1,0)
  
  # CD4 in  [200,350)
  
  lvl.cd4350.y1 <- .17
  lvl.cd4350.y1.select <- lvl*ifelse(cd4>=200 & cd4<350 & t==1,1,0)
  lvl.cd4350.yg1 <- weib.tp(t,41.4,shape)
  lvl.cd4350.yg1.select <- lvl*ifelse(cd4>=200 & cd4<350 & t>1,1,0)
  
  # CD4 >= 350
  
  lvl.cd4g350.y1 <- .17
  lvl.cd4g350.y1.select <- lvl*ifelse(cd4>=350 & t==1,1,0)
  lvl.cd4g350.yg1 <- weib.tp(t,59.1,shape)
  lvl.cd4g350.yg1.select <- lvl*ifelse(cd4>=350 & t>1,1,0)
  
  # Each block above computes a vector of probabilities and a vector that can be used to select which
  # cases fit in a given category (for example CD4 <50, t==1 and lvl). So, to get the probabilities for each 
  # case we multiply the selection vector by the probability vector in each category and then summ the results
  # obtained from all the categories.
  
  return(  hvl.cd450.y1.select*hvl.cd450.y1 + hvl.cd450.yg1.select*hvl.cd450.yg1+
             hvl.cd4100.y1.select*hvl.cd4100.y1 + hvl.cd4100.yg1.select*hvl.cd4100.yg1+
             hvl.cd4200.y1.select*hvl.cd4200.y1 + hvl.cd4200.yg1.select*hvl.cd4200.yg1+
             hvl.cd4350.y1.select*hvl.cd4350.y1 + hvl.cd4350.yg1.select*hvl.cd4350.yg1+
             hvl.cd4g350.y1.select*hvl.cd4g350.y1 + hvl.cd4g350.yg1.select*hvl.cd4g350.yg1+
             lvl.cd450.y1.select*lvl.cd450.y1 + lvl.cd450.yg1.select*lvl.cd450.yg1+
             lvl.cd4100.y1.select*lvl.cd4100.y1 + lvl.cd4100.yg1.select*lvl.cd4100.yg1+
             lvl.cd4200.y1.select*lvl.cd4200.y1 + lvl.cd4200.yg1.select*lvl.cd4200.yg1+
             lvl.cd4350.y1.select*lvl.cd4350.y1 + lvl.cd4350.yg1.select*lvl.cd4350.yg1+
             lvl.cd4g350.y1.select*lvl.cd4g350.y1 + lvl.cd4g350.yg1.select*lvl.cd4g350.yg1
  )
}


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



#' Probability of death for HIV positive children
#' 
#' Using cumulative child mortality due to AIDS from Walker, Hill, and Zhao (2012).
#' Probability at ages less than 0 and greater than 4 is zero.
#' 
#' @param ages (integer) vector of children ages
#' @return (numeric) Probability of death for HIV positive children
baby.death.hiv <- function(ages){

  # cumulative child mortality due to AIDS from Walker, Hill, and Zhao (2012)
  
  hivchild_mort = c(0.376,  0.2019,	0.1184,	0.07061,	0.0588,	0.0234375)  

  # Empty vector
  
  baby.death.hiv <- vector(,length(ages) )
  
  names(baby.death.hiv) <- ages
  
  # Assign mortality to each age. For ages <0 and greater than 4 prob is 0
  for (a in ages) {
    if(a<0){baby.death.hiv[as.character(a)]=0}
    if(a>4){baby.death.hiv[as.character(a)]=0}
    if(a==0){baby.death.hiv[as.character(a)]=hivchild_mort[1]}   
    if(a==1){baby.death.hiv[as.character(a)]=hivchild_mort[2]}   
    if(a==2){baby.death.hiv[as.character(a)]=hivchild_mort[3]}   
    if(a==3){baby.death.hiv[as.character(a)]=hivchild_mort[4]}   
    if(a==4){baby.death.hiv[as.character(a)]=hivchild_mort[5]}   
  }
  
  baby.death.hiv
    
}


##### HIV INFECTION #####

#' Annual probability of HIV infection
#' 
#' We start selecting the incidence value for the simulation year from 
#' the incidence values provided in hivinc_s (these are the values for a specific country,taken from 
#' Dan Hogan and based on data from ANC clinics, 1970-2015. Then, age-specific incidence is obtained using
#' relative age incidence from P Heuveline. Finally, age-specific incidence is adjusted by age structure so 
#' that overall incidence matches Hogan & Salomon
#'
#' @param age (integer) current age
#' @param year (integer) current year in simulation
#' @param hivinc_s (data.frame) HIV incidence curve estimated by Hogan and Salomon (2012) for a specific country
#' @param c15 (integer) number of people in the simulation between 15 and 19 years
#' @param c20 (integer) number of people in the simulation between 20 and 24 years
#' @param c25 (integer) number of people in the simulation between 25 and 29 years
#' @param c30 (integer) number of people in the simulation between 30 and 34 years
#' @param c35 (integer) number of people in the simulation between 35 and 39 years
#' @param c40 (integer) number of people in the simulation between 40 and 44 years
#' @param c45 (integer) number of people in the simulation between 45 and 49 years
#' @return (numeric) Annual probability of HIV infection
prob.hiv <- function(age,year,hivinc_s,c15,c20,c25,c30,c35,c40,c45){
  
  # Prob 0 before 1975
  if(year<1975){
    return(0)
  }else{
    
    # Get the incidence value for current year
    hivcol = (year-1970)+2
    hivinc = hivinc_s[hivcol]
    
    # Compute probability at specific ages.
    
    # p(HIV=positive|age group,year)=(incidence(year)*relative incidence age group)/sum(relative incidence age group * fraction of age gorup in population)
    
    # First compute incidence for the reference group: ages 25-29 
    
    inc2529=hivinc/(.594*(c15/(c15+c20+c25+c30+c35+c40+c45))
                    +1.325*(c20/(c15+c20+c25+c30+c35+c40+c45))
                    +(c25/(c15+c20+c25+c30+c35+c40+c45))
                    +.752*(c30/(c15+c20+c25+c30+c35+c40+c45))
                    +.635*(c35/(c15+c20+c25+c30+c35+c40+c45))
                    +.551*(c40/(c15+c20+c25+c30+c35+c40+c45))
                    +.356*(c45/(c15+c20+c25+c30+c35+c40+c45))
    )
    
    # Now, use use age-specific HIV incidence ratios from Heuveline (2003) to compute risk at each age group
    
    # Ages 15 to 19
    if(age>14&age<20){
      hrisk=.594*inc2529
      return(hrisk)
    }
    
    # Ages 20 to 24
    if(age>19&age<25){
      hrisk=1.325*inc2529
      return(hrisk)
    }  
    
    # Ages 25 to 29
    if(age>24&age<30){
      hrisk=inc2529
      return(hrisk)
    }  
    
    # Ages 30 to 34
    if(age>29&age<35){
      hrisk=.752*inc2529
      return(hrisk)
    }
    
    # Ages 35 to 39
    if(age>34&age<40){
      hrisk=.635*inc2529
      return(hrisk)
    }	
    
    # Ages 40 to 44
    if(age>39&age<45){
      hrisk=.551*inc2529
      return(hrisk)
    }	
    # Ages 45 to 49
    if(age>44&age<50){
      hrisk=.356*inc2529
      return(hrisk)
    }
    
    # People older than 49, have probability 0 
    if(age>49){
      hrisk=0
      return(hrisk)
    }	
  }
}


#' Annual probability of HIV infection for each age year combination
#' 
#' Uses phivneg.death to compute annual probability of death for each age-year combination
#' 
#' @param ages (numeric) vector of ages
#' @param years (numeric) vector of years in simulation
#' @param hivinc_s (data.frame) HIV incidence curve estimated by Hogan and Salomon (2012) for a specific country
#' @param c15 (integer) number of people in the simulation between 15 and 19 years
#' @param c20 (integer) number of people in the simulation between 20 and 24 years
#' @param c25 (integer) number of people in the simulation between 25 and 29 years
#' @param c30 (integer) number of people in the simulation between 30 and 34 years
#' @param c35 (integer) number of people in the simulation between 35 and 39 years
#' @param c40 (integer) number of people in the simulation between 40 and 44 years
#' @param c45 (integer) number of people in the simulation between 45 and 49 years
#' @return (matrix) (length(ages) x length(years)) Annual probability of HIV infection.
prob.hiv.ages.years <- function(ages,years,hivinc_s,c15,c20,c25,c30,c35,c40,c45){
  
  # Create empty matrix (length(ages) x length(years))
  
  prob.hiv.vec <- matrix(NA,length(ages),length(years))
  row.names(prob.hiv.vec) <- ages
  colnames(prob.hiv.vec) <- years
  
  # Compute probability of HIV for each age an year. 
  for (a in ages) {
    for (y in years) {
      
      # probability of HIV
      res <- as.numeric(prob.hiv(a,y,hivinc_s,c15,c20,c25,c30,c35,c40,c45))
      
      
      if (length(res)>0 ){
        prob.hiv.vec[as.character(a),as.character(y)] <- res
      }else { # If we don't get a result, that means that the age/year is outside our range, assign probability 0
        prob.hiv.vec[as.character(a),as.character(y)] <- 0
      }
    }
  }  
  
  prob.hiv.vec  
}


#' Probability of mother to child transmission
#'
#' Probability of mother-to-child transmission of HIV was taken from Stover et al. (2008). 
#' Transmission depends on breastfeeding duration and ART. Using estimates for single-dose nevirapine.
#' 
#' @param art (integer) 0 if not in ART 1 if in ART
#' @param bfeed (integer) breastfeeding duration (months) in the general population (must be 6, 12 or 18)
#' @return (numeric) Probability of mother to child transmission
vert_trans <- function(art,bfeed){
  if(bfeed==6 & art==0){
    vt=.23
    return(vt)}
  if(bfeed==6 & art==1){
    vt=.17
    return(vt)}
  if(bfeed==12 & art==0){
    vt=.305
    return(vt)}
  if(bfeed==12 & art==1){
    vt=.215
    return(vt)}
  if(bfeed==18 & art==0){
    vt=.35
    return(vt)}
  if(bfeed==18 & art==1){
    vt=.26
    return(vt)}
}


#' CD4 progression
#' 
#' square root of CD4 is assumed to decline linearly over time. Parameters
#' derived from Hallett (2008) 
#' 
#' @param cd4 (numeric) initial CD4 count
#' @param cd4dec (numeric) CD4 decline per year
#' @param hivdate (integer) year of HIV infection
#' @param year (integer) current year in simulation
#' @return (numeric) updated CD4 count
cd4.prog <- function(cd4,cd4dec, hivdate,year){
  a = cd4^(.5)
  b = cd4dec
  ts = year-hivdate	
  cd4new = (a-b*ts)^2
  return(cd4new)
}

##### SIMULATION #####


#' Date of birth for initial age structure
#' 
#' @param growth (numeric) yearly population growth rate
#' @param initialpop (integer) initial population
#' @return (numeric) date of birth
initial.DOBs <- function(growth,initialpop){

  # Compute total population for each step
  
  ratios <- rep(1,50)
  for(i in 1:50){
    ratios[i+1]=ratios[i]*(1+growth)
  }
  population = ratios*initialpop
  
  # Compute DOB for each step, starting at 1897
  
  dobs = NA
  counter=1
  for(i in 1:50){
    k = population[i]
    for(j in 1:k){
      dobs[counter]=i+1896
      counter=counter+1
    }
  }
  
  dobs  
  
}

#' Create empty population matrix of women
#' 
#' Creates an empty matrix of length(dobs) women
#' 
#' @param dobs (numeric) vector of years of birth
#' @returm (matrix) empty population matrix with women only
women.empty.matrix <- function(dobs){

  w <- matrix(NA,length(dobs),19)
  colnames(w) <- c("id", "momid","dob","age","ceb", "hiv","hiv_date","death_date","hivdeath","cd4","cd4dec","art","art_date","art_e","momage","momhiv","male","cd","dead")
  w[,1] <- c(1:length(dobs))
  w [,"ceb"]=0
  w[,"hiv"]=0
  w[,"hiv_date"]=NA
  w[,"hivdeath"]=0 
  w[,"art"]=0 
  w[,"art_date"]=0
  w[,"art_e"]=NA
  w[,"dob"] = dobs
  w[,"momid"] = NA
  w[,"male"] = 0
  w[,"cd"]=0
  w[,"dead"]=0
  
  w
    
}


#' Empty matrix for birth counts by age group
#' 
#' @param yrstart (numeric) simulation start year
#' @param yrend (numeric) simulation end year
#' @return (matrix) empty matrix to store birth counts for each age group
birth.counts.by.age.empty.matrix <- function(yrstart,yrend){

  # The matrix has as many rows as the length of the simulation times 7 age groups
  
  births.age.yr <- matrix(NA, (yrend-yrstart+1)*7,4)
  colnames(births.age.yr) <- c("year","agegrp","births","women")

  # Fill years
  
  b<-vector()
  for(j in yrstart:yrend){
    a <- rep(j,7)
    b <- c(b,a)
  }
  births.age.yr[,"year"] <- b
  # Fill age groups
  
  c <- seq(15,45,5)
  d <- rep(c,length(births.age.yr[,1])/7)
  births.age.yr[,"agegrp"] <- d
  
  births.age.yr
}


#' Updates birth count by age for one year
#' 
#' @param births.age.yr (matrix) Matrix of birth counts by age and year, 
#' like the one provided by `birth.counts.by.age.empty.matrix`
#' @param nextbabies (matrix) population matrix of newborns, like the one provided by `next.babies`
#' @param yr (integer) current year in simulation
#' @return (matrix)
update.birth.counts.by.age <- function(births.age.yr,nextbabies,yr){
  
  # Compute number of births per mother age category and store in births.age.yr
  
  # Ages 15-19
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==15] <-sum(nextbabies[,"momage"]<20 & nextbabies[,"momage"]>14)
  
  # Ages 20-24
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==20] <-sum(nextbabies[,"momage"]<25 & nextbabies[,"momage"]>19)
  
  # Ages 25-29
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==25] <-sum(nextbabies[,"momage"]<30 & nextbabies[,"momage"]>24)
  
  # Ages 30-34
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==30] <-sum(nextbabies[,"momage"]<35 & nextbabies[,"momage"]>29)
  
  # Ages 35-39
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==35] <-sum(nextbabies[,"momage"]<40 & nextbabies[,"momage"]>34)
  
  # Ages 40-44
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==40] <-sum(nextbabies[,"momage"]<45 & nextbabies[,"momage"]>39)
  
  # Ages 45-49
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==45] <-sum(nextbabies[,"momage"]<50 & nextbabies[,"momage"]>44)
  
  
  births.age.yr
}

#' Empty matrix for birth counts by HIV status
#' 
#' @param yrstart (numeric) simulation start year
#' @param yrend (numeric) simulation end year
#' @return (matrix) empty matrix to store birth counts for each HIV status
birth.counts.by.hiv.status.empty.matrix <- function(yrstart,yrend){

  # The matrix has as one row per year
  
  hivbirths.momshiv <- matrix(NA, (yrend-yrstart+1),3)
  
  colnames(hivbirths.momshiv) <- c("year","birthpos","birthmompos")
  
  # Fill the years
  
  hivbirths.momshiv[,"year"]<-seq(yrstart,yrend,1)
  
  
  hivbirths.momshiv
    
}

#' Counts the number of people in each age group
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) matrix of population
#' @returm (numeric) named vector of counts for each group, names define the groups
count.women.age.groups <- function(yr,w){
  
  # Initialize counts to 0
  
  c15=0
  c20=0
  c25=0
  c30=0
  c35=0
  c40=0
  c45=0
  
  # For each age category, count hoy many people are not dead in that age category 
  
  
  # Ages 15-19
  x = is.na(w[,"death_date"]) & yr-w[,"dob"]<20 & yr-w[,"dob"]>14
  x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<20 & yr-w[,"dob"]>14 & yr<w[,"death_date"] 
  c15 =   sum(x==T) + sum(x1==T)
  
  # Ages 20-24
  x = is.na(w[,"death_date"]) & yr-w[,"dob"]<25 & yr-w[,"dob"]>19
  x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<25 & yr-w[,"dob"]>19 & yr<w[,"death_date"] 
  c20 =   sum(x==T) + sum(x1==T)
  
  # Ages 25-29
  x = is.na(w[,"death_date"]) & yr-w[,"dob"]<30 & yr-w[,"dob"]>24
  x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<30 & yr-w[,"dob"]>24 & yr<w[,"death_date"] 
  c25 =   sum(x==T) + sum(x1==T)
  
  # Ages 30-34
  x = is.na(w[,"death_date"]) & yr-w[,"dob"]<35 & yr-w[,"dob"]>29
  x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<35 & yr-w[,"dob"]>29 & yr<w[,"death_date"] 
  c30 =   sum(x==T) + sum(x1==T)
  
  # Ages 35-39
  x = is.na(w[,"death_date"]) & yr-w[,"dob"]<40 & yr-w[,"dob"]>34
  x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<40 & yr-w[,"dob"]>34 & yr<w[,"death_date"] 
  c35 =   sum(x==T) + sum(x1==T)
  
  # Ages 40-44
  x = is.na(w[,"death_date"]) & yr-w[,"dob"]<45 & yr-w[,"dob"]>39
  x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<45 & yr-w[,"dob"]>39 & yr<w[,"death_date"] 
  c40 =   sum(x==T) + sum(x1==T)
  
  # Ages 45-49
  x = is.na(w[,"death_date"]) & yr-w[,"dob"]<50 & yr-w[,"dob"]>44
  x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<50 & yr-w[,"dob"]>44 & yr<w[,"death_date"] 
  c45 =   sum(x==T) + sum(x1==T)

  # Return vector with counts
  
  c(c15=c15,c20=c20,c25=c25,c30=c30,c35=c35,c40=c40,c45=c45)
}


#' Updates women age
#' 
#' Current age = current year - year of birth
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) Population matrix
#' @return (matrix) Population matrix with ages updated for the current year
update.women.age <- function(yr,w){
  
  # current age = current year - year of birth
  
  w[,"age"] = yr-w[,"dob"]
  
  w
  
}

#' Randomly determine if a women has a child this year according to her probability 
#' of giving birth this year.
#' 
#' 
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) Population matrix
#' @param prob.birth.all (matrix) Annual probability of birth as a function of calendar 
#' year and mother’s age. HIV negative. As given by `prob.birth.ages.years`.
#' @param prob.birth.hiv (matrix) Reduction in prob of giving birth due to HIV for each art, age combination. As provided by `prob.birth.hiv`
#' @return (logical) Vector of length=nrow(w). Each element determines whether a woman gives birth 
#' that year (TRUE) or not (FALSE).
new.babies <- function(yr,w,prob.birth.all,prob.birth.hiv){
  
  # Get probability of birth this year for each woman in w. HIV negative
  prob.birth.thisyear <- prob.birth.all[as.character(yr),as.character(w[,"age"])]
  
  # Compute probability of birth thi yeat for each woman. HIV positive in ART
  prob.birth.thisyear.hiv.art <- prob.birth.thisyear*prob.birth.hiv[as.character(w[,"age"]),"1"]
  
  # Compute probability of birth thi yeat for each woman. HIV positive not in ART
  prob.birth.thisyear.hiv.noart <- prob.birth.thisyear*prob.birth.hiv[as.character(w[,"age"]),"0"]
  
  # Combine the three vectors above to get the probability of birth for each woman
  
  prob.birth.thisyear.adj <- (1-w[,"male"])* # If male, prob 0
    (prob.birth.thisyear*(1-w[,"hiv"]) + # Woman HIV negative
       prob.birth.thisyear.hiv.art*w[,'art'] + # Woman HIV positive in ART
       prob.birth.thisyear.hiv.noart*(1-w[,'art'])*w[,'hiv']) # Woman HIV positive not in ART
  
  # Randomly create vector of TRUE (birth) or FALSE (no birth), according to the probability of birth of each
  # individual.
  
  
  newbaby <- runif(nrow(w))<prob.birth.thisyear.adj
  
  newbaby
}


#' Creates new people matrix for newborns
#' 
#' Probability of being male is 102.5/202.5
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) Population matrix
#' @param newbaby (logical) Vector of length=nrow(w). Each element determines whether a woman gives birth 
#' that year (TRUE) or not (FALSE). As computed by `new.babies`
#' @return (matrix) Population matrix
next.babies <- function(yr,w,newbaby){

  # Empty matrix with the same columns as w and as many rows as newborns
  nextbabies <- matrix(NA,length(which(newbaby)),length(w[1,]))
  colnames(nextbabies) <- colnames(w)
  
  # Fill the columns
  
  # Mother's data
  nextbabies[,"momid"] <- w[newbaby,"id"]
  nextbabies[,"momage"] <- w[newbaby,"age"]
  nextbabies[,"momhiv"] <- w[newbaby,"hiv"]
  nextbabies[,"art"] <- w[newbaby,"art"]
  
  # Newborn data
  nextbabies[,"dob"] <- yr
  nextbabies[,"id"] <- seq(max(w[,"id"])+1,max(w[,"id"])+length(nextbabies[,"id"]),1) # New Ids
  nextbabies[,"age"] <- 0
  nextbabies[,"ceb"] <- 0
  nextbabies[,"cd"] <- 0
  
  # Determine randomly if is male/female
  nextbabies[,"male"] <- as.numeric(runif(nrow(nextbabies))<=102.5/202.5)  
  
  nextbabies
    
}

#' Updates ceb when individuals have a baby
#' 
#' @param w (matrix) Population matrix
#' @param newbaby (logical) Vector of length=nrow(w). Each element determines whether a woman gives birth 
#' that year (TRUE) or not (FALSE). As computed by `new.babies`
#' @return (matrix) Population matrix updated
update.women.ceb <- function(w,newbaby){

  w[newbaby,"ceb"] <- w[newbaby,"ceb"]+1
  
  w
}
