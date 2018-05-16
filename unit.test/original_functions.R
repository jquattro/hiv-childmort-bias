original.simulation <- function(yrstart,yrend,ages,years,asfr_s,tfr_s,sexactive15,arts,mort_s,adultmort,am_cntry,matmort,u5m_c,bfeed,growth,initialpop,hivinc_s,artprobs,threshold){
  
  
  
  # Annual prob of giving birth is a function of age and calendar year,
  prob.birth <- function(age,yr,asfr_s,tfr_s){
    if(yr<min(tfr_s[,"year"])){yr=min(tfr_s[,"year"])}
    tfr1 = tfr_s[,"tfr"][tfr_s[,"year"]==yr]
    #find row with nearest year (rounds down in case of tie)
    row = which.min(abs(asfr_s[,"refyr"]-yr))
    tfr2 = asfr_s[row,"tfr"]
    if(age>14 & age<20){
      pb=asfr_s[row,"asfr15"]*(tfr1/tfr2)/1000
      return(pb)
    }
    if(age>19 & age<25){
      pb=asfr_s[row,"asfr20"]*(tfr1/tfr2)/1000
      return(pb)
    }
    if(age>24 & age<30){
      pb=asfr_s[row,"asfr25"]*(tfr1/tfr2)/1000
      return(pb)
    }
    if(age>29 & age<35){
      pb=asfr_s[row,"asfr30"]*(tfr1/tfr2)/1000
      return(pb)
    }
    if(age>34 & age<40){
      pb=asfr_s[row,"asfr35"]*(tfr1/tfr2)/1000
      return(pb)
    }
    if(age>39 & age<45){
      pb=asfr_s[row,"asfr40"]*(tfr1/tfr2)/1000
      return(pb)
    }
    if(age>44 & age<50){
      pb=asfr_s[row,"asfr45"]*(tfr1/tfr2)/1000
      return(pb)
    }
  }
  
  
  prob.birth.all <- matrix(NA,length(years),length(ages) )
  row.names(prob.birth.all) <- years
  colnames(prob.birth.all) <- ages
  #head(prob.birth.all)
  for (y in years) {
    for (a in ages) {
      res <- prob.birth(a,y,asfr_s,tfr_s)
      if (length(res)>0 ){prob.birth.all[as.character(y),as.character(a)] <- res}
      else {
        prob.birth.all[as.character(y),as.character(a)] <- 0
      }
    }
  }
  
  #foo <- prob.birth(20,1946,asfr_s,tfr_s)
  #aa <- sample(ages,10)
  #yy <- sample(years,10)
  #prob.birth.all[as.character(yy),as.character(aa)]
  
  # Reduction in prob of giving birth due to HIV 
  fert_hiv <- function(age,sexactive15,art){
    if(art==0){
      if(age>14 & age<20){
        fert_hiv=2.528-.031*sexactive15
        return(fert_hiv)
      }
      if(age>19 & age<25){
        fert_hiv=.765
        return(fert_hiv)
      }
      if(age>24 & age<30){
        fert_hiv=.706
        return(fert_hiv)
      }
      if(age>29 & age<35){
        fert_hiv=.647
        return(fert_hiv)
      }
      if(age>34 & age<40){
        fert_hiv=.588
        return(fert_hiv)
      }
      if(age>39 & age<45){
        fert_hiv=.529
        return(fert_hiv)
      }
      if(age>44 & age<50){
        fert_hiv=.470
        return(fert_hiv)
      }
    }else{
      if(age>14 & age<20){
        fert_hiv=2.528-.031*sexactive15
        return(fert_hiv)
      }
      if(age>19 & age<25){
        fert_hiv=.8825
        return(fert_hiv)
      }
      if(age>24 & age<30){
        fert_hiv=.853
        return(fert_hiv)
      }
      if(age>29 & age<35){
        fert_hiv=.824
        return(fert_hiv)
      }
      if(age>34 & age<40){
        fert_hiv=.794
        return(fert_hiv)
      }
      if(age>39 & age<45){
        fert_hiv=.765
        return(fert_hiv)
      }
      if(age>44 & age<50){
        fert_hiv=.735
        return(fert_hiv)
      }
    }
  }
  
  prob.birth.hiv <- matrix(NA,length(ages),length(arts))
  row.names(prob.birth.hiv) <- ages
  colnames(prob.birth.hiv) <- arts
  #head(prob.birth.hiv)
  for (a in ages) {
    for (b in arts) {
      res <- fert_hiv(a,sexactive15,b)
      if (length(res)>0 ){prob.birth.hiv[as.character(a),as.character(b)] <- res}
      else {
        prob.birth.hiv[as.character(a),as.character(b)] <- 0
      }
    }
  }
  #head(prob.birth.hiv)
  
  ### MORTALITY ###
  # Maternal mortality = per birth prob of death
  # Hogan et al IHME find annual rates of decline, SSA, 1990-2008, no HIV: 0% to 7.4%
  # MMR (no HIV) range, 2008: 53 to 879 per 100,000 births (implies 123 to 1296 in 1990)
  # Blanc et al find MMR 5-10x greater for 45-49 y/o compared to 20-24 y/o
  
  pmatmort_vec <- function(mmr0,mmr_dec,age,year){
    mmr = mmr0*(1-mmr_dec)^(year-1990)
    pmm_l25_l1990 <- mmr0
    pmm_g25_l1990 <- mmr0+((age-25)/25)*2*mmr0
    pmm_l25_g1990 <- mmr
    pmm_g25_g1990 <- mmr+((age-25)/25)*2*mmr
    l25 <- ifelse(age>=0 & age<25,1,0)
    g25 <- ifelse(age>=25,1,0)
    l0 <- ifelse(age<0,1,0)
    pmatmortl1990 = l25*pmm_l25_l1990 + g25*pmm_g25_l1990 + l0*0
    pmatmortg1990 = l25*pmm_l25_g1990 + g25*pmm_g25_g1990 + l0*0
    if (year>1990) {
      return(pmatmortg1990)
    } else {
      return(pmatmortl1990)
    }
  }
  
  # Mat mort in HIV pos women
  # The mortality rate ratio of HIV-infected to HIV-uninfected women was 8?2 (5?7?11?8) in pregnant or post-partum women. 
  mmr_hiv = 8.2
  
  # Annual prob of dying for HIV- adults
  #Use same country as fertcountry to provide time series of 45q15
  #mort_s = mort_series[mort_series[,"country"]==fertcountry,]
  
  #If user wants to select type of MLT here
  #mort <- adultmort[adultmort$Type=="CD East" & adultmort$Sex=="Female",]
  # Otherwise just match to nearest 45q15 regardless of type
  
  # Prob of death for HIV neg children (UN IGME)
  baby.death.nohiv <- matrix(NA,length(years),length(ages) )
  row.names(baby.death.nohiv) <- years
  colnames(baby.death.nohiv) <- ages
  #  head(baby.death.nohiv)
  for (y in years) {
    for (a in ages) {
      if(y<min( u5m_c$year)){yr=min(u5m_c$year)
      }else{yr=y}
      if(a<0){baby.death.nohiv[as.character(y),as.character(a)]=0}
      if(a>4){baby.death.nohiv[as.character(y),as.character(a)]=0}
      if(a==0){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_0[u5m_c$year==yr]}   
      if(a==1){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_1[u5m_c$year==yr]}   
      if(a==2){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_2[u5m_c$year==yr]}   
      if(a==3){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_3[u5m_c$year==yr]}   
      if(a==4){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_4[u5m_c$year==yr]}   
    }
  }
  
  # Probability of death if HIV negative
  phivneg.death <- function(age,year,mort_s,adultmort,am_cntry,matmort,u5m_c){
    if(year<1970){q45_15 = mort_s[,"q45_15"][mort_s[,"year"]==1970]}
    if(year>=1970){q45_15 = mort_s[,"q45_15"][mort_s[,"year"]==year]}
    #find row of adultmort with 45q15 (rounds down in case of tie) to that in mort_s
    min45 = which.min(abs(adultmort[,"q45_15"]-q45_15))
    e0 = adultmort[min45,"E.0."]
    if(age<0){return(0)}
    if(year<min( u5m_c$year)){yr=min(u5m_c$year)
    }else{yr=year}
    if(age==0){return(u5m_c$q1_0[u5m_c$year==yr])}
    if(age==1){return(u5m_c$q1_1[u5m_c$year==yr])} 
    if(age==2){return(u5m_c$q1_2[u5m_c$year==yr])} 
    if(age==3){return(u5m_c$q1_3[u5m_c$year==yr])} 
    if(age==4){return(u5m_c$q1_4[u5m_c$year==yr])}   
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
    if(age>49){return(0)}
  }
  
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
  
  # Prob death HIV pos adults = f(time since infection) 
  # Using Walker Hill cum AIDS mort
  aidsmort = c(0.01,0.02020202,0.041237113,0.053763441,0.079545455,0.098765432,0.123287671,0.15625,0.185185185,0.204545455,0.228571429,	0.296296296,0.263157895,0.357142857,0.333333333,0.333333333,0.5,0.5)
  aidsmort = c(aidsmort,rep(1,50))
  
  phiv.death <- function(hiv_date, year){
    if(year<hiv_date){return("error: year<hiv_date")}
    duration = year - hiv_date
    #Assume no one survives more than 18 yrs with HIV
    if(duration>17){pd=1
    }else{
      pd = aidsmort[duration+1]
    }
    return(pd)
  }
  
  # Prob death for HIV pos adults on ART (Hallett et al 2008 supplement)
  # Using "medium" scenario
  # ART-LINC ART-CC Lancet 2006 50% patients have high viral load
  
  # Weibull distribution transition probability
  weib.tp <- function(t,scale,shape){
    tp = 1 - exp(-((1/scale)*t^shape - (1/scale)*(t-1)^shape))
    return(tp)
  }
  
  art.surv <- function(year,cd4,art_date){
    shape=1.6
    #High viral load
    if(runif(1)<.5){
      if(cd4<=50){
        if(year-art_date==1){
          pd=.109
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,13.7,shape)
          return(pd)    
        }}	
      if(cd4>50 & cd4<100){
        if(year-art_date==1){
          pd=.067
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,16,shape)
          return(pd)
        }}
      if(cd4>=100 & cd4<200){
        if(year-art_date==1){
          pd=.046
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,16.9,shape)
          return(pd)
        }}
      if(cd4>=200 & cd4<350){
        if(year-art_date==1){
          pd=.017
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,23.3,shape)
          return(pd)
        }}
      if(cd4>=350){
        if(year-art_date==1){
          pd=.017
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,33.3,shape)
          return(pd)
        }}
      #Low viral load
    }else{
      if(cd4<=50){
        if(year-art_date==1){
          pd=.109
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,24.4,shape)
          return(pd)		
        }}
      if(cd4>50 & cd4<100){
        if(year-art_date==1){
          pd=.067
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,28.4,shape)
          return(pd)
        }}
      if(cd4>=100 & cd4<200){
        if(year-art_date==1){
          pd=.046
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,30.1,shape)
          return(pd)
        }}
      if(cd4>=200 & cd4<350){
        if(year-art_date==1){
          pd=.017
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,41.4,shape)
          return(pd)
        }}
      if(cd4>=350){
        if(year-art_date==1){
          pd=.017
          return(pd)
        }
        if(year-art_date>1){
          t=year-art_date
          pd=weib.tp(t,59.1,shape)
          return(pd)
        }}
    }
  }
  
  art.surv.vec <- function(year,cd4,art_date){
    shape=1.6
    #High viral load
    hvl <- ifelse(runif(length(cd4))<0.5,1,0)
    lvl <- 1 - hvl
    t = year-art_date
    hvl.cd450.y1 <- .109
    hvl.cd450.y1.select <- hvl*ifelse(cd4<50 & t==1,1,0)
    hvl.cd450.yg1 <- weib.tp(t,13.7,shape)
    hvl.cd450.yg1.select <- hvl*ifelse(cd4<50 & t>1,1,0)
    hvl.cd4100.y1 <- .67
    hvl.cd4100.y1.select <- hvl*ifelse(cd4>=50 & cd4<100 & t==1,1,0)
    hvl.cd4100.yg1 <- weib.tp(t,16,shape)
    hvl.cd4100.yg1.select <- hvl*ifelse(cd4>=50 & cd4<100 & t>1,1,0)
    hvl.cd4200.y1 <- .46
    hvl.cd4200.y1.select <- hvl*ifelse(cd4>=100 & cd4<200 & t==1,1,0)
    hvl.cd4200.yg1 <- weib.tp(t,16.9,shape)
    hvl.cd4200.yg1.select <- hvl*ifelse(cd4>=100 & cd4<200 & t>1,1,0)
    hvl.cd4350.y1 <- .17
    hvl.cd4350.y1.select <- hvl*ifelse(cd4>=200 & cd4<350 & t==1,1,0)
    hvl.cd4350.yg1 <- weib.tp(t,23.3,shape)
    hvl.cd4350.yg1.select <- hvl*ifelse(cd4>=200 & cd4<350 & t>1,1,0)
    hvl.cd4g350.y1 <- .17
    hvl.cd4g350.y1.select <- hvl*ifelse(cd4>=350 & t==1,1,0)
    hvl.cd4g350.yg1 <- weib.tp(t,33.3,shape)
    hvl.cd4g350.yg1.select <- hvl*ifelse(cd4>=350 & t>1,1,0)
    
    lvl.cd450.y1 <- .109
    lvl.cd450.y1.select <- lvl*ifelse(cd4<50 & t==1,1,0)
    lvl.cd450.yg1 <- weib.tp(t,24.4,shape)
    lvl.cd450.yg1.select <- lvl*ifelse(cd4<50 & t>1,1,0)
    lvl.cd4100.y1 <- .67
    lvl.cd4100.y1.select <- lvl*ifelse(cd4>=50 & cd4<0 & t==1,1,0)
    lvl.cd4100.yg1 <- weib.tp(t,28.4,shape)
    lvl.cd4100.yg1.select <- lvl*ifelse(cd4>=50 & cd4<100 & t>1,1,0)
    lvl.cd4200.y1 <- .46
    lvl.cd4200.y1.select <- lvl*ifelse(cd4>=100 & cd4<200 & t==1,1,0)
    lvl.cd4200.yg1 <- weib.tp(t,30.1,shape)
    lvl.cd4200.yg1.select <- lvl*ifelse(cd4>=100 & cd4<200 & t>1,1,0)
    lvl.cd4350.y1 <- .17
    lvl.cd4350.y1.select <- lvl*ifelse(cd4>=200 & cd4<350 & t==1,1,0)
    lvl.cd4350.yg1 <- weib.tp(t,41.4,shape)
    lvl.cd4350.yg1.select <- lvl*ifelse(cd4>=200 & cd4<350 & t>1,1,0)
    lvl.cd4g350.y1 <- .17
    lvl.cd4g350.y1.select <- lvl*ifelse(cd4>=350 & t==1,1,0)
    lvl.cd4g350.yg1 <- weib.tp(t,59.1,shape)
    lvl.cd4g350.yg1.select <- lvl*ifelse(cd4>=350 & t>1,1,0)
    
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
  
  
  # Prob of death for HIV pos children
  #Using Walker Hill cum child mort due to AIDS
  hivchild_mort = c(0.376,  0.2019,	0.1184,	0.07061,	0.0588,	0.0234375)
  
  baby.death.hiv <- vector(,length(ages) )
  names(baby.death.hiv) <- ages
  #  head(baby.death.hiv)
  for (a in ages) {
    if(a<0){baby.death.hiv[as.character(a)]=0}
    if(a>4){baby.death.hiv[as.character(a)]=0}
    if(a==0){baby.death.hiv[as.character(a)]=hivchild_mort[1]}   
    if(a==1){baby.death.hiv[as.character(a)]=hivchild_mort[2]}   
    if(a==2){baby.death.hiv[as.character(a)]=hivchild_mort[3]}   
    if(a==3){baby.death.hiv[as.character(a)]=hivchild_mort[4]}   
    if(a==4){baby.death.hiv[as.character(a)]=hivchild_mort[5]}   
  }
  
  ### HIV INFECTION ###
  # Annual prob of HIV infection is a function of age and calendar year
  # Incidence curves from Dan Hogan, based on data from ANC clinics, 1970-2015
  # Relative age incidence is from P Heuveline [see Hallett et al 2010 supp for alternative]
  # Age-specific incidence is adjusted by age structure so that overall incidence matches Hogan & Salomon
  prob.hiv <- function(age,year,hivinc_s,c15,c20,c25,c30,c35,c40,c45){
    if(year<1975){return(0)}
    else{
      hivcol = (year-1970)+2
      hivinc = hivinc_s[hivcol]
      inc2529=hivinc/(.594*(c15/(c15+c20+c25+c30+c35+c40+c45))
                      +1.325*(c20/(c15+c20+c25+c30+c35+c40+c45))
                      +(c25/(c15+c20+c25+c30+c35+c40+c45))
                      +.752*(c30/(c15+c20+c25+c30+c35+c40+c45))
                      +.635*(c35/(c15+c20+c25+c30+c35+c40+c45))
                      +.551*(c40/(c15+c20+c25+c30+c35+c40+c45))
                      +.356*(c45/(c15+c20+c25+c30+c35+c40+c45))
      )
      if(age>14&age<20){hrisk=.594*inc2529
      return(hrisk)}  
      if(age>19&age<25){hrisk=1.325*inc2529
      return(hrisk)}  
      if(age>24&age<30){hrisk=inc2529
      return(hrisk)}  
      if(age>29&age<35){hrisk=.752*inc2529
      return(hrisk)}  
      if(age>34&age<40){hrisk=.635*inc2529
      return(hrisk)}	
      if(age>39&age<45){hrisk=.551*inc2529
      return(hrisk)}	
      if(age>44&age<50){hrisk=.356*inc2529
      return(hrisk)}	
      if(age>49){hrisk=0
      return(hrisk)}	
    }
  }
  
  # Prob of mother to child transmission (Stover et al 2008)
  #User inputs duration (months) of breastfeeding in the general population (must be 6, 12 or 18)
  # Using estimates for single-dose nevirapine
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
  
  # Vectorize!
  prob.vt.noart <- vert_trans(0,bfeed)
  prob.vt.art <- vert_trans(1,bfeed)
  
  # CD4 progression (from Hallett 2008)
  cd4.prog <- function(cd4,cd4dec, hivdate,year){
    a = cd4^(.5)
    b = cd4dec
    ts = year-hivdate	
    cd4new = (a-b*ts)^2
    return(cd4new)
  }
  
  #
  
  # DEFINE bigsim
  
  #DOBs for initial age structure  
  
  ratios <- rep(1,50)
  for(i in 1:50){
    ratios[i+1]=ratios[i]*(1+growth)
  }
  ratios = ratios*initialpop
  
  dobs = NA
  counter=1
  for(i in 1:50){
    k = ratios[i]
    for(j in 1:k){
      dobs[counter]=i+1896
      counter=counter+1
    }
  }
  
  
  # Empty matrix of women
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
  
  # Empty matrix for birth counts by age group
  births.age.yr <- matrix(NA, (yrend-yrstart+1)*7,4)
  colnames(births.age.yr) <- c("year","agegrp","births","women")
  b<-vector()
  for(j in yrstart:yrend){
    a <- rep(j,7)
    b <- c(b,a)
  }
  births.age.yr[,"year"] <- b
  c <- seq(15,45,5)
  d <- rep(c,length(births.age.yr[,1])/7)
  births.age.yr[,"agegrp"] <- d
  
  # Empty matrix for birth counts by HIV status
  hivbirths.momshiv <- matrix(NA, (yrend-yrstart+1),3)
  colnames(hivbirths.momshiv) <- c("year","birthpos","birthmompos")
  hivbirths.momshiv[,"year"]<-seq(yrstart,yrend,1)
  
  ##############  START OF LOOP FOR 'W' ######################################################
  
  ptm <- proc.time()
  for (i in yrstart:yrend) {
    #for (i in 1980:1981){
    # count women by age group to normalize hiv incidence
    #i=1980
    yr = i
    c15=0
    c20=0
    c25=0
    c30=0
    c35=0
    c40=0
    c45=0
    #if(i>1974){
    x = is.na(w[,"death_date"]) & yr-w[,"dob"]<20 & yr-w[,"dob"]>14
    x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<20 & yr-w[,"dob"]>14 & yr<w[,"death_date"] 
    c15 =   sum(x==T) + sum(x1==T)
    x = is.na(w[,"death_date"]) & yr-w[,"dob"]<25 & yr-w[,"dob"]>19
    x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<25 & yr-w[,"dob"]>19 & yr<w[,"death_date"] 
    c20 =   sum(x==T) + sum(x1==T)
    x = is.na(w[,"death_date"]) & yr-w[,"dob"]<30 & yr-w[,"dob"]>24
    x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<30 & yr-w[,"dob"]>24 & yr<w[,"death_date"] 
    c25 =   sum(x==T) + sum(x1==T)
    x = is.na(w[,"death_date"]) & yr-w[,"dob"]<35 & yr-w[,"dob"]>29
    x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<35 & yr-w[,"dob"]>29 & yr<w[,"death_date"] 
    c30 =   sum(x==T) + sum(x1==T)
    x = is.na(w[,"death_date"]) & yr-w[,"dob"]<40 & yr-w[,"dob"]>34
    x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<40 & yr-w[,"dob"]>34 & yr<w[,"death_date"] 
    c35 =   sum(x==T) + sum(x1==T)
    x = is.na(w[,"death_date"]) & yr-w[,"dob"]<45 & yr-w[,"dob"]>39
    x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<45 & yr-w[,"dob"]>39 & yr<w[,"death_date"] 
    c40 =   sum(x==T) + sum(x1==T)
    x = is.na(w[,"death_date"]) & yr-w[,"dob"]<50 & yr-w[,"dob"]>44
    x1 = !is.na(w[,"death_date"]) & yr-w[,"dob"]<50 & yr-w[,"dob"]>44 & yr<w[,"death_date"] 
    c45 =   sum(x==T) + sum(x1==T)
    #}
    
    # Update vector of HIV incidence rates
    rm(a,y)
    prob.hiv.vec <- matrix(NA,length(ages),length(years))
    row.names(prob.hiv.vec) <- ages
    colnames(prob.hiv.vec) <- years
    #head(prob.hiv.vec)
    for (a in ages) {
      for (y in years) {
        res <- as.numeric(prob.hiv(a,y,hivinc_s,c15,c20,c25,c30,c35,c40,c45))
        if (length(res)>0 ){prob.hiv.vec[as.character(a),as.character(y)] <- res
        }else {
          prob.hiv.vec[as.character(a),as.character(y)] <- 0
        }
      }
    }    
    
    # Fertility  
    w[,"age"] = i-w[,"dob"]
    prob.birth.thisyear <- prob.birth.all[as.character(i),as.character(w[,"age"])]
    prob.birth.thisyear.hiv.art <- prob.birth.thisyear*prob.birth.hiv[as.character(w[,"age"]),"1"]
    prob.birth.thisyear.hiv.noart <- prob.birth.thisyear*prob.birth.hiv[as.character(w[,"age"]),"0"]
    prob.birth.thisyear.adj <- (1-w[,"male"])*(prob.birth.thisyear*(1-w[,"hiv"]) + prob.birth.thisyear.hiv.art*w[,'art'] + prob.birth.thisyear.hiv.noart*(1-w[,'art'])*w[,'hiv'])
    # Create vector of TRUE or FALSE
    newbaby <- runif(nrow(w))<prob.birth.thisyear.adj
    # Create matrix that is 'num of babies' by 'num of columns in women matrix'
    nextbabies <- matrix(NA,length(which(newbaby)),length(w[1,]))
    colnames(nextbabies) <- colnames(w)
    nextbabies[,"momid"] <- w[newbaby,"id"]
    nextbabies[,"momage"] <- w[newbaby,"age"]
    nextbabies[,"momhiv"] <- w[newbaby,"hiv"]
    nextbabies[,"art"] <- w[newbaby,"art"]
    nextbabies[,"dob"] <- i
    nextbabies[,"id"] <- seq(max(w[,"id"])+1,max(w[,"id"])+length(nextbabies[,"id"]),1)
    nextbabies[,"age"] <- 0
    nextbabies[,"ceb"] <- 0
    nextbabies[,"cd"] <- 0
    nextbabies[,"male"] <- as.numeric(runif(nrow(nextbabies))<=102.5/202.5)  
    w[newbaby,"ceb"] <- w[newbaby,"ceb"]+1
    
    # births by age group for tfr
    births.age.yr[,"births"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==15] <-sum(nextbabies[,"momage"]<20 & nextbabies[,"momage"]>14)
    births.age.yr[,"births"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==20] <-sum(nextbabies[,"momage"]<25 & nextbabies[,"momage"]>19)
    births.age.yr[,"births"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==25] <-sum(nextbabies[,"momage"]<30 & nextbabies[,"momage"]>24)
    births.age.yr[,"births"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==30] <-sum(nextbabies[,"momage"]<35 & nextbabies[,"momage"]>29)
    births.age.yr[,"births"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==35] <-sum(nextbabies[,"momage"]<40 & nextbabies[,"momage"]>34)
    births.age.yr[,"births"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==40] <-sum(nextbabies[,"momage"]<45 & nextbabies[,"momage"]>39)
    births.age.yr[,"births"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==45] <-sum(nextbabies[,"momage"]<50 & nextbabies[,"momage"]>44)
    
    # births to HIV positive women
    hivbirths.momshiv[,"birthmompos"][hivbirths.momshiv[,"year"]==i]<-sum(nextbabies[,"momhiv"]==1)
    
    # women by age group for tfr
    births.age.yr[,"women"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==15] <-c15
    births.age.yr[,"women"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==20] <-c20
    births.age.yr[,"women"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==25] <-c25
    births.age.yr[,"women"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==30] <-c30
    births.age.yr[,"women"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==35] <-c35
    births.age.yr[,"women"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==40] <-c40
    births.age.yr[,"women"][births.age.yr[,"year"]==i & births.age.yr[,"agegrp"]==45] <-c45
    
    # Vertical transmission of HIV
    prob.vt.thisyear.adj <- prob.vt.noart*(nextbabies[,'momhiv']-nextbabies[,'art'])+prob.vt.art*nextbabies[,'art']
    hivbaby <- runif(nrow(nextbabies))<prob.vt.thisyear.adj
    nextbabies[,"hiv"] <- hivbaby
    
    # HIV positive births tracker for realized VT
    hivbirths.momshiv[,"birthpos"][hivbirths.momshiv[,"year"]==i]<-sum(hivbaby==TRUE)
    
    w <- rbind(w,nextbabies)
    
    # Mortality
    prob.death.thisyear <- prob.death.all[as.character(i),as.character(w[,"age"])]
    prob.death.thisyear.hiv.art <- art.surv.vec(i,w[,"cd4"],w[,"art_date"])
    prob.death.thisyear.hiv.noart <- aidsmort[1+i-w[,"hiv_date"]] # because R counts from 1 and not 0
    
    prob.death.thisyear.hiv.art[is.na(prob.death.thisyear.hiv.art)] <- 0
    prob.death.thisyear.hiv.noart[is.na(prob.death.thisyear.hiv.noart)] <- 0
    
    prob.death.thisyear.adj <- prob.death.thisyear*(1-w[,"hiv"]) + prob.death.thisyear.hiv.art*w[,'art'] + prob.death.thisyear.hiv.noart*(1-w[,'art'])*w[,'hiv']
    died <- runif(nrow(w))<prob.death.thisyear.adj
    diedthisyear <- died & is.na(w[,"death_date"])
    w[diedthisyear,"death_date"] = i
    w[diedthisyear,"hivdeath"] = w[diedthisyear,"hiv"]
    
    # HIV infection
    prob.hiv.thisyear <- prob.hiv.vec[as.character(w[,"age"]),as.character(i)]*(1-w[,"hiv"])
    gothiv <- runif(nrow(w))<prob.hiv.thisyear
    newlyinfected <- gothiv & !diedthisyear
    w[newlyinfected,"hiv"]=TRUE
    w[newlyinfected,"hiv_date"]=i
    
    w[newlyinfected,"cd4"] = rnorm(length(which(newlyinfected)),25.91,.61)^2
    
    cd4decl35 <- rnorm(nrow(w),1.32,1)
    cd4decg35 <- rnorm(nrow(w),2,1)
    
    w[newlyinfected & w[,'age']<35,'cd4dec'] = cd4decl35[newlyinfected & w[,'age']<35]
    w[newlyinfected & w[,'age']>=35,'cd4dec'] = cd4decg35[newlyinfected & w[,'age']>=35]
    
    oldinfected <- w[,"hiv"] & !newlyinfected
    w[oldinfected,"cd4"] <- cd4.prog(w[oldinfected,"cd4"],w[oldinfected,"cd4dec"], w[oldinfected,"hiv_date"],i)
    
    # ART intiation
    noart = is.na(w[,"art_e"]) & w[,"hiv"]
    w[noart,"art_e"] = ifelse(w[noart,"cd4"]<threshold,i,NA)
    
    couldgetart = !is.na(w[,"art_e"]) & !w[,"art"]
    newlyart = runif(nrow(w))<artprobs[artprobs[,"yr"]==i,2]
    w[couldgetart & newlyart,"art"] = TRUE
    w[couldgetart & newlyart,"art_date"] = i
    
  }
  
  # END OF LOOP SIMULATING 'W' ##################################################
  list(w=w,births.age.yr=births.age.yr,hivbirths.momshiv=hivbirths.momshiv)
}