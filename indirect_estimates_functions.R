abc = read.csv("./data/UNgeneral_MLT_ABC.csv", head=TRUE)
efg = read.csv("./data/UNgeneral_MLT_EFG.csv", head=TRUE)


ind_est_computations <- function(surv_moms,survey){
  
  #Sum women, CEB, CD by agegroup & caclulate CD/CEB & parity ratios
  
  byagegroup <- surv_moms %>% group_by(agegroup) %>% 
    summarise(totalceb=sum(ceb),cd=sum(cd),women=length(cd)) %>% 
    ungroup() %>%
    mutate(cdceb=cd/totalceb,
           ceb=totalceb/women,
           mpagegroup=seq(17,47,5),
           fxplus2=ceb*mpagegroup
    ) %>% as.data.frame()
  
  
  # totalceb <- tapply(surv_moms$ceb, surv_moms$agegroup,sum)
  # cd <- tapply(surv_moms$cd, surv_moms$agegroup,sum)
  # women <- tapply(surv_moms$cd, surv_moms$agegroup,length)
  # cdceb <- cd/totalceb
  # ceb <- totalceb/women
  # mpagegroup <- seq(17,47,5)
  # fxplus2 <- ceb*mpagegroup
  m <- byagegroup %>% summarise(m=sum(fxplus2)/sum(ceb)) %>% pull(m)
  # byagegroup = cbind(c(1:7),totalceb,cd,women,cdceb,ceb)
  
  #Parity ratios
  p1520 = byagegroup[byagegroup$agegroup==1,"ceb"]/byagegroup[byagegroup$agegroup==2,"ceb"]
  p2025 = byagegroup[byagegroup$agegroup==2,"ceb"]/byagegroup[byagegroup$agegroup==3,"ceb"]
  
  
  # SURVIVING WOMEN: Indirect estimation of U5M 
  nq0=vector()
  # Choose life table (1=north, 2=south, 3=east,4=west)
  lt = 1
  # Estimate n_q_0   
  for(i in 1:7){
    nq0[i] = (abc[lt,i+2]+ abc[lt+1,i+2]*p1520 + abc[lt+2,i+2]*p2025 +abc[lt+3,i+2]*m) * byagegroup[byagegroup$agegroup==i,"cdceb"]
  }
  
  
  
  
  #Estimate time reference
  t_ref = matrix()
  for(i in 1:7){
    t_ref[i] = efg[lt,i+2]+ efg[lt+1,i+2]*p1520 + efg[lt+2,i+2]*p2025 
  }
  
  #Convert each n q 0 into 5q0
  # Use UN General Female, e(0)=60
  #n = c(1,2,3,5,10,15,20)
  l = c(.92688,.91025,.90151,.89193,.89534,.89002,.88195)
  q = 1-l
  logitMLT=.5*log(q/(1-q))
  
  alpha = NA
  fiveq0 = NA
  logit_nq0 = NA
  
  
  for(i in 1:7){
    logit_nq0[i] = .5*log(nq0[i]/(1-nq0[i]))
    alpha[i] = logit_nq0[i] - logitMLT[i]
    fiveq0[i] = exp(2*(alpha[i]+logitMLT[4]))/(1+exp(2*(alpha[i]+logitMLT[4])))
  }
  
  ind_est <- cbind(fiveq0,t_ref)
  refdate <- survey-ind_est[,"t_ref"]
  ind_est <- cbind(fiveq0,t_ref,refdate,byagegroup[,"ceb"],byagegroup[,"women"],byagegroup[,"cdceb"])
  
  colnames(ind_est) = c("fiveq0","t_ref","refdate","ceb","women","cdceb")
  
  
  return(ind_est)
}

ind_est <- function(momkidsclean){
  
  # Keep only surviving women
  surv_momsfull = momkidsclean[is.na(momkidsclean$death_date),]
  #dead_moms = momkidsclean[!is.na(momkidsclean$death_date),]
  
  #Create age group variable
  yrend = 2010
  survey = yrend  
  surv_momsfull$endage <- survey-surv_momsfull$dob
  surv_momsfull <- surv_momsfull[surv_momsfull$endage<50,]
  surv_momsfull <- surv_momsfull[surv_momsfull$endage>14,]
  surv_momsfull <- surv_momsfull[surv_momsfull$male==0,]
  
  #Use all surviving women
  surv_moms <- surv_momsfull
  
  cutoffs <- c(49,45, 40, 35, 30, 25, 20, 15)
  surv_moms$agegroup <- cut(surv_moms$endage,cutoffs, right=FALSE, include.lowest=TRUE)
  
  ind_est <- ind_est_computations(surv_moms,survey)
  
  colnames(ind_est) = c("fiveq0_surv","t_ref_surv","refdate_surv","ceb_surv","women_surv","cdceb_surv")
  
  
  return(ind_est)
}




#############################################
### USING ALL WOMEN
###############################################

ind_est_all <- function(momkidsclean){
  
  # RANDOM SAMPLE OF SURVIVING WOMEN: Calculate CEB/CS 
  all_momsfull = momkidsclean
  
  #surv_momsfull = momkidsclean[is.na(momkidsclean$death_date),]
  #dead_moms = momkidsclean[!is.na(momkidsclean$death_date),]
  
  #Create age group variable
  yrend = 2010
  survey = yrend  
  all_momsfull$endage <- survey-all_momsfull$dob
  all_momsfull <- all_momsfull[all_momsfull$endage<50,]
  all_momsfull <- all_momsfull[all_momsfull$endage>14,]
  all_momsfull <- all_momsfull[all_momsfull$male==0,]
  
  #Random sample from living women 15-49 at time of survey
  all_moms <- all_momsfull
  
  cutoffs <- c(49,45, 40, 35, 30, 25, 20, 15)
  all_moms$agegroup <- cut(all_moms$endage,cutoffs, right=FALSE, include.lowest=TRUE)
  
 
  ind_est <- ind_est_computations(surv_moms,survey)
  
  colnames(ind_est) = c("fiveq0_all","t_ref_all","refdate_all","ceb_all","women_all","cdceb_all")
  return(ind_est)
}

#############################################################
### USING SURVIVING WOMEN AND WOMEN WHO DIED FROM HIV
#########################################################

ind_est_hiv <- function(momkidsclean){
  
  # Keep surviving moms and moms who died from HIV
  #Surviving moms
  surv_momsfull = momkidsclean[is.na(momkidsclean$death_date),]
  #Moms who died from HIV
  hiv_momsfull = momkidsclean[momkidsclean$hivdeath==1 & !is.na(momkidsclean$hivdeath),]
  all_momsfull <- rbind(surv_momsfull,hiv_momsfull)
  
  #surv_momsfull = momkidsclean[is.na(momkidsclean$death_date),]
  #dead_moms = momkidsclean[!is.na(momkidsclean$death_date),]
  
  #Create age group variable
  yrend = 2010
  survey = yrend  
  all_momsfull$endage <- survey-all_momsfull$dob
  all_momsfull <- all_momsfull[all_momsfull$endage<50,]
  all_momsfull <- all_momsfull[all_momsfull$endage>14,]
  all_momsfull <- all_momsfull[all_momsfull$male==0,]
  
  #Random sample from living women 15-49 at time of survey
  all_moms <- all_momsfull
  
  cutoffs <- c(49,45, 40, 35, 30, 25, 20, 15)
  all_moms$agegroup <- cut(all_moms$endage,cutoffs, right=FALSE, include.lowest=TRUE)
  
  ind_est <- ind_est_computations(surv_moms,survey)
  
  colnames(ind_est) = c("fiveq0_hiv","t_ref_hiv","refdate_hiv","ceb_hiv","women_hiv","cdceb_hiv")
  return(ind_est)
}
