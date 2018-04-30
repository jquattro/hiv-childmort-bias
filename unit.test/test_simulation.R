test_prob.birth_01 <- function(){
  
  fertcountry <- "Botswana"
  
  worldfert <-  read.csv(file.path(base.path,"data/world_fert.csv"),head=TRUE)
  tfr_series <-  read.csv(file.path(base.path,"data/tfr_gapminder_long.csv"),head=TRUE)
  
  tfr_s <-  tfr_series[tfr_series[,"country"]==fertcountry,]
  asfr_s <- worldfert[worldfert[,"country"]==fertcountry,]
  
  current_year <- 2000
  
  ages <- c(seq(12,52,5),15,50)
  
  target <- lapply(ages,function(age) {
    # age <- ages[2]
    
    age <- cut(age,breaks = c(15,20,25,30,35,40,45,50),right = FALSE,labels = c("15","20","25","30","35","40","45"))
    
    if(!is.na(age)){
      tfr_current <- tfr_s %>% filter(year==current_year) %>% pull("tfr")
      nearest_year <- asfr_s$refyr[which.min(abs(asfr_s$refyr-current_year))]
      tfr_nearest <- asfr_s %>% filter(refyr==nearest_year) %>% pull("tfr")
      asfr_nearest <- asfr_s %>% filter(refyr==nearest_year) %>% pull(paste0("asfr",age))
      
      prob <- asfr_nearest*(tfr_current/tfr_nearest)/1000
    }
    
    
    
  }) %>% unlist
  
  test <- lapply(ages,function(age) {
    
    
    prob.birth(age,current_year,asfr_s,tfr_s)
    
    
  }) %>% unlist
  
  checkEquals(target,test)
  
}


test_prob.birth.ages.years_01 <- function(){
  
  fertcountry <- "Botswana"
  
  worldfert <-  read.csv(file.path(base.path,"data/world_fert.csv"),head=TRUE)
  tfr_series <-  read.csv(file.path(base.path,"data/tfr_gapminder_long.csv"),head=TRUE)
  
  tfr_s <-  tfr_series[tfr_series[,"country"]==fertcountry,]
  asfr_s <- worldfert[worldfert[,"country"]==fertcountry,]
  
  years <- c(1946:2010)
  ages <- c(-100:120)
  
  
  target <- expand.grid(years,ages) %>% apply(1,function(x){
    
    pr.birth <- prob.birth(x[2],x[1],asfr_s,tfr_s)
    
    data.frame(age=x[2],year=x[1],pr.birth=ifelse(is.null(pr.birth),0,pr.birth))
    
  }) %>% bind_rows() %>% spread(age,pr.birth) %>% set_rownames(.$year) %>% select(-year) %>% as.matrix
  
  
  test <- prob.birth.ages.years(ages,years,asfr_s,tfr_s)
  
  
  
  checkEquals(target,test)
  
}

# ART 0
test_fert_hiv_01 <- function(){
  
  
  ages <- seq(17,47,5)
  
  sexactive15 <- 0.75
  ages_cat <-cut(ages,breaks = c(15,20,25,30,35,40,45,50),right = FALSE,labels = c("15","20","25","30","35","40","45"))
  
  
  target <-  case_when(
    ages_cat== "15" ~ 2.528-.031*sexactive15, # aged 15-19
    ages_cat=="20" ~ .765, # aged 20-24
    ages_cat=="25" ~ .706, # aged 25-29
    ages_cat=="30" ~ .647, # aged 30-34
    ages_cat=="35" ~ .588, # aged 35-39
    ages_cat=="40" ~ .529, # aged 40-44
    ages_cat=="45" ~ .470 # aged 45-49
    
  )
  
  
  test <- lapply(ages,function(age){
    fert_hiv(age,sexactive15,0)
  }) %>% unlist
  
  
  checkEquals(target,test)
}


# ART 1
test_fert_hiv_02 <- function(){
  
  
  
  ages <- seq(17,47,5)
  
  sexactive15 <- 0.75
  ages_cat <-cut(ages,breaks = c(15,20,25,30,35,40,45,50),right = FALSE,labels = c("15","20","25","30","35","40","45"))
  
  
  target <-  case_when(
    ages_cat== "15" ~ 2.528-.031*sexactive15, # aged 15-19
    ages_cat=="20" ~ .765, # aged 20-24
    ages_cat=="25" ~ .706, # aged 25-29
    ages_cat=="30" ~ .647, # aged 30-34
    ages_cat=="35" ~ .588, # aged 35-39
    ages_cat=="40" ~ .529, # aged 40-44
    ages_cat=="45" ~ .470 # aged 45-49
    
  )
  
  target <- ifelse(ages_cat!="15",(1-target)/2+target,target)
  
  test <- lapply(ages,function(age){
    fert_hiv(age,sexactive15,1)
  }) %>% unlist
  
  
  checkEquals(target,test)
  
}

test_prob.birth.hiv_01 <- function(){
  
  arts <- c(0,1)
  ages <- c(-100:120)
  
  sexactive15 <- 0.6
  
  target <- expand.grid(arts,ages) %>% apply(1,function(x){
    
    pr.reduction <- fert_hiv(x[2],sexactive15,x[1])
    
    data.frame(age=x[2],art=x[1],fert_hiv=ifelse(is.null(pr.reduction),0,pr.reduction))
    
  }) %>% bind_rows() %>% spread(art,fert_hiv) %>% set_rownames(.$age) %>% select(-age) %>% as.matrix
  
  
  test <- prob.birth.hiv(ages,sexactive15,arts)
  
  
  
  checkEquals(target,test)
}


test_baby.death.nohiv_01 <- function(){
  
  years <- c(1946:2010)
  ages <- c(-100:120)
  
  cm_cntry <- "Mali"
  
  u5m_edit<- read.csv(file.path(base.path,"data/u5m_edit.csv"),head=TRUE) 
  u5m_c <- subset(u5m_edit, country==cm_cntry)
  
  target <- expand.grid(years,ages) %>% apply(1,function(x){
    
    yr <- max(x[1],min(u5m_c$year))
    
    
    data.frame(age=x[2],year=x[1],mort=ifelse(x[2] <0 | x[2] >4,0,u5m_c[u5m_c$year==yr,paste0("q1_",as.integer(x[2]))]))
    
  }) %>% bind_rows() %>% spread(age,mort) %>% set_rownames(.$year) %>% select(-year) %>% as.matrix
  
  
  test <- baby.death.nohiv(ages,years,u5m_c)
  
  
  
  checkEquals(target,test)
  
  
  
}


test_phivneg.death_01 <- function(){
  
  matmort = read.csv(file.path(base.path,"data/matmort.csv"),head=TRUE)
  mort_series = read.csv(file.path(base.path,"data/IHME_female_mortSMALL.csv"),head=TRUE)
  adultmort = read.csv(file.path(base.path,"data/MLTfemSMALL.csv"),head=TRUE)
  
  yr <- 1989
  
  ages <- c(-1,1:10,seq(12,52,5))
  
  am_cntry <- "Madagascar"
  
  cm_cntry <- "Mali"
  
  
  mort_s <- mort_series %>% filter(country==am_cntry)
  
  
  u5m_edit<- read.csv(file.path(base.path,"data/u5m_edit.csv"),head=TRUE) 
  u5m_c <- subset(u5m_edit, country==cm_cntry)
  
  # Get 45q15 from the Institute for Health Metrics and Evaluation for the model country
  #  during the simulation year. If the simulation year is earlier than 1970, get 1970 value.
  
  
  
  q45_15 <-  mort_s %>% filter(year==max(yr,1970)) %>% pull("q45_15")
  
  # Get E.0 from the UN model life table with the closest 45q15 (rounds down in case of tie) to that 
  # in Institute for Health Metrics series
  
  min45 <- which.min(abs(adultmort[,"q45_15"]-q45_15))
  e0 <- adultmort[min45,"E.0."]
  
  adultmort <- adultmort %>% filter(E.0.==e0)
  # Compute probability
  
  
  c_pd5yr <- function(x,adultmort) adultmort %>% filter(age==x) %>% pull("q.x.n.")
  
  c_q1_y <- function(x,adultmort) 1-(1-c_pd5yr(x,adultmort))^(1/5)
  
  # Probability 7 years
  
  c_q1_7 <- function(adultmort) c_q1_y(5,adultmort)
  
  # Probability 10 years
  
  c_q1_10 <- function(adultmort) c_q1_y(10,adultmort)
  
  # Probability 4 years
  u5m_c_min_year <- min(u5m_c$year)
  u5m_c_year <- max(yr,u5m_c_min_year)
  c_q1_4 <- function(u5m_c,y) u5m_c %>% filter(year==u5m_c_year) %>% pull("q1_4")
  
  target <- lapply(ages,function(age){
    
    age_cat <-cut(age,breaks = c(15,20,25,30,35,40,45,50),right = FALSE,labels = c("15","20","25","30","35","40","45"))
     
    if(age >=0 & age <=4){  
      result <- u5m_c %>% filter(year==u5m_c_year) %>% pull(paste0("q1_",as.integer(age)))
    }else if (age>=5 & age <=6){
      result <- approx(x=c(4,7),y=c(c_q1_4(u5m_c,yr),c_q1_7(adultmort)),age,method="linear")$y
    }else if(age==7){
      result <- c_q1_7(adultmort)
    }else if(age>=8 & age <=9) {
      result <- approx(x=c(7,10),y=c(c_q1_7(adultmort),c_q1_10(adultmort)),age,method="linear")$y
    }else if(age>=10 & age <=14){
      result <- c_q1_10(adultmort)
    }else if(age >=15 & age <=49){
      result <- c_q1_y(as.integer(as.character(age_cat)),adultmort)- matmort[,paste("X",yr,sep="")][matmort$Country==am_cntry & matmort$Agegroup==as.integer(age_cat)]
    }else{
      result <- 0
    }
    
    
    result
    
  }) %>% unlist
  
  
  
  test <- lapply(ages,function(age) phivneg.death(age,yr,mort_s,adultmort,am_cntry,matmort,u5m_c)) %>% unlist
  
  checkEquals(target,test)
}


test_phivneg.death.ages.years_01 <- function(){
  
  matmort = read.csv(file.path(base.path,"data/matmort.csv"),head=TRUE)
  mort_series = read.csv(file.path(base.path,"data/IHME_female_mortSMALL.csv"),head=TRUE)
  adultmort = read.csv(file.path(base.path,"data/MLTfemSMALL.csv"),head=TRUE)
  
  years <- c(1946:2010)
  ages <- c(-100:120)
  
  
  am_cntry <- "Madagascar"
  
  cm_cntry <- "Mali"
  
  
  mort_s <- mort_series %>% filter(country==am_cntry)
  
  
  u5m_edit<- read.csv(file.path(base.path,"data/u5m_edit.csv"),head=TRUE) 
  u5m_c <- subset(u5m_edit, country==cm_cntry)
  
  target <- expand.grid(years,ages) %>% apply(1,function(x){
    
    result <- phivneg.death(x[2],x[1],mort_s,adultmort,am_cntry,matmort,u5m_c)
    
    data.frame(age=x[2],year=x[1],result=ifelse(is.null(result),0,result))
    
  }) %>% bind_rows() %>% spread(age,result) %>% set_rownames(.$year) %>% select(-year) %>% as.matrix
  
  
  test <- phivneg.death.ages.years(ages,years,mort_s,adultmort,am_cntry,matmort,u5m_c)
  
  
  
  checkEquals(target,test)
  
}