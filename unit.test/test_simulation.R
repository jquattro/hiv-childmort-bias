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


test_art.surv.vec_01 <- function(){
  
  shape=1.6
  
  set.seed(12345)
  
  year <- 1984
  
  art_date <- year - sample(c(1:10),100,replace=TRUE)
  
  cd4 <- sample(c(25,75,150,225,400),100,replace=TRUE)
  
  
  
  
  set.seed(12345)
  
  
  hvl <- ifelse(runif(length(cd4))<0.5,1,0)
  
  y1_prob <- list(hvl=c(CD450=.109,CD4100=.67,CD4200=.46,CD4350=.17,CD4G350=.17),
       lvl=c(CD450=.109,CD4100=.67,CD4200=.46,CD4350=.17,CD4G350=.17))
  
  weib_scale <- list(hvl=c(CD450=13.7,CD4100=16,CD4200=16.9,CD4350=23.3,CD4G350=33.3),
                  lvl=c(CD450=24.4,CD4100=28.4,CD4200=30.1,CD4350=41.4,CD4G350=59.1))
  
  
  target <- data.frame(hvl=hvl,cd4=cd4,art_date=art_date) %>%
    mutate(t=year-art_date,
           cd4_cat=factor(findInterval(cd4,c(50,100,200,350),left.open=FALSE),labels = c("CD450","CD4100","CD4200","CD4350","CD4G350")),
           hvl_cat=ifelse(hvl==1,"hvl","lvl")) %>% 
    rowwise() %>%
    mutate(result=
      case_when(t==1 ~ y1_prob[[hvl_cat]][[cd4_cat]],
                t>1 ~ weib.tp(t,weib_scale[[hvl_cat]][[cd4_cat]],shape)
                )
    ) %>% pull("result")
  
  set.seed(12345)
  
  test <- art.surv.vec(year,cd4,art_date)

  
  checkEquals(target,test)    
}

test_phiv.death_01 <- function(){
  
  
  target <- c(0.01,0.02020202,0.041237113,0.053763441,0.079545455,0.098765432,0.123287671,0.15625,0.185185185,0.204545455,0.228571429,	0.296296296,0.263157895,0.357142857,0.333333333,0.333333333,0.5,0.5,1)
  
  year <- 1993
  
  hiv_dates <- 1993-c(0:18)

  test <- lapply(hiv_dates,function(hiv_date) phiv.death(hiv_date,year)) %>% unlist

  checkEquals(target,test)
      
}


test_baby.death.hiv_01 <- function(){
  
  
  ages <- -1:5
  
  target <-  c(0,0.376,  0.2019,	0.1184,	0.07061,	0.0588,0)  
  names(target) <- ages
  
  test <- baby.death.hiv(ages)
  
  checkEquals(target,test)
}


test_prob.hiv_01 <- function(){
  
  hivhogan = read.csv(file.path(base.path,"data/inc_curves.csv"),head=TRUE)
  
  curve= "BotswanaUrban"
  hivinc_s = hivhogan[hivhogan$Country==curve,]  

  set.seed(54343)
    
  ages_dist <- sample(paste0("c",seq(15,45,5)),100,replace=TRUE) %>% table() 
  
  ages <- seq(17,52,5)
  
  year <- 1984
  
  hivcol = (year-1970)+2
  hivinc = hivinc_s[hivcol] %>% as.numeric
  
  rel_inc <- c(.594,1.325,1,.752,.635,.551,.356)
  
  nomraliz <- ages_dist %>% prop.table() %>% multiply_by(rel_inc) %>% sum %>% as.vector
  
  incidence_ref <- hivinc/nomraliz 
  
  target <- c(rel_inc*incidence_ref,0)
  
  test <- lapply(ages, function(age) prob.hiv(age,year,hivinc_s,ages_dist["c15"],ages_dist["c20"],ages_dist["c25"],ages_dist["c30"],ages_dist["c35"],ages_dist["c40"],ages_dist["c45"])) %>% unlist %>% as.vector

  checkEquals(target,test)
    
}

test_prob.hiv.ages.years_01 <- function(){
  
  years <- c(1946:2010)
  ages <- c(-100:120)
  
  hivhogan = read.csv(file.path(base.path,"data/inc_curves.csv"),head=TRUE)
  
  curve= "BotswanaUrban"
  hivinc_s = hivhogan[hivhogan$Country==curve,]  
  
  set.seed(54343)
  
  ages_dist <- sample(paste0("c",seq(15,45,5)),100,replace=TRUE) %>% table() 
  
  target <- expand.grid(years,ages) %>% apply(1,function(x){
    
    pr <- prob.hiv(x[2],x[1],hivinc_s,ages_dist["c15"],ages_dist["c20"],ages_dist["c25"],ages_dist["c30"],ages_dist["c35"],ages_dist["c40"],ages_dist["c45"]) %>% as.numeric
    
    data.frame(age=x[2],year=x[1],result=ifelse(length(pr)==0,0,pr))
    
  }) %>% bind_rows() %>% spread(year,result) %>% set_rownames(.$age)  %>% select(-age) %>% as.matrix 
  
  

  test <- prob.hiv.ages.years(ages,years,hivinc_s,ages_dist["c15"],ages_dist["c20"],ages_dist["c25"],ages_dist["c30"],ages_dist["c35"],ages_dist["c40"],ages_dist["c45"])

  checkEquals(target,test)    
}


test_vert_trans_01 <- function(){
  
  # no ART
  
  bfeed <- c(6,12,18)
  
  target <- c(.23,.305,.35)
  
  test <- lapply(bfeed,function(x) vert_trans(0,x)) %>% unlist

  checkEquals(target,test)

  # in ART
  
  target <- c(.17,.215,.26)
  
  test <- lapply(bfeed,function(x) vert_trans(1,x)) %>% unlist
  
  checkEquals(target,test)
  
      
}

test_cd4.prog_01 <- function(){
  
  cd4 <- 25.9^2
  
  cd4_dec <- 1.32
  
  year <- 2000
  
  hiv_date <- 1990
  
  target <- (25.9-1.32*10)^2
  
  test <- cd4.prog(cd4,cd4_dec,hiv_date,year)

  checkEquals(target,test)  
    
}


test_init.dob_01 <-function(){
  
  
  initialpop=100
  
  growth=0.03
  
  
  population <- Reduce(function(x,y) x*(1+growth),1:50,accumulate = TRUE)*initialpop %>% floor
  
  target <- mapply(function(pop,year) rep(year,pop), population,1897:(1896+50)) %>% unlist
  
  test <- initial.DOBs(growth,initialpop)
  
  checkEquals(target,test)
  
}

test_women.empty.matrix_01 <- function(){
  
  dobs <- 1:300
  
  target <- data.frame(id=1:length(dobs),
                       ceb=0,
                       hiv=0,
                       hiv_date=NA,
                       art=0,
                       art_date=0,
                       art_e=NA,
                       dob=dobs,
                       momid=NA,
                       male=0,
                       cd=0,
                       dead=0,
                       age=NA,
                       death_date=NA,
                       hivdeath=0,
                       cd4=NA,
                       cd4dec=NA,
                       momage=NA,
                       momhiv=NA) %>%
    select("id", "momid","dob","age","ceb", "hiv","hiv_date","death_date","hivdeath","cd4","cd4dec","art","art_date","art_e","momage","momhiv","male","cd","dead")  %>% 
    as.matrix()
  
  
  
  test <- women.empty.matrix(dobs)

  checkEquals(target,test)
    
}


test_birth.counts.by.age.empty.matrix_01 <- function(){

  yrstart <- 1940
  
  yrend <- 1960
  
  target <- data.frame(year=rep(yrstart:yrend,each=7),agegrp=rep(seq(15,45,5),1960-1940+1),births=NA,women=NA) %>% 
    as.matrix
  

  test <- birth.counts.by.age.empty.matrix(yrstart,yrend)

  checkEquals(target,test)
  
}


test_birth.counts.by.hiv.status.empty.matrix_01 <- function(){
  
  yrstart <- 1940
  
  yrend <- 1960
  
  target <- data.frame(year=yrstart:yrend,birthpos=NA,birthmompos=NA) %>% 
    as.matrix
  
  test <- birth.counts.by.hiv.status.empty.matrix(yrstart,yrend)
  
  checkEquals(target,test)
  
}


test_count.women.age.groups_01 <- function(){
  
  set.seed(5434)
  
  target <- sample(20:50,7) %>% set_names(paste0("c",seq(15,45,5)))
  
  yr <- 1988
 
  
  dobs <- lapply(names(target),function(age_group){
    age <- gsub("c","",age_group) %>% as.integer
    
    rep(yr-age-2,target[age_group])
    
  }) %>% unlist()
  
  w <- data.frame(dob=dobs,death_date=NA) %>% bind_rows(data.frame(dob=dobs,death_date=1980))

  
  test <- count.women.age.groups(yr,w)  
  
  checkEquals(target,test)
}


test_update.women.age_01 <- function(){
  
  yr <- 1988
  set.seed(100)
  w <- data.frame(id=1:10,dob=sample(1950,2000,10),age=NA)

  target <-   w %>% mutate(age=yr-dob) %>%  as.matrix
  
  test <- update.women.age(yr,w%>% as.matrix)
  
  checkEquals(target,test)
  
}


test_new.babies_01 <- function(){
  
  
  fertcountry <- "Botswana"
  
  worldfert <-  read.csv(file.path(base.path,"data/world_fert.csv"),head=TRUE)
  tfr_series <-  read.csv(file.path(base.path,"data/tfr_gapminder_long.csv"),head=TRUE)
  
  tfr_s <-  tfr_series[tfr_series[,"country"]==fertcountry,]
  asfr_s <- worldfert[worldfert[,"country"]==fertcountry,]
  
  years <- c(1946:2010)
  ages <- c(-100:120)
  
  prob.birth.all <- prob.birth.ages.years(ages,years,asfr_s,tfr_s)
  
  arts <- c(0,1)
  
  
  sexactive15 <- 0.6
  
  prob.birth.hiv <-  prob.birth.hiv(ages,sexactive15,arts)
  
  yr <- 1988
  
  w <- expand.grid(hiv=c(0,1),art=c(0,1),male=c(0,1),age=0:60) %>% filter(!(hiv==0 & art==1)) %>% as.matrix
  
  set.seed(100)
  
  target <- w %>% data.frame %>% mutate(prob.birth=case_when(male==1~ 0,
                                        hiv==0 ~ prob.birth.all[as.character(yr),as.character(age)],
                                        hiv==1 & art==0 ~ prob.birth.all[as.character(yr),as.character(age)] * prob.birth.hiv[as.character(age),"0"],
                                        hiv==1 & art==1 ~ prob.birth.all[as.character(yr),as.character(age)] * prob.birth.hiv[as.character(age),"1"]
                                        )) %>% mutate(newbaby=runif(nrow(.))<prob.birth) %>% pull("newbaby")
  
  set.seed(100)
  
  test <- new.babies(yr,w,prob.birth.all,prob.birth.hiv) %>% as.vector

  checkEquals(target,test)    

  
}


test_next.babies_01 <- function(){
  
  
  fertcountry <- "Botswana"
  
  worldfert <-  read.csv(file.path(base.path,"data/world_fert.csv"),head=TRUE)
  tfr_series <-  read.csv(file.path(base.path,"data/tfr_gapminder_long.csv"),head=TRUE)
  
  tfr_s <-  tfr_series[tfr_series[,"country"]==fertcountry,]
  asfr_s <- worldfert[worldfert[,"country"]==fertcountry,]
  
  years <- c(1946:2010)
  ages <- c(-100:120)
  
  prob.birth.all <- prob.birth.ages.years(ages,years,asfr_s,tfr_s)
  
  arts <- c(0,1)
  
  
  sexactive15 <- 0.6
  
  prob.birth.hiv <-  prob.birth.hiv(ages,sexactive15,arts)
  
  yr <- 1988
  set.seed(100)
  w <- expand.grid(hiv=c(0,1),art=c(0,1),male=c(0,1),age=0:60) %>% filter(!(hiv==0 & art==1)) %>% mutate(id=1:nrow(.),momid=NA,momage=NA,momhiv=NA,dob=yr-age,ceb=NA,cd=NA) %>% as.matrix
  
  
  
  newbaby <- new.babies(yr,w,prob.birth.all,prob.birth.hiv) %>% as.vector
  
  set.seed(200)
  target <- w %>% as.data.frame() %>% filter(newbaby) %>% select(momid=id,momage=age,momhiv=hiv,art) %>% mutate(id=(max(w[,"id"])+1):(max(w[,"id"])+nrow(.)),dob=yr,ceb=0,cd=0,male=as.numeric(runif(nrow(.))<=102.5/202.5),age=0,hiv=NA  )
  
  
  set.seed(200)
  
  
  test <- next.babies(yr,w,newbaby)
  
  
  target <- target %>% select(one_of(dimnames(test)[[2]])) %>% as.matrix

  checkEquals(target,test)  
}

test_update.women.ceb_01 <- function(){
  
  set.seed(100)

  w <- data.frame(id=1:10,ceb=sample(1:3,10,replace = TRUE)) %>% as.matrix

  newbaby <- sample(c(TRUE,FALSE),10,replace = TRUE)

  
  target <- w %>% as.data.frame() %>% mutate(ceb=ifelse(newbaby,ceb+1,ceb)) %>% as.matrix
  
  
  test <- update.women.ceb(w,newbaby)
  
  checkEquals(target,test)
}