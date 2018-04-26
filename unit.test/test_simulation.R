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


test_prob.birth.years.ages_01 <- function(){
  
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
  
  
  test <- prob.birth.years.ages(ages,years,asfr_s,tfr_s)
  
  
  
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