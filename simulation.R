source("simulation_functions.R")

source("indirect_estimates_functions.R")

if(!dir.exists("./results/figdata")){
  dir.create("./results/figdata",recursive = TRUE)
}

if(!dir.exists("./results/regdata")){
  dir.create("./results/regdata",recursive = TRUE)
}

if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(dplyr)


if(!require(foreach)){
  install.packages("foreach",dependencies = TRUE,repos='http://cran.us.r-project.org')
}

require(foreach)


if(!require(iterators)){
  install.packages("iterators",dependencies = TRUE,repos='http://cran.us.r-project.org')
}

require(iterators)

if(!require(parallel)){
  install.packages("parallel",dependencies = TRUE,repos='http://cran.us.r-project.org')
}

require(parallel)

# The code in this script does an extensive use of the foreach package for better performance we need to register a 
# parallel backend. Here we use the doRedis package that offers an interface to a Redis server installed
# on the computer. The number of workers should be adjusted to the memory and cpu resources on the computer.
# 
if(!require(doRedis)){
  install.packages("doRedis",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(doRedis)

redisConnect()

removeQueue("process")

registerDoRedis("process")

startLocalWorkers(n=detectCores(),queue="process")

options('redis:num'=TRUE)

##### SIMULATION #####

# Read data

hivhogan = read.csv("./data/inc_curves.csv",head=TRUE)
mort_series = read.csv("./data/IHME_female_mortSMALL.csv",head=TRUE)
adultmort = read.csv("./data/MLTfemSMALL.csv",head=TRUE)
worldfert= read.csv("./data/world_fert.csv",head=TRUE)
tfr_series= read.csv("./data/tfr_gapminder_long.csv",head=TRUE)
art_series= read.csv("./data/sampleART.csv",head=TRUE)
u5m_edit= read.csv("./data/u5m_edit.csv",head=TRUE)
matmort = read.csv("./data/matmort.csv",head=TRUE)



# Add HIV-free populations to hivhogan
hivhogan$Country <- factor(hivhogan$Country,levels=c(levels(hivhogan$Country),"zero"))
hivhogan[63,1] = "zero"
hivhogan[63,c(2:47)]=0


# Create parameters sets




fertcountry <- c("Botswana","Uganda")
cm_cntry <-  c("Mali","Morocco")
am_cntry <-  c("Madagascar","Sudan")
sexactive15 <-  c(30,70)
mmr0 <- c(0.0012,0.012)
mmr_dec <- c(0,0.073) # annual percent decline in MMR
curve <- c("BotswanaUrban","LesothoRural","MalawiRural","UgandaRural","CamerounRural","zero","BotUrb2x")
bfeed <- c(6,18)
art_col <- c("zero","Botswana","Cameroon","Malawi","Bot_dub")
growth <-  c(0.03)
yrend <- 2010
# Starting year
yrstart <- 1906

# CD4 threshold

threshold <- 200


# expand.grid() will create dataset with unique combinations in each row

inputs <- expand.grid(fertcountry, cm_cntry, am_cntry, sexactive15,mmr0,mmr_dec,curve,bfeed,art_col,growth,yrend,yrstart,threshold)
names(inputs) = c("fertcountry", "cm_cntry","am_cntry","sexactive15","mmr0","mmr_dec","curve","bfeed","art_col","growth","yrend","yrstart","threshold")

save(inputs,file = "results/models/inputs.RData")

file_number_format <- paste0("%0",nchar(as.character(nrow(inputs))),"d")

# Parameter sets already processed

# psets_already <- table(
#   c(
#    as.integer(gsub("regdata\\.|\\.Rdata","",list.files("./results/regdata",pattern = "regdata.\\d+",full.names = FALSE))),
#    as.integer(gsub("figdata\\.|\\.Rdata","",list.files("./results/figdata",pattern = "figdata.\\d+",full.names = FALSE))))
# 
# )
# 
# psets_already <- data.frame(psets_already)
# 
# psets_already <- as.integer(as.character(psets_already[psets_already$Freq==2,"Var1"]))

psets_already <- as.integer(gsub("models|\\.Rdata","",list.files("./results/models",pattern = "models\\d+",full.names = FALSE)))

psets_to_process <- setdiff(1:nrow(inputs),psets_already)

#Set size of initial population

ip<- 22500

years <- c(1906:2010)
ages <- c(0:120)


# Run simulation for each set of parameters

foreach(pset=psets_to_process,.packages = c("dplyr")) %dopar%{
  # pset <- psets_to_process[1]
  inp <- inputs[pset,]
  
  source("simulation_functions.R")
  
  source("indirect_estimates_functions.R")
  
  
  
  # Generate population
  set.seed(1000)
  results <- bigsim(inp,initialpop=ip,years,ages,hivhogan,mort_series,adultmort,worldfert,tfr_series,art_series,u5m_edit,matmort)
  
  save(results,file=file.path("results/models",paste("models",sprintf(file_number_format,pset),".Rdata",sep="")))
}


hiv2000 <- foreach(pset=psets_to_process,.packages = c("dplyr")) %dopar%{
  # pset <- psets_to_process[1]
  
  c("p7500","p15000","p22500") %>% lapply(function(popini){
    load(file.path("results/models/",popini,paste("models",sprintf(file_number_format,pset),".Rdata",sep="")))
    r <- results[[1]]$hiv2000
    names(r) <- popini
    r
  }) %>% unlist() %>% t %>% data.frame(input_set=pset,.)
  
  
} %>% bind_rows

hiv2000 <- hiv2000 %>% mutate(p7500_minus_p15000=p7500-p15000,p15000_minus_p22500=p15000-p22500)

save(hiv2000,file = "results/hiv2000.RData")



##### INDIRECT ESTIMATES #####

psets_already <- intersect(as.integer(gsub("figdata.|\\.Rdata","",list.files("./results/figdata/p22500",pattern = "figdata.\\d+",full.names = FALSE))),
                           as.integer(gsub("regdata.|\\.Rdata","",list.files("./results/regdata/p22500",pattern = "regdata.\\d+",full.names = FALSE))))

psets_to_process <- setdiff(1:nrow(inputs),psets_already)


foreach(pset=psets_to_process,.packages = c("dplyr")) %dopar%{
  # pset <- psets_to_process[1]
  source("indirect_estimates_functions.R")
  
    load(file.path("results/models/p22500",paste("models",sprintf(file_number_format,pset),".Rdata",sep="")))

  
  w <- results[[7]]
  
  # Are babies of HIV moms getting HIV?
  # What percent of HIV moms babies have HIV?
  # How many born with HIV? 
  # df = as.data.frame(w)
  # y <- subset(df, hiv_date==2009)
  # plot(df$dob, df$hiv_date)  
  ##################################################  
  
  # format data
  momkidsclean <- as.data.frame(w)
  
  # Using all surviving women
  isf <-ind_est(momkidsclean)
  
  
  # Using all women
  isf_all <-ind_est_all(momkidsclean)
  
  #Using surviving women and women who died from HIV
  isf_hiv <- ind_est_hiv(momkidsclean)
  
  ie.three <- cbind(isf,isf_all,isf_hiv,pset)
  
  ###################################################################
  # Save results for regressions and for figures
  ###################################################################
  
  
inp.plus <- as.data.frame(results[1])    
regdata <- cbind(ie.three,inp.plus[1,])
figdata <- results[c(2:6,8:9)]
  
save(regdata,file=file.path("results/regdata/p22500",paste("regdata.",sprintf(file_number_format,pset),".Rdata",sep="")))
save(figdata,file=file.path("results/figdata/p22500",paste("figdata.",sprintf(file_number_format,pset),".Rdata",sep="")))

rm(results,regdata,figdata,momkidsclean,isf,isf_all,isf_hiv,ie.three)
}

##### COLLECT DATA FROM ALL SIMULATIONS #####


# Merge regdata

psets_to_process <- as.integer(gsub("regdata.|\\.Rdata","",list.files("./results/regdata/p22500",pattern = "regdata.\\d+",full.names = FALSE))) %>% sort

nbd2k <- psets_to_process %>% lapply(function(pset){
  # pset <- 1
  filename <- file.path("results/regdata/p22500",paste("regdata.",sprintf(file_number_format,pset),".Rdata",sep=""))
  tryCatch({
    load(filename)
    
    regdata %>% mutate(i=pset) %>% select(i,everything())
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}) %>% bind_rows() %>% as.data.frame()


save(nbd2k,file="./results/regdata/p22500/regdata_all.Rdata")



h <- list()
tf<- list()
ar<- list()
hd<- list()
u5m <- list()
inps <- list()
arp <- list()
u5mhiv <- list()
hivinc <- list()


for(filename in sort(list.files("./results/figdata",pattern = "figdata.\\d+",full.names = TRUE))){
  
  tryCatch({
    load(filename)
    
    i <- as.integer(gsub("figdata\\.|\\.Rdata","",basename(filename)))
    h[[i]] <- figdata[[3]]
    tf[[i]] <- figdata[[1]]
    ar[[i]] <- figdata[[2]]
    hd[[i]] <- figdata[[4]]
    u5m[[i]] <- figdata[[5]]
    inps[[i]] <- figdata[[6]]
    arp[[i]] <- figdata[[7]]
    u5mhiv[[i]] <- figdata[[8]]
    hivinc[[i]] <- figdata[[9]]
    rm(figdata)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

save(h,tf,ar,hd,u5m,inps,arp,u5mhiv,hivinc,file="./results/figdata_all.Rdata")



nbd2k=NA

# Merge regdata

psets_to_process <- 1:nrow(inputs)


psets_to_process %>% lapply(function(pset) {
  
  tryCatch({
    load(filename)
    nbd2k <- rbind(nbd2k,regdata)
    rm(regdata)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

nbd2k <- nbd2k[2:length(nbd2k[,1]),]

save(nbd2k,file="./results/regdata_all.Rdata")
