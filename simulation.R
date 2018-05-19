source("simulation_functions.R")

source("indirect_estimates_functions.R")

if(!dir.exists("./results/figdata")){
  dir.create("./results/figdata",recursive = TRUE)
}

if(!dir.exists("./results/regdata")){
  dir.create("./results/regdata",recursive = TRUE)
}


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
yrstart <- 1946

# CD4 threshold

threshold <- 200


# expand.grid() will create dataset with unique combinations in each row

inputs <- expand.grid(fertcountry, cm_cntry, am_cntry, sexactive15,mmr0,mmr_dec,curve,bfeed,art_col,growth,yrend,yrstart,threshold)
names(inputs) = c("fertcountry", "cm_cntry","am_cntry","sexactive15","mmr0","mmr_dec","curve","bfeed","art_col","growth","yrend","yrstart","threshold")

file_number_format <- paste0("%0",nchar(as.character(nrow(inputs))),"d")

# Parameter sets already processed

psets_already <- table(
  c(
    as.integer(gsub("regdata\\.|\\.Rdata","",list.files("./results/regdata",pattern = "regdata.\\d+",full.names = FALSE))),
    as.integer(gsub("figdata\\.|\\.Rdata","",list.files("./results/figdata",pattern = "figdata.\\d+",full.names = FALSE))))
)

psets_already <- data.frame(psets_already)

psets_already <- as.integer(as.character(psets_already[psets_already$Freq==2,"Var1"]))
           
psets_to_process <- setdiff(1:nrow(inputs),psets_already)
 
#Set size of initial population
 
ip<-10

years <- c(1946:2010)
ages <- c(-100:120)

         
# Run simulation for each set of parameters

for(pset in psets_to_process){
  
  inp <- inputs[pset,]
  
  
  
  
  
  # Generate population
  set.seed(1000)
  
  results <- bigsim(inp,initialpop=ip,years,ages,hivhogan,mort_series,adultmort,worldfert,tfr_series,art_series,u5m_edit,matmort)
  
  
  ##### INDIRECT ESTIMATES #####
  
  
  
  w <- results[[7]]
  
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
  # merge ie.three.Rdata with necessary info from pop20000vec.iZ.Rdata
  ###################################################################
  
  regdata <- cbind(ie.three,results[[1]])
  figdata <- results[c(2:7,10:12)]
  
  save(regdata,file=file.path("results/regdata",paste("regdata.",sprintf(file_number_format,pset),".Rdata",sep="")))
  save(figdata,file=file.path("results/figdata",paste("figdata.",sprintf(file_number_format,pset),".Rdata",sep="")))
  
  rm(results,regdata,figdata,momkidsclean,isf,isf_all,isf_hiv,ie.three)
}

##### COLLECT DATA FROM ALL SIMULATIONS #####

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


for(filename in sort(list.files("./results/regdata",pattern = "regdata.\\d+",full.names = TRUE))){
  
  tryCatch({
    load(filename)
    nbd2k <- rbind(nbd2k,regdata)
    rm(regdata)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

save(nbd2k,file="./results/regdata_all.Rdata")
