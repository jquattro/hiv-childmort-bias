##### SIMULATION #####

source("simulation_functions.R")

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


# Select one set of parameters

pset <- 1
inp <- inputs[pset,]


#Set size of initial population
ip<-18000

years <- c(1946:2010)
ages <- c(-100:120)


# Generate population
set.seed(1000)

results <- bigsim(inp,initialpop=ip,years,ages,hivhogan,mort_series,adultmort,worldfert,tfr_series,art_series,u5m_edit,matmort)


##### INDIRECT ESTIMATES #####

source("indirect_estimates_functions.R")

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

save(regdata,file=paste("180228regdata.",pset,".Rdata",sep=""))
save(figdata,file=paste("180228figdata.",pset,".Rdata",sep=""))
