
if(!require(data.table)){
  install.packages("data.table",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(data.table)


if(!require(pls)){
  install.packages("pls",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(pls)

if(!require(glmnet)){
  install.packages("glmnet",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(glmnet)

if(!require(officer)){
  install.packages("officer",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(officer)

if(!require(flextable)){
  install.packages("flextable",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(flextable)

if(!require(magrittr)){
  install.packages("magrittr",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(magrittr)


if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(tidyverse)


load("./results/regdata/p22500/regdata_all.Rdata")
load("./results/models/p22500/inputs.RData")



# add agegroup variable

nbd2k %<>% mutate(agegroup=rep(c(1:7),nrow(nbd2k)/7))


# Generate abs error and rel error

nbd2k %<>% mutate(abs.err=fiveq0_surv-fiveq0_hiv,
                  rel.err=(fiveq0_surv-fiveq0_hiv)/fiveq0_hiv
)


# new dep var 

nbd2k %<>% mutate(corr=fiveq0_hiv-fiveq0_surv,
                  corr2=corr+2.100105e-05)



##################################################################
##### MODEL FITTING AND MODEL SELECTION #####
##################################################################

# Prepare data

to.model <- nbd2k %>% mutate(
  agegroup=as.factor(agegroup)) 
  


full.model.formula <- corr ~ fiveq0_surv + agegroup + agegroup*hiv1990 + agegroup*hiv2000+  agegroup*hiv2010 + 
  agegroup*art_prev2005+ agegroup*art_prev2007+ agegroup*art_prev2009 + tfr2000 + tfr2010

##### PCR #####

set.seed(100)
best.fitting.model <- pcr(full.model.formula,data=to.model,scale=TRUE,validation="CV", jackknife=TRUE)

ncomp <- 20


# To use later to compute prediction intervals

# Compute covariance of the PCR regression coefficients (Faver & Kowalski, 1997,PROPAGATION OF MEASUREMENT ERRORS FOR THE VALIDATION OF PREDICTIONS OBTAINED BY PRINCIPAL COMPONENT REGRESSION AND PARTIAL LEAST SQUARES)

x <- prcomp(model.matrix(best.fitting.model),scale. = TRUE)

cv <- 0

for(i in 1:9){
  
  ev <-  x$sdev[i]^2
  
  r <- as.matrix(x$rotation)[,i,drop=FALSE]
  
  cv <- cv+1/ev*r%*%t(r)
  
}

# sqrt of the variance of the measurement errors

s <- sd(residuals(best.fitting.model,ncomp=ncomp))

# Degrees of freedom (Faver & Kowalski, 1997)

df <- nrow(to.model) - ncomp -1

# t value for the 0.05 CI interval (Faver & Kowalski, 1997)

t <- dt(0.025,df)

# summary(best.fitting.model)
# Data: 	X dimension: 31360 51 
# Y dimension: 31360 1
# Fit method: svdpc
# Number of components considered: 51
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps   2 comps   3 comps   4 comps   5 comps   6 comps   7 comps   8 comps   9 comps  10 comps
# CV         0.01261  0.01036  0.009542  0.009206  0.009023  0.008446  0.008142  0.008122  0.006469  0.006094  0.006079
# adjCV      0.01261  0.01036  0.009542  0.009450  0.008474  0.008221  0.008141  0.008121  0.006469  0.006094  0.006102
# 11 comps  12 comps  13 comps  14 comps  15 comps  16 comps  17 comps  18 comps  19 comps  20 comps  21 comps
# CV     0.006063  0.006053  0.006029  0.005819  0.005417  0.004444  0.004323  0.004234  0.004095   0.00399  0.003841
# adjCV  0.006100  0.006089  0.006031  0.005818  0.005417  0.004443  0.004331  0.004098  0.004089   0.00399  0.003841
# 22 comps  23 comps  24 comps  25 comps  26 comps  27 comps  28 comps  29 comps  30 comps  31 comps  32 comps
# CV     0.003834  0.003824  0.003819  0.003815  0.003567  0.003532  0.003216  0.003207  0.003126  0.003104  0.003072
# adjCV  0.003840  0.003840  0.003828  0.003818  0.003567  0.003531  0.003216  0.003207  0.003125  0.003124  0.003078
# 33 comps  34 comps  35 comps  36 comps  37 comps  38 comps  39 comps  40 comps  41 comps  42 comps  43 comps
# CV      0.00305  0.003031  0.002996  0.002996  0.002996  0.002995  0.002995  0.002993  0.002993   0.00298  0.002978
# adjCV   0.00307  0.003039  0.002996  0.002996  0.002996  0.002996  0.002995  0.002995  0.002993   0.00298  0.002978
# 44 comps  45 comps  46 comps  47 comps  48 comps  49 comps  50 comps  51 comps
# CV     0.002978  0.002978  0.002977  0.002977  0.002977  0.002977  0.002977  0.002977
# adjCV  0.002978  0.002977  0.002976  0.002976  0.002977  0.002977  0.002977  0.002977
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps  11 comps  12 comps
# X       13.47    24.62    35.48    46.34    57.20    68.06    71.94    75.12    77.64     80.01     82.38     84.75
# corr    32.54    42.73    44.40    55.50    57.61    58.34    58.55    73.70    76.68     76.69     76.69     76.78
# 13 comps  14 comps  15 comps  16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps  23 comps
# X        87.12     89.44     90.97     91.94     92.81     93.67     94.54     95.41     96.27     96.84     97.42
# corr     77.20     78.74     81.57     87.61     88.24     89.50     89.50     90.07     90.74     90.74     90.74
# 24 comps  25 comps  26 comps  27 comps  28 comps  29 comps  30 comps  31 comps  32 comps  33 comps  34 comps
# X        98.00     98.58     99.14     99.37     99.52     99.61     99.68     99.73     99.78     99.84     99.89
# corr     90.81     90.86     92.02     92.18     93.51     93.55     93.87     93.88     94.06     94.09     94.22
# 35 comps  36 comps  37 comps  38 comps  39 comps  40 comps  41 comps  42 comps  43 comps  44 comps  45 comps
# X        99.95     99.96     99.97     99.97     99.98     99.98     99.99     99.99    100.00    100.00    100.00
# corr     94.37     94.37     94.37     94.37     94.38     94.38     94.39     94.43     94.44     94.44     94.44
# 46 comps  47 comps  48 comps  49 comps  50 comps  51 comps
# X       100.00    100.00    100.00    100.00    100.00    100.00
# corr     94.45     94.45     94.45     94.45     94.45     94.45




# Coefficients table

# WARNING
# The jackknife variance estimates are known to be biased (see var.jack). Also, the distribution 
# of the regression coefficient estimates and the jackknife variance estimates are unknown (at least in PLSR/PCR). 
# Consequently, the distribution (and in particular, the degrees of freedom) of the resulting t statistics is unknown. 
# The present code simply assumes a t distribution with m - 1 degrees of freedom, where m is the number of cross-validation segments.
# Therefore, the resulting p values should not be used uncritically, and should perhaps be regarded as mere indicator of (non-)significance.

ft <- jack.test(best.fitting.model,ncomp = ncomp) %$% data.frame(coefficients,sd,tvalues,pvalues)  %>% mutate(var=rownames(.)) %>% set_colnames(c("coef","se","t","p.value","var")) %>% mutate_if(is.numeric,funs(sprintf("%0.6f",.))) %>%  select(var,everything()) %>% flextable()

doc <- read_docx() %>%
  body_add_par(value = paste("PCR. ncomp=",ncomp), style = "table title") %>% 
  body_add_flextable(ft) %>%
  body_add_par(value = "WARNING: The jackknife variance estimates are known to be biased (see var.jack). Also, the distribution of the regression coefficient estimates and the jackknife variance estimates are unknown (at least in PLSR/PCR).Consequently, the distribution (and in particular, the degrees of freedom) of the resulting t statistics is unknown. The present code simply assumes a t distribution with m - 1 degrees of freedom, where m is the number of cross-validation segments. Therefore, the resulting p values should not be used uncritically, and should perhaps be regarded as mere indicator of (non-)significance.")

print(doc,"./tables/PCR_coefs.docx")



##### Plot checks ########


to.plot <- to.model %>% filter(agegroup %in% c("3","4","5","6","7")) %>% 
  mutate(agegroup=factor(as.character(agegroup),levels=c("3","4","5","6","7"),labels=c("Age group 25-29 years","Age group 30-34 years","Age group 35-39 years","Age group 40-44 years","Age group 45-49 years")))


ggplot(to.plot, aes(x = hiv1990, y = corr)) + 
  geom_point()+
  facet_wrap(~agegroup,ncol=3,scales = "free_x") + 
  theme_classic() + 
  theme(legend.position="none",
        strip.background = element_blank()) +
  xlab("HIV prevalence in 1990")+
  ylab("Bias")+
  coord_cartesian()

ggplot(to.plot, aes(x = hiv2010, y = corr)) + 
  geom_point()+
  facet_wrap(~agegroup,ncol=3,scales = "free_x") + 
  theme_classic() + 
  theme(legend.position="none",
        strip.background = element_blank()) +
  xlab("HIV prevalence in 2010")+
  ylab("Bias")+
  coord_cartesian()


##### Figure 4 #####

# Compute prediction. Use the mean for numerical values

# hiv1990 from 0 to 0.2 and all agegroups


to.plot <- expand.grid(agegroup=unique(to.model$agegroup),hiv1990=seq(0,0.2,length.out = 100))


# Use mean for other vars

other_vars <- to.model %>% summarise_if(is.numeric,mean)

# Compute prediction

newx <- bind_cols(to.plot,other_vars[rep(1,nrow(to.plot)),]) %>% select(one_of(all.vars(full.model.formula))) 




prediction <- predict(best.fitting.model,newdata = newx, type = "response",se=TRUE,ncomp = ncomp) %>% as.data.frame %>% set_colnames("fit")



to.plot <- bind_cols(to.plot, prediction)


# Prepare data to plot

to.plot %<>% filter(agegroup %in% c("3","4","5","6","7")) %>% 
  mutate(agegroup=factor(as.character(agegroup),levels=c("3","4","5","6","7"),labels=c("Age group 25-29 years","Age group 30-34 years","Age group 35-39 years","Age group 40-44 years","Age group 45-49 years")))

# Plot

ggplot(to.plot, aes(x = hiv1990, y = fit)) + 
  geom_line(size = 0.75)+
  facet_wrap(~agegroup,ncol=3,scales = "free_x") + 
  theme_classic() + 
  theme(legend.position="none",
        strip.background = element_blank()) +
  xlab("HIV prevalence in 1990")+
  ylab("Bias")+
  coord_cartesian(ylim=c(0,.05))


ggsave("figures/figure4.png",width = 7,height = 5)



##### Figure 5 #####

# Compute prediction. Use the mean for numerical values

# hiv2010 from 0 to 0.2 and all agegroups

to.plot <- expand.grid(agegroup=unique(to.model$agegroup),hiv2010=seq(0,0.2,length.out = 100))


# Use mean for other vars

other_vars <- to.model %>% summarise_if(is.numeric,mean)

# Compute prediction

newx <- bind_cols(to.plot,other_vars[rep(1,nrow(to.plot)),]) %>% select(one_of(all.vars(full.model.formula)))



prediction <- predict(best.fitting.model,newdata = newx, type = "response",se=TRUE,ncomp = ncomp) %>% as.data.frame %>% set_colnames("fit")



to.plot <- bind_cols(to.plot, prediction)


# Prepare data to plot

to.plot %<>% filter(agegroup %in% c("3","4","5","6","7")) %>% 
  mutate(agegroup=factor(as.character(agegroup),levels=c("3","4","5","6","7"),labels=c("Age group 25-29 years","Age group 30-34 years","Age group 35-39 years","Age group 40-44 years","Age group 45-49 years")))

# Plot

ggplot(to.plot, aes(x = hiv2010, y = fit)) + 
  geom_line(size = 0.75)+
  facet_wrap(~agegroup,ncol=3,scales = "free_x") + 
  theme_classic() + 
  theme(legend.position="none",
        strip.background = element_blank()) +
  xlab("HIV prevalence in 2010")+
  ylab("Bias")+
  coord_cartesian(ylim=c(0,.05))

ggsave("figures/figure5.png",width = 7,height = 5)






###################################################################
### APPLICATION TO DHS DATA
##################################################################

# import empirical data
fv <- fread("./data/facevalidity.csv") %>% set_colnames(c("country","agegroup", "ceb","cs","tfr2010","tfr2000","hiv1990", "hiv2000", "hiv2010","art2005","art2006","art2007","art2008","art2009","art2010","art2011","art_prev2005","art_prev2006","art_prev2007","art_prev2008","art_prev2009","prop15to19_2010","prev15to19_2010","hiv2005","hiv2006","hiv2007","hiv2008","hiv2009")) %>% mutate(cd=ceb-cs)

countries <- c(figure6="Malawi",figure7="Tanzania")

for(fig_name in names(countries)){

  fig_country <- countries[fig_name]
  
  # subset each country
  cntry <- fv %>% filter(country==fig_country)  
  
  
  
  # indirect estimates with empirical data
  
  source("indirect_estimates_functions.R")
  
  
  
  ind_surv <- ind_est_computations(cntry,2010) %>% as.data.frame() %>% mutate(agegroup=1:7)
  
  ind_surv %<>% 
    select(agegroup,fiveq0=fiveq0, t_ref,refdate)
  
  
  # merge indirect estimates with data on HIV, ART and fertility
  
  
  iep <- cntry %>% left_join(ind_surv,by="agegroup") %>% rename(fiveq0_surv=fiveq0) %>% mutate(agegroup=factor(agegroup))
  
  
  # Compute prediction intervals (Faver & Kowalski, 1997)
  
  
  # Get the new predictions in matrix format
  nx <- model.matrix(full.model.formula,iep %>% mutate(corr=0))[,-1]
  
  # For each row, compute half amplited of the prediciton interval
  ci <- c()
  
  for(i in 1:nrow(iep)){
  
    xu <- (nx[i,,drop=FALSE]-x$center)/x$scale
    
    ci[i] <- s*t*sqrt(1/nrow(to.model)+xu%*%cv%*%t(xu))  
    
  }
  
  # Make predictions
  
  prediction <- predict(best.fitting.model,newdata = iep, type = "response",ncomp = ncomp) %>% 
    as.data.frame %>% 
    set_colnames("PredictedAdj")
  
  
  
  to.plot <- bind_cols(iep, prediction) %>% mutate(fit=fiveq0_surv+PredictedAdj,lwr=fit-ci,upr=fit+ci)%>% 
    select(refdate,upr,lwr,Adjusted=fit,Unadjusted=fiveq0_surv) %>% gather(type,value,-refdate,-upr,-lwr)
  
  ggplot(to.plot,aes(x=refdate,y=value,shape=type)) +
    geom_point(color="black", size=3) +
    #ylim(min(m4$lwr)-.03,max(fiveq0WZ)+0.03)+
    geom_errorbar(aes(ymin=lwr, ymax=upr))+ 
    xlab("Year") +
    ylab("Under-five mortality (deaths per live birth)") +
    scale_shape_discrete(name="Estimate type", # Legend label, use darker colors
                         labels=c("Adjusted", "Unadjusted"))+
    labs(title= fig_country) + 
    theme_classic() +
    theme(axis.text = element_text(color="black"))
  
  
  ggsave(paste0("./figures/",fig_name,".png"),width = 7,height = 5,dpi=300)
  
  
  
    
}

