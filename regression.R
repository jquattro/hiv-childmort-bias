
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

compute_error_measures <- function(y,prediction,method,sample){
  
  # Absolute error
  abs.error <- abs(y-prediction)
  
  # Relative error
  relative.error <- abs.error/abs(y)
  
  # Root mean square error
  
  rmse <- sqrt(mean(abs.error^2))
  
  # Root median square error
  
  rmedse <- sqrt(median(abs.error^2))
  
  # Mean relative error
  
  mre <- mean(Filter(function(x) !is.infinite(x),relative.error),na.rm=TRUE)
  
  # Median relative error
  
  medre <- median(relative.error)
  
  data.frame(method=method,sample=sample,rmse=rmse,rmedse=rmedse,mre=mre,medre=medre,stringsAsFactors = FALSE)
  
}

# Prepare data

to.model <- nbd2k %>% mutate(
  agegroup=as.factor(agegroup)) 
  


full.model.formula <- corr ~ fiveq0_surv + agegroup + agegroup*hiv1990 + agegroup*hiv2000+  agegroup*hiv2010 + 
  agegroup*art_prev2005+ agegroup*art_prev2007+ agegroup*art_prev2009 + tfr2000 + tfr2010

minimum.model.formula <- corr ~ fiveq0_surv

# Out of sample data

fullsamp <- unique(to.model$i)

set.seed(400)

# dataset with 80% of full sample
ns1 <- sample(fullsamp, length(fullsamp)*.8, replace=FALSE)

# Select those i's from nbd2k

train.sample <- to.model %>% filter(i %in% ns1)

# dataset with 20% of full sample
tw1 <- setdiff(fullsamp,ns1)

test.sample <- to.model %>% filter(i %in% tw1)


error_table <- data.frame()

##### Full linear model #####


fit <- lm(full.model.formula,data=train.sample)

prediction <- predict(fit,newx = train.sample)

error_table %<>% bind_rows(compute_error_measures(train.sample$corr,prediction,"Full Linear Model","in-sample"))

prediction <- predict(fit,newx = test.sample)

error_table %<>% bind_rows(compute_error_measures(test.sample$corr,prediction,"Full Linear Model","out-of-sample"))

##### Forward and Backward selection #####

# maximum model

model.max <- glm(formula=full.model.formula,
                 family = "gaussian"(link="identity"), data=train.sample)

# minimum model

model.min <- glm(formula=minimum.model.formula, family = "gaussian"(link="identity"), data=train.sample)


# Forward BIC

fit <- step(object=model.min, scope=list(upper=model.max,lower=~1), direction="forward",k=log(nrow(train.sample)))

prediction <- predict(fit,newx = train.sample)

error_table %<>% bind_rows(compute_error_measures(train.sample$corr,prediction,"Forward Sel. BIC","in-sample"))

prediction <- predict(fit,newx = test.sample)

error_table %<>% bind_rows(compute_error_measures(test.sample$corr,prediction,"Forward Sel. BIC","out-of-sample"))

# Forward AIC

fit <- step(object=model.min, scope=list(upper=model.max,lower=~1), direction="forward",k=2)

prediction <- predict(fit,newx = train.sample)

error_table %<>% bind_rows(compute_error_measures(train.sample$corr,prediction,"Forward Sel. AIC","in-sample"))

prediction <- predict(fit,newx = test.sample)

error_table %<>% bind_rows(compute_error_measures(test.sample$corr,prediction,"Forward Sel. AIC","out-of-sample"))

# Backward BIC

fit <- step(model.max, direction="backward",k=log(nrow(to.model)))

prediction <- predict(fit,newx = train.sample)

error_table %<>% bind_rows(compute_error_measures(train.sample$corr,prediction,"Backward Sel. BIC","in-sample"))

prediction <- predict(fit,newx = test.sample)

error_table %<>% bind_rows(compute_error_measures(test.sample$corr,prediction,"Backward Sel. BIC","out-of-sample"))

# Backward AIC

fit <- step(model.max, direction="backward",k=2)

prediction <- predict(fit,newx = train.sample)

error_table %<>% bind_rows(compute_error_measures(train.sample$corr,prediction,"Backward Sel. AIC","in-sample"))

prediction <- predict(fit,newx = test.sample)

error_table %<>% bind_rows(compute_error_measures(test.sample$corr,prediction,"Backward Sel. AIC","out-of-sample"))

##### Glmnet #####
set.seed(500)
glmnet.errors <- lapply(c(0,0.5,1.0),function(alpha){
  
    
    # Get output vector for glmnet
    
    y <- train.sample %>% pull(all.vars(full.model.formula)[1])
    
    
    # Get predictors for glmnet
    
    x <- model.matrix(full.model.formula,train.sample)[,-1]
    
    fit <- cv.glmnet(x,y,family="gaussian",alpha=alpha)
      
      
      prediction <- predict(fit,newx = x,s = "lambda.min")
      
      train.error <- compute_error_measures(train.sample$corr,prediction,paste0("glmnet, alpha=",alpha),"in-sample")
      
      xnew <- model.matrix(full.model.formula,test.sample)[,-1]
      
      prediction <- predict(fit,newx = xnew,s = "lambda.min")
      
      test.error <- compute_error_measures(test.sample$corr,prediction,paste0("glmnet, alpha=",alpha),"out-of-sample")
      
      bind_rows(train.error,test.error)
    
}) %>% bind_rows()

error_table %<>% bind_rows(glmnet.errors)

##### PCR #####

set.seed(100)
fit <- pcr(full.model.formula,data=train.sample,scale=TRUE,validation="CV", jackknife=TRUE)

ncomps <- c(20,30,selectNcomp(fit,method="onesigma",plot=TRUE)) %>% sort

pcr_errors <- lapply(ncomps,function(ncomp){

  prediction <- predict(fit,newdata = train.sample, type = "response",ncomp = ncomp)
  
  train.error <- compute_error_measures(train.sample$corr,prediction,paste0("PCR, ncomp=",ncomp),"in-sample")
  
  prediction <- predict(fit,newx = test.sample,ncomp = ncomp)
  
  test.error <- compute_error_measures(test.sample$corr,prediction,paste0("PCR, ncomp=",ncomp),"out-of-sample")
  
  bind_rows(train.error,test.error)
    
}) %>% bind_rows()


error_table %<>% bind_rows(pcr_errors)


##### PLS #####

set.seed(100)
fit <- plsr(full.model.formula,data=train.sample,scale=TRUE,validation="CV", jackknife=TRUE)
summary(fit)
validationplot(fit)

# 32 components are enough to account for at least 90% of variaiblity in both X and Y. Also use optimal number of components

ncomps <- c(32,selectNcomp(fit,method="onesigma",plot=TRUE)) %>% sort



pls_errors <- lapply(ncomps,function(ncomp){
  
  prediction <- predict(fit,newdata = train.sample, type = "response",ncomp = ncomp)
  
  train.error <- compute_error_measures(train.sample$corr,prediction,paste0("PLS, ncomp=",ncomp),"in-sample")
  
  prediction <- predict(fit,newx = test.sample,ncomp = ncomp)
  
  test.error <- compute_error_measures(test.sample$corr,prediction,paste0("PLS, ncomp=",ncomp),"out-of-sample")
  
  bind_rows(train.error,test.error)
  
}) %>% bind_rows()


error_table %<>% bind_rows(pls_errors)

# Save table

ft <- error_table  %>% 
  mutate_at(vars(one_of("rmse","rmedse")),funs(sprintf("%0.6f",.))) %>%
  flextable() %>% set_header_labels(rmse="Root mean\nsquare error",
                                    rmedse="Root median\nsquare error",
                                    mre="Mean relative\nerror",
                                    medre="Median relative\nerror") %>%
  merge_v(j=1)

doc <- read_docx() %>%
  body_add_par(value = "Model selection.", style = "table title") %>% 
  body_add_flextable(ft)


# Best fitting model


# Get output vector for glmnet

y <- to.model %>% pull(all.vars(full.model.formula)[1])


# Get predictors for glmnet

x <- model.matrix(full.model.formula,to.model)[,-1]

alpha <- 1.0

set.seed(600)
best.fitting.model <- cv.glmnet(x,y,family="gaussian",alpha=alpha)





##### Figure 4 #####

# Compute prediction. Use the mean for numerical values

# hiv1990 from 0 to 0.2 and all agegroups


to.plot <- expand.grid(agegroup=unique(to.model$agegroup),hiv1990=seq(0,0.2,length.out = 100))


# Use mean for other vars

other_vars <- to.model %>% summarise_if(is.numeric,mean)

# Compute prediction

newx <- bind_cols(to.plot,other_vars[rep(1,nrow(to.plot)),]) %>% select(one_of(all.vars(full.model.formula))) %>% model.matrix(full.model.formula,data=.)

newx <- newx[,-1]

prediction <- predict(best.fitting.model, newx = newx, type = "link",se=TRUE) %>% as.data.frame %>% set_colnames("fit")



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

newx <- bind_cols(to.plot,other_vars[rep(1,nrow(to.plot)),]) %>% select(one_of(all.vars(full.model.formula))) %>% model.matrix(full.model.formula,data=.)

newx <- newx[,-1]

prediction <- predict(best.fitting.model, newx = newx, type = "link",se=TRUE) %>% as.data.frame %>% set_colnames("fit")

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
  # fig_name <- "figure6"
  fig_country <- countries[fig_name]
  
  # subset each country
  cntry <- fv %>% filter(country==fig_country)  
  
  
  
  # indirect estimates with empirical data
  
  source("indirect_estimates_functions.R")
  
  
  
  ind_surv <- ind_est_computations(cntry,2010) %>% as.data.frame() %>% mutate(agegroup=1:7)
  
  ind_surv %<>% 
    select(agegroup,fiveq0=fiveq0, t_ref,refdate)
  
  
  # merge indirect estimates based on empirical data from Malawi or Tanzania with
  # data on HIV, ART and fertility from that same country
  
  # ind_est contains the results of indirect_estimates_functions applied to the data from Malawi or Tanzania
  # cntry contains the data from Malawi or Tanzania (from facevalidity.csv)
  
  
  iep <- cntry %>% 
    left_join(ind_surv,by="agegroup") %>% 
    rename(fiveq0_surv=fiveq0) %>% 
    mutate(agegroup=factor(agegroup)) %>% 
    select(-cd)
  
  
 
  
  # also need nq0 from results of indirect_estimates_functions (see line 30 below)
  # may need to edit indirect_estimates_functions to output nq0 as part of ind_est
  
  
  colnames(iep) <- c("country","agegroup", "ceb","cs","tfr2010","tfr2000","hiv1990", "hiv2000", "hiv2010","art2005","art2006","art2007","art2008","art2009","art2010","art2011","art_prev2005","art_prev2006","art_prev2007","art_prev2008","art_prev2009","prop15to19_2010","prev15to19_2010","hiv2005","hiv2006","hiv2007","hiv2008","hiv2009","fiveq0_surv", "t_ref","refdate")
  
  # Ward & Zaba (2008) coefficients
  a = c(.1134,.1202,.1107,.1720,.1749,.1504,.2552)
  b=c(-.0226,-.0438,-.0882,-.1412,-.1758,-.1849,-.3269)
  c=c(-.1112,-.0978,-.0373,.0163,.042,.1807,.5594)
  
  # W&Z adjustment factor from applying coefficients to empirical data
  nz = a*iep$hiv2000+b*(iep$hiv2000^2)+c*iep$prev15to19_2010
  
  #Convert each n q 0 + n(z) into 5q0
  
  # Standard indirect estimates, adding Ward&Zaba adj factors
  # n = c(1,2,3,5,10,15,20)
  # UN General Female LT, e(0)=60, for above n
  l = c(.92688,.91025,.90151,.89193,.89534,.89002,.88195)
  q = 1-l
  logitMLT=.5*log(q/(1-q))
  alpha = NA
  fiveq0WZ = NA
  Y5 = NA
  
  nq0 <- ind_est_computations_core(cntry,2010)$nq0
  
  for(i in 1:7){
    Y5[i] = .5*log((nq0[i]+nz[i])/(1-(nq0[i]+nz[i])))
    alpha[i] = Y5[i] - logitMLT[i]
    fiveq0WZ[i] = exp(2*(alpha[i]+logitMLT[4]))/(1+exp(2*(alpha[i]+logitMLT[4])))
  }
  
  # fiveq0WZ are the Ward & Zaba estimates of U5M that should be plotted with the crude estimates and
  # the estimates from our model
  
  
 
  
  # Get the new predictions in matrix format
  nx <- model.matrix(full.model.formula,iep %>% mutate(corr=0))[,-1]
  
  
  
  
  
  # Make predictions
  
  
  
  prediction <- predict(best.fitting.model,newx = nx,se=TRUE,s="lambda.min") %>% 
    as.data.frame %>% 
    set_colnames("PredictedAdj")
  
  # Function to compute prediction SEs for glmnet. 
  
  glmnet_prediciton_se <- function(xnew,xs,y,yhat,my_mod){
    
    
    # Note, you can't estimate an intercept here
    
    # Betas' covariance matrix (Tibshirani, 1996)
    
    n <- dim(xs)[1]
    
    # Number of coefficients that are not 0 minus 1 for the intercept
    
    k <- coefficients(best.fitting.model,s = "lambda.min") %>% as.matrix %>% equals(0) %>% not %>% sum %>% add(-1)
    
    # residual variance.
    
    sigma_sq <- sum((y-yhat)^2)/ (n-k)
    
    i_lams <- Matrix(diag(x=1,nrow=dim(xs)[2],ncol=dim(xs)[2]),sparse=TRUE)
    
    xpx <- t(xs)%*%xs
    
    lam <- MASS::ginv(as.matrix(abs(as.vector(coef.cv.glmnet(my_mod,s="lambda.min")))[-1]*i_lams))
    
    xpxinvplam <- solve(xpx+my_mod$lambda.min*lam)
    
    var_cov_beta <- sigma_sq*(xpxinvplam %*% xpx %*% xpxinvplam)
    
    
    # Prefiction variance
    
    var_pred <- diag(nx %*% var_cov_beta %*% t(nx))
    
    h <- var_pred/sigma_sq
    
    # We are making predicitions on a new observation, so se is sigma*sqrt(1+h) isntead of sigma*sqrt(h)
    
    se_pred <- sqrt(sigma_sq)*sqrt(1+h)
    
    
    print('NOTE: These standard errors are very biased.')
    return(se_pred)
  }
  
  l_yhat <- predict(best.fitting.model,newx = x,s="lambda.min")
  
  prediction %<>% mutate(ci=qt(1-0.025,df = dim(x)[1]-dim(x)[2])*glmnet_prediciton_se(xn,x,y,l_yhat,best.fitting.model))
  
  
  
  
  to.plot <- bind_cols(iep, prediction,data.frame(fiveq0WZ=fiveq0WZ)) %>% 
    mutate(fit=fiveq0_surv+PredictedAdj,lwr=fit-ci,upr=fit+ci)%>% # Compute adjusted values and prediction interval upper and lower limits
    select(refdate,upr,lwr,Adjusted=fit,Unadjusted=fiveq0_surv,`Ward & Zaba`=fiveq0WZ) %>% # Transform data for plot
    gather(type,value,-refdate,-upr,-lwr)
  
  ggplot(to.plot,aes(x=refdate,y=value,shape=type)) +
    geom_point(color="black", size=3) +
    #ylim(min(m4$lwr)-.03,max(fiveq0WZ)+0.03)+
    geom_errorbar(aes(ymin=lwr, ymax=upr))+ 
    xlab("Year") +
    ylab("Under-five mortality (deaths per live birth)") +
    scale_shape_discrete(name="Estimate type", # Legend label, use darker colors
                         labels=c("Adjusted", "Unadjusted","Ward & Zaba"))+
    labs(title= fig_country) + 
    theme_classic() +
    theme(axis.text = element_text(color="black"))
  
  
  ggsave(paste0("./figures/",fig_name,".png"),width = 7,height = 5,dpi=300)
  
  
  
    
}

