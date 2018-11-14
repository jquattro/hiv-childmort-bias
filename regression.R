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

minimum.model.formula <- corr ~ fiveq0_surv


# Forward selection

# maximum model

model.max <- glm(formula=full.model.formula,
                   family = "gaussian"(link="identity"), data=to.model)



# minimum model

model.min <- glm(formula=minimum.model.formula, family = "gaussian"(link="identity"), data=nbd2k)

forward.BIC <- step(object=model.min, scope=list(upper=model.max,lower=~1), direction="forward",k=log(nrow(to.model)))



# Call:  glm(formula = corr ~ fiveq0_surv + hiv2000 + agegroup + hiv1990 + 
#              hiv2010 + hiv2000:agegroup + agegroup:hiv1990 + agegroup:hiv2010, 
#            family = gaussian(link = "identity"), data = nbd2k)
# 
# Coefficients:
#   (Intercept)       fiveq0_surv           hiv2000          agegroup           hiv1990           hiv2010  
# 0.0013649        -0.0142237        -0.0048914         0.0002069        -0.0499804         0.0186521  
# hiv2000:agegroup  agegroup:hiv1990  agegroup:hiv2010  
# 0.0165021         0.0199525        -0.0060539  
# 
# Degrees of Freedom: 31359 Total (i.e. Null);  31351 Residual
# Null Deviance:	    4.986 
# Residual Deviance: 1.095 	AIC: -232800

forward.AIC <- step(object=model.min, scope=list(upper=model.max,lower=~1), direction="forward",k=2)

# Call:  glm(formula = corr ~ fiveq0_surv + hiv2000 + agegroup + hiv1990 + 
#              hiv2010 + art_prev2009 + art_prev2005 + art_prev2007 + hiv2000:agegroup + 
#              agegroup:hiv1990 + agegroup:hiv2010 + agegroup:art_prev2009 + 
#              agegroup:art_prev2005 + agegroup:art_prev2007, family = gaussian(link = "identity"), 
#            data = nbd2k)
# 
# Coefficients:
#   (Intercept)            fiveq0_surv                hiv2000               agegroup                hiv1990  
# 0.001327              -0.014178              -0.002884               0.000219              -0.049910  
# hiv2010           art_prev2009           art_prev2005           art_prev2007       hiv2000:agegroup  
# 0.014603               0.174254              -2.591366               0.269457               0.016005  
# agegroup:hiv1990       agegroup:hiv2010  agegroup:art_prev2009  agegroup:art_prev2005  agegroup:art_prev2007  
# 0.019860              -0.004939              -0.013344               1.251603              -0.256641  
# 
# Degrees of Freedom: 31359 Total (i.e. Null);  31345 Residual
# Null Deviance:	    4.986 
# Residual Deviance: 1.093 	AIC: -232900


# Backward selection


back.BIC <- step(model.max, direction="backward",k=log(nrow(to.model)))

# Step:  AIC=-275483.8
# corr ~ fiveq0_surv + agegroup + hiv1990 + hiv2000 + hiv2010 + 
#   art_prev2005 + art_prev2007 + tfr2000 + tfr2010 + agegroup:hiv1990 + 
#   agegroup:hiv2000 + agegroup:hiv2010 + agegroup:art_prev2005 + 
#   agegroup:art_prev2007
# 
# Df Deviance     AIC
# <none>                      0.27691 -275484
# - tfr2010                1  0.27707 -275476
# - tfr2000                1  0.27707 -275476
# - agegroup:art_prev2005  6  0.27815 -275406
# - agegroup:art_prev2007  6  0.27820 -275400
# - agegroup:hiv2010       6  0.27996 -275202
# - fiveq0_surv            1  0.30530 -272433
# - agegroup:hiv2000       6  0.36206 -267138
# - agegroup:hiv1990       6  0.61191 -250680

# Call:  glm(formula = corr ~ fiveq0_surv + agegroup + hiv1990 + hiv2000 + 
#              hiv2010 + art_prev2005 + art_prev2007 + tfr2000 + tfr2010 + 
#              agegroup:hiv1990 + agegroup:hiv2000 + agegroup:hiv2010 + 
#              agegroup:art_prev2005 + agegroup:art_prev2007, family = gaussian(link = "identity"), 
#            data = to.model)
# 
# Coefficients:
#   (Intercept)             fiveq0_surv               agegroup2               agegroup3  
# 2.321e-04              -1.093e-02              -1.935e-05               1.927e-04  
# agegroup4               agegroup5               agegroup6               agegroup7  
# 2.775e-04               4.629e-04               6.519e-04               9.960e-04  
# hiv1990                 hiv2000                 hiv2010            art_prev2005  
# -5.297e-03               1.175e-02              -1.633e-03               1.729e-01  
# art_prev2007                 tfr2000                 tfr2010       agegroup2:hiv1990  
# -2.763e-02               1.587e-03              -1.590e-03               8.421e-03  
# agegroup3:hiv1990       agegroup4:hiv1990       agegroup5:hiv1990       agegroup6:hiv1990  
# -3.128e-04              -9.826e-03               3.325e-02               1.077e-01  
# agegroup7:hiv1990       agegroup2:hiv2000       agegroup3:hiv2000       agegroup4:hiv2000  
# 1.081e-01              -7.399e-03               2.397e-02               8.133e-02  
# agegroup5:hiv2000       agegroup6:hiv2000       agegroup7:hiv2000       agegroup2:hiv2010  
# 1.170e-01               8.374e-02               5.758e-02               7.742e-03  
# agegroup3:hiv2010       agegroup4:hiv2010       agegroup5:hiv2010       agegroup6:hiv2010  
# 8.430e-03               2.741e-03              -8.319e-03              -1.751e-02  
# agegroup7:hiv2010  agegroup2:art_prev2005  agegroup3:art_prev2005  agegroup4:art_prev2005  
# -2.507e-02              -1.014e-01              -1.680e+00              -1.046e+00  
# agegroup5:art_prev2005  agegroup6:art_prev2005  agegroup7:art_prev2005  agegroup2:art_prev2007  
# 4.105e+00               8.182e+00               4.829e+00               2.357e-02  
# agegroup3:art_prev2007  agegroup4:art_prev2007  agegroup5:art_prev2007  agegroup6:art_prev2007  
# 3.696e-01               2.038e-01              -9.496e-01              -1.825e+00  
# agegroup7:art_prev2007  
# -1.080e+00  
# 
# Degrees of Freedom: 31359 Total (i.e. Null);  31315 Residual
# Null Deviance:	    4.986 
# Residual Deviance: 0.2769 	AIC: -275900

back.AIC <- step(model.max, direction="backward",k=2)

# Start:  AIC=-275868
# corr ~ fiveq0_surv + agegroup + agegroup * hiv1990 + agegroup * 
#   hiv2000 + agegroup * hiv2010 + agegroup * art_prev2005 + 
#   agegroup * art_prev2007 + agegroup * art_prev2009 + tfr2000 + 
#   tfr2010
# 
# Df Deviance     AIC
# <none>                      0.27671 -275868
# - agegroup:art_prev2009  6  0.27685 -275864
# - tfr2010                1  0.27686 -275853
# - tfr2000                1  0.27686 -275853
# - agegroup:art_prev2007  6  0.27705 -275842
# - agegroup:art_prev2005  6  0.27763 -275776
# - agegroup:hiv2010       6  0.27869 -275657
# - fiveq0_surv            1  0.30510 -272808
# - agegroup:hiv2000       6  0.34308 -269138
# - agegroup:hiv1990       6  0.59981 -251619

# Call:  glm(formula = corr ~ fiveq0_surv + agegroup + agegroup * hiv1990 + 
#              agegroup * hiv2000 + agegroup * hiv2010 + agegroup * art_prev2005 + 
#              agegroup * art_prev2007 + agegroup * art_prev2009 + tfr2000 + 
#              tfr2010, family = gaussian(link = "identity"), data = to.model)
# 
# Coefficients:
#   (Intercept)             fiveq0_surv               agegroup2               agegroup3  
# 2.526e-04              -1.093e-02              -1.908e-05               1.912e-04  
# agegroup4               agegroup5               agegroup6               agegroup7  
# 2.735e-04               4.575e-04               6.514e-04               9.996e-04  
# hiv1990                 hiv2000                 hiv2010            art_prev2005  
# -5.359e-03               1.194e-02              -1.989e-03               3.238e-01  
# art_prev2007            art_prev2009                 tfr2000                 tfr2010  
# -1.449e-01               4.657e-02               1.538e-03              -1.538e-03  
# agegroup2:hiv1990       agegroup3:hiv1990       agegroup4:hiv1990       agegroup5:hiv1990  
# 8.440e-03              -4.261e-04              -1.011e-02               3.288e-02  
# agegroup6:hiv1990       agegroup7:hiv1990       agegroup2:hiv2000       agegroup3:hiv2000  
# 1.077e-01               1.083e-01              -7.493e-03               2.454e-02  
# agegroup4:hiv2000       agegroup5:hiv2000       agegroup6:hiv2000       agegroup7:hiv2000  
# 8.274e-02               1.189e-01               8.387e-02               5.622e-02  
# agegroup2:hiv2010       agegroup3:hiv2010       agegroup4:hiv2010       agegroup5:hiv2010  
# 7.896e-03               7.513e-03               4.431e-04              -1.138e-02  
# agegroup6:hiv2010       agegroup7:hiv2010  agegroup2:art_prev2005  agegroup3:art_prev2005  
# -1.772e-02              -2.285e-02              -1.601e-01              -1.329e+00  
# agegroup4:art_prev2005  agegroup5:art_prev2005  agegroup6:art_prev2005  agegroup7:art_prev2005  
# -1.671e-01               5.277e+00               8.260e+00               3.982e+00  
# agegroup2:art_prev2007  agegroup3:art_prev2007  agegroup4:art_prev2007  agegroup5:art_prev2007  
# 6.677e-02               1.116e-01              -4.431e-01              -1.812e+00  
# agegroup6:art_prev2007  agegroup7:art_prev2007  agegroup2:art_prev2009  agegroup3:art_prev2009  
# -1.883e+00              -4.572e-01              -1.678e-02               1.002e-01  
# agegroup4:art_prev2009  agegroup5:art_prev2009  agegroup6:art_prev2009  agegroup7:art_prev2009  
# 2.512e-01               3.348e-01               2.249e-02              -2.419e-01  
# 
# Degrees of Freedom: 31359 Total (i.e. Null);  31308 Residual
# Null Deviance:	    4.986 
# Residual Deviance: 0.2767 	AIC: -275900


model.formula.selected.1 <- formula(back.AIC)

# corr ~ fiveq0_surv + agegroup + agegroup * hiv1990 + agegroup * 
#   hiv2000 + agegroup * hiv2010 + agegroup * art_prev2005 + 
#   agegroup * art_prev2007 + agegroup * art_prev2009 + tfr2000 + 
#   tfr2010


model.formula.selected.2 <- update(model.formula.selected.1, ~. - agegroup:art_prev2009)

# corr ~ fiveq0_surv + agegroup + hiv1990 + hiv2000 + hiv2010 + 
#   art_prev2005 + art_prev2007 + art_prev2009 + tfr2000 + tfr2010 + 
#   agegroup:hiv1990 + agegroup:hiv2000 + agegroup:hiv2010 + 
#   agegroup:art_prev2005 + agegroup:art_prev2007


model.formula.selected.3 <- update(model.formula.selected.1, ~. - tfr2010)

model.selected.1 <- glm(model.formula.selected.1,family="gaussian"(link="identity"),data=to.model)
model.selected.2 <- glm(model.formula.selected.2,family="gaussian"(link="identity"),data=to.model)
model.selected.3 <- glm(model.formula.selected.3,family="gaussian"(link="identity"),data=to.model)

# In-sample comparison

# Root mean absolute error

e0 <- model.max$fitted.values-(to.model$corr)
e1 <- model.selected.1$fitted.values-(to.model$corr)
e2 <- model.selected.2$fitted.values-(to.model$corr)
e3 <- model.selected.3$fitted.values-(to.model$corr)

mean(e0^2)^.5
# [1] 0.002970475
mean(e1^2)^.5
# [1] 0.002970475
mean(e2^2)^.5
# [1] 0.002971231
mean(e3^2)^.5
# [1] 0.002971265

# root mean relative error

e0r <- e0/to.model$corr
e1r <- e1/to.model$corr
e2r <- e2/to.model$corr
e3r <- e3/to.model$corr

mean(e0r^2)^.5
# Inf
mean(e1r^2)^.5
# Inf
mean(e2r^2)^.5
# Inf
mean(e3r^2)^.5
# Inf

var(e0)^.5
# [1] 0.002970522
var(e1)^.5
# [1] 0.002970522
var(e2)^.5
# [1] 0.002971279
var(e3)^.5
# [1] 0.002971312


# Out of sample

fullsamp <- unique(to.model$i)

set.seed(400)

# dataset with 80% of full sample
ns1 <- sample(fullsamp, length(fullsamp)*.8, replace=FALSE)

# Select those i's from nbd2k

sample1 <- to.model %>% filter(i %in% ns1)

# dataset with 20% of full sample
tw1 <- setdiff(fullsamp,ns1)

sample2 <- to.model %>% filter(i %in% tw1)

# Take same models used in in-sample tests

full.model_sample1 <- glm(formula=full.model.formula,family = "gaussian"(link="identity"), data=sample1)
model.selected.1_sample1 <- glm(model.formula.selected.1,family="gaussian"(link="identity"),data=sample1)
model.selected.2_sample1 <- glm(model.formula.selected.2,family="gaussian"(link="identity"),data=sample1)
model.selected.3_sample1 <- glm(model.formula.selected.3,family="gaussian"(link="identity"),data=sample1)


pcorr.full.model <- predict(full.model_sample1,sample2,interval="predict",level=.95) # "predict" is the appropriate interval for individual prediction (rather than mean)
pcorr.model.1 <- predict(model.selected.1_sample1,sample2,interval="predict",level=.95) # "predict" is the appropriate interval for individual prediction (rather than mean)
pcorr.model.2 <- predict(model.selected.2_sample1,sample2,interval="predict",level=.95) # "predict" is the appropriate interval for individual prediction (rather than mean)
pcorr.model.3 <- predict(model.selected.3_sample1,sample2,interval="predict",level=.95) # "predict" is the appropriate interval for individual prediction (rather than mean)


e.full.model <- pcorr.full.model-(sample2$corr)
e.model.1 <- pcorr.model.1-(sample2$corr)
e.model.2 <- pcorr.model.2-(sample2$corr)
e.model.3 <- pcorr.model.3-(sample2$corr)

# Root mean squared absolute error

mean(e.full.model^2)^.5
# 0.00292759
mean(e.model.1^2)^.5
# 0.00292759
mean(e.model.2)^.5
# 0.01186993
mean(e.model.3^2)^.5
# 0.00292356


# model 3 is the best one in out-of-sample

# corr ~ fiveq0_surv + agegroup + hiv1990 + hiv2000 + hiv2010 + 
#   art_prev2005 + art_prev2007 + art_prev2009 + tfr2000 + agegroup:hiv1990 + 
#   agegroup:hiv2000 + agegroup:hiv2010 + agegroup:art_prev2005 + 
#   agegroup:art_prev2007 + agegroup:art_prev2009


best.fitting.model.formula <- model.formula.selected.3


best.fitting.model <- glm(best.fitting.model.formula,family="gaussian"(link="identity"),data=to.model)

##### Figure 4 #####

# Compute prediction. Use the mean for numerical values

# hiv1990 from 0 to 0.2 and all agegroups

to.plot <- expand.grid(agegroup=unique(to.model$agegroup),hiv1990=seq(0,0.2,length.out = 100))

# Use mean for other vars

other_vars <- to.model %>% summarise_if(is.numeric,mean)

# Compute prediction

to.plot <- bind_cols(to.plot,other_vars[rep(1,nrow(to.plot)),])

prediction <- predict(best.fitting.model, newdata = to.plot, type = "link",se=TRUE) %>% as.data.frame

to.plot <- bind_cols(to.plot, prediction)

# Compute confidence interval limits

to.plot %<>% mutate(LL=fit - (1.96 * se.fit),UL=fit + (1.96 * se.fit))

# Prepare data to plot

to.plot %<>% filter(agegroup %in% c("3","4","5","6","7")) %>% 
  mutate(agegroup=factor(as.character(agegroup),levels=c("3","4","5","6","7"),labels=c("Age group 25-29 years","Age group 30-34 years","Age group 35-39 years","Age group 40-44 years","Age group 45-49 years")))

# Plot

ggplot(to.plot, aes(x = hiv1990, y = fit)) + 
  geom_ribbon(aes(ymin = LL,ymax = UL),fill="grey", alpha=0.1) + 
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

to.plot <- bind_cols(to.plot,other_vars[rep(1,nrow(to.plot)),])

prediction <- predict(best.fitting.model, newdata = to.plot, type = "link",se=TRUE) %>% as.data.frame

to.plot <- bind_cols(to.plot, prediction)

# Compute confidence interval limits

to.plot %<>% mutate(LL=fit - (1.96 * se.fit),UL=fit + (1.96 * se.fit))

# Prepare data to plot

to.plot %<>% filter(agegroup %in% c("3","4","5","6","7")) %>% 
  mutate(agegroup=factor(as.character(agegroup),levels=c("3","4","5","6","7"),labels=c("Age group 25-29 years","Age group 30-34 years","Age group 35-39 years","Age group 40-44 years","Age group 45-49 years")))

# Plot

ggplot(to.plot, aes(x = hiv2010, y = fit)) + 
  geom_ribbon(aes(ymin = LL,ymax = UL),fill="grey", alpha=0.1) + 
  geom_line(size = 0.75)+
  facet_wrap(~agegroup,ncol=3,scales = "free_x") + 
  theme_classic() + 
  theme(legend.position="none",
        strip.background = element_blank()) +
  xlab("HIV prevalence in 2010")+
  ylab("Bias")+
  coord_cartesian(ylim=c(0,.05))

ggsave("figures/figure5.png",width = 7,height = 5)
