rm(list=ls())

###############################################################################
# Name: 002_figures
# Author: John Quattrochi (john.quattrochi@gmail.com)
# Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
# Purpose: Plots figures 1 to 3
# The script assumes the following folder structure:
# Scripts are stored in "[project folder]/R"
# Data are stored in "[project folder]/data"
# Results are stored in "[project folder]/results"
# Figures are saved in "[project folder]/figures"
###############################################################################

# R version and load packages and install if necessary

# version
# 
# _                           
# platform       x86_64-apple-darwin15.6.0   
# arch           x86_64                      
# os             darwin15.6.0                
# system         x86_64, darwin15.6.0        
# status                                     
# major          3                           
# minor          5.2                         
# year           2018                        
# month          12                          
# day            20                          
# svn rev        75870                       
# language       R                           
# version.string R version 3.5.2 (2018-12-20)
# nickname       Eggshell Igloo         

if(!require(grid)){
  install.packages("grid",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(grid)
packageVersion("grid")
# 3.5.2

if(!require(gridExtra)){
  install.packages("gridExtra",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(gridExtra)
packageVersion("gridExtra")
# 2.3

if(!require(magrittr)){
  install.packages("magrittr",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(magrittr)
packageVersion("magrittr")
#1.5

if(!require(ggrepel)){
  install.packages("ggrepel",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(ggrepel)
packageVersion("ggrepel")
# 0.8.0

if(!require(directlabels)){
  install.packages("directlabels",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(directlabels)
packageVersion("directlabels")
# 2018.5.22

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(tidyverse)
# ── Attaching packages ───────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
# ✔ ggplot2 3.1.0       ✔ purrr   0.3.0  
# ✔ tibble  2.0.1       ✔ dplyr   0.8.0.1
# ✔ tidyr   0.8.2       ✔ stringr 1.4.0  
# ✔ readr   1.1.1       ✔ forcats 0.3.0  
# ── Conflicts ──────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
# ✖ dplyr::filter() masks stats::filter()
# ✖ dplyr::lag()    masks stats::lag()

##### FIGURE 1 #####
# Total fertility rates, adult and under-five mortality rates, and HIV
# incidence rates from selected countries.

load("results/figdata/p22500/figdata_all.Rdata")

# Get simulation years

years <- lapply(tf,function(x) x[,"year"]) %>% unlist %>% unique

##### Total Fertility Rates Panel #####

# Get the fertility rate countries

fertcntrys <- lapply(inps,function(x) x$fertcountry) %>% unlist %>% unique

# Get total fertility rate series and extract the ones for the years and countries in the simulations

tfr_series= read.csv("./data/tfr_gapminder_long.csv",head=TRUE)

to.plot <- tfr_series %>% filter(country %in% fertcntrys & year %in% years) %>% select(year,country,tfr)

# Panel title

g_a_title <- textGrob("a. Total Fertility Rates",just="left",x=unit(0.1,"npc"),gp = gpar(fontface="bold"))

# Plot the panel and save for later

g_a <-  ggplot(to.plot,aes(x=year,y=tfr,color=country)) + 
  geom_line(size=1) + 
  geom_text_repel(data=to.plot %>% filter(year==1990),aes(label=country),color="black",point.padding = unit(3,"mm"),min.segment.length = unit(1,"cm"),fontface="bold") +
  theme_bw() + 
  scale_colour_grey(start = 0, end = .7) +
  theme(panel.grid = element_blank(),legend.position = "none") +
  xlab("Year")+
  ylab("Total fertility rate")+
  ylim(0,10)

##### Adult mortality rates panel #####

# Get the adult mortality countries

am_cntrys <- lapply(inps,function(x) x$am_cntry) %>% unlist %>% unique

# Get the adult mortality series

mort_series = read.csv("./data/IHME_female_mortSMALL.csv",head=TRUE)

# Extract the adult mortality rates used in the simulations

to.plot <- mort_series %>% filter(country %in% am_cntrys & year %in% years) %>% select(country,year,q45_15)

# Panel title

g_b_title <- textGrob("b. Adult Mortality Rates",just="left",x=unit(0.1,"npc"),gp = gpar(fontface="bold"))

# Plot the panel and sabe for later

g_b <- ggplot(to.plot,aes(x=year,y=q45_15,color=country)) + 
  geom_line(size=1) + 
  geom_text_repel(data=to.plot %>% filter(year==1990),aes(label=country),color="black",point.padding = unit(3,"mm"),min.segment.length = unit(1,"cm"),fontface="bold") +
  theme_bw() + 
  scale_colour_grey(start = 0, end = .7) +
  theme(panel.grid = element_blank(),legend.position = "none") +
  xlab("Year")+
  ylab("45q15") +
  ylim(0,1)


##### Under five mortality rates panel #####

# Get the countries for the U5MR data

cm_cntrys <- lapply(inps,function(x) x$cm_cntry) %>% unlist %>% unique

# Get the U5MR series

u5m_edit= read.csv("./data/u5m_edit.csv",head=TRUE)

# Extract the U5MR data used in the simulations

to.plot <- u5m_edit %>% filter(country %in% cm_cntrys & year %in% years) %>% select(year,country,q5_0)

# Panel title

g_c_title <- textGrob("c. Under-five Mortality Rates",just="left",x=unit(0.1,"npc"),gp = gpar(fontface="bold"))

# Plot the panel and save for later

g_c <-ggplot(to.plot,aes(x=year,y=q5_0)) + 
  geom_line(aes(color=country),size=1) + 
  geom_text(data=to.plot %>% filter(year==1970),aes(label=country),color="black",vjust=-2,fontface="bold") +
  scale_colour_grey(start = 0, end = .7) +
  theme_bw() + 
  theme(panel.grid = element_blank(),legend.position = "none") +
  xlab("Year")+
  ylab("5q0")
  



##### HIV incidence curves panel #####

# Get the countries for the HIV incidence data used in the simulations

hiv_curves_countries <- lapply(inps,function(x) x$curve) %>% unlist %>% unique %>% setdiff("BotUrb2x")

# Get the HIV incidence series

hivhogan = read.csv("./data/inc_curves.csv",head=TRUE) %>% gather(year,hiv_inc,-Country) %>% mutate(year=as.numeric(gsub("X","",year)))

# Extract the HIV incidence data used in the simulations

to.plot <- hivhogan %>% filter(Country %in% hiv_curves_countries) 

# Label the data

label_data <- to.plot %>% filter(year==1996 & Country=="BotswanaUrban" | 
                                   year==1988 & Country=="CamerounRural" | 
                                   year==1998 & Country=="LesothoRural" | 
                                   year==1990 & Country=="UgandaRural" | 
                                   year==2000 & Country=="MalawiRural"  ) %>%
  left_join(data.frame(Country=c("BotswanaUrban","CamerounRural","LesothoRural","UgandaRural","MalawiRural"),
                       vjust=c(-0.2,-0.2,-0.2,-0.2,-0.2),
                       hjust=c(1,1,0,1,0.5),
                       stringsAsFactors=FALSE),by="Country") %>%
  mutate(Country= factor(Country,levels=c("BotswanaUrban","CamerounRural","LesothoRural","UgandaRural","MalawiRural"),labels=c("Urban Botswana","Rural Cameroun","Rural Lesotho","Rural Uganda","Rural Malawi")))

# Panel title

g_d_title <- textGrob("d. HIV incidence curves",just="left",x=unit(0.1,"npc"),gp = gpar(fontface="bold"))

# Plot the panel and save for later

g_d <- ggplot(to.plot,aes(x=year,y=hiv_inc)) + 
  geom_line(aes(color=Country),size=1) + 
  geom_text(data=label_data,aes(label=Country,hjust=hjust,vjust=vjust),color="black",fontface="bold") +
  scale_colour_grey(start = 0, end = .8) +
  theme_bw() + 
  theme(panel.grid = element_blank(),legend.position = "none") +
  xlab("Year")+
  ylab("HIV incidence")

##### Arrange panels in one figure and save #####

# Make sure that labels are always in the same place.
set.seed(100)


figure_1 <- arrangeGrob(g_a_title,g_a,g_b_title,g_b,g_c_title,g_c,g_d_title,g_d,
             layout_matrix=matrix(c(1,3,
                                    2,4,
                                    5,7,
                                    6,8
               
             ),byrow = TRUE,nrow = 4),heights=c(0.05,0.45,0.05,0.45),widths=c(0.5,0.5))



ggsave("figures/Figure1.png",width = 8.5,height = 8.5,dpi=300,figure_1)



###### FIGURE 2 #####
# Probabilities of anti-retroviral therapy initiation used in simulations

# Get the countries for the ART data used in the simulations

art_countries <- lapply(inps,function(x) x$art_col) %>% unlist %>% unique %>% setdiff("zero")

# Get the ART series

art_series= read.csv("./data/sampleART.csv",head=TRUE)

# Extract the ART data used in the simulations
to.plot <- art_series   %>% gather(country,art,-yr) %>% rename(year=yr) %>% filter(country %in% art_countries) %>% filter(year>=2000)

# Labe the data
label_data <- to.plot %>% filter(year==2013) %>%
  mutate(country= factor(country,levels=c("Botswana","Cameroon","Malawi","Bot_dub"),labels=c("Botswana","Cameroon","Malawi","Botswana doubled")))


# Plot

figure_2 <- ggplot(to.plot,aes(x=year,y=art)) + 
  geom_line(aes(color=country),size=1) + 
  geom_text(data=label_data,aes(label=country),color="black",fontface="bold",vjust=-1) +
  scale_colour_grey(start = 0, end = .8) +
  theme_bw() + 
  theme(panel.grid = element_blank(),legend.position = "none") +
  xlab("Year")+
  ylab("Probability of ART initiation given eligibility") +
  ylim(0,0.2)





ggsave("figures/Figure2.png",width = 5,height = 5,dpi = 300,figure_2)



##### FIGURE 3 ####
# Indirect estimates from reports of surviving women versus indirect estimates from
# reports of surviving women and women who died from HIV/AIDS

rm(list=ls())

# Load the data

load("./results/regdata/p22500/regdata_all.Rdata")
load("./results/models/p22500/inputs.RData")

# Plot

figure_3 <- ggplot(nbd2k,aes(x=fiveq0_hiv,y=fiveq0_surv)) + 
  geom_point(alpha=0.25) +
  xlab("Indirect estimates, surviving women & HIV deaths")+
  ylab("Indirect estimates, surviving women only")+
ylim(0,.3)+
xlim(0,.3)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+
  theme(panel.grid = element_blank())

ggsave("figures/Figure3.png",width = 5,height = 5,dpi = 300,figure_3)
