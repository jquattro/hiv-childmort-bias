if(!require(grid)){
  install.packages("grid",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(grid)

if(!require(gridExtra)){
  install.packages("gridExtra",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(gridExtra)


if(!require(magrittr)){
  install.packages("magrittr",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(magrittr)


if(!require(ggrepel)){
  install.packages("ggrepel",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(ggrepel)

if(!require(directlabels)){
  install.packages("directlabels",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(directlabels)

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(tidyverse)



load("results/figdata/p22500/figdata_all.Rdata")

years <- lapply(tf,function(x) x[,"year"]) %>% unlist %>% unique

# Total Fertility Rates

fertcntrys <- lapply(inps,function(x) x$fertcountry) %>% unlist %>% unique

tfr_series= read.csv("./data/tfr_gapminder_long.csv",head=TRUE)

to.plot <- tfr_series %>% filter(country %in% fertcntrys & year %in% years) %>% select(year,country,tfr)

g_a_title <- textGrob("a. Total Fertility Rates",just="left",x=unit(0.1,"npc"),gp = gpar(fontface="bold"))

g_a <-  ggplot(to.plot,aes(x=year,y=tfr,color=country)) + 
  geom_line(size=1) + 
  geom_text_repel(data=to.plot %>% filter(year==1990),aes(label=country),color="black",point.padding = unit(3,"mm"),min.segment.length = unit(1,"cm"),fontface="bold") +
  theme_bw() + 
  scale_colour_grey(start = 0, end = .7) +
  theme(panel.grid = element_blank(),legend.position = "none") +
  xlab("Year")+
  ylab("Total fertility rate")+
  ylim(0,10)

# Adult mortality rates

am_cntrys <- lapply(inps,function(x) x$am_cntry) %>% unlist %>% unique

mort_series = read.csv("./data/IHME_female_mortSMALL.csv",head=TRUE)



to.plot <- mort_series %>% filter(country %in% am_cntrys & year %in% years) %>% select(country,year,q45_15)

g_b_title <- textGrob("b. Adult Mortality Rates",just="left",x=unit(0.1,"npc"),gp = gpar(fontface="bold"))

g_b <- ggplot(to.plot,aes(x=year,y=q45_15,color=country)) + 
  geom_line(size=1) + 
  geom_text_repel(data=to.plot %>% filter(year==1990),aes(label=country),color="black",point.padding = unit(3,"mm"),min.segment.length = unit(1,"cm"),fontface="bold") +
  theme_bw() + 
  scale_colour_grey(start = 0, end = .7) +
  theme(panel.grid = element_blank(),legend.position = "none") +
  xlab("Year")+
  ylab("45q15") +
  ylim(0,1)


# Under five mortality rates

cm_cntrys <- lapply(inps,function(x) x$cm_cntry) %>% unlist %>% unique

u5m_edit= read.csv("./data/u5m_edit.csv",head=TRUE)

to.plot <- u5m_edit %>% filter(country %in% cm_cntrys & year %in% years) %>% select(year,country,q5_0)

g_c_title <- textGrob("c. Under-five Mortality Rates",just="left",x=unit(0.1,"npc"),gp = gpar(fontface="bold"))

g_c <-ggplot(to.plot,aes(x=year,y=q5_0)) + 
  geom_line(aes(color=country),size=1) + 
  geom_text(data=to.plot %>% filter(year==1970),aes(label=country),color="black",vjust=-2,fontface="bold") +
  scale_colour_grey(start = 0, end = .7) +
  theme_bw() + 
  theme(panel.grid = element_blank(),legend.position = "none") +
  xlab("Year")+
  ylab("5q0")
  



# HIV incidence curves

hiv_curves_countries <- lapply(inps,function(x) x$curve) %>% unlist %>% unique %>% setdiff("BotUrb2x")


hivhogan = read.csv("./data/inc_curves.csv",head=TRUE) %>% gather(year,hiv_inc,-Country) %>% mutate(year=as.numeric(gsub("X","",year)))

to.plot <- hivhogan %>% filter(Country %in% hiv_curves_countries) 

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

g_d_title <- textGrob("d. HIV incidence curves",just="left",x=unit(0.1,"npc"),gp = gpar(fontface="bold"))



g_d <- ggplot(to.plot,aes(x=year,y=hiv_inc)) + 
  geom_line(aes(color=Country),size=1) + 
  geom_text(data=label_data,aes(label=Country,hjust=hjust,vjust=vjust),color="black",fontface="bold") +
  scale_colour_grey(start = 0, end = .8) +
  theme_bw() + 
  theme(panel.grid = element_blank(),legend.position = "none") +
  xlab("Year")+
  ylab("HIV incidence")

set.seed(100)



figure_1 <- arrangeGrob(g_a_title,g_a,g_b_title,g_b,g_c_title,g_c,g_d_title,g_d,
             layout_matrix=matrix(c(1,3,
                                    2,4,
                                    5,7,
                                    6,8
               
             ),byrow = TRUE,nrow = 4),heights=c(0.05,0.45,0.05,0.45),widths=c(0.5,0.5))



ggsave("figures/Figure1.png",width = 8.5,height = 8.5,dpi=300,figure_1)



# Figure 2





art_countries <- lapply(inps,function(x) x$art_col) %>% unlist %>% unique %>% setdiff("zero")


art_series= read.csv("./data/sampleART.csv",head=TRUE)



to.plot <- art_series   %>% gather(country,art,-yr) %>% rename(year=yr) %>% filter(country %in% art_countries) %>% filter(year>=2000)

label_data <- to.plot %>% filter(year==2013) %>%
  mutate(country= factor(country,levels=c("Botswana","Cameroon","Malawi","Bot_dub"),labels=c("Botswana","Cameroon","Malawi","Botswana doubled")))




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



# Figure 3

rm(list=ls())

load("./results/regdata/p22500/regdata_all.Rdata")
load("./results/models/p22500/inputs.RData")

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
