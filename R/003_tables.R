rm(list=ls())

###############################################################################
# Name: 003_tables.R
# Author: John Quattrochi (john.quattrochi@gmail.com)
# Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
# Purpose: Produces tables 2 and 3
# The script assumes the following folder structure:
# Scripts are stored in "[project folder]/R"
# Data are stored in "[project folder]/data"
# Results are stored in "[project folder]/results"
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

if(!require(officer)){
  install.packages("officer",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(officer)
packageVersion("officer")
# 0.3.2

if(!require(flextable)){
  install.packages("flextable",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(flextable)
packageVersion("flextable")
# 0.5.1

if(!require(magrittr)){
  install.packages("magrittr",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(magrittr)
packageVersion("magrittr")
# 1.5

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



###### TABLE 2 #####
# Outcomes for simulated populations, summary statistics


# Construct variable for HIV deaths/surviving women

nbd2k %<>% mutate(hivdeaths = women_hiv - women_surv,
                  hiv_surv_ratio = hivdeaths/women_surv)

var_labels <- c(hiv1990="HIV prevalence, 1990",
                hiv2000="HIV prevalence, 2000",
                hiv2010="HIV prevalence, 2010",
                art2004="ART coverage, 2004",
                art2008="ART coverage, 2008",
                art2010="ART coverage, 2010",
                artprev2005="ART prevalence, 2005",
                artprev2007="ART prevalence, 2007",
                artprev2009="ART prevalence, 2009",
                tfr2000="Total fertility rate, 2000",
                tfr2010="Total fertility rate, 2010"
                )

to.plot <- nbd2k %>% 
  filter(agegroup==1) %>% 
  select(hiv1990,hiv2000,hiv2010,art2004,art2008,art2010,artprev2005=art_prev2005,artprev2007=art_prev2007,artprev2009=art_prev2009,tfr2000,tfr2010) %>%
  summarise_all(funs(mean,sd,median,min,max),na.rm=TRUE) %>% 
  gather(var,val) %>%
  separate(var,c("variable","parameter"),sep = "_",extra = "merge" ) %>%
  spread(parameter,val) %>%
  mutate_at(vars(mean,sd,median,min,max),funs(case_when( .==0 ~ "0",variable %in% c("artprev2005","artprev2007","artprev2009") ~ sprintf("%0.4f",.), TRUE ~ sprintf("%0.2f",.)))) %>%
  mutate(variable=factor(variable,levels = c("hiv1990","hiv2000","hiv2010","art2004","art2008","art2010","artprev2005","artprev2007","artprev2009","tfr2000","tfr2010"),labels=var_labels)
         ) %>%
  arrange(variable) %>%
  select(variable,mean,sd,median,min,max)


ft <- to.plot %>% flextable() %>% 
  set_header_labels(variable="Variable",mean="Mean",sd="Std dev",median="Median",min="Min",max="Max") %>%
  border_remove() %>%
  hline_bottom(part="head",border = fp_border(width = 2) ) %>%
  hline_bottom(part="body",border = fp_border(width = 1) ) %>%
  width(j=1,width = 2) %>%
  align(part="all",align = "left") %>%
  bold(part="head") %>%
  bg(i=seq(1,nrow(to.plot),2),bg = "gray80")


doc <- read_docx() %>%
  body_add_par(value = "Table 2 Outcomes for simulated populations, summary statistics", style = "table title") %>% 
  body_add_flextable(ft) %>%
  body_add_par(value = paste0("Notes: Based on ",scales::comma_format()(nrow(inputs))," simulated populations. ART coverage is defined as the percent of women with a CD4 count under 200 who are on ART. ART prevalence is defined as the percent of all women who are on ART."))
  


print(doc,"./tables/table2.docx")





###### TABLE 3 #####
# Bias in indirect estimates in 4,480 simulated populations

agegroups <- c("X15_19","X20_24","X25_29","X30_34","X35_39","X40_44","X45_49")

var_labels <- c(abs.err="Absolute bias",
                rel.err="Relative bias", 
                t.ref.surv="Yrs before survey\nthat estimates\npertain to",
                women.surv="Surviving women",
                women.hiv="Women who died\nfrom HIV/AIDS",
                ceb.surv="Children ever born,\nsurv women",
                ceb.hiv="Children ever born,\nsurv women + HIV deaths",
                cdceb.surv="Dead children,\nsurviving women",
                cdceb.hiv="Dead children, surv\nwomen + HIV\ndeaths",
                hiv.surv.ratio="Ratio of HIV\ndeaths to surviving\nwomen")

to.plot <- nbd2k %>% 
  select(agegroup,abs.err,rel.err, t_ref_surv,women_surv,women_hiv,ceb_surv,ceb_hiv,cdceb_surv,cdceb_hiv,hiv_surv_ratio) %>%
  rename_all(funs(gsub("_","\\.",.))) %>%
  group_by(agegroup) %>%
  summarise_all(funs(mean,sd,median,min,max),na.rm=TRUE) %>%
  ungroup %>% 
  gather(var,val,-agegroup) %>%
  separate(var,c("variable","statistic"),sep = "_",extra = "merge" )%>%
  filter(variable %in% c("abs.err","rel.err") | statistic=="mean") %>%
  mutate(agegroup=agegroups[agegroup]) %>%
  spread(agegroup,val) %>%
  mutate_if(is.numeric,funs(case_when( variable %in% c("rel.err") ~ sprintf("%0.1f%%",.*100),
                                       .==0 ~ "0",
                                       variable %in% c("abs.err") ~ sprintf("%0.3f",.),
                                       variable %in% c("t.ref.surv") ~ sprintf("%0.1f",.),
                                       variable %in% c("rel.err") ~ sprintf("%0.1f%%",.*100), 
                                       variable %in% c("women.hiv","women.surv") ~ scales::comma_format()(round(.)), 
                                       TRUE ~ sprintf("%0.2f",.)))) %>% 
  mutate(variable=factor(variable,levels = c("abs.err","rel.err","t.ref.surv","women.surv","women.hiv","ceb.surv","ceb.hiv","cdceb.surv","cdceb.hiv","hiv.surv.ratio"),
                         labels=var_labels),
         statistic=factor(statistic,levels=c("mean","sd","median","min","max"),labels = c("mean","std dev","median","min","max"))
  ) %>%
  arrange(variable,statistic)


ft <- to.plot %>% flextable()

new_headers <- agegroups %>% gsub("X","",.) %>% gsub("_","-",.) %>% set_names(agegroups) %>% as.list

ft <- do.call(set_header_labels,c(list(ft),new_headers))

ft %<>% 
  set_header_labels(variable="Outcome variable") %>%
  border_remove() %>%
  hline_bottom(part="head",border = fp_border(width = 2) ) %>%
  hline_bottom(part="body",border = fp_border(width = 1) ) %>%
  hline(i=c(5,10),part="body",border  = fp_border(width = 1) ) %>%
  width(j=1,width = 2) %>%
  align(part="all",align = "left") %>%
  bold(part="head") %>%
  italic(j=2,part="body") %>%
  merge_v(j=1) %>%
  bg(j=2:ncol(to.plot),i=seq(1,nrow(to.plot),2),bg = "gray80")
  

doc <- read_docx() %>%
  body_add_par(value = paste0("Table 3 Bias in indeitect estimates applied to ",scales::comma_format()(nrow(inputs))," simulated populations"), style = "table title") %>% 
  body_add_flextable(ft)
  



print(doc,"./tables/table3.docx")
