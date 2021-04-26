library(mosaic)
library(MASS)
library(magrittr)
library(pscl)
library(rsq)
library(gridExtra)
library(ggfortify)
library(ggplot2)
library(rcompanion)
library(tidyverse)
library(car)
library(lubridate)
library(visreg)
library(ggpubr)
library(cowplot)
library(extrafont)



###PART 1: REGRESSSION WITH PUBLIC HEALTH ORDERS AS REPONSE
load("Data/ga_tidy_df")


df%<>%dplyr::select(c(sd_date, shelter_in_place, deaths, partisan, commute_out, pop, first_case, black, hispanic, median_HHI)) 

reg_df<-df

cutoffs<-c(.001, .002, 1)
resps<-c("sd_date", "shelter_in_place")


mod.list<-list()
cf.list<-list()

for(resp in resps){
  reg_df2<-dplyr::select(reg_df, partisan:median_HHI, resp=resp) %>%
    mutate_at(vars(partisan:median_HHI), ~zscore(as.numeric(.x), na.rm=TRUE)) %>%
    mutate_at(vars(resp), ~ifelse(.x<max(as.numeric(.x)), 1, 0))
  
  for(cutoff in cutoffs){
    reg_df3<-reg_df2[df$deaths/exp(df$pop)<cutoff,]
    
    mod<-stepAIC(glm(resp ~ .,
                     data=reg_df3, family=binomial(link="logit")), direction="both", trace=FALSE)
    
    coeffs<-tail(names(mod$coefficients), -1)
    
    cf.list[[paste(resp,cutoff, sep="-")]]<-data.frame("name"=coeffs, "coeff"=mod$coefficients[coeffs], "Model"=paste0(resp, " (per capita deaths less than ", cutoff, ")"), "lwr"=confint(mod)[,1][coeffs], "upr"=confint(mod)[,2][coeffs], "vif"=vif(mod)[coeffs], "pval"=summary(mod)$coefficients[,4][coeffs])
    mod.list[[paste(resp,cutoff, sep="-")]]<-mod
    
  }
  
}

cf_mat<-data.frame(do.call(rbind, cf.list)) 


#Fig 3: Partial Residual Plot

partisan_sd<-visreg(mod.list$`sd_date-1`, "partisan", gg=TRUE, points=list(size=.5, stroke=.2, col="#177ca3", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#bce1f4"), line=list(size=.3, col="#177ca3", alpha=1))+
  theme_classic()+labs(x="Republican-Democratic \nVote Difference", y="Local Social Distancing \nLikelihood (Partial Residual)")+theme(text=element_text(size=9))+scale_x_continuous(breaks=-3:1, labels=round(-3:1*sd(df$partisan, na.rm=TRUE)+mean(df$partisan, na.rm=TRUE), digits=2))
pop_sd<-visreg(mod.list$`sd_date-1`, "pop", gg=TRUE, points=list(size=.5, stroke=.2, col="#177ca3", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#bce1f4"), line=list(size=.3, col="#177ca3", alpha=1))+
  theme_classic()+labs(x="Population (Log Scale)", y="Local Social Distancing \nLikelihood(Partial Residual)")+theme(text=element_text(size=9))+scale_x_continuous(breaks=(log(c(5000, 50000, 500000))-mean(df$pop, na.rm=TRUE))/sd(df$pop, na.rm=TRUE), labels=c("5000", "50000", "500000"))


firstcase_sip<-visreg(mod.list$`shelter_in_place-1`, "first_case", gg=TRUE, jitter=TRUE, points=list(size=.5, stroke=.2, col="#177ca3", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#bce1f4"), line=list(size=.3, col="#177ca3", alpha=1))+
  theme_classic()+labs(x="Date First Case Reported", y="Local Shelter-in-Place \nLikelihood (Partial Residual)")+theme(text=element_text(size=9))+scale_x_continuous(breaks=c(-2.5, 0, 2.5, 5), labels=c("Mar 2", "Mar 25", "Apr 15", "May 7"))
pop_sip<-visreg(mod.list$`shelter_in_place-1`, "pop", gg=TRUE, points=list(size=.5, stroke=.2, col="#177ca3", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#bce1f4"), line=list(size=.3, col="#177ca3", alpha=1))+
  theme_classic()+labs(x="Population (Log Scale)", y="Local Shelter-in-Place \nLikelihood (Partial Residual)")+theme(text=element_text(size=9))+scale_x_continuous(breaks=(log(c(5000, 50000, 500000))-mean(df$pop, na.rm=TRUE))/sd(df$pop, na.rm=TRUE), labels=c("5000", "50000", "500000"))
  

fig3_pres<- arrangeGrob(pop_sd, partisan_sd, pop_sip, firstcase_sip, ncol=2)

fig3_pres <- as_ggplot(fig3_pres) +                               
  draw_plot_label(label = c("A. Population", "C. Population", "B. Partisanship", "D. First Case"), size = 10,
                  x = c(.09, .09, .59, .59)+.05, y = c(1, .5, 1, .5), hjust=0) 

ggsave(file="Figs/fig3_pres.pdf", fig3_pres, device=cairo_pdf, dpi=300, height=10.7, width=10.7, units="cm")

###PART 4: REGRESSSION WITH MOBILTY AS REPONSE

load("Data/ga_tidy_df")

df%<>%dplyr::select(!c(cases, commute_in, prop_food_stamp_SNAP_recip, prop_uninsured, prop_medicaid_eligible, atl_dist, white, asian, indigenous, hispanic, income_inequal, commute_out)) %>%
  filter(!is.na(end_mob))

reg_df<-df


cutoffs<-c(.001, .002, 1)
resps<-c("end_mob")

mod.list<-list()
cf.list<-list()


for(resp in resps){
 reg_df2<-dplyr::select(reg_df, median_HHI:black, age, resp=resp) %>%
    mutate_at(vars(median_HHI:resp), ~zscore(as.numeric(.x), na.rm=TRUE)) 
  
  for(cutoff in cutoffs){
    reg_df3<-reg_df2[df$deaths/exp(df$pop)<cutoff,]
    
    mod<-stepAIC(glm(resp ~ .,
                     data=reg_df3), direction="both", trace=FALSE)

    coeffs<-tail(names(mod$coefficients), -1)
    
    cf.list[[paste(resp,cutoff, sep="-")]]<-data.frame("name"=coeffs, "coeff"=mod$coefficients[coeffs], "Model"=paste0(resp, " (per capita deaths less than ", cutoff, ")"), "lwr"=confint(mod)[,1][coeffs], "upr"=confint(mod)[,2][coeffs], "vif"=vif(mod)[coeffs], "pval"=summary(mod)$coefficients[,4][coeffs])
    mod.list[[paste(resp,cutoff, sep="-")]]<-mod
    
  }
  
}

cf_mat<-data.frame(do.call(rbind, cf.list)) 

#Fig 5: Partial Residual for Mobility
age_mob<-visreg(mod, "age", gg=TRUE, points=list(size=.5, stroke=.2, col="#ec744e", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#f5c69d"), line=list(size=.5, col="#ec744e", alpha=1))+
  theme_classic()+labs(x="Weighted Infection Fatality Rate", y="Mobility (Partial Residual)")+theme(text=element_text(size=6))+scale_y_continuous(limits=(c(0, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), breaks=(c(0, 20, 40, 60, 80, 100, 120, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), labels=c(0, 20, 40, 60, 80, 100, 120, 140))+scale_x_continuous(breaks=c(-3, -1, 1, 3), labels=round(c(-3, -1, 1, 3)*sd(df$age, na.rm=TRUE)+mean(df$age, na.rm=TRUE), digits=3))
income_mob<-visreg(mod, "median_HHI", gg=TRUE, points=list(size=.5, stroke=.2, col="#ec744e", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#f5c69d"), line=list(size=.5, col="#ec744e", alpha=1))+
  theme_classic()+labs(x="Median Household Income", y="Mobility (Partial Residual)")+theme(text=element_text(size=6))+scale_y_continuous(limits=(c(0, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), breaks=(c(0, 20, 40, 60, 80, 100, 120, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), labels=c(0, 20, 40, 60, 80, 100, 120, 140))+scale_x_continuous(breaks=(c(40, 60, 80, 100)-mean(df$median_HHI, na.rm=TRUE))/sd(df$median_HHI, na.rm=TRUE), labels=c("40000", "60000", "80000", "100000"))
race_mob<-visreg(mod, "black", gg=TRUE, points=list(size=.5, stroke=.2, col="#ec744e", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#f5c69d"), line=list(size=.5, col="#ec744e", alpha=1))+
  theme_classic()+labs(x="Black Population Proportion", y="Mobility (Partial Residual)")+theme(text=element_text(size=6))+scale_y_continuous(limits=(c(0, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), breaks=(c(0, 20, 40, 60, 80, 100, 120, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), labels=c(0, 20, 40, 60, 80, 100, 120, 140))+scale_x_continuous(breaks=-1:3, labels=round(-1:3*sd(df$black, na.rm=TRUE)+mean(df$black, na.rm=TRUE), digits=2))



fig5_pres<- arrangeGrob(age_mob, income_mob, race_mob, ncol=3)

fig5_pres <- as_ggplot(fig5_pres) +                                
  draw_plot_label(label = c("A. Age", "B. Income", "C. Race"), 
                  x = c(0, 1/3, 2/3)+.1, y = c(1, 1, 1), size=10, hjust=0) 

ggsave(file="Figs/fig5_pres.pdf", fig5_pres, device=cairo_pdf, dpi=300, height=10.7/3, width=10.7, units="cm")




###Part 4: Epidemiological Response

load("Data/ga_tidy_df")


df%>%dplyr::select(!c(cases, commute_in, prop_food_stamp_SNAP_recip, partisan, prop_uninsured, prop_medicaid_eligible, atl_dist, income_inequal, white, asian, indigenous, hypertension_hosp)) %>%
  filter(!is.na(end_mob)) -> df

reg_df<-df

cutoffs<-c(.001, .002, 1)
resps<-c("cases_4wks", "deaths_6wks")

  
mod.list<-list()
cf.list<-list()

for(resp in resps){
  
  reg_df2<-dplyr::select(reg_df, asthma:black, first_case, end_mob, resp=resp) %>%
    mutate_at(vars(asthma:end_mob), ~zscore(as.numeric(.x), na.rm=TRUE)) 

  for(cutoff in cutoffs){
    reg_df3<-reg_df2[df$deaths/exp(df$pop)<cutoff,]
    mod<-stepAIC(glm.nb(resp ~ .,
                        data=reg_df3), direction="both", trace=FALSE)
    
    coeffs<-tail(names(mod$coefficients), -1)
    cf.list[[paste(resp,cutoff, sep="-")]]<-data.frame("name"=coeffs, "coeff"=mod$coefficients[coeffs], "Model"=paste0(resp, " (per capita deaths less than ", cutoff, ")"), "lwr"=confint(mod)[,1][coeffs], "upr"=confint(mod)[,2][coeffs], "vif"=vif(mod)[coeffs], "pval"=summary(mod)$coefficients[,4][coeffs])
    mod.list[[paste(resp,cutoff, sep="-")]]<-mod
      
  }
  
}

cf_mat<-data.frame(do.call(rbind, cf.list)) 
rsq<-lapply(mod.list, function(x) nagelkerke(x)$Pseudo.R.squared.for.model.vs.null[3])
# 
# 
# 
#Figure 6: Partial Residual Plots
pop_cases<-visreg(mod.list$`cases_4wks-1`, "pop", gg=TRUE, points=list(size=.5, stroke=.2, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71"), line=list(size=.3, col="#8c2031", alpha=1))+
  theme_classic()+labs(x="Population \n(Log Scale)", y="COVID19 Cases \n(Partial Residual)")+theme(text=element_text(size=6))+scale_y_continuous(limits=c(1,9), breaks=log(c(1, 10, 100, 1000)), labels=c(1, 10, 100, 1000))+scale_x_continuous(breaks=(log(c(5000, 50000, 500000))-mean(df$pop, na.rm=TRUE))/sd(df$pop, na.rm=TRUE), labels=c("5000", "50000", "500000"))
poll_cases<-visreg(mod.list$`cases_4wks-1`, "annual_PM2.5", gg=TRUE, points=list(alpha=.7, size=.5, stroke=.2, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=.3, col="#8c2031", alpha=1))+
  theme_classic()+labs(x="Air Pollution \n(PM2.5)", y="COVID19 Cases \n(Partial Residual)")+theme(text=element_text(size=6))+scale_y_continuous(limits=c(1,9), breaks=log(c(1, 10, 100, 1000)), labels=c(1, 10, 100, 1000))+scale_x_continuous(breaks=c(-3, -1, 1, 3), labels=sprintf("%.2f", round(c(-3, -1, 1, 3)*sd(df$annual_PM2.5, na.rm=TRUE)+mean(df$annual_PM2.5, na.rm=TRUE), digits=2)))
pov_cases<-visreg(mod.list$`cases_4wks-1`, "prop_poverty", gg=TRUE, points=list(alpha=.7, size=.5, stroke=.2, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=.3, col="#8c2031", alpha=1))+
    theme_classic()+labs(x="Percentage of Households \nBelow Poverty Line", y="COVID19 Cases \n(Partial Residual)")+theme(text=element_text(size=6))+scale_y_continuous(limits=c(1,9), breaks=log(c(1, 10, 100, 1000)), labels=c(1, 10, 100, 1000))+scale_x_continuous(breaks=-1:1*2, labels=sprintf("%.2f", round(-1:1*2*sd(df$prop_poverty, na.rm=TRUE)+mean(df$prop_poverty, na.rm=TRUE), digits=2)))
 

pop_deaths<-visreg(mod.list$`deaths_6wks-1`, "pop", gg=TRUE, points=list(alpha=.7, size=.5, stroke=.2, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=.3, col="#8c2031", alpha=1))+
    theme_classic()+labs(x="Population (Log Scale)", y="COVID19 Deaths \n(Partial Residual)")+theme(text=element_text(size=6))+scale_y_continuous(limits=c(-1.5,5), breaks=log(c(1, 10, 100)), labels=c(1, 10, 100))+scale_x_continuous(breaks=(log(c(5000, 50000, 500000))-mean(df$pop, na.rm=TRUE))/sd(df$pop, na.rm=TRUE), labels=c("5000", "50000", "500000"))
poll_deaths<-visreg(mod.list$`deaths_6wks-1`, "annual_PM2.5", gg=TRUE, points=list(alpha=.7, size=.5, stroke=.2, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=.3, col="#8c2031", alpha=1))+
  theme_classic()+labs(x="Air Pollution (PM2.5)", y="COVID19 Deaths \n(Partial Residual)")+theme(text=element_text(size=6))+scale_y_continuous(limits=c(-1.5,5), breaks=log(c(1, 10, 100, 1000)), labels=c(1, 10, 100, 1000))+scale_x_continuous(breaks=c(-3, -1, 1, 3), labels=sprintf("%.2f", round(c(-3, -1, 1, 3)*sd(df$annual_PM2.5, na.rm=TRUE)+mean(df$annual_PM2.5, na.rm=TRUE), digits=2)))
firstcase_deaths<-visreg(mod.list$`deaths_6wks-1`, "first_case", gg=TRUE, points=list(alpha=.7, size=.5, stroke=.2, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=.3, col="#8c2031", alpha=1))+
    theme_classic()+labs(x="Date First Case Reported", y="COVID19 Deaths \n(Partial Residual)")+theme(text=element_text(size=6))+scale_y_continuous(limits=c(-1.5,5), breaks=log(c(1, 10, 100)), labels=c(1, 10, 100))+scale_x_continuous(breaks=c(-2.5, 0, 2.5, 5), labels=c("Mar 2", "Mar 25", "Apr 15", "May 7"))
 

fig6_pres<- arrangeGrob(pop_cases, poll_cases, pov_cases, pop_deaths, poll_deaths, firstcase_deaths, nrow=2)
 
fig6_pres <- as_ggplot(fig6_pres) +
   draw_plot_label(label = c("A. Population", "D. Population", "B. Pollution", "E. Pollution", "C. Poverty", "F. First Case"), size=10,
                    x = c(0, 0, 1/3, 1/3, 2/3, 2/3)+.13, y = c(1, .5, 1, .5, 1, .5), hjust=0)

ggsave(file="Figs/fig6_pres.pdf", fig6_pres, device=cairo_pdf, dpi=300, height=10.7*2/3, width=10.7, units="cm")
# 
# 




