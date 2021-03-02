library(mosaic)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(magrittr)
library(gridExtra)

load("C:/Users/mallj/Onedrive/Mordecai/COVID/Data/ga_tidy_df")
load("C:/Users/mallj/Onedrive/Mordecai/COVID/Data/mobility_df")


#Prepare dataframe from event studies
#Dates for which data are available
my_dates<-as_date("2020-02-29")+c(1:49, 51:61)


#intialize vectors
day<-c()
mobility<-c()
county<-c()
sd<-c()
sip<-c()
sip_days<-c()
sd_days<-c()


#loop through all counties in Georgia with available mobility data and append to vectors
for(county_name in df$county){
  if(county_name %in% mobility_df$county){
    
    #mobility for selected county
    my_mobility<-filter(mobility_df, county==county_name)
    mobility<-c(mobility, tail(unlist(my_mobility), -1))
    
    my_df<-filter(df, county==county_name)
    day<-c(day, 1:49, 51:61)
    county<-c(county, rep(county_name, 60))
    
    
    #binary response: was public health order present?
    sd<-c(sd, ifelse(my_dates>=my_df$sd_date, 1, 0))
    sip<-c(sip, ifelse(my_dates>=my_df$shelter_in_place, 1, 0))
    
    #days since public health order
    sd_days<-c(sd_days, my_dates-my_df$sd_date)
    sip_days<-c(sip_days, my_dates-my_df$shelter_in_place)
  }
}

#intialize data frame
es<-data.frame("mobility"=as.numeric(mobility), "day"=day, "county"=county, "sd"=sd, "sip"=sip, "sd_days"=sd_days, "sip_days"=sip_days)


###Event Study 1: Effect of Social Distancing
#limit to 10 days pre/post order and set policy_day prior to order introduced as -1 (control period)
es_sd<-es[-which(abs(es$sd_days)>10 | es$sd_days==10),]
es_sd$sd_days<-as.factor(ifelse(es_sd$sd_days<0, -1, es_sd$sd_days))


#remove counties that are missing mobility-days 
missing_sd<-sapply(unique(es_sd$county), function(x) sum(is.na(es_sd$mobility[es_sd$county==x]))>0) %>%
  unique(es_sd$county)[.]

es_sd %<>% filter(! county %in% missing_sd)

#fit model
effect_sd<-lm(mobility~as.factor(day)+as.factor(county)+as.factor(sd_days), data=es_sd)


#extract coefficients for policy_days
sd_coeffs<-effect_sd$coefficients[grep("sd_days", names(effect_sd$coefficients))] %>%
            c(rep(0,10), .) %>%
            data.frame("y"=., "x"=-10:9)


#data frame used for plotting event study results (figure 5)
sd_df<-data.frame(mob = effect_sd$model$mobility,
                  int = as.numeric(effect_sd$coefficients[1]),
                  day = effect_sd$model$`as.factor(day)`,
                  county = effect_sd$model$`as.factor(county)`) %>%
  
        #Find marginal effect of sd_days given county & day fixed effect
        mutate_at(vars(day), ~paste0("as.factor(day)", as.character(.x)))  %>%
        mutate_at(vars(day), ~effect_sd$coefficients[.x]) %>%
        mutate_at(vars(county), ~paste0("as.factor(county)", as.character(.x))) %>%
        mutate_at(vars(county), ~effect_sd$coefficients[.x]) %>%
        mutate_at(vars(day, county), ~ifelse(is.na(.x), 0, .x)) %>%
        mutate(y = mob - (int+county+day)) %>%

        #x axis for event study plot - difference (in days) between observation and public health order introduction
        mutate(x=rep(-10:9, length(unique(effect_sd$model$`as.factor(county)`))))


###Event Study 1: Effect of Shelter-in-Place
#limit to 10 days pre/post order and set policy_day prior to order introduced as -1 (control period)
es_sip<-es[-which(abs(es$sip_days)>10 | es$sip_days==10),]
es_sip$sip_days<-as.factor(ifelse(es_sip$sip_days<0, -1, es_sip$sip_days))


#remove counties that are missing mobility-days 
missing_sip<-sapply(unique(es_sip$county), function(x) sum(is.na(es_sip$mobility[es_sip$county==x]))>0) %>%
  unique(es_sip$county)[.]

es_sip %<>% filter(! county %in% missing_sip)

#fit model
effect_sip<-lm(mobility~as.factor(day)+as.factor(county)+as.factor(sip_days), data=es_sip)


#extract coefficients for policy_days
sip_coeffs<-effect_sip$coefficients[grep("sip_days", names(effect_sip$coefficients))] %>%
  c(rep(0,10), .) %>%
  data.frame("y"=., "x"=-10:9)


#data frame used for plotting event study results (figure 5)
sip_df<-data.frame(mob = effect_sip$model$mobility,
                  int = as.numeric(effect_sip$coefficients[1]),
                  day = effect_sip$model$`as.factor(day)`,
                  county = effect_sip$model$`as.factor(county)`) %>%
  
  #Find marginal effect of sd_days given county & day fixed effect
  mutate_at(vars(day), ~paste0("as.factor(day)", as.character(.x)))  %>%
  mutate_at(vars(day), ~effect_sip$coefficients[.x]) %>%
  mutate_at(vars(county), ~paste0("as.factor(county)", as.character(.x))) %>%
  mutate_at(vars(county), ~effect_sip$coefficients[.x]) %>%
  mutate_at(vars(day, county), ~ifelse(is.na(.x), 0, .x)) %>%
  mutate(y = mob - (int+county+day)) %>%
  
  #x axis for event study plot - difference (in days) between observation and public health order introduction
  mutate(x=rep(-10:9, length(unique(effect_sip$model$`as.factor(county)`))))



###Plot Figure 4
#Get significance stars  
get_signif_stars<-function(mod){
  pvals<-tail(summary(mod)$coefficients[,4], 10) %>%
    cut(c(1, .05, .01, .001,0), labels=c("...", "..", ".", " "))
  
  return(pvals)
  
}


#Fig 4A
sd_plot<-ggplot()+geom_jitter(data=sd_df, mapping=aes(y=y, x=x), color="#a2d2a3", pch=1, width=.15, size=1)+
  geom_point(data=sd_coeffs, mapping=aes(y=y, x=x), size=3, shape=15, color="#49945b")+xlab("Days Since Policy")+
  geom_text(data=sd_coeffs, mapping=aes(y=y+2, x=x), label=c(rep("", 10), as.character(get_signif_stars(effect_sd))), colour="white", size=3, fontface="bold")+
  ylab("Change in Mobility (%)")+ggtitle("A. Social Distancing")+theme_bw()+
  theme(text=element_text(size=15), legend.position="none", plot.title = element_text(size=20, hjust=.5), 
        panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(aes(xintercept=-.5), linetype="dotted")+ylim(-60, 70)

#Fig 4B
sip_plot<-ggplot()+geom_jitter(data=sip_df, mapping=aes(y=y, x=x), color="#a2d2a3", pch=1, width=.15, size=1)+
  geom_point(data=sip_coeffs, mapping=aes(y=y, x=x), color="#49945b", size=3, shape=15)+xlab("Days Since Policy")+
  geom_text(data=sip_coeffs, mapping=aes(y=y+2, x=x), label=c(rep("", 10), as.character(get_signif_stars(effect_sip))), colour="white", size=3, fontface="bold")+
  ylab("Change in Mobility (%)")+ggtitle("B. Shelter-in-Place")+theme_bw()+
  theme(text=element_text(size=15), legend.position="none", plot.title = element_text(size=20, hjust=.5), 
        panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(aes(xintercept=-.5), linetype="dotted")+ylim(c(-60, 70))

#Combine and save pdf
fig4<- arrangeGrob(sd_plot, sip_plot, ncol=2)
ggsave(file="C:/Users/mallj/OneDrive/Mordecai/COVID/Figs/fig4.pdf", fig4, device=cairo_pdf, dpi=300, height=4, width=7.4, units="in")


#Supplementary Tables 3 and 4: Fixed Effects of County and Date
county_effect<-data.frame("county"=df$county) %>%
  mutate(
    "social distancing"=format(round(effect_sd$coefficients[paste0("as.factor(county)",county)], digits=2), nsmall=2),
    "shelter-in-place"=format(round(effect_sip$coefficients[paste0("as.factor(county)",county)], digits=2), nsmall=2)
  )

date_effect<-data.frame("index"=c(1:49, 51:61)) %>%
  transmute(
    "date"=as_date("2020-02-29")+index,
    "social distancing"=format(round(effect_sd$coefficients[paste0("as.factor(day)",index)], digits=2), nsmall=2),
    "shelter-in-place"=format(round(effect_sip$coefficients[paste0("as.factor(day)",index)], digits=2), nsmall=2)
  ) 

write.csv(county_effect, "C:/Users/mallj/OneDrive/Mordecai/COVID/Data/Results/county_effect.csv")
write.csv(date_effect, "C:/Users/mallj/OneDrive/Mordecai/COVID/Data/Results/date_effect.csv")



