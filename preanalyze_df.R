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
load("C:/Users/mallj/Onedrive/Mordecai/COVID/Data/ga_tidy_df")

df%<>%dplyr::select(!c(cases, commute_in, prop_food_stamp_SNAP_recip, partisan, prop_uninsured, prop_medicaid_eligible, atl_dist, CHD_hosp, income_inequal, commute_out, white, asian, hispanic, indigenous, prop_poverty, ob_prev_adj, hypertension_hosp)) %>%
  filter(!is.na(auc_mob) & !is.na(min_mob))

####[ART ONE: LM
cutoffs<-c(1, .002, .001)
resps<-c("cases_4wks", "deaths_4wks")
npi_measures<-c("end_mob")

coeffs<-c("age", "black", "pop", "asthma", "prop_diabetes", "annual_PM2.5", "education", "median_HHI", "unemployment_rate", "npi", "housing_density", "first_case")
p<-list()
mod.list<-list()
model_names<-c("Cases (All)", "Cases (per capita deaths  <  2 per 1,000)", "Cases (per capita deaths < 1 per 1,000)", "Deaths (All)", "Deaths (per capita deaths < 2 per 1,000)", "Deaths (per capita deaths < 1 per 1,000)")



for(npi_type in npi_measures){
  i<-1
  p[[npi_type]]<-list()
  mod.list[[npi_type]]<-list()
  
  
  all_rsq<-c()
  
  for(resp in resps){
    df2<-dplyr::select(df, asthma:black, first_case, npi=npi_type, resp=resp) %>%
      mutate_at(vars(asthma:npi), ~zscore(as.numeric(.x), na.rm=TRUE)) 
    
    
    for(cutoff in cutoffs){
      df3<-df2[df$deaths/exp(df$pop)<cutoff,]
      
      mod<-stepAIC(glm.nb(resp ~ .,
                          data=df3), direction="both", trace=FALSE)
      
      if(cutoff==1 & resp=="cases_4wks"){
        pop_cases<-visreg(mod, "pop", gg=TRUE, points=list(size=2.5, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71"), line=list(size=1, col="#8c2031", alpha=1))+
          theme_classic()+labs(x="Population (Log Scale)", y="COVID19 Cases \n(Partial Residual)")+theme(text=element_text(size=20))+scale_y_continuous(limits=c(1,9), breaks=log(c(1, 10, 100, 1000)), labels=c(1, 10, 100, 1000))+scale_x_continuous(breaks=(log(c(5000, 50000, 500000))-mean(df$pop, na.rm=TRUE))/sd(df$pop, na.rm=TRUE), labels=c("5000", "50000", "500000"))
        race_cases<-visreg(mod, "black", gg=TRUE, points=list(alpha=.7, size=2.5, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=1, col="#8c2031", alpha=1))+
          theme_classic()+labs(x="Black Population Proportion", y="COVID19 Cases \n(Partial Residual)")+theme(text=element_text(size=20))+scale_y_continuous(limits=c(1,9), breaks=log(c(1, 10, 100, 1000)), labels=c(1, 10, 100, 1000))+scale_x_continuous(breaks=-1:3, labels=round(-1:3*sd(df$black, na.rm=TRUE)+mean(df$black, na.rm=TRUE), digits=2))
        age_cases<-visreg(mod, "age", gg=TRUE, points=list(alpha=.7, size=2.5, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=1, col="#8c2031", alpha=1))+
          theme_classic()+labs(x="Weighted Infection Fatality Rate", y="COVID19 Cases \n(Partial Residual)")+theme(text=element_text(size=20))+scale_y_continuous(limits=c(1,9), breaks=log(c(1, 10, 100, 1000)), labels=c(1, 10, 100, 1000))+scale_x_continuous(breaks=-1:2*2, labels=round(-1:2*2*sd(df$age, na.rm=TRUE)+mean(df$age, na.rm=TRUE), digits=3))
        
      }
      
      if(cutoff==1 & resp=="deaths_4wks"){
        pop_deaths<-visreg(mod, "pop", gg=TRUE, points=list(alpha=.7, size=2.5, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=1, col="#8c2031", alpha=1))+
          theme_classic()+labs(x="Population (Log Scale)", y="COVID19 Deaths \n(Partial Residual)")+theme(text=element_text(size=19))+scale_y_continuous(limits=c(-1.5,5), breaks=log(c(1, 10, 100)), labels=c(1, 10, 100))+scale_x_continuous(breaks=(log(c(5000, 50000, 500000))-mean(df$pop, na.rm=TRUE))/sd(df$pop, na.rm=TRUE), labels=c("5000", "50000", "500000"))
        edu_deaths<-visreg(mod, "education", gg=TRUE, points=list(alpha=.7, size=2.5, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=1, col="#8c2031", alpha=1))+
          theme_classic()+labs(x="Educational Attainment", y="COVID19 Deaths \n(Partial Residual)")+theme(text=element_text(size=19))+scale_y_continuous(limits=c(-1.5,5), breaks=log(c(1, 10, 100)), labels=c(1, 10, 100))+scale_x_continuous(breaks=-2:2, labels=round((-2:2*sd(df$education, na.rm=TRUE)+mean(df$education, na.rm=TRUE))/100, digits=2))
        firstcase_deaths<-visreg(mod, "first_case", gg=TRUE, points=list(alpha=.7, size=2.5, col="#8c2031", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .3, 1)), fill.par=list(fill="#db7e71", alpha=1), line=list(size=1, col="#8c2031", alpha=1))+
          theme_classic()+labs(x="Date First Case Reported", y="COVID19 Deaths \n(Partial Residual)")+theme(text=element_text(size=19))+scale_y_continuous(limits=c(-1.5,5), breaks=log(c(1, 10, 100)), labels=c(1, 10, 100))+scale_x_continuous(breaks=c(-2.5, 0, 2.5, 5), labels=c("Mar 2", "Mar 25", "Apr 15", "May 7"))
        
      }
      
      
      mod.list[[npi_type]][[i]]<-data.frame("name"=coeffs, "coeff"=ifelse(coeffs %in% names(mod$coefficients), mod$coefficients[coeffs], NA), "y"=c(length(coeffs):1)-(i+1)/8,"modname"=paste(resp, cutoff), "Model"=model_names[i], "lwr"=ifelse(coeffs %in% names(mod$coefficients), confint(mod)[,1][coeffs], NA), "upr"=ifelse(coeffs %in% names(mod$coefficients), confint(mod)[,2][coeffs], NA), "vif"=ifelse(coeffs %in% names(mod$coefficients), vif(mod)[coeffs], NA), "pval"=ifelse(coeffs %in% names(mod$coefficients), summary(mod)$coefficients[,4][coeffs], NA), "rsq"=rep(rsq(mod), length(coeffs)))
      
      coeff_nopolicy<-c("resp", names(mod$coefficients)[! names(mod$coefficients) %in% c("(Intercept)", "npi")]) 
      
      mod_nopolicy<-glm.nb(resp ~ ., data=df3[coeff_nopolicy])
      ptl_rsq<-rsq.partial(mod, mod_nopolicy, type="lr")$partial.rsq
      
      
      all_rsq<-c(all_rsq, nagelkerke(mod)$Pseudo.R.squared.for.model.vs.null[3])
      
      exp<-predict(mod, df2)
      
      obs<-log(df[[resp]]) %>%
        ifelse(. <= -1, -1, .)
      
      
      #p[[i]]<-data.frame("obs"=obs, "exp"=exp, "early_policy"=factor(df$policy_delay<0, levels=c("FALSE", "TRUE")), "did_sd"=df$sd_date<18345) %>%
      #        ggplot(., aes(x=exp, y=obs, color=early_policy, alpha=.2))+geom_segment(x=-10, xend=10, y=-10, yend=10, size=2, color="#c83616")+geom_point(size=3)+scale_color_manual(values=c("#fc8d59", "#1a9850"))+
      #            theme(text=element_text(size=20), legend.position="none")+xlab("Expected (log)")+ylab("Observed (log)")+ggtitle( paste(str_to_title(resp), " (R² = ", round(all_rsq, 3), "; Partial R² = ", round(ptl_rsq, 3), ")", sep=""))
      
      i<-i+1
    }
    
  }
  
  cf_mat<-data.frame(do.call(rbind, mod.list[[npi_type]])) %>%
    mutate_at(vars(Model), ~factor(.x, levels=model_names)) 
  
  print(paste(npi_type, max(cf_mat$vif, na.rm=TRUE)))
  #add labels to scale_y_continuous scale_y_continuous(name="", breaks=c(0:9)+.5)+
  p[[npi_type]]<-ggplot(cf_mat, aes(x=coeff, y=y, color=Model))+geom_point(size=2)+scale_y_continuous(name="", breaks=c(0:(length(coeffs)-1))+.5, labels=rev(c("Age", "Race", "Population", "Asthma", "Diabetes", "Pollution", "Education", "Income", "Unemployment", "Mobility", "Housing Density", "First Case")))+
    scale_color_manual(values=c("#0f572e", "#1a9850", "#91cf60", "#c83616", "#fc8d59", "#f8c536"))+
    geom_hline(yintercept=c(1:11-1/12))+
    geom_vline(xintercept=0, color="gray")+
    theme(text=element_text(size=30), legend.position="bottom", element_blank())+
    guides(colour=guide_legend(ncol=1, title=NULL))+
    xlab("Effect Size")+xlim(-1.5, 1.5)+
    geom_segment(aes(x=lwr, y=y, xend=upr, yend=y, color=Model), size=1, data=cf_mat)
}  


cf_mat

ggsave("C:/Users/mallj/OneDrive/Mordecai/COVID/Figs/coefplot_epi.png", plot=p[[1]],
       device="png", width=10, height=14,
       units="in")


fig6_pres<- arrangeGrob(pop_cases, race_cases, age_cases, pop_deaths, firstcase_deaths, edu_deaths, nrow=2)

fig6_pres <- as_ggplot(fig6_pres) +                                
  draw_plot_label(label = c("A. Population", "D. Population", "B. Race", "E. First Case", "C. Age", "F. Education"), size = 30,
                  x = c(0+.01, 0, 1/3+.01, 1/3, 2/3+.01, 2/3)+.08, y = c(1, .5, 1, .5, 1, .5), hjust=0) 

ggsave(file="C:/Users/mallj/OneDrive/Mordecai/COVID/Figs/fig6_pres.png", fig6_pres, device="png", height=10, width=15, units="in") 



###Part Two: PCA###
coeffs<-c("end_mob", "PC1", "PC2")
mod.list<-list()
p<-list()
i<-1

for(resp in resps){
  pca<-dplyr::select(df, c(asthma:nonwhite_pct, first_case)) %>%
    mutate_all(~zscore(as.numeric(.x), na.rm=TRUE)) %>%
    prcomp() 
  
  pca_df<-pca[["x"]] %>%
    .[,1:2] %>%
    apply(2, function(x) x*-1) %>%
    cbind(dplyr::select(df, end_mob)) %>%
    mutate_all(~zscore(.x)) %>%
    add_column(resp = unlist(dplyr::select(df, resp))) 
  
  for(cutoff in cutoffs){
    pca_df2<-pca_df[df$deaths/exp(df$pop)<cutoff,]

    reg<-list()
    
    reg[[1]]<-ggplot(data=pca_df2, aes(y=end_mob, x=PC1))+stat_summary(fun.data= mean_cl_normal) + 
      geom_smooth(method='lm')+theme(text=element_text(size=20))+ylab("Mobility")+ggtitle(paste0("RÂ²: ", round(cor(pca_df$PC1, pca_df$end_mob)^2, digits=3)))
    
    reg[[2]]<-ggplot(data=pca_df2, aes(y=end_mob, x=PC2))+stat_summary(fun.data= mean_cl_normal) + 
      geom_smooth(method='lm')+theme(text=element_text(size=20))+ylab("Mobility")+ggtitle(paste0("RÂ²: ", round(cor(pca_df$PC2, pca_df$end_mob)^2, digits=3)))
    
    grid.arrange(reg[[1]], reg[[2]], ncol=2)
    
    mod_nopolicy<-glm.nb(resp ~ PC1 + PC2, data=data.frame(pca_df2))
    mod<-glm.nb(resp ~ .,
                         data=data.frame(pca_df2))
    
    all_rsq<-rsq(mod, type='lr')
    ptl_rsq<-rsq.partial(mod, mod_nopolicy, type="lr")$partial.rsq
    exp<-predict(mod, data.frame(pca_df))
    obs<-log(df[[resp]])
    obs<-ifelse(obs<=-1, -1, obs)
  
    
    my_res<-list("obs"=obs, "exp"=exp, "all_rsq"=all_rsq, "ptl_rsq"=ptl_rsq)
    mod.list[[i]]<-data.frame("Model"=model_names[i], "name"=coeffs, "coeff"=as.numeric(mod$coefficients[coeffs]), "y"=c(3:1)-(i+1)/8, "lwr"=as.numeric(confint(mod)[,1][coeffs]), "upr"=as.numeric(confint(mod)[,2][coeffs]), "pval"=summary(mod)$coefficients["end_mob",4])
  
    
    #obs_exp<-data.frame("obs"=obs, "exp"=exp, "early_policy"=factor(df$policy_delay<0, levels=c("FALSE", "TRUE")), "did_sd"=df$sd_date<18345) 
    #p[[i]]<-ggplot(obs_exp, aes(x=exp, y=obs, color=early_policy, alpha=.2))+geom_segment(x=-10, xend=10, y=-10, yend=10, size=2, color="#c83616")+geom_point(size=3)+scale_color_manual(values=c("#fc8d59", "#1a9850"))+
    #  theme(text=element_text(size=20), legend.position="none")+xlab("Expected (log)")+ylab("Observed (log)")+ggtitle( paste(str_to_title(resp), " (RÂ² = ", round(all_rsq, 3), "; Partial RÂ² = ", round(ptl_rsq, 3), ")", sep=""))
    
    i<-i+1
  }
}


cf_mat<-data.frame(do.call(rbind, mod.list))
cf_mat$Model<-factor(cf_mat$Model, level=model_names)

ggplot(cf_mat, aes(x=coeff, y=y, color=Model))+geom_hline(yintercept=c(1:2)-1/12)+
  geom_vline(xintercept=seq(-1, 1, by=.5), color=c("white", "white", "gray", "white", "white"))+
  geom_point(size=3)+scale_y_continuous(name="", breaks=c(0:2)+.5, labels=c("PC2 \n(Race)", "PC1 \n(Household Income, \nPopulation Size)", "Mobility"))+
  scale_color_manual(values=c("#0f572e", "#1a9850", "#91cf60", "#c83616", "#fc8d59", "#f8c536"))+
  theme(text=element_text(size=30), element_blank())+
  xlab("Effect Size")+xlim(-1.2, 1.2)+
  geom_segment(aes(x=lwr, y=y, xend=upr, yend=y, color=Model), data=cf_mat)


grid.arrange(p[[1]], p[[4]], ncol=2)
grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], ncol=3)




autoplot(pca, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size =5)+theme(text=element_text(size=20))


p<-ggplot(pca,aes(x=PC1,y=PC2, color=mat[,"did_sip"], label=row.names(mat)))+
p<-p+geom_point()+ geom_text(size=3) + labs(color = "Had SIPÂ²") 
p

pca

t(res)
pred2<-exp(predict(mod2, mat))

###RATIO V MOBILITY, POLICY DELAY
pred<-predict(mod, mat)
ratio<-mat$deaths_percap/pred

par(mfrow=c(1,2))
plot(mat$min_mobility, ratio, ylim=c(0, max(ratio)))
abline(h=1, col="red")
plot(mat$min_mobility, ratio, ylim=c(0, max(ratio)))
abline(h=1, col="red")


plot(mat$policy_delay, ratio)


###PART 4: REGRESSSION WITH MOBILTY AS REPONSE

load("C:/Users/mallj/Onedrive/Mordecai/COVID/Data/ga_tidy_df")

df%<>%dplyr::select(!c(cases, commute_in, prop_food_stamp_SNAP_recip, prop_uninsured, prop_medicaid_eligible, atl_dist, CHD_hosp, white, asian, indigenous, hispanic, income_inequal, commute_out)) %>%
  filter(!is.na(auc_mob) & !is.na(end_mob))

####[ART ONE: LM
cutoffs<-c(1, .002, .001)
cutoff_names<-c("(all)", "(per capita deaths < 2 per 1,000)", "(per capita deaths < 1 per 1,000)")
resps<-c("end_mob")

coeffs<-c("age", "median_HHI", "black")
p<-list()
fig5<-list()
mod.list<-list()
resp_vect<-rep(cutoff_names, 1)
rep_vect<-rep("Mobility", each=3)

model_names<-paste(rep_vect, resp_vect)

i<-1

for(resp in resps){
  df2<-dplyr::select(df, median_HHI:black, age, resp=resp) %>%
    mutate_at(vars(median_HHI:resp), ~zscore(as.numeric(.x), na.rm=TRUE)) 
  
  all_rsq<-c()
  
  for(cutoff in cutoffs){
    df3<-df2[df$deaths/exp(df$pop)<cutoff,]
    
    mod<-stepAIC(glm(resp ~ .,
                     data=df3), direction="both", trace=FALSE)
    
    if(cutoff==1){
      age_mob<-visreg(mod, "age", gg=TRUE, points=list(size=2.5, col="#ec744e", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#f5c69d"), line=list(size=1, col="#ec744e", alpha=1))+
        theme_classic()+labs(x="Weighted Infection Fatality Rate", y="Mobility (Partial Residual)")+theme(text=element_text(size=20))+scale_y_continuous(limits=(c(0, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), breaks=(c(0, 20, 40, 60, 80, 100, 120, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), labels=c(0, 20, 40, 60, 80, 100, 120, 140))+scale_x_continuous(breaks=c(-3, -1, 1, 3), labels=round(c(-3, -1, 1, 3)*sd(df$age, na.rm=TRUE)+mean(df$age, na.rm=TRUE), digits=3))
        income_mob<-visreg(mod, "median_HHI", gg=TRUE, points=list(size=2.5, col="#ec744e", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#f5c69d"), line=list(size=1, col="#ec744e", alpha=1))+
        theme_classic()+labs(x="Median Household Income", y="Mobility (Partial Residual)")+theme(text=element_text(size=20))+scale_y_continuous(limits=(c(0, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), breaks=(c(0, 20, 40, 60, 80, 100, 120, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), labels=c(0, 20, 40, 60, 80, 100, 120, 140))+scale_x_continuous(breaks=(c(40, 60, 80, 100)-mean(df$median_HHI, na.rm=TRUE))/sd(df$median_HHI, na.rm=TRUE), labels=c("40000", "60000", "80000", "100000"))
      race_mob<-visreg(mod, "black", gg=TRUE, points=list(size=2.5, col="#ec744e", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#f5c69d"), line=list(size=1, col="#ec744e", alpha=1))+
        theme_classic()+labs(x="Black Population Proportion", y="Mobility (Partial Residual)")+theme(text=element_text(size=20))+scale_y_continuous(limits=(c(0, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), breaks=(c(0, 20, 40, 60, 80, 100, 120, 140)-mean(df$end_mob, na.rm=TRUE))/sd(df$end_mob, na.rm=TRUE), labels=c(0, 20, 40, 60, 80, 100, 120, 140))+scale_x_continuous(breaks=-1:3, labels=round(-1:3*sd(df$black, na.rm=TRUE)+mean(df$black, na.rm=TRUE), digits=2))
        
      
      #"#0f572e", "#1a9850", "#91cf60", "#c83616", "#fc8d59", "#f8c536"
    }
    
    mod.list[[i]]<-data.frame("name"=coeffs, "coeff"=ifelse(coeffs %in% names(mod$coefficients), mod$coefficients[coeffs], NA), "y"=c(length(coeffs):1)-(i+1)/5, "Model"=model_names[i], "lwr"=ifelse(coeffs %in% names(mod$coefficients), confint(mod)[,1][coeffs], NA), "upr"=ifelse(coeffs %in% names(mod$coefficients), confint(mod)[,2][coeffs], NA), "vif"=ifelse(i %in% c(11, 12, 18), rep(NA, length(coeffs)), ifelse(coeffs %in% names(mod$coefficients), vif(mod)[coeffs], NA)), "pval"=summary(mod)$coefficients[coeffs, 4])
    
    coeff_nopolicy<-c("resp", names(mod$coefficients)[! names(mod$coefficients) %in% c("(Intercept)")]) 
    
    #mod_nopolicy<-glm.nb(resp ~ ., data=df3[coeff_nopolicy])
    #ptl_rsq<-rsq.partial(mod, mod_nopolicy, type="lr")$partial.rsq
    
    
    all_rsq<-c(all_rsq, rsq(mod))
    
    #exp<-predict(mod, df2)
    
    #obs<-log(df[[resp]]) %>%
    #  ifelse(. <= -1, -1, .)
    
    
    #p[[i]]<-data.frame("obs"=obs, "exp"=exp, "early_policy"=factor(df$policy_delay<0, levels=c("FALSE", "TRUE")), "did_sd"=df$sd_date<18345) %>%
    #        ggplot(., aes(x=exp, y=obs, color=early_policy, alpha=.2))+geom_segment(x=-10, xend=10, y=-10, yend=10, size=2, color="#c83616")+geom_point(size=3)+scale_color_manual(values=c("#fc8d59", "#1a9850"))+
    #            theme(text=element_text(size=20), legend.position="none")+xlab("Expected (log)")+ylab("Observed (log)")+ggtitle( paste(str_to_title(resp), " (R² = ", round(all_rsq, 3), "; Partial R² = ", round(ptl_rsq, 3), ")", sep=""))
    
    i<-i+1
  }
  
}

cf_mat<-data.frame(do.call(rbind, mod.list)) %>%
  mutate_at(vars(Model), ~factor(.x, levels=model_names)) 


print(paste(model_names, max(cf_mat$vif, na.rm=TRUE)))

#add labels to scale_y_continuous scale_y_continuous(name="", breaks=c(0:9)+.5)+
p<-ggplot(cf_mat, aes(x=coeff, y=y, color=Model))+geom_point(size=5)+scale_y_continuous(name="", breaks=c(0:(length(coeffs)-1))+.5, labels=c("Unemployment", "Income", "Age"))+
  scale_color_manual(values=c("#9c0a00", "#ff2819", "#ff968f", "#4e0094", "#c98cff", "#d5a6ff", "#b8a500", "#ffe81c", "#fff496", "#067a00", "#25d61c", "#a6e8a2","#ba4e00", "#ff7714", "#ffb27a", "#006691", "#38aee0", "#a6e4ff"))+
  geom_hline(yintercept=c(1:2)-1/24)+
  geom_vline(xintercept=0, color="gray")+
  theme(text=element_text(size=30), legend.position="bottom", element_blank())+
  guides(colour=guide_legend(ncol=1, title=NULL))+
  xlab("Effect Size")+xlim(-1.5, 1.5)+
  geom_segment(aes(x=lwr, y=y, xend=upr, yend=y, color=Model), size=2, data=cf_mat)#+ggtitle(paste0(npi_type, ": ", sprintf("%0.4f", round(mean(all_rsq), digits = 4))))

p

ggsave("C:/Users/mallj/OneDrive/Mordecai/COVID/Figs/coefplot_mobility.png", plot=p,
       device="png", width=10, height=10,
       units="in")

fig5_pres<- arrangeGrob(age_mob, income_mob, race_mob, ncol=3)

fig5_pres <- as_ggplot(fig5_pres) +                                
  draw_plot_label(label = c("A. Age", "B. Income", "C. Race"), size = 30,
                  x = c(.06, 1/3+.06, 2/3+.06), y = c(1, 1, 1), hjust=0) 

ggsave(file="C:/Users/mallj/OneDrive/Mordecai/COVID/Figs/fig5_pres.png", fig5_pres, device="png", height=6, width=14.8, units="in") 



####

nual(values=c("#9c0a00", "#ff2819", "#ff968f", "#4e0094", "#c98cff", "#d5a6ff", "#b8a500", "#ffe81c", "#fff496", "#067a00", "#25d61c", "#a6e8a2","#ba4e00", "#ff7714", "#ffb27a", "#006691", "#38aee0", "#a6e4ff"))+
  geom_hline(yintercept=c(1:3)-1/24)+
  geom_vline(xintercept=0, color="gray")+
  theme(text=element_text(size=30), legend.position="bottom", element_blank())+
  guides(colour=guide_legend(ncol=1, title=NULL))+
  xlab("Effect Size")+xlim(-3.5, 3.5)+
  geom_segment(aes(x=lwr, y=y, xend=upr, yend=y, color=Model), size=2, data=cf_mat)#+ggtitle(paste0(npi_type, ": ", sprintf("%0.4f", round(mean(all_rsq), digits = 4))))

####
load("C:/Users/mallj/Onedrive/Mordecai/COVID/Data/ga_tidy_df")

df%<>%dplyr::select(c(sd_date, shelter_in_place, deaths, partisan, commute_out, pop, first_case, black, hispanic, median_HHI)) 

####[ART ONE: LM
cutoffs<-c(1, .002, .001)
cutoff_names<-c("(all)", "(per capita deaths < 2 per 1,000)", "(per capita deaths < 1 per 1,000)")
resps<-c("sd_date", "shelter_in_place")

coeffs<-c("partisan", "commute_out", "black", "hispanic", "pop", "first_case", "median_HHI")

p<-list()
mod.list<-list()
all_rsq<-c()
resp_vect<-rep(cutoff_names, 2)
rep_vect<-rep(c("Social Distancing", "Shelter in Place"), each=3)

model_names<-paste(rep_vect, resp_vect)

i<-1

for(resp in resps){
  df2<-dplyr::select(df, partisan:median_HHI, resp=resp) %>%
    mutate_at(vars(partisan:median_HHI), ~zscore(as.numeric(.x), na.rm=TRUE)) %>%
    mutate_at(vars(resp), ~ifelse(.x<max(as.numeric(.x)), 1, 0))
  

  
  for(cutoff in cutoffs){
    df3<-df2[df$deaths/exp(df$pop)<cutoff,]
    
    mod<-stepAIC(glm(resp ~ .,
                     data=df3, family=binomial(link="logit")), direction="both", trace=FALSE)
    
    
    if(cutoff==1 & resp=="sd_date"){
      partisan_sd<-visreg(mod, "partisan", gg=TRUE, points=list(size=2.5, col="#177ca3", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#bce1f4"), line=list(size=1, col="#177ca3", alpha=1))+
        theme_classic()+labs(x="Republican-Democratic Vote Difference", y="Local Social Distancing \nLikelihood (Partial Residual)")+theme(text=element_text(size=20))+scale_x_continuous(breaks=-3:1, labels=round(-3:1*sd(df$partisan, na.rm=TRUE)+mean(df$partisan, na.rm=TRUE), digits=2))
      pop_sd<-visreg(mod, "pop", gg=TRUE, points=list(size=2.5, col="#177ca3", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#bce1f4"), line=list(size=1, col="#177ca3", alpha=1))+
        theme_classic()+labs(x="Population (Log Scale)", y="Local Social Distancing \nLikelihood(Partial Residual)")+theme(text=element_text(size=20))+scale_x_continuous(breaks=(log(c(5000, 50000, 500000))-mean(df$pop, na.rm=TRUE))/sd(df$pop, na.rm=TRUE), labels=c("5000", "50000", "500000"))
      
    }
    
    if(cutoff==1 & resp=="shelter_in_place"){
      firstcase_sip<-visreg(mod, "first_case", gg=TRUE, jitter=TRUE, points=list(size=2.5, col="#177ca3", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#bce1f4"), line=list(size=1, col="#177ca3", alpha=1))+
        theme_classic()+labs(x="Date First Case Reported", y="Local Shelter-in-Place \nLikelihood (Partial Residual)")+theme(text=element_text(size=20))+scale_x_continuous(breaks=c(-2.5, 0, 2.5, 5), labels=c("Mar 2", "Mar 25", "Apr 15", "May 7"))
      pop_sip<-visreg(mod, "pop", gg=TRUE, points=list(size=2.5, col="#177ca3", pch=ifelse(df$deaths/exp(df$pop)>.001, 16, 1), alpha=ifelse(df$deaths/exp(df$pop)>.001 & df$deaths/exp(df$pop)<.002, .5, 1)), fill.par=list(fill="#bce1f4"), line=list(size=1, col="#177ca3", alpha=1))+
        theme_classic()+labs(x="Population (Log Scale)", y="Local Shelter-in-Place \nLikelihood (Partial Residual)")+theme(text=element_text(size=20))+scale_x_continuous(breaks=(log(c(5000, 50000, 500000))-mean(df$pop, na.rm=TRUE))/sd(df$pop, na.rm=TRUE), labels=c("5000", "50000", "500000"))
      
    }
    
    
    mod.list[[i]]<-data.frame("name"=coeffs, "coeff"=ifelse(coeffs %in% names(mod$coefficients), mod$coefficients[coeffs], NA), "y"=c(length(coeffs):1)-(i+1)/8, "mod name"=paste(resp, cutoff),  "Model"=model_names[i], "lwr"=ifelse(coeffs %in% names(mod$coefficients), confint(mod)[,1][coeffs], NA), "upr"=ifelse(coeffs %in% names(mod$coefficients), confint(mod)[,2][coeffs], NA), "vif"=ifelse(coeffs %in% names(mod$coefficients), vif(mod)[coeffs], NA), "pval"=ifelse(coeffs %in% names(mod$coefficients), summary(mod)$coefficients[,4][coeffs], NA))
    
    coeff_nopolicy<-c("resp", names(mod$coefficients)[! names(mod$coefficients) %in% c("(Intercept)")]) 
    
    #mod_nopolicy<-glm.nb(resp ~ ., data=df3[coeff_nopolicy])
    #ptl_rsq<-rsq.partial(mod, mod_nopolicy, type="lr")$partial.rsq
    
    
    all_rsq<-c(all_rsq, nagelkerke(mod)$Pseudo.R.squared.for.model.vs.null[3])
    
    #exp<-predict(mod, df2)
    
    #obs<-log(df[[resp]]) %>%
    #  ifelse(. <= -1, -1, .)
    
    
    #p[[i]]<-data.frame("obs"=obs, "exp"=exp, "early_policy"=factor(df$policy_delay<0, levels=c("FALSE", "TRUE")), "did_sd"=df$sd_date<18345) %>%
    #        ggplot(., aes(x=exp, y=obs, color=early_policy, alpha=.2))+geom_segment(x=-10, xend=10, y=-10, yend=10, size=2, color="#c83616")+geom_point(size=3)+scale_color_manual(values=c("#fc8d59", "#1a9850"))+
    #            theme(text=element_text(size=20), legend.position="none")+xlab("Expected (log)")+ylab("Observed (log)")+ggtitle( paste(str_to_title(resp), " (R² = ", round(all_rsq, 3), "; Partial R² = ", round(ptl_rsq, 3), ")", sep=""))
    
    i<-i+1
  }
  
}

cf_mat<-data.frame(do.call(rbind, mod.list)) %>%
  mutate_at(vars(Model), ~factor(.x, levels=model_names)) 


print(paste(model_names, max(cf_mat$vif, na.rm=TRUE)))
#add labels to scale_y_continuous scale_y_continuous(name="", breaks=c(0:9)+.5)+
p<-ggplot(cf_mat, aes(x=coeff, y=y, color=Model))+geom_point(size=5)+scale_y_continuous(name="", breaks=c(0:(length(coeffs)-1))+.5, labels=c("Partisanship", "Population", "Commuters", "First Case"))+
  scale_color_ma
p

ggsave("C:/Users/mallj/OneDrive/Mordecai/COVID/Figs/coefplot_order.png", plot=p,
       device="png", width=12, height=12,
       units="in")


fig3_pres<- arrangeGrob(pop_sd, partisan_sd, pop_sip, firstcase_sip, ncol=2)

fig3_pres <- as_ggplot(fig3_pres) +                               
  draw_plot_label(label = c("A. Population", "C. Population", "B. Partisanship", "D. First Case"), size = 30,
                  x = c(.07, .07, .59, .59), y = c(1.01, .51, 1.01, .51), hjust=0) 

ggsave(file="C:/Users/mallj/OneDrive/Mordecai/COVID/Figs/fig3_pres.png", fig3_pres, device="png", height=12, width=14.8, units="in") 
beep()

####MAPS#####
setwd("C:/Users/mallj/OneDrive/Mordecai/COVID/Data/Maps")
install.packages("https://cran.r-project.org/src/contrib/Archive/tibble/tibble_2.1.3.tar.gz",repos=NULL, type="source")
library(sf)
library(grid)
library(cowplot)

load("C:/Users/mallj/Onedrive/Mordecai/COVID/Data/ga_tidy_df")


hosp<-read.csv("ga_hosp.csv", header=FALSE)
open_hosp<-read.csv("ga_open_hosp.csv")
ga_map<-st_read("Counties_Georgia.shp")
ga_map <- cbind(ga_map, st_coordinates(st_centroid(ga_map)))

open_hosp<-open_hosp[-75,]


peach_theme<-c("#a2d2a3", "#fff3c6", "#ffda44", "#ff9154", "#ff6a71", "#8c2031")
plot_map<-function(covar, title=TRUE, hosp=FALSE, zscore=FALSE, with_text=FALSE){
  
  if(title==TRUE){
    title<-covar
  }
  
  ga_map[[covar]]<-sapply(str_to_lower(ga_map$NAME10), function(x) df[df$county==x, covar])
  if(covar != "range_mob"){
    ga_map[[covar]]<-ifelse(ga_map[[covar]]<0, NA, ga_map[[covar]])
  }
  ga_map[[covar]]<-ifelse(is.nan(ga_map[[covar]]), NA, ga_map[[covar]])
  
  if(zscore){
    ga_map[[covar]]<-zscore(ga_map[[covar]])
  }
  
  
  
  if(covar=="deaths_percap_bin"){
    map<-ggplot(data = ga_map) +
      geom_sf(aes(fill = factor(ga_map[[covar]], labels=c("<1 per \nthousand", "> 1 per \nthousand", "> 2 per \nthousand")))) +
      coord_sf(datum = NA)+
      theme_void()+
      scale_fill_manual(values=c("white", "#ffda44", "#ff6a71", breaks=c("<1 per \nthousand", "> 1 per 1,000", "> 2 per thousand")), na.value="white", name="") +
      geom_text(data = ga_map, aes(X, Y, label = NAME10), size = 3, color=ifelse(with_text, "black", NA)) +
      geom_point(data=open_hosp, aes(x=V10, y=V11), col=ifelse(hosp, "purple", NA))+theme_void()+ggtitle(paste(title, ifelse(zscore, "(z-score)", "")))+theme(plot.title = element_text(hjust = 0))
  }
  
  else if(covar=="first_case"){
    map<-ggplot(data = ga_map) +  
      geom_sf(aes(fill = -1*ga_map[[covar]])) +
      coord_sf(datum = NA)+
      theme_void()+
      scale_fill_gradientn(colors=peach_theme, na.value="white", name="", breaks=c(-18340, -18360, -18380), labels=c("Mar 19","Apr 8","Apr 28")) +
      geom_text(data = ga_map, aes(X, Y, label = NAME10), size = 3, color=ifelse(with_text, "black", NA)) +
      geom_point(data=open_hosp, aes(x=V10, y=V11), col=ifelse(hosp, "purple", NA))+theme_void()+ggtitle(paste(title, ifelse(zscore, "(z-score)", "")))+
      theme(plot.title = element_text(hjust = 0))
    
  }
  else if(covar=="pop"){
    map<-ggplot(data = ga_map) +
      geom_sf(aes(fill = ga_map[[covar]])) +
      coord_sf(datum = NA)+
      theme_void()+
      scale_fill_gradientn(colors=peach_theme, na.value="white", name="", breaks=log(c(5000, 50000, 500000)), labels=c("5,000", "50,000", "500,000")) +
      geom_text(data = ga_map, aes(X, Y, label = NAME10), size = 3, color=ifelse(with_text, "black", NA)) +
      geom_point(data=open_hosp, aes(x=V10, y=V11), col=ifelse(hosp, "purple", NA))+theme_void()+ggtitle(paste(title, ifelse(zscore, "(z-score)", "")))+
      theme(plot.title = element_text(hjust = 0))
  }
  
  else if(covar=="deaths_4wks"){
    map<-ggplot(data = ga_map) +
      geom_sf(aes(fill = log(ga_map[[covar]]))) +
      coord_sf(datum = NA)+
      theme_void()+
      scale_fill_gradientn(colors=peach_theme, na.value="white", name="", breaks=log(c(1, 10, 100)), labels=c(1, 10, 100)) +
      geom_text(data = ga_map, aes(X, Y, label = NAME10), size = 3, color=ifelse(with_text, "black", NA)) +
      geom_point(data=open_hosp, aes(x=V10, y=V11), col=ifelse(hosp, "purple", NA))+theme_void()+ggtitle(paste(title, ifelse(zscore, "(z-score)", "")))+
      theme(plot.title = element_text(hjust = 0))
  }
  else if(length(unique(ga_map[[covar]]))>2){
    map<-ggplot(data = ga_map) +
      geom_sf(aes(fill = ga_map[[covar]])) +
      coord_sf(datum = NA)+
      theme_void()+
      scale_fill_gradientn(colors=peach_theme, na.value="white", name="") +
      geom_text(data = ga_map, aes(X, Y, label = NAME10), size = 3, color=ifelse(with_text, "black", NA)) +
      geom_point(data=open_hosp, aes(x=V10, y=V11), col=ifelse(hosp, "purple", NA))+theme_void()+ggtitle(paste(title, ifelse(zscore, "(z-score)", "")))+
      theme(plot.title = element_text(hjust = 0))
  }
  
  else{
    map<-ggplot(data = ga_map) +
      geom_sf(aes(fill = factor(ga_map[[covar]], labels=c("No", "Yes")))) +
      coord_sf(datum = NA)+
      theme_void()+
      scale_fill_manual(values=c("#fff3c6", "#ff6a71"), na.value="white", name="") +
      geom_text(data = ga_map, aes(X, Y, label = NAME10), size = 3, color=ifelse(with_text, "black", NA)) +
      geom_point(data=open_hosp, aes(x=V10, y=V11), col=ifelse(hosp, "purple", NA))+theme_void()+ggtitle(paste(title, ifelse(zscore, "(z-score)", "")))+
      theme(plot.title = element_text(hjust = 0))
  }
  return(map)
}

df$did_sd<-ifelse(df$sd_date != as_date("2020-03-24"), TRUE, FALSE)
df$deaths_percap<-df$deaths/exp(df$pop)
df$deaths_percap_bin<-cut(df$deaths/exp(df$pop), c(-1, .001, .002, 1), right=FALSE, labels=FALSE)

firstcase_map<-plot_map("first_case", "")
sd_map<-plot_map("did_sd", "")
race_map<-plot_map("black", "")
mob_map<-plot_map("end_mob", "")
pop_map<-plot_map("pop", "")
deaths_map<-plot_map("deaths_4wks", "")
dpc_map<-plot_map("deaths_percap_bin", "")
hist<-ggplot(df, aes(x=deaths_percap, fill=factor(deaths_percap_bin, labels=c("<1 per \nthousand", "> 1 per \nthousand", "> 2 per \nthousand")))) +
  geom_rect(data=NULL,aes(xmin=.001001,xmax=.002,ymin=-Inf,ymax=78),fill="#ffda44")+
  geom_rect(data=NULL,aes(xmin=.002,xmax=.003,ymin=-Inf,ymax=78), fill="#ff6a71")+
  geom_histogram(colour="black", boundary=0, binwidth=.0001) +scale_fill_manual(values=c("white", "#ffda44", "#ff6a71")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0), legend.title = element_blank(), panel.background=element_rect(fill="white")) +
  xlab("Deaths Per Capita") + ylab("Count") +  labs(fill = "Deaths Per \nCapita \nCutoffs")+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks=c(0,20,40,60)) +  scale_x_continuous(expand = c(0,0))

g<- arrangeGrob(firstcase_map, sd_map, race_map, mob_map, pop_map, deaths_map, dpc_map, hist, nrow=4, ncol=2)

g <- as_ggplot(g) +                                
  draw_plot_label(label = c("A. First Case", "C. Race", "E. Population", "G. Deaths Per Capita Cutoffs", 
                            "B. Local Social Distancing?", "D. Mobility", "F. Deaths", "H. Deaths Per Capita Distribution"), size = 15,
                  x = c(0, 0, 0, 0, .5, .5, .5, .5), y = c(1, .75, .5, .26, 1, .75, .5, .26), hjust=0) 

ggsave(file="C:/Users/mallj/OneDrive/Mordecai/COVID/Figs/ga_covid_maps.png", g, height=8, width=7.4, units="in") 
###PLOT: TS

ts<-data.frame(
  "County" = factor(str_to_title(rep(rownames(df)[order(df$first_case, df$first_case, decreasing=TRUE)], each=2)), levels=str_to_title(rownames(df)[order(df$first_case, df$first_case, decreasing=TRUE)])),
  "Event" = rep(c("First Case", "Social Distancing"), nrow(df)),
  "Date" = as.vector(sapply(order(df$first_case, df$first_case, decreasing=FALSE), function(i) unlist(df[i, c("first_case", "sd_date")])))
)

ts$Date<-ifelse(is.na(ts$Date), 0, ts$Date)

ggplot(ts, aes(y=Date, x=County, color=Event))+geom_point(size=3)+#geom_segment(x=filter(ts, Event=="First Case")$Date, xend=filter(ts, Event=="Social Distancing"), y=unique(ts$County), yend=unique(ts$County))+
  theme_minimal()+theme(axis.text.x=element_blank(), axis.ticks.y=element_blank(), text=element_text(size=15))+
  scale_color_manual(values=c("#fc8d59", "#1a9850"))+scale_y_continuous(limits=c(18320, 18390), breaks=c(18322, 18353, 18383), labels=c("March", "April", "May"))


####INTERVENTION PLOT###
ath_row<-mat["clarke",]

setwd("C:/Users/mallj/OneDrive/Mordecai/COVID/Data/Johns Hopkins Tracker/csse_covid_19_data/csse_covid_19_time_series")
cases<-read.csv("time_series_covid19_confirmed_US.csv")
deaths<-read.csv("time_series_covid19_deaths_US.csv")

get_epi<-function(county, state){
  my_cases<-tail(as.numeric(cases[str_to_lower(cases$Admin2)==county & str_to_lower(cases$Province_State)==state, ]), -11)
  my_deaths<-tail(as.numeric(deaths[str_to_lower(deaths$Admin2)==county & str_to_lower(cases$Province_State)==state, ]), -12)
  my_dates<-as.Date("01-22-2020", format="%m-%d-%y")+1:length(my_cases)-1
  policy_delay<-my_dates[min(which(my_cases>0))]
  first_death<-my_dates[min(which(my_deaths>0))]
  pop<-deaths[str_to_lower(deaths$Admin2)==county & str_to_lower(cases$Province_State)==state, "Population"]
  my_epi<-list("deaths"=my_deaths, "cases"=my_cases, "dates"=my_dates, "first_death"=first_death, "policy_delay"=policy_delay, "pop"=pop, "death_424"=my_deaths[94], "case_424"=my_cases[94])
  return(my_epi)
}

mobility_df<-read.csv("DL-COVID-19/DL-us-m50_index.csv")

get_mobility<-function(county, date_intervention=NA){
  my_mobility<-head(tail(unlist(mobility_df[str_to_lower(mobility_df$admin2)==paste(county, "county") & mobility_df$admin1=="Georgia",]), -5), 34)
  time<-as.Date("03-01-2020", format="%m-%d-%Y")+0:33
  date_intervention<-as.Date(date_intervention, origin="1970-01-01")
  return(list("avg_mobility"=mean(my_mobility, na.rm=TRUE), "min_mobility"=ifelse(is.infinite(min(my_mobility, na.rm=TRUE)), NA, min(my_mobility, na.rm=TRUE)), "sip_efficacy"=ifelse(is.na(date_intervention), NA, mean(my_mobility[which(time==date_intervention)+0:6]))))
}


ath_epi<-get_epi("clarke", "georgia")



