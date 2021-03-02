####MAPS#####
)
install.packages("https://cran.r-project.org/src/contrib/Archive/tibble/tibble_2.1.3.tar.gz",repos=NULL, type="source")
library(sf)
library(grid)
library(magrittr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggpubr)


load("Data/ga_tidy_df")

#Additional covariates for plotting
df$did_sd<-ifelse(df$sd_date != as_date("2020-03-24"), TRUE, FALSE)
df$deaths_percap<-df$deaths/exp(df$pop)
df$deaths_percap_bin<-cut(df$deaths/exp(df$pop), c(-1, .001, .002, 1), right=FALSE, labels=FALSE)


#Join shapefile and dataframe
ga_map<-st_read("Counties_Georgia.shp")
ga_map$NAME10 %<>% str_to_lower()
ga_map %<>% merge(df, by.x="NAME10", by.y="county")

peach_theme<-c("#a2d2a3", "#fff3c6", "#ffda44", "#ff9154", "#ff6a71", "#8c2031")

plot_map<-function(covar){

  #Instructions for plotting Georgia maps, with certain modifications made for particular covariates for interpretability
  
  if(covar=="deaths_percap_bin"){ #Deaths per capita: odified three-color scheme
    map<-ggplot(data = ga_map) +
      theme_void() +
      geom_sf(aes(fill = factor(ga_map[[covar]], labels=c("<1 per \nthousand", "> 1 per \nthousand", "> 2 per \nthousand")))) +
      scale_fill_manual(values=c("white", "#ffda44", "#ff6a71", breaks=c("<1 per \nthousand", "> 1 per 1,000", "> 2 per thousand")), na.value="white", name="") 
  }
  
  else if(covar=="first_case"){ #First case: reverse legend to be more intuitive and label key with date names
    map<-ggplot(data = ga_map) +  
      theme_void() +
      geom_sf(aes(fill = -1*as.numeric(ga_map[[covar]]))) +
      scale_fill_gradientn(colors=peach_theme, na.value="white", breaks=c(-18340, -18360, -18380), labels=c("Mar 19","Apr 8","Apr 28"), name="")
    
  }
  else if(covar=="pop"){ #Population size: add commas to legend for readability, breaks at intuitive positions
    map<-ggplot(data = ga_map) +
      theme_void() +
      geom_sf(aes(fill = ga_map[[covar]])) +
      scale_fill_gradientn(colors=peach_theme, na.value="white", breaks=log(c(5000, 50000, 500000)), labels=c("5,000", "50,000", "500,000"), name="")
  }
  
  else if(covar=="deaths_6wks"){ #Deaths after 6 weeks: log-transform data and add breaks at intuitive positions
    map<-ggplot(data = ga_map) +
      geom_sf(aes(fill = log(ga_map[[covar]]))) +
      scale_fill_gradientn(colors=peach_theme, na.value="white", breaks=log(c(1, 10, 100)), labels=c(1, 10, 100), name="") +
      theme_void()
  }
  
  else if(length(unique(ga_map[[covar]]))==2){ #Presence of Local Social Distancing order (and other binary variables): yes/no legend label and custom color scheme
    map<-ggplot(data = ga_map) +
      theme_void() +
      geom_sf(aes(fill = factor(ga_map[[covar]], labels=c("No", "Yes")))) +
      scale_fill_manual(values=c("#fff3c6", "#ff6a71"), na.value="white", name="") 
  }
  
  else { #All other non-binary variables (in this case, race and mobility)
    map<-ggplot(data = ga_map) +
      theme_void() +
      geom_sf(aes(fill = ga_map[[covar]])) +
      scale_fill_gradientn(colors=peach_theme, na.value="white", name="") 
  }
  
  
  return(map)
}


#Generate all maps (1A-G)
firstcase_map<-plot_map("first_case")+theme(plot.margin = unit(c(1, 0, 0, 0), "lines"))
sd_map<-plot_map("did_sd")+theme(plot.margin = unit(c(1, 0, 0, 0), "lines"))
race_map<-plot_map("black")+theme(plot.margin = unit(c(1, 0, 0, 0), "lines"))
mob_map<-plot_map("end_mob")+theme(plot.margin = unit(c(1, 0, 0, 0), "lines"))
pop_map<-plot_map("pop")+theme(plot.margin = unit(c(1, 0, 0, 0), "lines"))
deaths_map<-plot_map("deaths_6wks")+theme(plot.margin = unit(c(1, 0, 0, 0), "lines"))
dpc_map<-plot_map("deaths_percap_bin")+theme(plot.margin = unit(c(1, 0, 0, 0), "lines"))


#Generate histogram of deaths per capita (1H)
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
                  x = c(0, 0, 0, 0, .5, .5, .5, .5), y = c(1, .75, .5, .25, 1, .75, .5, .25), hjust=0) 


ggsave(file="Figs/fig1_maps.pdf", g, device=cairo_pdf, dpi=300, height=8, width=7.4, units="in")

