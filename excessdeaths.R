library(tidyverse)
library(cansim)
library(devtools)
library(ggtech)
library(ggthemes)
library(viridis)
library(scales)
library(ggrepel)
library(lubridate)
library(geomtextpath)
library(stringr)


fulldeaths <- get_cansim("13-10-0784-01") 
fulldeaths$GEO<- str_replace_all(fulldeaths$GEO, ", place of occurrence", "")
fulldeaths$REF_DATE<-ymd(fulldeaths$REF_DATE)
adjdeaths<- fulldeaths %>% filter(grepl('2016A0002', DGUID), `Characteristics` == "Adjusted number of deaths") %>% select(REF_DATE, GEO, DGUID,val_norm) %>% rename(adjdeaths = val_norm)
expdeaths<- fulldeaths %>% filter(grepl('2016A0002', DGUID), `Characteristics` == "Expected number of deaths") %>% select(REF_DATE, GEO, DGUID,val_norm) %>% rename(expdeaths = val_norm)

excessdeaths<-left_join(x=adjdeaths,y=expdeaths, by=c("DGUID", "REF_DATE", "GEO")) %>% mutate(exdeaths = 100*((adjdeaths-expdeaths)/expdeaths))


plotprovs <- c("Ontario", "Quebec", "Alberta")
plotprovs2<- c("British Columbia")

ggplot() + 
  geom_textsmooth(subset(excessdeaths, GEO %in% plotprovs),mapping=aes(REF_DATE,exdeaths, group=GEO, color=GEO, label=GEO), method="loess", span = 0.2, se=FALSE, size = 4, linewidth=1.3, alpha = 0.8 , text_smoothing=25,hjust = .38) +
  geom_textsmooth(subset(excessdeaths, GEO %in% plotprovs2),mapping=aes(REF_DATE,exdeaths, group=GEO, color=GEO, label=GEO), method="loess", span = 0.2, se=FALSE, size = 4, linewidth=1.3, alpha = 0.8 , text_smoothing=25,hjust = .735) +
    theme_minimal() + 
  theme(legend.position="none") +
  scale_x_date(breaks = scales::pretty_breaks(n = 12)) +
  ylab("Excess Deaths Per 100 Expected Deaths") + 
  xlab("Time") + 
  ggtitle("Excess Deaths as Percentage of Expected Deaths") +
  geom_hline(yintercept=0, linetype='dotted', col = 'black')


  


  
  
    
    
    geom_text_repel(data=filter(totdon,REF_DATE==2019),aes(label = pr),nudge_x = 2, direction = "y", hjust = "left") 

