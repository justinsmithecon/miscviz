library(tidyverse)
library(cansim)
library(devtools)
library(ggtech)
library(ggthemes)
library(viridis)
library(scales)
library(ggrepel)

chardata<- get_cansim("11-10-0130-0")
cpidata <- get_cansim("18-10-0005-01") %>% filter(grepl('2016A0002', DGUID), REF_DATE >=1997, REF_DATE <=2019, `Products and product groups` == "All-items") %>% select(REF_DATE,VALUE, DGUID, GEO) 
cpidata<- cpidata %>% filter(REF_DATE==2019) %>% select(DGUID, VALUE ) %>% rename(cpi19=VALUE) %>% left_join(x=cpidata,y=., by="DGUID") %>% mutate(cpi = 100*VALUE/cpi19) %>% select(REF_DATE, DGUID, cpi)
incdata<-get_cansim("11-10-0239-01") %>% filter(REF_DATE>=1997, Sex == "Both sexes", `Income source` == "Total income", grepl('2016A0002', DGUID), Statistics == "Aggregate income", `Age group` == "16 years and over") %>% select(REF_DATE, DGUID, val_norm) %>% rename(realinc = val_norm)
provabbrev <- chardata %>% filter(grepl('2016A0002', DGUID), `Donors and donations` == "Total charitable donations", REF_DATE == 1997) %>% select(DGUID) %>% add_column(pr = c("NL", "PEI", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YK", "NT", "NU"))


##Graphs for Provinces

totdon<-chardata %>% filter(grepl('2016A0002', DGUID), `Donors and donations` == "Total charitable donations") %>% left_join(x=.,y=provabbrev, by="DGUID") %>% left_join(x=.,y=cpidata, by=c("REF_DATE", "DGUID")) %>% mutate(realdon = val_norm*(100/cpi)) %>% left_join(x=., y=incdata, by = c("REF_DATE", "DGUID")) %>% mutate(dpinc = realdon/realinc)
totdon<- totdon %>% filter(REF_DATE==1997) %>% select(pr, dpinc ) %>% rename(dpinc97=dpinc) %>% left_join(x=totdon,y=., by="pr") %>% mutate(dpincindex = 100*dpinc/dpinc97) 
meddon<-chardata %>% filter(grepl('2016A0002', DGUID), `Donors and donations` == "Median donations") %>% left_join(x=.,y=provabbrev, by="DGUID") %>% left_join(x=.,y=cpidata, by="REF_DATE") %>% mutate(realdon = val_norm*(100/cpi)) 


baseplot<-ggplot(meddon, aes(REF_DATE,realdon, group=GEO, color=GEO))

baseplot + 
  geom_line(size = 1.3, alpha = 0.8 ) + 
  theme_minimal() + 
  theme(legend.position="none") +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 10), expand = expansion(add=c(0,4)))+
   geom_text_repel(data=filter(totdon,REF_DATE==2019),aes(label = pr),nudge_x = 2, direction = "y", hjust = "left") 



baseplot2<-ggplot(totdon, aes(REF_DATE,dpincindex, group=GEO, color=GEO))

baseplot2 + 
  geom_hline(yintercept=100, linetype="dashed", color = "black") +
  geom_line(size = 1.3, alpha = 0.8 ) + 
  theme_minimal() + 
  theme(legend.position="none") +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 10), expand = expansion(add=c(0,4)))+
  geom_text_repel(data=filter(totdon,REF_DATE==2019),aes(label = pr),nudge_x = 2, direction = "y", hjust = "left") +
  ylab("Dontaions/Income (1997=100)") + 
  xlab("Year") + 
  ggtitle("Donations Relative to Income Over Time Indexed to 1997") 
  


  