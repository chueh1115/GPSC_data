rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
library(openxlsx)
#abundance####
GPSC_macro_abundance <- read_excel("data/GPSC_macro_sorting/GPSC_macro_sorting_2021.11.23.xlsx", 
                                   sheet = "Specimen")
levels(factor(GPSC_macro_abundance$Station))
GC1_macro_abundance<-GPSC_macro_abundance%>% filter(Station=="GC1")
GS1_macro_abundance<-GPSC_macro_abundance%>% filter(Station=="GS1")

#GC1####
abu<-rbind(GC1_macro_abundance,GS1_macro_abundance)
abu %>% 
  ggplot(aes(x=Cruise,y=Abundance))+
  geom_bar(stat = "identity",position = "stack",
            aes(fill=Taxon))+
  ylab(expression(Abundance~(n)))+
  ylim(0, NA)+
  facet_wrap(~Station,scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(title = "Macrofauna")
#calculation
GC1mean<-GC1_macro_abundance %>% 
  group_by(Cruise,Station) %>% 
  summarize(sum=sum(Abundance))
GC1mean$mean<-mean(GC1mean$sum)
GS1mean<-GS1_macro_abundance %>% 
  group_by(Cruise,Station) %>% 
  summarize(sum=sum(Abundance))
GS1mean$mean<-mean(GS1mean$sum)
206.875/18.75
