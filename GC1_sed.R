rm(list = ls())
library(readxl)
GPSC_sediment<- read_excel("Canyon_sediment.xlsx")
GC1_sed<-GPSC_sediment %>% 
  filter(Station=="GC1") %>% 
  filter(Section=="0-1")
colnames(GC1_sed)


TOC<-mean(y$TOC,na.rm = T)
DW<-mean(y$DW,na.rm = T)
#0.008659015
area<-pi*(10.5/2)^2/10000#m2
y<- GC1_sed %>% 
  group_by(Cruise, Station, Deployment, Section,DW) %>% 
  mutate(DW*TOC/area) %>% 
  summarise(OC=mean(y$`DW * TOC/area`,na.rm = T))

library(ggplot2)
y %>% 
  ggplot(aes(x = Cruise, y = TOC))+
    geom_point(color = "red")+
  ylab("TOC")
