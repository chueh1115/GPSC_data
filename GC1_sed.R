rm(list = ls())
library(readxl)
library(dplyr)
GPSC_sediment<- read_excel("Canyon_sediment.xlsx")
GC1_sed<-GPSC_sediment %>% 
  filter(Station=="GC1") %>% 
  filter(Section=="0-1") %>% 
  filter(TOC&DW!="NA")
colnames(GC1_sed)


area<-pi*(10.5/2)^2/10000 #m2
y<- GC1_sed %>% 
  group_by(Cruise, Station, Deployment, Section,DW,TOC)%>%
  summarise(OC=mean(DW*1000*TOC/100/area,na.rm = T)) #mg/m2
#DW=dry weight(g)
#TOC=(%)
SED<-mean(y$OC)

library(ggplot2)
y %>% 
  ggplot(aes(x = Cruise, y = OC))+
    geom_point(color = "red")+
  ylab("OC(mgC/m2)")+
  geom_hline(yintercept = mean(y$OC))

