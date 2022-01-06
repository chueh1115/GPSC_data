rm(list = ls())
library(readxl)
library(dplyr)
GPSC_sediment<- read_excel("Canyon_sediment.xlsx")
GC1_sed<-GPSC_sediment %>% 
  filter(Station=="GC1") %>% 
  filter(TOC!="NA")
colnames(GC1_sed)
volume<-pi*(10.5/2)^2*1#cm3
area<-pi*(10.5/2)^2/10000 #m2
density<-2.65#sediment bulk density g/cm3 (Eleftheriou, 2013)

y<- GC1_sed %>% 
  group_by(Cruise, Station, Deployment, Section,TOC)%>%
  summarise(OC=volume*TOC/100,#g
            SED=OC*1000/area) %>% #mg/m2
  group_by(Cruise, Station,Deployment,Section) %>% 
  summarise(SED=SED)
library(ggplot2)
y %>% 
  ggplot(aes(x = Cruise, y = SED))+
  geom_point()
  ylab("OC(mgC/m2)")

multilayer<- c("OR1_1102","OR1_1114","OR1_1126")
  
# multilayer 0-5 cm
ml_0_5<- c("0-1", "1-2", "2-3", "3-4", "4-5")
  
ml_sum05<-subset(y, Cruise %in% multilayer & Section %in% ml_0_5, Cruise & SED) %>% 
  group_by(Cruise)  %>% 
  summarize(SED=sum(SED=SED))
ml_sum910<-y %>% 
  filter(Section=="9-10") %>%
  group_by(Cruise) %>% 
  summarise(SED=5*SED)
ml_sum<-rbind(ml_sum05,ml_sum910) %>% group_by(Cruise) %>% 
  summarise(SED=sum(SED))

`%!in%`<- Negate(`%in%`)

sing_sum<-subset(y, Cruise %!in% multilayer, Cruise & SED)%>%
  group_by(Cruise) %>% 
  summarize(SED=10*SED)
SEDsum<-rbind(sing_sum,ml_sum) %>% arrange(Cruise)
SEDsum<-data.frame(SEDsum,Season=c("AU","AU","SP","SP","SU","AU","SP","AU","SP","SP","AU"))


SEDsum %>% 
  ggplot(aes(x = Cruise, y = SED))+
    geom_point(aes(color =Season ),size=3)+
  ylab("OC(mgC/m2)")+
  geom_hline(yintercept = mean(SEDsum$SED))
mean(SEDsum$SED)
sd(SEDsum$SED)
