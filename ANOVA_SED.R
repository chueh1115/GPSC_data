rm(list = ls())
library(readxl)
library(dplyr)
volume<-pi*(10.5/2)^2*1#cm3
area<-pi*(10.5/2)^2/10000 #m2
density<-2.65#sediment bulk density g/cm3 (Eleftheriou, 2013)

#GC1####
GPSC_sediment<- read_excel("Canyon_sediment.xlsx")
GC1_sed<-GPSC_sediment %>% 
  filter(Station=="GC1") %>% 
  filter(TOC!="NA")
colnames(GC1_sed)

GC1<- GC1_sed %>% 
  group_by(Cruise, Station, Deployment, Section,TOC)%>%
  summarise(OC=volume*TOC/100,#g
            SED=OC*1000/area) %>% #mg/m2
  group_by(Cruise, Station,Deployment,Section) %>% 
  summarise(SED=SED)



#find multilayer
GC1 %>% 
  ggplot(aes(x = Cruise, y = SED))+
  geom_point()
ylab("OC(mgC/m2)")
GC1_multilayer<- c("OR1_1102","OR1_1114","OR1_1126")

# multilayer 0-5 cm
GC1_ml_0_5<- c("0-1", "1-2", "2-3", "3-4", "4-5")

GC1_ml_sum05<-subset(GC1, Cruise %in% GC1_multilayer & Section %in% GC1_ml_0_5, Cruise & SED) %>% 
  group_by(Cruise)  %>% 
  summarize(SED=sum(SED=SED))
GC1_ml_sum910<-GC1 %>% 
  filter(Section=="9-10") %>%
  group_by(Cruise) %>% 
  summarise(SED=5*SED)
GC1_ml_sum<-rbind(GC1_ml_sum05,GC1_ml_sum910) %>% group_by(Cruise) %>% 
  summarise(SED=sum(SED))

`%!in%`<- Negate(`%in%`)

GC1_sing_sum<-subset(GC1, Cruise %!in% GC1_multilayer, Cruise & SED)%>%
  group_by(Cruise) %>% 
  summarize(SED=10*SED)
GC1_SEDsum<-rbind(GC1_sing_sum,GC1_ml_sum) %>% arrange(Cruise)
GC1_SEDsum<-data.frame(GC1_SEDsum,Season=c("AU","AU","SP","SP","SU","AU","SP","AU","SP","SP","AU"))

#GS1####
GPSC_sediment <- read_excel("data/GPSC_sediment/GPSC_sediment_2021.08.16_ysl.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "text", "text", "numeric"))
GS1_sed<-GPSC_sediment %>% filter(Station=="GS1"&Cruise!="OR1_1132")%>%  filter(TOC!="NA")
colnames(GS1_sed)

GS1<- GS1_sed %>% 
  group_by(Cruise, Station, Deployment, Section,TOC)%>%
  summarise(OC=volume*TOC/100,#g
            SED=OC*1000/area) %>% #mg/m2
  group_by(Cruise, Station,Deployment,Section) %>% 
  summarise(SED=SED)
library(ggplot2)
#find multilayer
GS1 %>% 
  ggplot(aes(x = Cruise, y = SED))+
  geom_point()
ylab("OC(mgC/m2)")

GS1_multilayer<- c("OR1_1102","OR1_1114","OR1_1126")

# multilayer 0-5 cm
GS1_ml_0_5<- c("0-1", "1-2", "2-3", "3-4", "4-5")

GS1_ml_sum05<-subset(GS1, Cruise %in% GS1_multilayer & Section %in% GS1_ml_0_5, Cruise & SED) %>% 
  group_by(Cruise)  %>% 
  summarize(SED=sum(SED=SED))
GS1_ml_sum910<-GS1 %>% 
  filter(Section=="9-10") %>%
  group_by(Cruise) %>% 
  summarise(SED=5*SED)
GS1_ml_sum<-rbind(GS1_ml_sum05,GS1_ml_sum910) %>% group_by(Cruise) %>% 
  summarise(SED=sum(SED))

GS1_sing_sum<-subset(GS1, Cruise %!in% GS1_multilayer, Cruise & SED)%>%
  group_by(Cruise) %>% 
  summarize(SED=10*SED)
GS1_SEDsum<-rbind(GS1_sing_sum,GS1_ml_sum) %>% arrange(Cruise)
GS1_SEDsum
GS1_SEDsum<-data.frame(GS1_SEDsum,Season=c("AU","SP","SP","SU","AU","AU","SP","SP","AU"))

#remove OR1_1242 outlier
GS1_mean<-mean(GS1_SEDsum$SED[1:8])
GS1_sd<-sd(GS1_SEDsum$SED[1:8])

#plot GC1 and GS1 together
GC1_SEDsum$Station<-"GC1"
GS1_SEDsum$Station<-"GS1"
SED<-rbind(GC1_SEDsum,GS1_SEDsum)

#check: if there's seasonal difference?
#SED_Shapiro-Wilk normality test (N<50)####
shapiro.test(SED$SED)
#W = 0.9242, p-value = 0.1194(>0.05)
#cannot reject H0 and conclude that SED$SED is normally distributed
#F test####
SED_AUSP<-SED %>% filter(Season%in%c("AU","SP"))
var.test(SED~Season, SED_AUSP)
#p-value = 0.4138 (>0.05)
SED_SUSP<-SED %>% filter(Season%in%c("SU","SP"))
var.test(SED~Season, SED_SUSP)
#p-value = 0.01127 (significant)
SED_SUAU<-SED %>% filter(Season%in%c("SU","AU"))
var.test(SED~Season, SED_SUAU)
#p-value = 0.01521 (significant)
#ANOVA####
anovaSED<-aov(SED~Season, SED)
summary(anovaSED)
#Pr(>F)=0.25(>0.05)
#accept H0 and conclude that there is no statistically difference between the mean of seasons

#check: if there's difference between habitats?
#t.test
t.test(SED~Station, SED)
#data:  biomass by Station
#data:  SED by Station
#t = -4.4109, df = 15.603, p-value = 0.0004626(significant)