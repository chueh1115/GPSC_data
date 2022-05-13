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
GC1_SEDsum$Station<-"GC1"
GS1_SEDsum$Station<-"GS1"
SED<-rbind(GC1_SEDsum,GS1_SEDsum)

library(ggplot2)
ggplot(data = SED, aes(x=Season,y=SED,color=Station))+
  geom_boxplot()+
  geom_point()

library(vegan)
library(dplyr)
SED<-SED %>% filter(Season!="SU")

set.seed(100)

#check variance:PERMDISP####
#1 calculate distance
sed_dist<-vegdist(SED$SED,method = "euclidean")
#2 group->calculate data dispersion:Station
beta_station<-betadisper(sed_dist,group = SED$Station)
#3 permutation grouping and calculate variance difference
permu_station<-permutest(beta_station,permutations = 9999)
permu_station
#2 group->calculate data dispersion:Season
beta_season<-betadisper(sed_dist,group = SED$Season)
#3 permutation grouping and calculate variance difference
permu_season<-permutest(beta_season,permutations = 9999)
permu_season

set.seed(100)
#PERMANOVA####
permanova_sed<-adonis2(SED$SED~Season*Station,data = SED, permutations = 9999,
                       method = "euclidean",sqrt.dist = F)
permanova_sed

