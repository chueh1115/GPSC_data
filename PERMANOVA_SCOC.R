rm(list = ls())
library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(doBy)
library(tidyr)
library(vegan)
#TOU####
GPSC_TOU <- read_csv("C:/Users/user/Downloads/labWei/Tung_thesis/GPSC_data/data/GPSC_incubation/GPSC_TOU.csv")
GPSC_TOU<-as.data.frame(GPSC_TOU)
#filter GC1 and not include NOR1_T011(no det data)
GPSC_TOU_GC1<-GPSC_TOU %>% 
  filter(Station=="GC1"& Cruise!="NOR1_T011") 
#add season to find if there is seasonality
TOU_GC1_AU<-GPSC_TOU_GC1%>%
  filter(Cruise%in% c("NOR1_T004","OR1_1151","OR1_1242")) %>% 
  mutate(Season="AU")
TOU_GC1_SP<-GPSC_TOU_GC1%>%
  filter(Cruise%in% c("OR1_1128","OR1_1190","OR1_1219")) %>% 
  mutate(Season="SP")
#filter GS1 and not include NOR1_T011(no det data), OR1_1114 and 1126 because scarce data and outlier
GPSC_TOU_GS1<-GPSC_TOU %>% 
  filter(Station=="GS1"& Cruise!="NOR1_T011"& Cruise!="OR1_1114"& Cruise!="OR1_1126")
#add season to find if there is seasonality
TOU_GS1_AU<-GPSC_TOU_GS1%>%
  filter(Cruise%in% c("OR1_1126","OR1_1242")) %>% 
  mutate(Season="AU")
TOU_GS1_SP<-GPSC_TOU_GS1%>%
  filter(Cruise%in% c("OR1_1190","OR1_1219")) %>% 
  mutate(Season="SP")

TOU_GC1<-rbind(TOU_GC1_AU,TOU_GC1_SP)
TOU_GS1<-rbind(TOU_GS1_AU,TOU_GS1_SP)
TOU<-rbind(TOU_GC1_AU,TOU_GC1_SP,TOU_GS1_AU,TOU_GS1_SP)
TOU_GC1<-TOU_GC1 %>% 
  mutate(SCOC=In_situ_DO_flux*(-1)*33.191*0.309)
TOU_GS1<-TOU_GS1 %>% 
  mutate(SCOC=In_situ_DO_flux*(-1)*33.191*0.309)
TOU<-rbind(TOU_GC1,TOU_GS1) 
TOU

library(ggplot2)
ggplot(data = TOU, aes(x=Season,y=SCOC,color=Habitat))+
  geom_boxplot()+
  geom_point()

library(vegan)
set.seed(100)

#check variance:PERMDISP####
#1 calculate distance
tou_dist<-vegdist(TOU$SCOC,method = "euclidean")
#2 group->calculate data dispersion:Station
beta_station<-betadisper(tou_dist,group = TOU$Habitat)
#3 permutation grouping and calculate variance difference
permu_station<-permutest(beta_station,permutations = 9999)
permu_station
#2 group->calculate data dispersion:Season
beta_season<-betadisper(tou_dist,group = TOU$Season)
#3 permutation grouping and calculate variance difference
permu_season<-permutest(beta_season,permutations = 9999)
permu_season

set.seed(100)
#PERMANOVA####
permanova_tou<-adonis2(TOU$SCOC~Season*Habitat,data = TOU, permutations = 9999,
                       method = "euclidean",sqrt.dist = F)
permanova_tou



rm(list = ls())
#DOU####
GPSC_DOU <- read_csv("data/GPSC_o2_profile/GPSC_DOU.csv")
GPSC_DOU<-as.data.frame(GPSC_DOU)
#filter GC1/GS1 and not include NOR1_T011(no det data)
DOU_GC1<-GPSC_DOU %>% filter(Station=="GC1"&Cruise!="NOR1_T011")
DOU_GS1<-GPSC_DOU %>% filter(Station=="GS1"&Cruise!="NOR1_T011")


#define seasons to find if there's seasonality
unique(DOU_GS1$Cruise)
DOU_GC1_AU<-DOU_GC1%>%
  filter(Cruise%in% c("NOR1_T004","OR1_1126","OR1_1151","OR1_1242")) %>% 
  mutate(Season="AU")
DOU_GC1_SP<-DOU_GC1%>%
  filter(Cruise%in% c("OR1_1128","OR1_1190","OR1_1219")) %>% 
  mutate(Season="SP")
DOU_GS1_AU<-DOU_GS1%>%
  filter(Cruise%in% c("OR1_1126","OR1_1151","OR1_1242")) %>% 
  mutate(Season="AU")
DOU_GS1_SP<-DOU_GS1%>%
  filter(Cruise%in% c("OR1_1190","OR1_1219")) %>% 
  mutate(Season="SP")
DOU<-rbind(DOU_GC1_AU,DOU_GC1_SP,DOU_GS1_AU,DOU_GS1_SP)
DOU$DOU<-DOU$In_situ_Integrated_Prod*(-864)*33.191*0.309


library(ggplot2)
ggplot(data = DOU, aes(x=Season,y=DOU,color=Habitat))+
  geom_boxplot()+
  geom_point()

library(vegan)
set.seed(100)

#check variance:PERMDISP####
#1 calculate distance
dou_dist<-vegdist(DOU$DOU,method = "euclidean")
#2 group->calculate data dispersion:Station
beta_station<-betadisper(dou_dist,group = DOU$Habitat)
#3 permutation grouping and calculate variance difference
permu_station<-permutest(beta_station,permutations = 9999)
permu_station
#2 group->calculate data dispersion:Season
beta_season<-betadisper(dou_dist,group = DOU$Season)
#3 permutation grouping and calculate variance difference
permu_season<-permutest(beta_season,permutations = 9999)
permu_season

set.seed(100)
#PERMANOVA####
permanova_dou<-adonis2(DOU$DOU~Season*Habitat,data = DOU, permutations = 9999,
                       method = "euclidean",sqrt.dist = F)
permanova_dou


#BMU=TOU-DOU####
rm(list = ls())
#filter GC1/GS1 and not include NOR1_T011(no det data)
GPSC_TOU <- as.data.frame(read_csv("data/GPSC_incubation/GPSC_TOU.csv"))
GPSC_DOU <- as.data.frame(read_csv("data/GPSC_o2_profile/GPSC_DOU.csv"))
TOU_GC1<-GPSC_TOU %>% filter(Station=="GC1"& Cruise!="NOR1_T011") 
TOU_GS1<-GPSC_TOU %>% filter(Station=="GS1"& Cruise!="NOR1_T011") 
DOU_GC1<-GPSC_DOU %>% filter(Station=="GC1"&Cruise!="NOR1_T011")
DOU_GS1<-GPSC_DOU %>% filter(Station=="GS1"&Cruise!="NOR1_T011")
#for TOU, use summaryBy() find mean and sd of DO_flux and In_situ_DO_flux of each tube
TOU_GC1<-summaryBy(DO_flux+In_situ_DO_flux~Cruise+Station+Deployment+Tube+Habitat+Region, data=TOU_GC1, FUN=c(mean, sd))
TOU_GS1<-summaryBy(DO_flux+In_situ_DO_flux~Cruise+Station+Deployment+Tube+Habitat+Region, data=TOU_GS1, FUN=c(mean, sd))
TOU<-rbind(TOU_GC1,TOU_GS1)
#for DOU, turn to mmolO2/m2/d first(*864)
#use summaryBy() find mean and sd of In_situ_Integrated_Prod of each tube
DOU_GC1$In_situ_Integrated_Prod <- DOU_GC1$In_situ_Integrated_Prod*864
DOU_GC1<-summaryBy(In_situ_Integrated_Prod~Cruise+Station+Deployment+Tube+Habitat+Region, data=DOU_GC1, FUN=c(mean, sd))
DOU_GS1$In_situ_Integrated_Prod <- DOU_GS1$In_situ_Integrated_Prod*864
DOU_GS1<-summaryBy(In_situ_Integrated_Prod~Cruise+Station+Deployment+Tube+Habitat+Region, data=DOU_GS1, FUN=c(mean, sd))
DOU<-rbind(DOU_GC1,DOU_GS1)

#with():modifying (a copy of) the original data->find Cruise, Station, Deployment, Tube from TOU and DOU dataset
id1 <- with(TOU, paste(Cruise, Station, Deployment, Tube))
id2 <- with(DOU, paste(Cruise, Station, Deployment, Tube))
#match():returns a vector of the positions of (first) matches of its first argument in its second.
#find TOU and DOU data with same c(Cruise, Station, Deployment, Tube)
TOU[match(id2, id1),c("DO_flux.mean", "In_situ_DO_flux.mean")]
com <- cbind(DOU, TOU[match(id2, id1), c("DO_flux.mean", "In_situ_DO_flux.mean")])
#rename
names(com)[7:10] <- c("DOU", "DOU.sd", "TOU", "In_situ_TOU")
#calculate BOU
com$BOU <- com$TOU-com$DOU
com<-com %>% 
  group_by(Cruise,Station,Habitat) %>% 
  summarise(DOU=mean(DOU),
            BOU=mean(BOU),
            TOU=mean(TOU))%>% 
  pivot_longer(cols = c("DOU","BOU","TOU"),
               names_to = "OU",
               values_to = "O2_flux")
unique(com$Cruise)
com_sp<-com %>% filter(Cruise%in%c("OR1_1190","OR1_1219","OR1_1128"))
com_sp$Season<-"SP"
com_au<-com %>% filter(Cruise%in%c("NOR1_T004","OR1_1242","OR1_1126","OR1_1151" ))
com_au$Season<-"AU"
com_season<-rbind(com_au,com_sp)
bou<-com_season %>% filter(OU=="BOU")
bou<-na.omit(bou)
  ggplot(data=bou,aes(x=Season,y=O2_flux,color=Habitat))+
  geom_boxplot()+
  geom_point()


set.seed(100)
#check variance:PERMDISP####
#1 calculate distance
bou_dist<-vegdist(bou$O2_flux,method = "euclidean")
#2 group->calculate data dispersion:Station
beta_station<-betadisper(bou_dist,group = bou$Habitat)
#3 permutation grouping and calculate variance difference
permu_station<-permutest(beta_station,permutations = 999)
permu_station
#2 group->calculate data dispersion:Season
beta_season<-betadisper(bou_dist,group = bou$Season)
#3 permutation grouping and calculate variance difference
permu_season<-permutest(beta_season,permutations = 999)
permu_season

set.seed(100)
#PERMANOVA####
permanova_bou<-adonis2(bou$O2_flux~Season*Habitat,data = bou, permutations = 999,
                       method = "euclidean",sqrt.dist = F)
permanova_bou
