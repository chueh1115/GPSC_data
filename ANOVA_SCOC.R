rm(list = ls())
library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(doBy)
library(tidyr)
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

#check: if there's seasonal difference?
#SED_Shapiro-Wilk normality test (N<50)####
shapiro.test(TOU$SCOC)
#W = 0.9242, p-value = 0.1244(>0.05)
#cannot reject H0 and conclude that TOU$SCOC is normally distributed
#F test####
var.test(SCOC~Season, TOU_GC1)
#p-value = 0.2572(>0.05)
var.test(SCOC~Season, TOU_GS1)
#p-value = 0.7121 (>0.05)
#check: if there's difference between habitats?
#t.test
t.test(SCOC~Station, TOU)
#data:  SCOC by Station
#t = 1.6779, df = 24.612, p-value = 0.106(n.s)


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
#DOU(mmol/m2/d->mgC/m2/d)
DOU$DOU<-DOU$In_situ_Integrated_Prod*(-864)*33.191*0.309
#check: if there's seasonal difference?
#SED_Shapiro-Wilk normality test (N<50)####
shapiro.test(DOU$DOU)
#W = 0.9242, p-value = 2.05e-11(significant)
#DOU_Nonparametric test####
kruskal.test(DOU~Season, data = DOU)
#p-value = 0.2331 (>0.05)
#cannot reject H0 and conclude that there is no statistically difference between the mean of seasons
#Nonparametric test: independent 2-group Mann-Whitney U Test#####
wilcox.test(DOU~Station, data = DOU)
#p-value = 0.0175(<0.05)
#reject H0 and conclude that is statistically difference between the mean of 2 stations


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
