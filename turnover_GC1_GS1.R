rm(list = ls())
library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(doBy)
library(tidyr)
#turnover rate
#calculation: OU/OCstock; compare direct measurment and model estimation OU
#OC_stock####
OC<-data.frame(GC1=c(350269.97,65.31,1.49,3.65),
               GS1=c(524425.66,42.80,33.39,80.20))
total_GC1<-sum(OC$GC1)
total_GS1<-sum(OC$GS1)
B_GC1<-sum(OC$GC1[3:4])
B_GS1<-sum(OC$GS1[3:4])

#TOU_MEAS####
GPSC_TOU <- read_csv("C:/Users/user/Downloads/labWei/Tung_thesis/GPSC_data/data/GPSC_incubation/GPSC_TOU.csv")
GPSC_TOU<-as.data.frame(GPSC_TOU)
#filter GC1 and not include NOR1_T011(no det data)
GPSC_TOU_GC1<-GPSC_TOU %>% 
  filter(Station=="GC1"& Cruise!="NOR1_T011") 
#filter GS1 and not include NOR1_T011(no det data), OR1_1114 and 1126 because scarce data and outlier
GPSC_TOU_GS1<-GPSC_TOU %>% 
  filter(Station=="GS1"& Cruise!="NOR1_T011"& Cruise!="OR1_1114"& Cruise!="OR1_1126")


#calculation: from In_situ_DO_flux (mmol O2/m2/d) to carbon unit(mg C/m2/d)
#mmolO2 -> mgO2:  33.191  mgO2/mmolO2 
#mgO2 -> mgC   :  0.309   mgC/mgO2  
GPSC_TOU_GC1<-GPSC_TOU_GC1 %>% 
  mutate(SCOC=In_situ_DO_flux*(-1)*33.191*0.309)
GPSC_TOU_GS1<-GPSC_TOU_GS1 %>% 
  mutate(SCOC=In_situ_DO_flux*(-1)*33.191*0.309)
TOU<-rbind(GPSC_TOU_GC1,GPSC_TOU_GS1) 
#acquire mean SCOC value of each cruise
TOU<-TOU %>% 
  group_by(Cruise,Habitat,Station) %>%
  summarise(SCOC=mean(SCOC))
#acquire mean/sd SCOC value of 2 stations
TOU$SCOC[TOU$Station=="GC1"]
TOU_mean_GC1<-mean(TOU$SCOC[TOU$Station=="GC1"])
TOU_mean_GS1<-mean(TOU$SCOC[TOU$Station=="GS1"])

#DOU_MEAS####
GPSC_DOU <- read_csv("data/GPSC_o2_profile/GPSC_DOU.csv")
GPSC_DOU<-as.data.frame(GPSC_DOU)
#filter GC1/GS1 and not include NOR1_T011(no det data)
DOU_GC1<-GPSC_DOU %>% filter(Station=="GC1"&Cruise!="NOR1_T011")
DOU_GS1<-GPSC_DOU %>% filter(Station=="GS1"&Cruise!="NOR1_T011")
#DOU(mmol/m2/d->mgC/m2/d)
DOU_GC1<-DOU_GC1 %>% 
  mutate(DOU=In_situ_Integrated_Prod*(-864)*33.191*0.309)
DOU_GS1<-DOU_GS1 %>% 
  mutate(DOU=In_situ_Integrated_Prod*(-864)*33.191*0.309)
#acquire mean DOU value of each cruise
DOU<-rbind(DOU_GC1,DOU_GS1) %>% 
  group_by(Cruise,Habitat,Station) %>%
  summarise(mean=mean(DOU))
#acquire mean/sd DOU value of 2 stations
DOU_mean_GC1<-mean(DOU$mean[DOU$Station=="GC1"])
DOU_mean_GS1<-mean(DOU$mean[DOU$Station=="GS1"])
#BOU_MEAS####
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
#summarise mean flux value of each cruise, 
#then use pivot_longer to rearrange dataframe 
com<-com %>% 
  group_by(Cruise,Station,Habitat) %>% 
  summarise(DOU=mean(DOU),
            BOU=mean(BOU),
            TOU=mean(TOU))%>% 
  pivot_longer(cols = c("DOU","BOU","TOU"),
               names_to = "OU",
               values_to = "O2_flux")
#mmolO2/m2/d->mgC/m2/d
com$O2_flux<-com$O2_flux*(-1)*33.191*0.309
#mean/sd value of each station
BOU_mean_GC1<-mean(com$O2_flux[com$Station=="GC1"& com$OU =="BOU"],na.rm = T)
BOU_mean_GS1<-mean(com$O2_flux[com$Station=="GS1"& com$OU =="BOU"],na.rm = T)

#GC1_MODEL####
load("GC1.Rdata")
GC1_LIM<-LIM
GC1_xs<-xs
GC1_LIM$Unknowns
#DOU="BAC->DIC_W"[9]
#BOU="MEI->DIC_W"[12]+"MAC->DIC_W"[15]
#TOU="BAC->DIC_W"+"MEI->DIC_W"+"MAC->DIC_W"
GC1_LA<-data.frame(flow=GC1_LIM$Unknowns, 
               mean=colMeans(GC1_xs$X),
               sd=sqrt(diag(var(GC1_xs$X))))
TOU_MOD_GC1<-GC1_LA$mean[9]+GC1_LA$mean[12]+GC1_LA$mean[15]
DOU_MOD_GC1<-GC1_LA$mean[9]
BOU_MOD_GC1<-GC1_LA$mean[12]+GC1_LA$mean[15]

#GS1_MODEL####
load("GS1.Rdata")
GS1_LIM<-LIM
GS1_xs<-xs
GS1_LIM$Unknowns
#DOU="BAC->DIC_W"[9]
#BOU="MEI->DIC_W"[12]+"MAC->DIC_W"[15]
#TOU="BAC->DIC_W"+"MEI->DIC_W"+"MAC->DIC_W"
GS1_LA<-data.frame(flow=GS1_LIM$Unknowns, 
                   mean=colMeans(GS1_xs$X),
                   sd=sqrt(diag(var(GS1_xs$X))))
TOU_MOD_GS1<-GS1_LA$mean[9]+GS1_LA$mean[12]+GS1_LA$mean[15]
DOU_MOD_GS1<-GS1_LA$mean[9]
BOU_MOD_GS1<-GS1_LA$mean[12]+GS1_LA$mean[15]
#1. TOU_MEAS/(total)OC####
TR_total_GC1<-TOU_mean_GC1/total_GC1
TR_total_GS1<-TOU_mean_GC1/total_GS1

#2. TOU_MODEL/(total)OC####
TR_MOD_total_GC1<-TOU_MOD_GC1/total_GC1
TR_MOD_total_GS1<-TOU_MOD_GS1/total_GS1
#3. DOU_MEAS/BAC_OC####
TR_BAC_GC1<-DOU_mean_GC1/OC$GC1[2]
TR_BAC_GS1<-DOU_mean_GS1/OC$GS1[2]
#4. DOU_MODEL/BAC_OC####
TR_MOD_BAC_GC1<-DOU_MOD_GC1/OC$GC1[2]
TR_MOD_BAC_GS1<-DOU_MOD_GS1/OC$GS1[2]
#5. BOU_MEAS/(MEI+MAC)_OC####
TR_B_GC1<-BOU_mean_GC1/B_GC1
TR_B_GS1<-BOU_mean_GS1/B_GS1
#6. BOU_MODEL/(MEI+MAC)_OC####
TR_MOD_B_GC1<-BOU_MOD_GC1/B_GC1
TR_MOD_B_GS1<-BOU_MOD_GS1/B_GS1
#compare####
TR_TOU<-data.frame()
TR_MEAS<-round(data.frame(GC1=c(TR_total_GC1,TR_BAC_GC1,TR_B_GC1),
                    GS1=c(TR_total_GS1,TR_BAC_GS1,TR_B_GS1)
                    ),4)
rownames(TR_MEAS)<-c("TOU","DOU","BOU")
TR_MOD<-round(data.frame(GC1=c(TR_MOD_total_GC1,TR_MOD_BAC_GC1,TR_MOD_B_GC1),
                   GS1=c(TR_MOD_total_GS1,TR_MOD_BAC_GS1,TR_MOD_B_GS1)),6)
rownames(TR_MOD)<-c("TOU","DOU","BOU")
TR<-rbind(TR_MEAS,TR_MOD)
