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

#boxplot of DO flux (from all data points from all cruises) with season label
TOU %>% 
  ggplot(aes(x = Station,y = In_situ_DO_flux*(-1)))+
  geom_boxplot()+
  # geom_point(aes(color=Season))+
  geom_text(aes(label=Cruise,color=Season))+
  ylab(expression(DO~flux~(mmol~O2~m^-2~day^-1)))+
  theme_bw()

#calculation: from In_situ_DO_flux (mmol O2/m2/d) to carbon unit(mg C/m2/d)
#mmolO2 -> mgO2:  33.191  mgO2/mmolO2 
#mgO2 -> mgC   :  0.309   mgC/mgO2  
TOU_GC1<-TOU_GC1 %>% 
  mutate(SCOC=In_situ_DO_flux*(-1)*33.191*0.309)
TOU_GS1<-TOU_GS1 %>% 
  mutate(SCOC=In_situ_DO_flux*(-1)*33.191*0.309)
TOU<-rbind(TOU_GC1,TOU_GS1) 
#acquire mean SCOC value of each cruise
TOU<-TOU %>% 
  group_by(Cruise,Habitat,Station) %>%
  summarise(SCOC=mean(SCOC))
#acquire mean/sd SCOC value of 2 stations
TOU$SCOC[TOU$Station=="GC1"]
meanGC1<-mean(TOU$SCOC[TOU$Station=="GC1"])
sdGC1<-sd(TOU$SCOC[TOU$Station=="GC1"])
meanGS1<-mean(TOU$SCOC[TOU$Station=="GS1"])
sdGS1<-sd(TOU$SCOC[TOU$Station=="GS1"])
#barplot of mean SCOC value of each cruise and with mean value of station 
TOU %>%   
  ggplot(aes(x=Cruise,y=SCOC,fill=Station))+
  geom_bar(stat="identity",position = "dodge")+
  ylab(expression(Total~Oxygen~Utilization~(mgC~m^-2~day^-1)))+
  ylim(0, NA)+
  geom_hline(aes(yintercept = meanGC1,linetype="GC1"),color="red")+
  geom_hline(aes(yintercept = meanGS1,linetype="GS1"),color="darkblue")+
  scale_linetype_manual(name = "Mean", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("red","darkblue"))))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

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


#boxplot of DO flux (from all data points from all cruises) with season label
#nmol/cm2/s -> mmol/m2/d
#1e4*3600*24/1e6 = 864
DOU %>% 
  ggplot(aes(x = Station,y = In_situ_Integrated_Prod*(-864),fill=Station))+
  geom_boxplot()+
  geom_text(aes(label=Cruise,color=Season))+
  ylab(expression(DO~flux~(mmol~O2~m^-2~day^-1)))+
  theme_bw()
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
DOU$mean[DOU$Station=="GC1"]
meanGC1<-mean(DOU$mean[DOU$Station=="GC1"])
sdGC1
sdGC1<-sd(DOU$mean[DOU$Station=="GC1"])
meanGS1<-mean(DOU$mean[DOU$Station=="GS1"])
sdGS1<-sd(DOU$mean[DOU$Station=="GS1"])
#barplot of mean DOU value of each cruise and with mean value of station 
DOU %>%   
  ggplot(aes(x=Cruise,y=mean,fill=Station))+
  geom_bar(stat="identity",position = "dodge")+
  ylab(expression(Diffusive~Oxygen~Uptake~(mgC~m^-2~day^-1)))+
  ylim(0, NA)+
  geom_hline(aes(yintercept = meanGC1,linetype="GC1"),color="red")+
  geom_hline(aes(yintercept = meanGS1,linetype="GS1"),color="darkblue")+
  scale_linetype_manual(name = "Mean", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("red","darkblue"))))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
meanGS1-sdGS1
#BMU=TOU-DOU
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
`%ni%` <- Negate(`%in%`)
#bar plot of BOU of each cruise and mean value of 2 stations (mmolO2/m2/d)
  #meanBOU_GC1<-mean(com$O2_flux[com$Station=="GC1"& com$OU =="BOU"],na.rm = T)
  #meanBOU_GS1<-mean(com$O2_flux[com$Station=="GS1"& com$OU =="BOU"],na.rm = T)
  #com %>% filter(OU=="BOU"&O2_flux<0) %>%
  #  ggplot(aes(x=Cruise,y=-O2_flux,fill=Station))+
  #  geom_bar(stat="identity",position = "dodge")+
  #  ylab(expression(Benthos~Mediated~oxygen~Utilization~(mmolO2~m^-2~day^-1)))+
  #  ylim(0, NA)+
  #  geom_hline(aes(yintercept = -meanBOU_GC1,linetype="GC1"),color="red")+
  #  geom_hline(aes(yintercept = -meanBOU_GS1,linetype="GS1"),color="darkblue")+
  #  scale_linetype_manual(name = "Mean", values = c(2, 2), 
  #                        guide = guide_legend(override.aes = list(color = c("red","darkblue"))))+
  #  theme_bw()+
  #  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#mmolO2/m2/d->mgC/m2/d
com$O2_flux<-com$O2_flux*(-1)*33.191*0.309
#mean/sd value of each station
meanBOU_GC1<-mean(com$O2_flux[com$Station=="GC1"& com$OU =="BOU"],na.rm = T)
sdBOU_GC1<-sd(com$O2_flux[com$Station=="GC1"& com$OU =="BOU"],na.rm = T)
meanBOU_GS1<-mean(com$O2_flux[com$Station=="GS1"& com$OU =="BOU"],na.rm = T)
sdBOU_GS1<-sd(com$O2_flux[com$Station=="GS1"& com$OU =="BOU"],na.rm = T)
#bar plot of BOU of each cruise and mean value of 2 stations (mgC/m2/d)
com %>% filter(OU=="BOU"&O2_flux>0) %>%
  ggplot(aes(x=Cruise,y=O2_flux,fill=Station))+
  geom_bar(stat="identity",position = "dodge")+
  ylab(expression(Benthos~Mediated~oxygen~Utilization~(mg~C~m^-2~day^-1)))+
  ylim(0, NA)+
  geom_hline(aes(yintercept = meanBOU_GC1,linetype="GC1"),color="red")+
  geom_hline(aes(yintercept = meanBOU_GS1,linetype="GS1"),color="darkblue")+
  scale_linetype_manual(name = "Mean", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("red","darkblue"))))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
#summary

com %>% filter(OU%ni%c("TOU","In_situ_TOU")) %>% 
  ggplot(aes(x=Cruise,y=O2_flux,fill=OU))+
  geom_bar(stat = "identity",position = "stack")+
  ylab(expression(Total~Oxygen~Utilization~(mgC~O2~m^-2~day^-1)))+
  ylim(0, NA)+
  facet_wrap(~Station)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
