rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
#abundance####
GPSC_meio_abundance <- read_excel("data/GPSC_meio_sorting/GPSC_meio_sorting_2016.08.18.xlsx", 
                                  sheet = "Specimen")
levels(factor(GPSC_meio_abundance$Station))
#number of NEM in GC1 cruises
GC1_meio_abundance<-GPSC_meio_abundance%>% filter(Station=="GC1")
GC1_Nem_abu<-GC1_meio_abundance %>% filter(Taxon=="Nematoda") %>% 
  group_by(Cruise,Station,Deployment,Tube,Subcore) %>% summarise(abu=sum(Abundance)) 
#number of NEM in GS1 cruises
GS1_meio_abundance<-GPSC_meio_abundance%>% filter(Station=="GS1")
GS1_Nem_abu<-GS1_meio_abundance %>% filter(Taxon=="Nematoda") %>% 
  group_by(Cruise,Station,Deployment,Tube,Subcore) %>% summarise(abu=sum(Abundance)) 


#size####
GPSC_meio_size<-NULL
for (i in 1:3){
  df<- read_excel("data/GPSC_meio_size/GPSC_meio_size_2020.08.02.xlsx",
                  sheet = i ,col_types = c("text", "text", "text", 
                                           "numeric", "numeric", "numeric", 
                                           "text", "text", "numeric", "text", 
                                           "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "text"))
  GPSC_meio_size<-rbind(GPSC_meio_size,df)
}
levels(factor(GPSC_meio_size$Station))
GC1_meio<-GPSC_meio_size%>%filter(Station=="GC1")
GS1_meio<-GPSC_meio_size%>%filter(Station=="GS1")
unique(GC1_meio$Taxon)
levels(factor(GC1_meio$Type))
#syringe area#####
area<-pi*(3/2)^2/10000# 0.0007068583m2


#GS1_calculate all volume####
GS1_meio_OC<-GS1_meio %>% mutate(Volume=L*(W^2)*C) %>% 
  mutate(DryW=Volume*1.13)%>% mutate(OC=DryW*0.012)
colnames(GS1_meio_OC)
#recalculate Nematoda: only measure the length and width of randomly chosen 
#300 samples per syringe
#->take average OC of 300 samples * total Nematodes abundance

GS1_Nem_OC<-GS1_meio_OC %>% filter(Taxon=="Nematoda") %>% 
  group_by(Cruise,Station,Deployment,Tube,Subcore) %>% 
  summarise(n=n(),sum_OC=sum(OC),mean=sum_OC/n)

GS1_Nem_abu$Subcore<-as.numeric(GS1_Nem_abu$Subcore)
GS1_Nem<-left_join(GS1_Nem_abu,GS1_Nem_OC,by=c("Cruise","Station","Deployment","Tube","Subcore"))
GS1_Nem$biomass_nem<-GS1_Nem$abu * GS1_Nem$mean

GS1_other<-GS1_meio_OC %>% filter(Taxon!="Nematoda") %>% 
  group_by(Cruise,Station,Deployment,Tube,Subcore) %>% 
  summarise(biomass_other=sum(OC))

GS1<-left_join(GS1_Nem,GS1_other,by=c("Cruise","Station","Deployment","Tube","Subcore"))
GS1$total_biomass<-GS1$biomass_nem+GS1$biomass_other
GS1<-GS1[,c(1:5,12)]
colnames(GS1)#"Cruise", "Station", "Deployment","Tube","Subcore","total_biomass"




#mean OC value of each cruise
GS1_3cru<-GS1 %>% group_by(Cruise) %>% 
  summarise(mean=mean(total_biomass)/area,
            sd=sd(total_biomass)/area)
#use as stock -> mean of all data 
GS1_mean<-mean(GS1_3cru$mean)
GS1_sd<-sd(GS1_3cru$mean)

#GC1_calculate all volume####
GC1_meio_OC<-GC1_meio %>% mutate(Volume=L*(W^2)*C) %>% 
  mutate(DryW=Volume*1.13)%>% mutate(OC=DryW*0.012)
colnames(GC1_meio_OC)
#recalculate Nematoda: only measure the length and width of randomly chosen 
#300 samples per syringe
#->take average OC of 300 samples * total Nematodes abundance

GC1_Nem_OC<-GC1_meio_OC %>% filter(Taxon=="Nematoda") %>% 
  group_by(Cruise,Station,Deployment,Tube,Subcore) %>% 
  summarise(n=n(),sum_OC=sum(OC),mean=sum_OC/n)

GC1_Nem_abu$Subcore[10]<-3#sample in water -> move to tube 3
GC1_Nem_abu<-GC1_Nem_abu %>% group_by(Cruise,Station,Deployment,Tube,Subcore) %>%
  summarise(abu=sum(abu))

GC1_Nem_abu$Subcore<-as.numeric(GC1_Nem_abu$Subcore)
GC1_Nem<-left_join(GC1_Nem_abu,GC1_Nem_OC,by=c("Cruise","Station","Deployment","Tube","Subcore"))
GC1_Nem$biomass_nem<-GC1_Nem$abu * GC1_Nem$mean

GC1_other<-GC1_meio_OC %>% filter(Taxon!="Nematoda") %>% 
  group_by(Cruise,Station,Deployment,Tube,Subcore) %>% 
  summarise(biomass_other=sum(OC))
GC1_miss<-data.frame(
  Cruise=c("OR1_1126","OR1_1126","OR1_1128","OR1_1128"),
  Station=c(    "GC1",     "GC1",     "GC1",     "GC1"),
  Deployment=c(    1,          1,        1,          1),
  Tube=c(         11,         11,        6,          6),
  Subcore=c(       1,          2,        2,          3),
  biomass_other=c( 0,          0,        0,          0)
)
GC1_other<-arrange(rbind(GC1_other,GC1_miss))
GC1<-left_join(GC1_Nem,GC1_other,by=c("Cruise","Station","Deployment","Tube","Subcore"))
GC1$total_biomass<-GC1$biomass_nem+GC1$biomass_other
GC1<-GC1[,c(1:5,12)]
colnames(GC1)#"Cruise", "Station", "Deployment","Tube","Subcore","total_biomass"
unique(GC1$Cruise)

#group:season
#season group
unique(GC1$Cruise)
unique(GS1$Cruise)

GC1_AU<-GC1 %>% filter(Cruise=="OR1_1126")
GC1_AU$Season<-"AU"
GC1_SP<-GC1%>% filter(Cruise=="OR1_1128")
GC1_SP$Season<-"SP"
GC1_SU<-GC1 %>% filter(Cruise=="OR1_1114")
GC1_SU$Season<-"SU"
GC1_season<-rbind(GC1_AU,GC1_SP,GC1_SU)

GS1_AU<-GS1 %>% filter(Cruise=="OR1_1126")
GS1_AU$Season<-"AU"
GS1_SP<-GS1%>% filter(Cruise=="OR1_1132")
GS1_SP$Season<-"SP"
GS1_SU<-GS1 %>% filter(Cruise=="OR1_1114")
GS1_SU$Season<-"SU"
GS1_season<-rbind(GS1_AU,GS1_SP,GS1_SU)

MEI<-rbind(GC1_season,GS1_season)
library(ggplot2)
ggplot(data = MEI, aes(x=Season,y=total_biomass,color=Station))+
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Station,scales = "free")
library(vegan)
library(dplyr)

set.seed(100)

#check variance:PERMDISP####
#1 calculate distance
mac_dist<-vegdist(MAC$OC,method = "euclidean")
#2 group->calculate data dispersion:Station
beta_station<-betadisper(mac_dist,group = MAC$Station)
#3 permutation grouping and calculate variance difference
permu_station<-permutest(beta_station,permutations = 9999)
permu_station
#2 group->calculate data dispersion:Season
beta_season<-betadisper(mac_dist,group = MAC$Season)
#3 permutation grouping and calculate variance difference
permu_season<-permutest(beta_season,permutations = 9999)
permu_season

set.seed(100)
#PERMANOVA####
permanova_mac<-adonis2(MAC$OC~Season*Station,data = MAC, permutations = 9999,
                       method = "euclidean",sqrt.dist = F)
permanova_mac
library(vegan)
library(dplyr)
set.seed(100)

#check variance:PERMDISP####
#1 calculate distance
mei_dist<-vegdist(MEI$total_biomass,method = "euclidean")
#2 group->calculate data dispersion:Station
beta_station<-betadisper(mei_dist,group = MEI$Station)
#3 permutation grouping and calculate variance difference
permu_station<-permutest(beta_station,permutations = 9999)
permu_station
#2 group->calculate data dispersion:Season
beta_season<-betadisper(mei_dist,group = MEI$Season)
#3 permutation grouping and calculate variance difference
permu_season<-permutest(beta_season,permutations = 9999)
permu_season

set.seed(100)
#PERMANOVA####
permanova_mei<-adonis2(MEI$total_biomass~Station,data = MEI, permutations = 9999,
                       method = "euclidean",sqrt.dist = F)
permanova_mei