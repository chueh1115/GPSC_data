rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
#if there's difference exist between different seasons in GC1?####
#H0: mean of all season are the same (mean sp = mean au = mean su)
#HA: at least mean of one season is different from the others
#assumptions for anova
#1.randomly sampled and samples are independent
#2.samples come from a normally distributed population with unknown but equal variances

#abundance####
GPSC_meio_abundance <- read_excel("data/GPSC_meio_sorting/GPSC_meio_sorting_2016.08.18.xlsx", 
                                  sheet = "Specimen")
levels(factor(GPSC_meio_abundance$Station))

#biomass####
#number of NEM in GC1 cruises
GC1_meio_abundance<-GPSC_meio_abundance%>% filter(Station=="GC1")
GC1_Nem_abu<-GC1_meio_abundance %>% filter(Taxon=="Nematoda") %>% 
  group_by(Cruise,Station,Deployment,Tube,Subcore) %>% summarise(abu=sum(Abundance)) 
#number of NEM in GS1 cruises
GS1_meio_abundance<-GPSC_meio_abundance%>% filter(Station=="GS1")
GS1_Nem_abu<-GS1_meio_abundance %>% filter(Taxon=="Nematoda") %>% 
  group_by(Cruise,Station,Deployment,Tube,Subcore) %>% summarise(abu=sum(Abundance)) 


#size
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

#Statistics####
GS1$OC<-GS1$total_biomass/area
GS1$Season[GS1$Cruise=="OR1_1114"]<-"SU"
GS1$Season[GS1$Cruise=="OR1_1126"]<-"AU"
GS1$Season[GS1$Cruise=="OR1_1132"]<-"SP"
GC1$OC<-GC1$total_biomass/area
GC1$Season[GC1$Cruise=="OR1_1114"]<-"SU"
GC1$Season[GC1$Cruise=="OR1_1126"]<-"AU"
GC1$Season[GC1$Cruise=="OR1_1128"]<-"SP"

#GC1_Shapiro-Wilk normality test (N<50)####
shapiro.test(GC1$OC)
#p-value = 0.001654(<0.05)
#reject H0 and conclude that GC1$OC is not normally distributed)
#GC1_Nonparametric test####
kruskal.test(OC~Season, data = GC1)
#p-value = 0.06081 (>0.05)
#cannot reject H0 and conclude that there is no statistically difference between the mean of seasons

#GS1_Shapiro-Wilk normality test (N<50)####
shapiro.test(GS1$OC)
#p-value = 0.2007(>0.05)
#cannot reject H0 and conclude that GS1$OC is normally distributed
#F test####
GS1_ausp<-GS1 %>% filter(Season%in%c("AU","SP"))
var.test(OC~Season, GS1_ausp)
#p-value = 0.3183 (>0.05)
GS1_ausu<-GS1 %>% filter(Season%in%c("AU","SU"))
var.test(OC~Season, GS1_ausu)
#p-value = 0.2387 (>0.05)
GS1_spsu<-GS1 %>% filter(Season%in%c("SP","SU"))
var.test(OC~Season, GS1_spsu)
#p-value = 0.05003 (>0.05)
#cannot reject H0 and conclude that there is no statistically difference between the variance of seasons
#ANOVA####
anovaGS1<-aov(OC~Season, GS1)
summary(anovaGS1)
#Pr(>F)=0.000488(<0.05)
#reject H0 and conclude that there is statistically difference between the mean of seasons

#check which group is statistically different
#Tukey post hoc test for pairwise comparison 
TukeyHSD(anovaGS1)
#          diff       lwr        upr     p adj
#SP-AU -27.72138 -47.11245  -8.330313 0.0109958
#SU-AU -52.94432 -72.33539 -33.553251 0.0003863 ->***
#SU-SP -25.22294 -44.61401  -5.831869 0.0169050


#2way anova####
 #Habitat(Canyon vs Slope)=Station(GC1,GS1)
 #Season(SP, SU, AU)
GC1GS1<-rbind(GC1,GS1)
twoway <- aov(OC ~ Station*Season, data = GC1GS1)
summary(twoway)
#               Df Sum Sq Mean Sq F value   Pr(>F)    
#Station         1   4579    4579  146.74 4.35e-08 ***
#Season          2   1870     935   29.96 2.16e-05 ***
#Station:Season  2   2352    1176   37.68 6.72e-06 ***

#if there's difference exist between 2 stations?####
#H0: mean of 2 stations are the same 
#HA: mean of 2 stations are the different
#Shapiro-Wilk normality test (N<50)####
shapiro.test(GC1GS1$OC) 
#p-value = 0.000264 (<0.05)
#reject H0 and conclude that GC1GS1$OC is not normally distributed
#Nonparametric test: independent 2-group Mann-Whitney U Test#####
wilcox.test(OC~Station,GC1GS1)
#p-value = 8.227e-05(<0.05)
#reject H0 and conclude that is statistically difference between the mean of 2 stations