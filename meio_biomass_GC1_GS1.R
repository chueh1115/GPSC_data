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

#mean OC value of each cruise
GC1_3cru<-GC1 %>% group_by(Cruise) %>% 
  summarise(mean=mean(total_biomass)/area,
            sd=sd(total_biomass)/area)
#use as stock -> mean of all data 
GC1_mean<-mean(GC1_3cru$mean)
GC1_sd<-sd(GC1_3cru$mean)

GC1_3cru$Station<-"GC1"
GS1_3cru$Station<-"GS1"

MEI_bar<-rbind(GC1_3cru,GS1_3cru)
MEI_point<-rbind(GC1,GS1)
#plot GC1 and GS1 together(bar+point)####

MEI_bar%>%
  ggplot(aes(x = Cruise, y = mean))+
  geom_bar(stat = "identity",position = position_dodge(),
           aes(fill=Station))+
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), size=0.5,width=0.1,alpha=0.9, colour="black")+
  ylab(expression(OC~(mg~C~m^-2)))+
  ylim(0, NA)+
  facet_wrap(~Station,scales = "free_y")+
  theme_bw()+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(title = "Meiofauna")+
  guides(color = guide_legend(override.aes = list(size = 3) ) )+
  theme(legend.text = element_text(size = 15))+
  geom_point(data=MEI_point,aes(x=Cruise,y=total_biomass/area), color = "darkblue",size=2)
ggsave("OC_meio.png",width = 12, height =9)  
