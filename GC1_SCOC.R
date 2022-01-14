rm(list = ls())

library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
#TOU
GPSC_TOU <- read_csv("C:/Users/user/Downloads/labWei/Tung_thesis/GPSC_data/data/GPSC_incubation/GPSC_TOU.csv")
GPSC_TOU<-as.data.frame(GPSC_TOU)
GPSC_TOU_GC1<-GPSC_TOU %>% 
  filter(Station=="GC1"& Cruise!="NOR1_T011") 
TOU_GC1_AU<-GPSC_TOU_GC1%>%
  filter(Cruise%in% c("NOR1_T004","OR1_1151","OR1_1242")) %>% 
  mutate(Season="AU")
TOU_GC1_SP<-GPSC_TOU_GC1%>%
  filter(Cruise%in% c("OR1_1128","OR1_1190","OR1_1219")) %>% 
  mutate(Season="SP")

GPSC_TOU_GS1<-GPSC_TOU %>% 
  filter(Station=="GS1"& Cruise!="NOR1_T011"& Cruise!="OR1_1114"& Cruise!="OR1_1126")
TOU_GS1_AU<-GPSC_TOU_GS1%>%
  filter(Cruise%in% c("OR1_1126","OR1_1242")) %>% 
  mutate(Season="AU")
TOU_GS1_SP<-GPSC_TOU_GS1%>%
  filter(Cruise%in% c("OR1_1190","OR1_1219")) %>% 
  mutate(Season="SP")
TOU_GC1<-rbind(TOU_GC1_AU,TOU_GC1_SP)
TOU_GS1<-rbind(TOU_GS1_AU,TOU_GS1_SP)

TOU<-rbind(TOU_GC1_AU,TOU_GC1_SP,TOU_GS1_AU,TOU_GS1_SP)
TOU %>% 
  ggplot(aes(x = Station,y = In_situ_DO_flux*(-1)*12))+
  geom_boxplot()+
  # geom_point(aes(color=Season))+
  geom_text(aes(label=Cruise,color=Season))+
  ylab(expression(Carbon~flux~(mg~C~m^-2~day^-1)))+
  theme_bw()
TOU_GC1<-TOU_GC1 %>% 
  mutate(SCOC=In_situ_DO_flux*(-1)*12)
TOU_GS1<-TOU_GS1 %>% 
  mutate(SCOC=In_situ_DO_flux*(-1)*12)
colnames(TOU)
TOU<-rbind(TOU_GC1,TOU_GS1) %>% 
  group_by(Station) %>%
  summarise(mean=mean(SCOC),
            sd=sd(SCOC))
TOU %>% 
  ggplot(aes(x = Station,y = mean,fill=Station))+
  geom_bar(stat="identity",color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  ylab(expression(Carbon~flux~(mg~C~m^-2~day^-1)))+
  theme_bw()
#DOU
