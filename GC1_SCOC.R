rm(list = ls())

library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
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
  filter(Station=="GS1"& Cruise!="NOR1_T011")
TOU_GS1_AU<-GPSC_TOU_GS1%>%
  filter(Cruise%in% c("OR1_1126","OR1_1242")) %>% 
  mutate(Season="AU")
TOU_GS1_SP<-GPSC_TOU_GS1%>%
  filter(Cruise%in% c("OR1_1190","OR1_1219")) %>% 
  mutate(Season="SP")
TOU_GS1_SU<-GPSC_TOU_GS1%>%
  filter(Cruise%in% c("OR1_1114")) %>% 
  mutate(Season="SU")
TOU<-rbind(TOU_GC1_AU,TOU_GC1_SP,TOU_GS1_AU,TOU_GS1_SP,TOU_GS1_SU)
TOU %>% 
  ggplot(aes(x = Station,y = In_situ_DO_flux*(-1)*12))+
  geom_boxplot()+
  # geom_point(aes(color=Season))+
  geom_text(aes(label=Cruise,color=Season))+
  ylab(expression(Carbon~flux~(mg~C~m^-2~day^-1)))+
  theme_bw()+
  facet_grid(~Cruise)






GPSC_TOU <- read_csv("C:/Users/user/Downloads/labWei/Tung_thesis/GPSC_data/data/GPSC_incubation/GPSC_TOU_st.csv")
GPSC_TOU<-as.data.frame(GPSC_TOU)
GPSC_TOU_GC1<-GPSC_TOU %>% 
  filter(Station=="GC1"& Cruise!="NOR1_T011") %>%
  mutate(Season=c("AU","SP","AU","SP","SP","AU"))

GPSC_TOU_GS1<-GPSC_TOU %>% 
  filter(Station=="GS1"& Cruise!="NOR1_T011") %>%
  mutate(Season=c("SU","AU","SP","SP","AU"))
  
TOU<-rbind(GPSC_TOU_GC1,GPSC_TOU_GS1)
TOU %>% 
  ggplot(aes(x = Station,y = In_situ_DO_flux.mean*(-1)*12))+
  geom_point(aes(color=Season))+
  ylab(expression(Carbon~flux~(mg~C~m^-2~day^-1)))+
  theme_bw()


ggplot(GPSC_TOU, aes(x = Station, y = In_situ_DO_flux.mean*(-1)*12, fill=Station))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x=Station, y=In_situ_DO_flux.mean*(-1)*12, ymax=In_situ_DO_flux.mean*(-1)*12+In_situ_DO_flux.sd*(1)*12, ymin=In_situ_DO_flux.mean*(-1)*12, colour=Station))+
  facet_wrap(~Cruise, scale="free_x")+
  ylab(expression(Carbon~flux~(mg~C~m^-2~day^-1)))+
  theme_bw()
#+#labs(title="Sediment Community Carbon Remineralization Rate")+
 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) %+replace% dark
