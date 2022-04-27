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

GC1_SEDsum %>% 
  ggplot(aes(x = Cruise, y = SED))+
    geom_point(aes(color =Season ),size=3)+
  ylab("OC(mgC/m2)")+
  geom_hline(yintercept = mean(GC1_SEDsum$SED))
GC1_mean<-mean(GC1_SEDsum$SED)
GC1_sd<-sd(GC1_SEDsum$SED)

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

GS1_SEDsum %>% 
  ggplot(aes(x = Cruise, y = SED))+
  geom_point(aes(color =Season ),size=3)+
  ylab("OC(mgC/m2)")+
  geom_hline(yintercept = mean(GS1_SEDsum$SED))

#remove OR1_1242 outlier
GS1_mean<-mean(GS1_SEDsum$SED[1:8])
GS1_sd<-sd(GS1_SEDsum$SED[1:8])

#plot GC1 and GS1 together
GC1_SEDsum$Station<-"GC1"
GS1_SEDsum$Station<-"GS1"
SED<-rbind(GC1_SEDsum,GS1_SEDsum)
Mean_SED<-data.frame(
  Mean=c(GC1_mean,GS1_mean),
  Station=c("GC1","GS1")
)
SED%>% 
  ggplot(aes(x = Cruise, y = SED))+
  geom_point(aes(color =Season ),size=3)+
  ylab(expression(OC~(mg~C~m^-2)))+
  facet_wrap(~Station)+  
  ylim(1e+05, NA)+
  geom_hline(data = Mean_SED, aes(yintercept = Mean))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
Mean_SED$Mean[2]/Mean_SED$Mean[1]

library(pals)
color<-as.vector(c(stepped(3)[-c(1:2)],stepped(11)[-c(1:10)],stepped(15)[-c(1:14)]))
SED%>% 
  ggplot(aes(x = Cruise, y = SED))+
  geom_bar(aes(fill =Season),stat = "identity",position = position_dodge())+
  ylab(expression(OC~(mg~C~m^-2)))+
  scale_fill_manual(values =color )+
  facet_wrap(~Station)+ 
  ylim(0, NA)+
  geom_hline(data = Mean_SED, aes(yintercept = Mean),
             linetype=5)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(size = 3) ) )+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("OC_sed.png",width = 12, height =9)  


rm(list = ls())
#burial####
#(Huh et al., 2009)
#GC1: 1.0g/cm2/y
#GS1: 0.43g/cm2/y
GC1<-1*10000/365 #g/m2/d
GS1<-0.43*10000/365 #g/m2/d\
GPSC_sediment<- read_excel("Canyon_sediment.xlsx")
GC1_sed<-GPSC_sediment %>% 
  filter(Station=="GC1") %>% 
  filter(TOC!="NA")
GPSC_sediment <- read_excel("data/GPSC_sediment/GPSC_sediment_2021.08.16_ysl.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "text", "text", "numeric"))
GS1_sed<-GPSC_sediment %>% filter(Station=="GS1"&Cruise!="OR1_1132")%>%  filter(TOC!="NA")

meanGC1<-mean(GC1_sed$TOC/100)
sdGC1<-sd(GC1_sed$TOC/100)
meanGS1<-mean(GS1_sed$TOC/100)
sdGS1<-sd(GS1_sed$TOC/100)

burial<-data.frame(max=c((meanGC1+sdGC1)*GC1*1000,(meanGS1+sdGS1)*GS1*1000),
                   min=c((meanGC1-sdGC1)*GC1*1000,(meanGS1-sdGS1)*GS1*1000),
                   station=c("GC1","GS1"))
meanGC1*GC1*1000
sdGC1*GC1*1000
meanGS1*GS1*1000
sdGS1*GS1*1000
