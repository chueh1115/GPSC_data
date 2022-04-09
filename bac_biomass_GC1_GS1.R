rm(list=ls())
library(readxl)
library(dplyr)

GPSC_bac <- read_excel("data/GPSC_bact/GPSC_bacteria_count_OR1-1190.xlsx")
GPSC_bac %>% select(`Bacterial Cell Count`) 
A<-9^2*pi   #well area (mm^2)
a<-0.11^2*pi #view area (mm^2)
V<-0.1/2    #sample volume	(ml=cm3)
#GC1####
GC1_bac<-data.frame(time=seq(1:10),
                    count=as.numeric(unlist(GPSC_bac[2:11,2])))

GC1_bac<-GC1_bac %>% 
  mutate(num=count*A/a/V) %>% #count* A/a/V *10 cm = # bac/ cm3
  mutate(biomass=num*10*10*10^(-12)/10^(-4))
# Convert to biomass (assuming 10 fgC cell-1, Deming & Capenter 2008)
# # bac/cm3 * 10cm *10 fgC/bac = #fgC/cm2->*E-12/E-4= # mgC/m2

library(ggplot2)
ggplot(data=GC1_bac,aes(x=time, y=biomass))+
  geom_point()+
  ylab("GC1 Bacteria biomass (mgC/m2)")+
  geom_hline(yintercept = mean(GC1_bac$biomass))

GC1_mean<-mean(GC1_bac$biomass)
GC1_sd<-sd(GC1_bac$biomass)

#GS1####
GS1_bac<-data.frame(time=seq(1:10),
                    count=as.numeric(unlist(GPSC_bac[2:11,7])))

GS1_bac<-GS1_bac %>% 
  mutate(num=count*A/a/V) %>% #count* A/a/V *10 cm = # bac/ cm3
  mutate(biomass=num*10*10*10^(-12)/10^(-4))
# Convert to biomass (assuming 10 fgC cell-1, Deming & Capenter 2008)
# # bac/cm3 * 10cm *10 fgC/bac = #fgC/cm2->*E-12/E-4= # mgC/m2


library(ggplot2)
ggplot(data=GS1_bac,aes(x=time, y=biomass))+
  geom_point()+
  ylab("GS1 Bacteria biomass (mgC/m2)")+
  geom_hline(yintercept = mean(GS1_bac$biomass))
GS1_mean<-mean(GS1_bac$biomass)
GS1_sd<-sd(GS1_bac$biomass)
GC1_mean/GS1_mean
GC1_bac$Station<-"GC1"
GS1_bac$Station<-"GS1"
BAC<-rbind(GC1_bac,GS1_bac)
Mean_BAC<-data.frame(
  Mean=c(GC1_mean,GS1_mean),
  Station=c("GC1","GS1")
)
#plot####
BAC%>% 
  ggplot(aes(x = time, y = biomass))+
  geom_bar(stat = "identity",position = position_dodge(),
           aes(fill=Station))+
  ylab(expression(OC~(mg~C~m^-2)))+
  xlab("Count")+ 
  ylim(0, NA)+
  geom_hline(data = Mean_BAC, aes(yintercept = Mean),color=c("red","darkblue"),
             linetype=5)+
  scale_linetype_manual(name = "Mean", values = c(2, 2),
                        guide = guide_legend(override.aes = list(color = c("red3","darkcyan"))))+
  theme_bw()+labs(title = "Bacteria")+
  guides(color = guide_legend(override.aes = list(size = 3) ) )+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("OC_bac.png",width = 12, height =9)  
