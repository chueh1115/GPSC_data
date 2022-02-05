rm(list=ls())
library(readxl)
library(dplyr)
GPSC_bac <- read_excel("data/GPSC_bact/GPSC_bacteria_count_OR1-1190.xlsx")
GPSC_bac %>% select(`Bacterial Cell Count`) 
A<-9^2*pi   #well area (mm^2)
a<-0.11^2*pi #view area (mm^2)
V<-0.1/2    #sample volume	(ml=cm3)
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

mean(GS1_bac$biomass)
sd(GS1_bac$biomass)
