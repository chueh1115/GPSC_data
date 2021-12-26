rm(list=ls())
library(readxl)
library(dplyr)
GPSC_bac <- read_excel("data/GPSC_bact/GPSC_bacteria_count_OR1-1190.xlsx")
GPSC_bac %>% select(`Bacterial Cell Count`) 
GC1_bac_count<-data.frame(time=seq(1:10),
                             count=as.numeric(unlist(GPSC_bac[2:11,2])))
library(ggplot2)
ggplot(data=GC1_bac_count,aes(x=time, y=count))+
  geom_point()


A<-9^2*pi   #well area (mm^2)
a<-0.11^2*pi #view area (mm^2)
V<-0.1/2    #sample volume	(ml)
GC1_bac_num<-mean(GC1_bac_count*A/a/V)#count* A/a/V = # bac/ ml

area<-pi*(3/2)^2/10000#m2
# Convert to biomass (assuming 10 fgC cell-1, Deming & Capenter 2008)

GC1_bac_biomass<-GC1_bac_num*10*V/1E-12/area #fgC/ml*V(ml)/syringe area(m2)/1E-12=mgC/m2


