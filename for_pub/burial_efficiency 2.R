rm(list = ls())
library(dplyr)
library(readxl)
#1. calculate the burial rate
#In Huh et al. (2009)
#GC1_br<- 0.3 (g/cm2/yr) 
#GS1_br<- 0.7 (g/cm2/yr)
GPSC_sediment<- read_excel("Canyon_sediment.xlsx")
GC1_sed<-GPSC_sediment %>% 
  filter(Station=="GC1") %>% 
  filter(TOC!="NA")
GC1_TOC<-mean(GC1_sed$TOC)
GPSC_sediment <- read_excel("data/GPSC_sediment/GPSC_sediment_2021.08.16_ysl.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "text", "text", "numeric"))
GS1_sed<-GPSC_sediment %>% filter(Station=="GS1"&Cruise!="OR1_1132")%>%  filter(TOC!="NA")
GS1_TOC<-mean(GS1_sed$TOC)
#br(g/cm2/yr)*1000/0.01/0.01/365*TOC(%)/100=(mgC/m2/d)
GC1_br<-0.3*1000/0.01/0.01/365*GC1_TOC/100 #=32.08
GS1_br<-0.7*1000/0.01/0.01/365*GS1_TOC/100 #=100.92

#In Hsu et al. (2014)
#GS1_br<-0.31(g/cm2/yr)< 0.7 #(g/cm2/yr)

#In Tsai and Chung(1989) 
#GC1_br<-0.723(cm/yr)
#br(cm/yr)*density(g/cm3)*1000/0.01/0.01/365*TOC(%)/100=(mgC/m2/d)
GC1density<-2.1
GC1_br2<-0.723*2.1*1000/0.01/0.01/365*GC1_TOC/100 #=162.34
#GC1_br2 >GC1_br ->use this data


#2. POC flux
#in Liu et al. (2006): 200- 700 g/ m2/ d
#in Liu et al. (2009): 200- 800 g/ m2/ d
#TOC content (0.4-0.6 %) 
GC1_POCmin<-200*1000*0.4/100 #mg C/ m2/ d
GC1_POCmax<-800*1000*0.6/100

GS1_POCmin<-115#Shih et al. 2020
GS1_POCmax<-200*1000*0.4/100#=GC1 min

#3. min burial efficiency= burial rate (mgC/m2/d) / poc flux (mgC/m2/d)=%
GC1_br2/GC1_POCmin #=0.2029224
GC1_br2/GC1_POCmax #=0.0338204
GS1_br/GS1_POCmin #=0.8774855
GS1_br/GS1_POCmax #=0.1261385
