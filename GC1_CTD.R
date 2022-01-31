rm(list=ls())
library(readxl)
library(dplyr)
GPSC_CTD<- read_excel("data/GPSC_CTD/GPSC_CTD_2021.08.15.xlsx")
#GC1####
GC1_CTD<-as.data.frame(GPSC_CTD %>%
                         filter(Station=="GC1"&Cruise!="NOR1_T011"))
GC1_CTD<-split(GC1_CTD,GC1_CTD$Cruise)
#average deepest 3 data =bottom water
deep_fun <- function(x) {
  dat <- x[order(x$pressure, decreasing=TRUE)[1:3],]
}
GC1_CTD<- lapply(GC1_CTD, FUN=deep_fun) 
GC1_CTD<-do.call("rbind", GC1_CTD)
# Set negtive transmissometer to zero
GC1_CTD$transmissometer[GC1_CTD$transmissometer<=0] <- 0 
# Average temperature, sigma_theta and density
GC1_CTD$Temperature <- rowMeans(GC1_CTD[, c("temperature_T1", "temperature_T2")], na.rm=TRUE)
GC1_CTD$Salinity    <- rowMeans(GC1_CTD[, c("salinity_T1C1", "salinity_T2C2")], na.rm=TRUE)
GC1_CTD$Sigma_theta <- rowMeans(GC1_CTD[, c("density_T1C1...11", "density_T2C2...12")], na.rm=TRUE)
GC1_CTD$Density     <- rowMeans(GC1_CTD[, c("density_T1C1...13", "density_T2C2...14")], na.rm=TRUE)
GC1_CTD<-GC1_CTD %>% group_by(Cruise,Station) %>% 
  summarise(Temperature=mean(Temperature),
            Salinity=mean(Salinity),
            Sigma_theta=mean(Sigma_theta),
            Density=mean(Density))
GC1T<-mean(GC1_CTD$Temperature)
`%nin%` = Negate(`%in%`)
#GS1####
GS1_CTD<-as.data.frame(GPSC_CTD %>%
                         filter(Station=="GS1"& Cruise%nin%c("OR1_1132","NOR1_T011")))
unique(GS1_CTD$Cruise)
GS1_CTD<-split(GS1_CTD,GS1_CTD$Cruise)
GS1_CTD<- lapply(GS1_CTD, FUN=deep_fun) 
GS1_CTD<-do.call("rbind", GS1_CTD)
# Set negtive transmissometer to zero
GS1_CTD$transmissometer[GS1_CTD$transmissometer<=0] <- 0 
# Average temperature, sigma_theta and density
GS1_CTD$Temperature <- rowMeans(GS1_CTD[, c("temperature_T1", "temperature_T2")], na.rm=TRUE)
GS1_CTD$Salinity    <- rowMeans(GS1_CTD[, c("salinity_T1C1", "salinity_T2C2")], na.rm=TRUE)
GS1_CTD$Sigma_theta <- rowMeans(GS1_CTD[, c("density_T1C1...11", "density_T2C2...12")], na.rm=TRUE)
GS1_CTD$Density     <- rowMeans(GS1_CTD[, c("density_T1C1...13", "density_T2C2...14")], na.rm=TRUE)
GS1_CTD<-GS1_CTD %>% group_by(Cruise,Station) %>% 
  summarise(Temperature=mean(Temperature),
            Salinity=mean(Salinity),
            Sigma_theta=mean(Sigma_theta),
            Density=mean(Density))
GS1T<-mean(GS1_CTD$Temperature)

#Tlim calculation####
#Q10=2; GC1T 
GC1Tlim<-2* exp((GC1T-20)/10)
GS1Tlim<-2* exp((GS1T-20)/10)

#plot CTD
GC1<-as.data.frame(GPSC_CTD %>%
                         filter(Station=="GC1"&Cruise!="NOR1_T011"))
GC1$Temperature <- rowMeans(GC1[, c("temperature_T1", "temperature_T2")], na.rm=TRUE)
GC1$Salinity    <- rowMeans(GC1[, c("salinity_T1C1", "salinity_T2C2")], na.rm=TRUE)
GC1$Sigma_theta <- rowMeans(GC1[, c("density_T1C1...11", "density_T2C2...12")], na.rm=TRUE)
GC1$Density     <- rowMeans(GC1[, c("density_T1C1...13", "density_T2C2...14")], na.rm=TRUE)

