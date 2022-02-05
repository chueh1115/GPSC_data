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


#plot CTD####
rm(list=ls())
library(ggplot2)
library(readxl)
library(dplyr)
library(ggthemes)

GPSC_CTD<- read_excel("data/GPSC_CTD/GPSC_CTD_2021.08.15.xlsx")

#GC1####
GC1<-as.data.frame(GPSC_CTD %>%
                         filter(Station=="GC1"&Cruise!="NOR1_T011"))
GC1$Temperature <- rowMeans(GC1[, c("temperature_T1", "temperature_T2")], na.rm=TRUE)
GC1$Salinity    <- rowMeans(GC1[, c("salinity_T1C1", "salinity_T2C2")], na.rm=TRUE)
GC1$Sigma_theta <- rowMeans(GC1[, c("density_T1C1...11", "density_T2C2...12")], na.rm=TRUE)
GC1$Density     <- rowMeans(GC1[, c("density_T1C1...13", "density_T2C2...14")], na.rm=TRUE)

#plot CTD #down+up data
library(ggplot2)
ggplot(data=GC1, aes(x=Temperature, y=pressure, colour=Cruise))+
  geom_point(size=0.5)+
  scale_y_reverse()+
  facet_wrap(~Station, scale="free")

#filter only downward data
GC1<-GC1 %>% group_split(Cruise)
GC1_CTD<-NULL
for (i in 1:11) { 
  out<-filter(GC1[[i]],!duplicated(GC1[[i]]$pressure,)==T)
  GC1_CTD<-rbind(GC1_CTD,out)
} 
#plot downward CTD data
ggplot(data=GC1_CTD, aes(x=Temperature, y=pressure, colour=Cruise))+
  geom_point(size=0.5)+
  scale_y_reverse()+
  facet_wrap(~Station, scale="free")+
  theme_base()
  




#GS1####
`%nin%` = Negate(`%in%`)
GS1<-as.data.frame(GPSC_CTD %>%
                     filter(Station=="GS1"&Cruise%nin%c("OR1_1132","NOR1_T011")))
GS1$Temperature <- rowMeans(GS1[, c("temperature_T1", "temperature_T2")], na.rm=TRUE)
GS1$Salinity    <- rowMeans(GS1[, c("salinity_T1C1", "salinity_T2C2")], na.rm=TRUE)
GS1$Sigma_theta <- rowMeans(GS1[, c("density_T1C1...11", "density_T2C2...12")], na.rm=TRUE)
GS1$Density     <- rowMeans(GS1[, c("density_T1C1...13", "density_T2C2...14")], na.rm=TRUE)

#plot CTD #down+up data
library(ggplot2)
ggplot(data=GS1, aes(x=Temperature, y=pressure, colour=Cruise))+
  geom_point(size=0.5)+
  scale_y_reverse()+
  facet_wrap(~Station, scale="free")

#filter only downward data
GS1<-GS1 %>% group_split(Cruise)
GS1_CTD<-NULL
for (i in 1:11) { 
  out<-filter(GS1[[i]],!duplicated(GS1[[i]]$pressure,)==T)
  GS1_CTD<-rbind(GS1_CTD,out)
} 
#plot downward CTD data
ggplot(data=GS1_CTD, aes(x=Temperature, y=pressure, colour=Cruise))+
  geom_point(size=0.5)+
  scale_y_reverse()+
  facet_wrap(~Station, scale="free")+
  theme_base()
#compare by cruise
GC1GS1<-rbind(GC1_CTD,GS1_CTD)
ggplot(data=GC1GS1, aes(x=Temperature, y=pressure, colour=Station))+
  geom_point(size=0.5)+
  scale_y_reverse()+
  xlab("Temperature(°C)")+
  ylab("Depth(m)")+
  facet_wrap(~Cruise, scale="free")+
  theme_base()


