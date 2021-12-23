rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)

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

levels(factor(GC1_meio$Type))
#calculate volume
GC1_meio_OC<-GC1_meio%>%
  mutate(Volume=L*(W^2)*C) %>% 
  mutate(DryW=Volume*1.13)%>%
  mutate(OC=DryW*0.012)
colnames(GC1_meio_OC)

y <- GC1_meio_OC %>% 
  group_by(Cruise, Habitat, Station, Deployment, Tube,Subcore) %>% 
  summarise(OC = sum(OC))

library(ggplot2)
y %>% 
  ggplot(aes(x = Cruise, y = OC/area))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = "jitter", color = "red")+
  geom_hline(yintercept = mean(y$OC)/area, linetype = 2, color = "blue")+
  ylab("OC/area(mgC/m2)")

#0.008659015
area<-pi*(3/2)^2/10000#m2
