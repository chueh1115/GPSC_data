rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
GPSC_macro_size<-read_excel("Canyon_macro.xlsx",
                            sheet = "Size")

levels(factor(GPSC_macro_size$Station))
GC1_macro<-GPSC_macro_size%>%filter(Station=="GC1")

levels(factor(GC1_macro$Type))
x<-GC1_macro%>%
  filter(Type=="Cylinder")%>%
  mutate(Type="cylinder")
GC1_macro<-GC1_macro%>%
  filter(Type!="Cylinder")%>%
  full_join(x)
levels(factor(GC1_macro$Type))


GC1_macro[is.na(GC1_macro$C),]
#LW2C
LW2C<-GC1_macro%>%
  mutate(Volume=L*(W^2)*C)%>%
  filter(!is.na(Volume))
#cone 
cone<-GC1_macro%>%
  filter(Type=="cone")%>%
  mutate(Volume=L*(W/2)^2*pi/3)

#cylinder 
cylinder<-GC1_macro%>%
  filter(Taxon%in%c("Nemertea","Sipuncula"))%>%
  mutate(Volume=L*(W/2)^2*pi)

#ellipsoid
ellipsoid<-GC1_macro%>%
  filter(Type=="ellipsoid")%>%
  mutate(Volume=L*(W/2)^2*pi*4/3)
GC1<-rbind(LW2C,cone,cylinder,ellipsoid)
GC1_macro_OC<-GC1%>%
  mutate(DryW=Volume*1.13)%>% #DryW(mg)=volume(mm3)*1.13(g/cm3=mg/mm3)
  mutate(OC=DryW*0.043) #0.043 (fraction)

y <-GC1_macro_OC %>% 
  group_by(Cruise, Habitat, Station, Deployment, Tube) %>% 
  summarise(OC = sum(OC)) 

#0.008659015
area<-pi*(10.5/2)^2/10000 #m2
library(ggplot2)
y %>% 
  ggplot(aes(x = Cruise, y = OC/area))+
  geom_boxplot()+
  geom_point(position = "jitter", color = "red")+
  geom_hline(yintercept = mean(y$OC)/area, linetype = 2, color = "blue")+
  ylab("OC/area(mgC/m2)")


MAC<-mean(y$OC/area)
sd(y$OC/area)

#outlier
Q <- quantile(y$OC, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(y$OC)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(y, y$OC > low & y$OC < up)

eliminated %>% 
  ggplot(aes(x = Cruise, y = OC/area))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = "jitter", color = "red")+
  geom_hline(yintercept = mean(eliminated$OC)/area, linetype = 2, color = "blue")+
  ylab("OC/area(mgC/m2)")
mean(eliminated$OC/area)
sd(eliminated$OC/area)
