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
  mutate(DryW=Volume*1.13)%>%
  mutate(OC=DryW*0.043)

colnames(GC1_macro_OC)

y <- GC1_macro_OC %>% 
  group_by(Cruise, Habitat, Station, Deployment, Tube) %>% 
  summarise(OC = sum(OC)) 
library(ggplot2)
y %>% 
  ggplot(aes(x = Cruise, y = OC/area))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = "jitter", color = "red")+
  geom_hline(yintercept = mean(y$OC)/area, linetype = 2, color = "blue")+
  ylab("OC/area(mgC/m2)")

#0.008659015
area<-pi*(10.5/2)^2/10000#m2
