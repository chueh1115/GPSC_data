rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
library(openxlsx)
#abundance####
GPSC_macro_abundance <- read_excel("data/GPSC_macro_sorting/GPSC_macro_sorting_2021.11.23.xlsx", 
                                   sheet = "Specimen")
levels(factor(GPSC_macro_abundance$Station))
GC1_macro_abundance<-GPSC_macro_abundance%>% filter(Station=="GC1")
GS1_macro_abundance<-GPSC_macro_abundance%>% filter(Station=="GS1")
#GC1_size####
GPSC_macro_size<-read_excel("Canyon_macro.xlsx",sheet = "Size")
levels(factor(GPSC_macro_size$Station))
GC1_macro<-GPSC_macro_size%>%filter(Station=="GC1")
#fix Cylinder and cylinder
levels(factor(GC1_macro$Type))
GC1_x<-GC1_macro %>% filter(Type=="Cylinder") %>% mutate(Type="cylinder")
GC1_macro<-GC1_macro %>% filter(Type!="Cylinder") %>% full_join(GC1_x)
levels(factor(GC1_macro$Type))

#with C value-> type=LW2C
GC1_macro[is.na(GC1_macro$C),]
#LW2C 125
GC1_LW2C<-GC1_macro %>% mutate(Volume=L*(W^2)*C) %>% filter(!is.na(Volume))
#cone 2
GC1_cone<-GC1_macro %>% filter(Type=="cone") %>% mutate(Volume=L*(W/2)^2*pi/3)
#cylinder 12
#GC1_macro%>%filter(Type=="cylinder")
#some polychaete were labelled as cylinder -> real cylinder 7
GC1_cylinder<-GC1_macro %>% filter(Taxon%in%c("Nemertea","Sipuncula")) %>%
  mutate(Volume=L*(W/2)^2*pi)
#ellipsoid
GC1_ellipsoid<-GC1_macro %>% filter(Type=="ellipsoid") %>%
  mutate(Volume=L*(W/2)^2*pi*4/3)
GC1<-rbind(GC1_LW2C,GC1_cone,GC1_cylinder,GC1_ellipsoid)
#V->OC
GC1_macro_OC<-GC1 %>%  mutate(DryW=Volume*1.13) %>% #DryW(mg)=volume(mm3)*1.13(g/cm3=mg/mm3)
  mutate(OC=DryW*0.043) #0.043 (fraction)
GC1 <-GC1_macro_OC %>% group_by(Cruise, Habitat, Station, Deployment, Tube) %>% 
  summarise(OC = sum(OC)) 

area<-pi*(10.5/2)^2/10000 #0.008659015m2
library(ggplot2)
GC1 %>% 
  ggplot(aes(x = Cruise, y = OC/area))+
  geom_boxplot()+
  geom_point(position = "jitter", color = "red")+
  geom_hline(yintercept = mean(GC1$OC)/area, linetype = 2, color = "blue")+
  ylab("OC/area(mgC/m2)")

GC1_mean<-mean(GC1$OC/area)
GC1_sd<-sd(GC1$OC/area)

#GC1_outlier####
GC1_Q   <- quantile(GC1$OC, probs=c(.25, .75), na.rm = FALSE)
GC1_iqr <- IQR(GC1$OC)
GC1_up  <- GC1_Q[2]+1.5*GC1_iqr # Upper Range  
GC1_low <- GC1_Q[1]-1.5*GC1_iqr # Lower Range
GC1_eliminated<- subset(GC1, GC1$OC > GC1_low & GC1$OC < GC1_up)

GC1_eliminated %>% 
  ggplot(aes(x = Cruise, y = OC/area))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = "jitter", color = "red")+
  geom_hline(yintercept = mean(GC1_eliminated$OC)/area, linetype = 2, color = "blue")+
  ylab("OC/area(mgC/m2)")
GC1_el_mean<-mean(GC1_eliminated$OC/area)
GC1_el_sd  <-sd(GC1_eliminated$OC/area)

#GS1_size####
GPSC_macro_size<-NULL
for (i in 1:15){
  df<- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                  sheet = i ,col_types = c("text","text", "text", 
                                           "numeric", "numeric", 
                                           "text", "text", "numeric",
                                           "numeric","text", "text",
                                           "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "text"))
  GPSC_macro_size<-rbind(GPSC_macro_size,df)
}

levels(factor(GPSC_macro_size$Station))
GS1_macro<-GPSC_macro_size %>% filter(Station=="GS1")
levels(factor(GS1_macro$Type))
#fix Cylinder and cylinder
GS1_x<-GS1_macro %>% filter(Type=="Cylinder") %>% mutate(Type="cylinder")
GS1_macro<-GS1_macro %>% filter(Type!="Cylinder") %>% full_join(GS1_x)
levels(factor(GS1_macro$Type))

#with C value-> type=LW2C
GS1_macro$C[is.na(GS1_macro$C)&GS1_macro$Taxon=="Polychaeta"]=0.53
GS1_macro[is.na(GS1_macro$C),]#91
#LW2C 1374
GS1_LW2C<-GS1_macro %>% mutate(Volume=L*(W^2)*C) %>% filter(!is.na(Volume))
#cone 3
GS1_cone<-GS1_macro %>% filter(Type=="cone") %>% mutate(Volume=L*(W/2)^2*pi/3)
#cylinder 78
GS1_cylinder<-GS1_macro %>% filter(Type=="cylinder" & 
           Taxon%in%c("Aplacophora","Sipuncula","Ophiuroidea","Nemertea")) %>%
  mutate(Volume=L*(W/2)^2*pi)
#ellipsoid 10
GS1_ellipsoid<-GS1_macro %>% filter(Type=="ellipsoid" & 
           Taxon%in%c("Ophiuroidea","Asteroidea" )) %>%
  mutate(Volume=L*(W/2)^2*pi*4/3)
GS1<-rbind(GS1_LW2C,GS1_cone,GS1_cylinder,GS1_ellipsoid)
#V->OC
GS1_macro_OC<-GS1%>%
  mutate(DryW=Volume*1.13)%>% #DryW(mg)=volume(mm3)*1.13(g/cm3=mg/mm3)
  mutate(OC=DryW*0.043) #0.043 (fraction)
GS1 <-GS1_macro_OC %>% 
  group_by(Cruise, Habitat, Station, Deployment, Tube) %>% 
  summarise(OC = sum(OC)) 

GS1 %>% 
  ggplot(aes(x = Cruise, y = OC/area))+
  geom_boxplot()+
  geom_point(position = "jitter", color = "red")+
  geom_hline(yintercept = mean(GS1$OC)/area, linetype = 2, color = "blue")+
  ylab("OC/area(mgC/m2)")

GS1_mean<-mean(GS1$OC/area)
GS1_sd<-sd(GS1$OC/area)

#GS1_outlier####
GS1_Q   <- quantile(GS1$OC, probs=c(.25, .75), na.rm = FALSE)
GS1_iqr <- IQR(GS1$OC)
GS1_up  <- GS1_Q[2]+1.5*GS1_iqr # Upper Range  
GS1_low <- GS1_Q[1]-1.5*GS1_iqr # Lower Range
GS1_eliminated <-subset(GS1, GS1$OC > GS1_low & GS1$OC < GS1_up)
GS1_eliminated%>%
  ggplot(aes(x = Cruise, y = OC/area))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = "jitter", color = "red")+
  geom_hline(yintercept = mean(GS1_eliminated$OC)/area, linetype = 2, color = "blue")+
  ylab("OC/area(mgC/m2)")
GS1_el_mean<-mean(GS1_eliminated$OC/area)
GS1_el_sd<-sd(GS1_eliminated$OC/area)
GS1_eliminated
#plot GC1 and GS1 together(bar+point)####
GC1_bar<-GC1_eliminated %>% group_by(Cruise, Station) %>% 
  summarise(mean=mean(OC/area),
            sd=sd(OC/area))
GS1_bar<-GS1_eliminated %>% group_by(Cruise, Station) %>% 
  summarise(mean=mean(OC/area),
            sd=sd(OC/area))
MAC_bar<-rbind(GC1_bar,GS1_bar)
MAC_point<-rbind(GC1_eliminated,GS1_eliminated)


MAC_bar%>%
  ggplot(aes(x = Cruise, y = mean))+
  geom_bar(stat = "identity",position = position_dodge(),
         aes(fill=Station))+
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), size=0.5,width=0.1,alpha=0.9, colour="black")+
  ylab(expression(OC~(mg~C~m^-2)))+
  ylim(0, NA)+
  facet_wrap(~Station,scales = "free_y")+
  theme_bw()+labs(title = "Macrofauna")+
  guides(color = guide_legend(override.aes = list(size = 3) ) )+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25))+
  geom_point(data=MAC_point,aes(x=Cruise,y=OC/area), color = "darkblue",size=2)
ggsave("OC_macro.png",width = 12, height =9)  
