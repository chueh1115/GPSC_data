rm(list = ls())
library(readxl)
library(dplyr)
library(rstatix)
#if there's difference exist between different seasons in GC1?####
 #H0: mean of all season are the same (mean sp = mean au = mean su)
 #HA: at least mean of one season is different from the others
#assumptions for anova
 #1.randomly sampled and samples are independent
 #2.samples come from a normally distributed population with unknown but equal variances

#steps
 #GC1_eliminated$Season==different season groups
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
 #GC1_outlier####
GC1_Q   <- quantile(GC1$OC, probs=c(.25, .75), na.rm = FALSE)
GC1_iqr <- IQR(GC1$OC)
GC1_up  <- GC1_Q[2]+1.5*GC1_iqr # Upper Range  
GC1_low <- GC1_Q[1]-1.5*GC1_iqr # Lower Range
GC1_eliminated<- subset(GC1, GC1$OC > GC1_low & GC1$OC < GC1_up)
GC1_eliminated$Season[GC1_eliminated$Cruise=="OR1_1114"]<-"SU"
GC1_eliminated$Season[GC1_eliminated$Cruise 
                      %in% c("OR1_1096","OR1_1126","OR1_1151")]<-"AU"
GC1_eliminated$Season[GC1_eliminated$Cruise 
                      %in% c("OR1_1099","OR1_1102","OR1_1128","OR1_1190")]<-"SP"





 #Shapiro-Wilk normality test (N<50)####
shapiro.test(GC1_eliminated$OC)
#p-value = 0.05805(>0.05)
#cannot reject H0 and conclude that GC1_eliminated$OC is normally distributed)
 #F test####
GC1_ausp<-GC1_eliminated %>% filter(Season%in%c("AU","SP"))
var.test(OC~Season, GC1_ausp)
#p-value = 0.5394 (>0.05)
GC1_ausu<-GC1_eliminated %>% filter(Season%in%c("AU","SU"))
var.test(OC~Season, GC1_ausu)
#p-value = 0.1052 (>0.05)
GC1_spsu<-GC1_eliminated %>% filter(Season%in%c("SP","SU"))
var.test(OC~Season, GC1_spsu)
#p-value = 0.2724 (>0.05)
#cannot reject H0 and conclude that there is no statistically difference between the variance of seasons
 #ANOVA####
anovaGC1<-aov(OC~Season, GC1_eliminated)
summary(anovaGC1)
#Pr(>F)=0.303(>0.05)
#cannot reject H0 and conclude that there is no statistically difference between the mean of seasons

 #Nonparametric test####
kruskal.test(OC~Season, data = GC1_eliminated)
#p-value = 0.1605 (>0.05)
#cannot reject H0 and conclude that there is no statistically difference between the mean of seasons

#GS1
 
#if there's difference exist between different seasons in GS1?####
 #H0: mean of all season are the same (mean sp = mean au = mean su)
 #HA: at least mean of one season is different from the others
#assumptions for anova
 #1.randomly sampled and samples are independent
 #2.samples come from a normally distributed population with unknown but equal variances

#steps
#GS1_eliminated$Season==different season groups

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

 #GS1_outlier####
GS1_Q   <- quantile(GS1$OC, probs=c(.25, .75), na.rm = FALSE)
GS1_iqr <- IQR(GS1$OC)
GS1_up  <- GS1_Q[2]+1.5*GS1_iqr # Upper Range  
GS1_low <- GS1_Q[1]-1.5*GS1_iqr # Lower Range
GS1_eliminated <-subset(GS1, GS1$OC > GS1_low & GS1$OC < GS1_up)
unique(GS1_eliminated$Cruise)
GS1_eliminated$Season[GS1_eliminated$Cruise=="OR1_1114"]<-"SU"
GS1_eliminated$Season[GS1_eliminated$Cruise 
                      %in% c("OR1_1096","OR1_1126","OR1_1151")]<-"AU"
GS1_eliminated$Season[GS1_eliminated$Cruise 
                      %in% c("OR1_1099","OR1_1102","OR1_1132","OR1_1190")]<-"SP"
 #Shapiro-Wilk normality test (N<50)####
shapiro.test(GS1_eliminated$OC)
#p-value = 0.03998(<0.05)
#reject H0 and conclude that GC1_eliminated$OC is not normally distributed)

 #Nonparametric test####
kruskal.test(OC~Season, data = GS1_eliminated)
#p-value = 0.4471 (>0.05)
#cannot reject H0 and conclude that there is no statistically difference between the mean of seasons


#if there's difference exist between 2 stations?####
GC1GS1<-rbind(GC1_eliminated,GS1_eliminated)
 #H0: mean of 2 stations are the same 
 #HA: mean of 2 stations are the different
 #Shapiro-Wilk normality test (N<50)####
shapiro.test(GC1GS1$OC) 
#p-value = 4.053e-08 (<0.05)
#reject H0 and conclude that GC1GS1$OC is not normally distributed
 #Nonparametric test: independent 2-group Mann-Whitney U Test#####
wilcox.test(OC~Habitat,GC1GS1)
#p-value = 5.079e-08(<0.05)
#reject H0 and conclude that is statistically difference between the mean of 2 stations