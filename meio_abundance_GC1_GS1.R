rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
#abundance####
GPSC_meio_abundance <- read_excel("data/GPSC_meio_sorting/GPSC_meio_sorting_2016.08.18.xlsx", 
                                   sheet = "Specimen")
levels(factor(GPSC_meio_abundance$Station))
GC1_meio_abundance<-GPSC_meio_abundance%>% filter(Station=="GC1")
GS1_meio_abundance<-GPSC_meio_abundance%>% filter(Station=="GS1")
abu<-rbind(GC1_meio_abundance,GS1_meio_abundance)
GC1_mean<-GC1_meio_abundance %>% 
  group_by(Cruise,Station) %>% 
  mutate(sum=sum(Abundance)) %>% 
  group_by(Station) %>% 
  mutate(mean=mean(sum))
GS1_mean<-GS1_meio_abundance %>% 
  group_by(Cruise,Station) %>% 
  mutate(sum=sum(Abundance)) %>% 
  group_by(Station) %>% 
  mutate(mean=mean(sum))
2109.905/629.625
library(ggplot2)
library(pals)

color<-as.vector(stepped(length(unique(abu$Taxon))-1+2)[-c(1:2)])

#plot
abu %>% filter(Taxon!="Nematoda") %>%  
  ggplot(aes(x=Cruise,y=Abundance))+
  geom_bar(stat = "identity",position = "stack",
           aes(fill=Taxon))+
  scale_fill_manual(values =color )+
  ylab(expression(Abundance~(n)))+
  ylim(0, NA)+
  facet_wrap(~Station,scales = "free_y")+
  theme_bw()+labs(title = "Meiofauna (Nematoda excluded)")+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("abu_meio_wo_nematode.png",width = 12, height =9)

color<-as.vector(stepped(length(unique(abu$Taxon))+5)[-c(1:5)])
abu %>%  
  ggplot(aes(x=Cruise,y=Abundance))+
  geom_bar(stat = "identity",position = "stack",
           aes(fill=Taxon))+
  scale_fill_manual(values =color )+
  ylab(expression(Abundance~(n)))+
  ylim(0, NA)+
  facet_wrap(~Station,scales = "free_y")+
  theme_bw()+labs(title = "Meiofauna")+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("abu_meio_nematode.png",width = 12, height =9)

                 