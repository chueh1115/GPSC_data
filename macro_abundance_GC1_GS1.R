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

#GC1####
library(pals)
abu<-rbind(GC1_macro_abundance,GS1_macro_abundance)
color<-as.vector(stepped(length(unique(abu$Taxon))+2)[-c(1:2)])
abu %>% 
  ggplot(aes(x=Cruise,y=Abundance))+
  geom_bar(stat = "identity",position = "stack",
            aes(fill=Taxon))+
  ylab(expression(Abundance~(n)))+
  labs(title = "Macrofauna")+
  scale_fill_manual(values =color )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25))+
  ylim(0, NA)+
  guides(fill=guide_legend(ncol=1))+
  facet_wrap(~Station,scales = "free_y")
  
  
ggsave("abu_macro.png",width = 12, height =9)  

#calculation
GC1mean<-GC1_macro_abundance %>% 
  group_by(Cruise,Station) %>% 
  summarize(sum=sum(Abundance))
GC1mean$mean<-mean(GC1mean$sum)
GS1mean<-GS1_macro_abundance %>% 
  group_by(Cruise,Station) %>% 
  summarize(sum=sum(Abundance))
GS1mean$mean<-mean(GS1mean$sum)
206.875/18.75
