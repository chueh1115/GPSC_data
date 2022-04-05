rm(list=ls())
library(marmap)
library(ggplot2)
library(dplyr)
library(viridis)
library(readxl)
library(ggrepel)
station<- read_excel("data/GPSC_macro_sorting/GPSC_macro_sorting_2021.11.23.xlsx")
GC1<-as.data.frame(station %>% filter(Station=="GC1"))
GS1<-as.data.frame(station %>% filter(Station=="GS1"))
long_GC1<-mean(GC1$Longitude)
lat_GC1<-mean(GC1$Latitude)
long_GS1<-mean(GS1$Longitude)
lat_GS1<-mean(GS1$Latitude)

GPSC<-read.table('119_121_21_23.xyz',
                 col.names = c("long", "lat", "depth")) %>%
  data.frame %>% 
  filter(long > 120.25 & long < 120.7) %>% 
  filter(lat<22.7& lat > 22.1)

#plot
?geom_tile
ggplot()+
  xlab("Longitude")+ylab("Latitude")+
  geom_tile(data = GPSC[GPSC$depth >= 0,], 
            aes(x = long, y = lat), fill = "black") +
  geom_raster(data = GPSC[GPSC$depth < 0 & GPSC$depth > -1000 ,],
              aes(x =long, y = lat, fill = depth)) +
  scale_fill_viridis(option = "G",limits = c(-1000, 0),
                     breaks = seq(0,-1000, -100),
                     labels = abs) +
  geom_contour(data =GPSC, aes(x=long, y=lat, z=depth),
               breaks=0,size=1.5,colour="black")+
  geom_contour(data =GPSC, aes(x=long, y=lat, z=depth),
               breaks=-100,size=0.3,colour="grey")+
  geom_contour(data =GPSC, aes(x=long, y=lat, z=depth),
               breaks=-300,size=0.3,colour="grey")+
  geom_contour(data =GPSC, aes(x=long, y=lat, z=depth),
               breaks=-500,size=0.3,colour="grey")+
  geom_contour(data =GPSC, aes(x=long, y=lat, z=depth),
               breaks=-700,size=0.3,colour="grey")+
  geom_point(aes(x = long_GC1, y = lat_GC1),color="black")+
  geom_text_repel(aes(x = long_GC1, y = lat_GC1, label = "GC1"), 
                  fontface = "bold", size=5,
                  nudge_x = c(-0.1), nudge_y = c(0.05)) +
  geom_point(aes(x = long_GS1, y = lat_GS1),color="black")+
  geom_text_repel(aes(x = long_GS1, y = lat_GS1, label = "GS1"), 
                  fontface = "bold", size=5,
                  nudge_x = c(0.1), nudge_y = c(0.05)) +
  coord_fixed(expand = FALSE) +
  labs(fill = "Depth (m)")+
  theme_bw()+
  ggsave("map.png")
