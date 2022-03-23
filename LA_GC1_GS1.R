rm(list=ls())
#-- Load the LIM (and limSolve) package 
library(LIM)
library(splus2R)
#MODEL SETUP####
#-- Define directory that contains the input file
DataDir <- "C:/Users/user/Downloads/labWei/Tung_thesis/GPSC_data/"
#GC1####
load(file="GC1.Rdata")
GC1_LIM<-LIM
GC1_xs<-xs
#GS1####
load(file="GS1.Rdata")
GS1_LIM<-LIM
GS1_xs<-xs

LA<-data.frame(flow=c(GC1_LIM$Unknowns,GS1_LIM$Unknowns), 
               mean=c(colMeans(GC1_xs$X),colMeans(GS1_xs$X)),
               station=c("GC1","GS1"))
library(ggplot2)
library(dplyr)
library(ggbreak)

LA%>% 
ggplot(aes(x = flow, y =mean,
           fill=station))+
  scale_y_continuous(trans = 'log10')+
  geom_bar(stat = "identity",
           aes(fill=station),width = 0.5)+
  ylab(expression(log10~(OC)~(mg~C~m^-2~d^-1)))+
  labs(title = "Carbon flow")+
  scale_fill_brewer(palette="Paired") + 
  theme_minimal()+
  coord_flip()
#not understandable