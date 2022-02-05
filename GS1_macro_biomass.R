rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
library(openxlsx)
#abundance####
GPSC_macro_abundance <- read_excel("data/GPSC_macro_sorting/GPSC_macro_sorting_2021.11.23.xlsx", 
                                   sheet = "Specimen")
levels(factor(GPSC_macro_abundance$Station))
GS1_macro_abundance<-GPSC_macro_abundance%>% filter(Station=="GS1")
#size####
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
GS1_macro_size<-GPSC_macro_size%>% filter(Station=="GS1")
