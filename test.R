rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
library(openxlsx)
#Sediment####
GPSC_sediment <- read_excel("data/GPSC_sediment/GPSC_sediment_2021.08.16_ysl.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "text", "text", "numeric"))
Canyon_sediment<-GPSC_sediment%>%filter(Station %in% c("GC1","GC2","GC3","GC4",
                                              "SC1","SC2","SC3",
                                              "FC1", "FC2", "FC2A", "FC3", "FC4", "FC5A"))
write_xlsx(Canyon_sediment,"Canyon_sediment.xlsx")
#CTD####
GPSC_CTD <- read_excel("data/GPSC_CTD/GPSC_CTD_2021.08.15.xlsx")
levels(factor(GPSC_CTD$Station))
Canyon_CTD<-GPSC_CTD%>% filter(Station%in%c("GC1","GC2","GC3","GC4","GC5","GC6",
                                            "SC1","SC2","SC3",
                                            "FC1", "FC2", "FC3", "FC4", "FC5A"))
write_xlsx(Canyon_CTD,"Canyon_CTD.xlsx")
#Macrofauna####
 #cruise
GPSC_macro_cruise <- read_excel("data/GPSC_macro_sorting/GPSC_macro_sorting_2021.11.23.xlsx")
levels(factor(GPSC_macro_cruise$Station))
Canyon_macro_cruise<-GPSC_macro_cruise%>% filter(Station %in% c("GC1","GC2","GC3","GC4","GC5","GC6",
                                                                "SC1","SC2","SC3",
                                                                "FC1", "FC2", "FC2A", "FC3", "FC4", "FC5A"))
 #abundance
GPSC_macro_abundance <- read_excel("data/GPSC_macro_sorting/GPSC_macro_sorting_2021.11.23.xlsx", 
                                            sheet = "Specimen")
levels(factor(GPSC_macro_abundance$Station))
Canyon_macro_abundance<-GPSC_macro_abundance%>% filter(Station %in% c("GC1","GC2","GC3","GC4","GC5","GC6",
                                                                "SC1","SC2","SC3",
                                                                "FC1", "FC2", "FC2A", "FC3", "FC4", "FC5A"))
 #taxa
#Polychaeta
Polychaeta <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                         sheet = "Polychaeta",col_types = c("text","text", "text", 
                                                            "numeric", "numeric", 
                                                            "text", "text", "numeric",
                                                            "numeric","text", "text",
                                                            "numeric", "numeric",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "text"))
#Amphipoda
Amphipoda <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                         sheet = "Amphipoda",col_types = c("text","text", "text", 
                                                           "numeric", "numeric", 
                                                           "text", "text", "numeric",
                                                           "numeric","text", "text",
                                                           "numeric", "numeric",
                                                           "numeric", "numeric", "numeric",
                                                           "numeric", "text"))
#Isopoda
Isopoda <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                        sheet = "Isopoda",col_types = c("text","text", "text", 
                                                        "numeric", "numeric", 
                                                        "text", "text", "numeric",
                                                        "numeric","text", "text",
                                                        "numeric", "numeric",
                                                        "numeric", "numeric", "numeric",
                                                        "numeric", "text"))
#Tanaidacea
Tanaidacea <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Tanaidacea",col_types = c("text","text", "text", 
                                                         "numeric", "numeric", 
                                                         "text", "text", "numeric",
                                                         "numeric","text", "text",
                                                         "numeric", "numeric",
                                                         "numeric", "numeric", "numeric",
                                                         "numeric", "text"))
#Cumacea
Cumacea <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Cumacea",col_types = c("text","text", "text", 
                                                      "numeric", "numeric", 
                                                      "text", "text", "numeric",
                                                      "numeric","text", "text",
                                                      "numeric", "numeric",
                                                      "numeric", "numeric", "numeric",
                                                      "numeric", "text"))

#Osrtracoda
Osrtracoda <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Osrtracoda",col_types = c("text","text", "text", 
                                                         "numeric", "numeric", 
                                                         "text", "text", "numeric",
                                                         "numeric","text", "text",
                                                         "numeric", "numeric",
                                                         "numeric", "numeric", "numeric",
                                                         "numeric", "text"))
#Scaphopoda
Scaphopoda <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Scaphopoda",col_types = c("text","text", "text", 
                                                         "numeric", "numeric", 
                                                         "text", "text", "numeric",
                                                         "numeric","text", "text",
                                                         "numeric", "numeric",
                                                         "numeric", "numeric", "numeric",
                                                         "numeric", "text"))
#Aplacophora
Aplacophora <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Aplacophora",col_types = c("text","text", "text", 
                                                          "numeric", "numeric", 
                                                          "text", "text", "numeric",
                                                          "numeric","text", "text",
                                                          "numeric", "numeric",
                                                          "numeric", "numeric", "numeric",
                                                          "numeric", "text"))
#Sipuncula
Sipuncula <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Sipuncula",col_types = c("text","text", "text", 
                                                        "numeric", "numeric", 
                                                        "text", "text", "numeric",
                                                        "numeric","text", "text",
                                                        "numeric", "numeric",
                                                        "numeric", "numeric", "numeric",
                                                        "numeric", "text"))
#Harpacticoida
Harpacticoida <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Harpacticoida",col_types = c("text","text", "text", 
                                                            "numeric", "numeric", 
                                                            "text", "text", "numeric",
                                                            "numeric","text", "text",
                                                            "numeric", "numeric",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "text"))
#Echinoderm
Echinoderm <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Echinoderm",col_types = c("text","text", "text", 
                                                         "numeric", "numeric", 
                                                         "text", "text", "numeric",
                                                         "numeric","text", "text",
                                                         "numeric", "numeric",
                                                         "numeric", "numeric", "numeric",
                                                         "numeric", "text"))
#Oligochaeta
Oligochaeta <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Oligochaeta",col_types = c("text","text", "text", 
                                                          "numeric", "numeric", 
                                                          "text", "text", "numeric",
                                                          "numeric","text", "text",
                                                          "numeric", "numeric",
                                                          "numeric", "numeric", "numeric",
                                                          "numeric", "text"))
#Bivalvia
Bivalvia <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Bivalvia", col_types = c("text","text", "text", 
                                                        "numeric", "numeric", 
                                                        "text", "text", "numeric",
                                                        "numeric","text", "text",
                                                        "numeric", "numeric",
                                                        "numeric", "numeric", "numeric",
                                                        "numeric", "text"))
#Nemertea
Nemertea <- read_excel("data/GPSC_macro_size/GPSC_macro_size_2020.08.03.xlsx",
                      sheet = "Nemertea",col_types = c("text","text", "text", 
                                                       "numeric", "numeric", 
                                                       "text", "text", "numeric",
                                                       "numeric","text", "text",
                                                       "numeric", "numeric",
                                                       "numeric", "numeric", "numeric",
                                                       "numeric", "text"))
GPSC_macro_size<-rbind(Polychaeta,Amphipoda,Isopoda,Tanaidacea,Cumacea,
                       Osrtracoda,Scaphopoda,Aplacophora,Sipuncula,
                       Harpacticoida,Echinoderm,Oligochaeta,Bivalvia,Nemertea)
levels(factor(GPSC_macro_size$Station))
Canyon_macro_size<-GPSC_macro_size%>% filter(Station %in% c("GC1","GC2","GC3","GC4","GC5","GC6",
                                                            "SC1","SC2","SC3",
                                                            "FC1", "FC2", "FC2A", "FC3", "FC4", "FC5A"))

macro <- createWorkbook() # workbook=excel in R
addWorksheet(macro, "cruise") # create sheet1
addWorksheet(macro, "abundance")#create sheet2
addWorksheet(macro, "size")#create sheet3
writeData(macro, "cruise", Canyon_macro_cruise) 
writeData(macro, "abundance", Canyon_macro_abundance)
writeData(macro, "size", Canyon_macro_size)
saveWorkbook(macro, "Canyon_macro.xlsx", overwrite = TRUE)


