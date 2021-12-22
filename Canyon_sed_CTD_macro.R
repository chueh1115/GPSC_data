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
Canyon_macro_size<-GPSC_macro_size%>% filter(Station %in% c("GC1","GC2","GC3","GC4","GC5","GC6",
                                                            "SC1","SC2","SC3",
                                                            "FC1", "FC2", "FC2A", "FC3", "FC4", "FC5A"))

# macro <- createWorkbook() # workbook=excel in R
# addWorksheet(macro, "cruise") # create sheet1
# addWorksheet(macro, "abundance")#create sheet2
# addWorksheet(macro, "size")#create sheet3
# writeData(macro, "cruise", Canyon_macro_cruise) 
# writeData(macro, "abundance", Canyon_macro_abundance)
# writeData(macro, "size", Canyon_macro_size)
# saveWorkbook(macro, "Canyon_macro.xlsx", overwrite = TRUE)

# writexl example
write_xlsx(list(Cruise = Canyon_macro_cruise,
                Abundance = Canyon_macro_abundance,
                Size = Canyon_macro_size),
           "Canyon_macro.xlsx")