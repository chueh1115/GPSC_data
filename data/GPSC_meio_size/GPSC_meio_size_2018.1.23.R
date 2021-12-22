library(readxl)

# Meiofauna sorting data
me <- read_excel("../../GPSC_data/GPSC_meio_sorting/GPSC_meio_sorting_2016.05.06.xlsx", sheet=2)

# Harpacticoid size data
col_types <- c("text", "text", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text", 
               "numeric", "numeric", "numeric", "text")

hs <- read_excel("../../GPSC_data/GPSC_meio_size/GPSC_meio_size_2018.1.23.xlsx", sheet=1, 
                 col_types=col_types)
keep <- match(paste(hs$Cruise, hs$Station, hs$Subcore), paste(me$Cruise, me$Station, me$Subcore))
hs$Deployment <- me$Deployment[keep]
Tube <- me$Tube[keep]
hs <- cbind(hs[, 1:4], Tube, hs[, 5:9], Species=NA, hs[,10:15])
#write.csv(hs, file="Harpacticoid.csv", row.names=FALSE, na="")


# Other taxa
ot <- read_excel("../../GPSC_data/GPSC_meio_size/GPSC_meio_size_2018.1.23.xlsx", sheet=2, 
                 col_types=col_types)
keep <- match(paste(ot$Cruise, ot$Station, ot$Subcore), paste(me$Cruise, me$Station, me$Subcore))
ot$Deployment <- me$Deployment[keep]
Tube <- me$Tube[keep]
ot <- cbind(ot[, 1:4], Tube, ot[, 5:9], Species=NA, ot[,10:15])
#write.csv(ot, file="Others.csv", row.names=FALSE, na="")
