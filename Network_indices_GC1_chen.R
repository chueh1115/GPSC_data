rm(list = ls())
library(LIM)
library(NetIndices)
setwd("C:\\Users\\user\\Desktop\\NetInd_test")
# #GC####
# # Load your .Rdata file and view the iteration samples.
# load("GC1_MR+BAC_10000_100.Rdata")
# GC_xs<-xs
# GC_LIM<-LIM
# # # of iteration= # of matrices #in our case=10000
# 
# # Decide which indices you want to test.
# #T.. ?€? Total system throughput
# #TST ?€? Total system throughflow
# #Ltot ?€? Number of links
# #C ?€? Connectance
# #Tij ?€? Average link weight
# 
# # Refer to documentation if necessary.
# Indices <- c("T..", "TST", "Ltot", "C", "Tij",
#              "TSTC","FCI","APL",
#              "AMI")
# 
# # Create a matrix for the indices to be calculated for each iteration
# NetInd_GC <- matrix(NA,nrow=nrow(GC_xs$X),ncol=length(Indices)) ##dimnames
# colnames(NetInd_GC) <- Indices
# #create loop 
# 
# fm_GC  <- Flowmatrix(GC_LIM, GC_xs$X[1,])
# #define: Import, Export, Dead
# #Some of the indices only concern the organisms
# #need to tell the computer which compartments can be used for the calculations
# 
# for (i in 1:nrow(GC_xs$X)){
#   # Specify which compartments are external to serve for import and export.
#   # Specify which compartments comprise of dead material.
#   Import <- "POC_W"
#   Export <- c("EXP_S","EXP_B","DIC_W") 
#   Dead <- "SED"
#   # Get flow matrix
#   fm_GC  <- Flowmatrix(GC_LIM, GC_xs$X[i,])
#   
#   # Remove rows/cols that sum to zero,
#   # and remove Imports from columns and Exports from rows.
#   RemCol <- unique(which(colSums(fm_GC)==0),
#                    which(colnames(fm_GC)%in% Import, arr.ind = TRUE))
#   RemRow <- unique(which(rowSums(fm_GC)==0),
#                    which(rownames(fm_GC)%in% Export, arr.ind = TRUE))
#   if(length(RemRow) != 0) {fm_GC <- fm_GC[-RemRow,] }
#   if(length(RemCol) != 0) {fm_GC <- fm_GC[,-RemCol] }
#   
#   # Redefine Import and Export if they were eliminated from fm
#   Import <- Import[which(Import%in%rownames(fm_GC))]
#   Export <- Export[which(Export%in%colnames(fm_GC))]      
#   
#   # Calculate indices
#   NetInd_GC[i, "T.."] <- GenInd (Flow=fm_GC, Import=Import, Export=Export)$T..
#   NetInd_GC[i, "TST"] <- GenInd (Flow=fm_GC, Import=Import, Export=Export)$TST
#   NetInd_GC[i, "Ltot"]<- GenInd (Flow=fm_GC, Import=Import, Export=Export)$Ltot
#   NetInd_GC[i, "C"]   <- GenInd (Flow=fm_GC, Import=Import, Export=Export)$C
#   NetInd_GC[i, "Tij"] <- GenInd (Flow=fm_GC, Import=Import, Export=Export)$Tij
#   NetInd_GC[i, "TSTC"] <-PathInd (Flow=fm_GC, Import=Import, Export=Export)$TSTC
#   NetInd_GC[i, "FCI"]  <-PathInd (Flow=fm_GC, Import=Import, Export=Export)$FCI
#   NetInd_GC[i, "APL"]  <-PathInd (Flow=fm_GC, Import=Import, Export=Export)$APL
#   NetInd_GC[i, "AMI"]  <- UncInd (Flow=fm_GC, Import=Import, Export=Export)$AMI
# 
#   }
# save(NetInd_GC,file="GC1_Indices.Rdata")
# 
# # Calculate the mean and standard deviation of all indices
# NI_mean_GC <- colMeans(NetInd_GC)
# NI_stdev_GC <- sqrt(diag(var(NetInd_GC)))
# 
# 
# #GS####
# # Load your .Rdata file and view the iteration samples.
# load("GS1_CR_10000_100.Rdata")
# GS_xs<-xs
# GS_LIM<-LIM
# # # of iteration= # of matrices #in our case=10000
# 
# # Create a matrix for the indices to be calculated for each iteration
# NetInd_GS <- matrix(NA,nrow=nrow(GS_xs$X),ncol=length(Indices)) ##dimnames
# colnames(NetInd_GS) <- Indices
# #create loop 
# 
# fm_GS  <- Flowmatrix(GS_LIM, GS_xs$X[1,])
# #define: Import, Export, Dead
# #Some of the indices only concern the organisms
# #need to tell the computer which compartments can be used for the calculations
# 
# for (i in 1:nrow(GS_xs$X)){
#   # Specify which compartments are external to serve for import and export.
#   # Specify which compartments comprise of dead material.
#   Import <- "POC_W"
#   Export <- c("EXP_S","EXP_B","DIC_W") 
#   Dead <- "SED"
#   # Get flow matrix
#   fm_GS  <- Flowmatrix(GS_LIM, GS_xs$X[i,])
#   
#   # Remove rows/cols that sum to zero,
#   # and remove Imports from columns and Exports from rows.
#   RemCol <- unique(which(colSums(fm_GS)==0),
#                    which(colnames(fm_GS)%in% Import, arr.ind = TRUE))
#   RemRow <- unique(which(rowSums(fm_GS)==0),
#                    which(rownames(fm_GS)%in% Export, arr.ind = TRUE))
#   if(length(RemRow) != 0) {fm_GS <- fm_GS[-RemRow,] }
#   if(length(RemCol) != 0) {fm_GS <- fm_GS[,-RemCol] }
#   
#   # Redefine Import and Export if they were eliminated from fm
#   Import <- Import[which(Import%in%rownames(fm_GS))]
#   Export <- Export[which(Export%in%colnames(fm_GS))]      
#   
#   # Calculate indices
#   NetInd_GS[i, "T.."] <- GenInd (Flow=fm_GS, Import=Import, Export=Export)$T..
#   NetInd_GS[i, "TST"] <- GenInd (Flow=fm_GS, Import=Import, Export=Export)$TST
#   NetInd_GS[i, "Ltot"]<- GenInd (Flow=fm_GS, Import=Import, Export=Export)$Ltot
#   NetInd_GS[i, "C"]   <- GenInd (Flow=fm_GS, Import=Import, Export=Export)$C
#   NetInd_GS[i, "Tij"] <- GenInd (Flow=fm_GS, Import=Import, Export=Export)$Tij
#   NetInd_GS[i, "TSTC"] <-PathInd (Flow=fm_GS, Import=Import, Export=Export)$TSTC
#   NetInd_GS[i, "FCI"]  <-PathInd (Flow=fm_GS, Import=Import, Export=Export)$FCI
#   NetInd_GS[i, "APL"]  <-PathInd (Flow=fm_GS, Import=Import, Export=Export)$APL
#   NetInd_GS[i, "AMI"]  <- UncInd (Flow=fm_GS, Import=Import, Export=Export)$AMI
#   
# }
# save(NetInd_GS,file="GS1_Indices.Rdata")
# 
# # Calculate the mean and standard deviation of all indices

# NI_mean_GS <- colMeans(NetInd_GS)
# NI_stdev_GS <- sqrt(diag(var(NetInd_GS)))

#plot####
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
# load ####
load(file="GC1_Indices.Rdata")
load(file="GS1_Indices.Rdata")

# data setup ####
GC <- as.data.frame(NetInd_GC)
GS <- as.data.frame(NetInd_GS)

# remove constants
GC$C <- NULL
GC$Ltot <- NULL
GS$C <- NULL
GS$Ltot <- NULL

# long data
ind <- c("T..", "TST", "Tij", "TSTC","FCI","APL","AMI")
GC_long <-
  GC %>% 
  mutate(order = 1:nrow(GC)) %>% 
  mutate(station = "GC1") %>% 
  pivot_longer(cols = all_of(ind),
               names_to = "index",
               values_to = "value")

GS_long <-
  GS %>% 
  mutate(order = 1:nrow(GS)) %>% 
  mutate(station = "GS1") %>% 
  pivot_longer(cols = all_of(ind),
               names_to = "index",
               values_to = "value")

ind<-full_join(GC_long,GS_long)

# PCA 
# conform data
GC_pca <- GC %>% mutate(station = "GC1")
GS_pca <- GS %>% mutate(station = "GS1")

# scale 
pca_data <- 
  full_join(GC_pca, GS_pca) %>% 
  mutate(across(all_of(colnames(GC_pca)[-8]), scale))

# run pca
pca <- vegan::rda(pca_data[-8])

# extract pca information
sites <- data.frame(pca$CA$u)
sites$station <- pca_data$station
species <- data.frame(pca$CA$v)

ggplot()+
  geom_point(data = sites,
             aes(x = PC1, y = PC2, color = station),
             alpha = 0.1,
             shape = 1)

# pairplot ####
#GC pairplot
jpeg("GC_pairs.jpg", width = 800, height = 600)
pairs(GC, alpha = 0.1)
dev.off()

# GS pairplot
jpeg("GS_pairs.jpg", width = 800, height = 600)
pairs(GS, alpha = 0.1, color = GS$order)
dev.off()


# boxplot ####
Net_ind_boxplot <- 
  ind %>%
  ggplot(aes(x = station, y = value))+
  geom_jitter(alpha = 0.1, width = 0.2, shape = 1, aes(color = order))+
  geom_boxplot(outlier.shape = NA, fill = NA)+
  facet_wrap(~index,scales = "free_y")+
  scale_color_continuous(type = "viridis")+
  theme_bw()

ggsave("Net_ind_boxplot.png",
       width = 8,
       height = 8,
       plot = Net_ind_boxplot)


# violin ####
Net_ind_violin <- 
  ind %>%
  ggplot(aes(x = station, y = value, color = order))+
  geom_jitter(alpha = 0.5, width = 0.2, shape = 1, size = 0.3, aes(color = order))+
  geom_violin(draw_quantiles = c(0.5), fill = NA)+
  facet_wrap(~index,scales = "free_y")+
  theme_bw() +
  scale_color_continuous(type = "viridis")+
  xlab("Station")+
  ylab("Value")

ggsave("Net_ind_violin.png",
       width = 8,
       height = 8,
       plot = Net_ind_violin)
