rm(list = ls())
library(LIM)
library(NetIndices)
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

# long data
ind <- c("T..", "TST","TSTC","FCI","AMI")
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
#GC_pca <- GC %>% mutate(station = "GC1")
#GS_pca <- GS %>% mutate(station = "GS1")

# scale 
#pca_data <- 
#  full_join(GC_pca, GS_pca) %>% 
#  mutate(across(all_of(colnames(GC_pca)[-8]), scale))

# run pca
#pca <- vegan::rda(pca_data[-8])

# extract pca information
#sites <- data.frame(pca$CA$u)
#sites$station <- pca_data$station
#species <- data.frame(pca$CA$v)

#ggplot()+
#  geom_point(data = sites,
#             aes(x = PC1, y = PC2, color = station),
#             alpha = 0.1,
#             shape = 1)

# pairplot ####
#GC pairplot
#jpeg("GC_pairs.jpg", width = 800, height = 600)
#pairs(GC, alpha = 0.1)
#dev.off()

# GS pairplot
#jpeg("GS_pairs.jpg", width = 800, height = 600)
#pairs(GS, alpha = 0.1, color = GS$order)
#dev.off()


# boxplot ####
Net_ind_boxplot <- 
  ind %>%
  ggplot(aes(x = station, y = value))+
  geom_boxplot(outlier.shape = NA, fill = NA)+
  facet_wrap(~index,scales = "free_y")+
  theme_bw()
#  geom_jitter(alpha = 0.1, width = 0.2, shape = 1, aes(color = order))+
#  scale_color_continuous(type = "viridis")+

ggsave("Net_ind_boxplot.png",
       width = 8,
       height = 8,
       plot = Net_ind_boxplot)


# violin ####
Net_ind_violin <- 
  ind %>%
  ggplot(aes(x = station, y = value))+
  geom_violin(draw_quantiles = c(0.5), fill = NA)+
  facet_wrap(~index,scales = "free_y")+
  theme_bw()+
  xlab("Station")+
  ylab("Value")

ggsave("Net_ind_violin.png",
       width = 8,
       height = 8,
       plot = Net_ind_violin)
