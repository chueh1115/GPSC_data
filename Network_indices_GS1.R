rm(list = ls())
library(LIM)
library(NetIndices)
# Load your .Rdata file and view the iteration samples.
load("GS1_CR_10000_100.Rdata")
xs$X

# # of iteration= # of matrices #in our case=10000

# Decide which indices you want to test.
#T.. – Total system throughput
#TST – Total system throughflow
#Ltot – Number of links
#C – Connectance
#Tij – Average link weight

# Refer to documentation if necessary.
Indices <- c("T..", "TST", "Ltot", "C", "Tij")

# Create a matrix for the indices to be calculated for each iteration
NetInd <- matrix(NA,nrow=nrow(xs$X),ncol=length(Indices)) ##dimnames
colnames(NetInd) <- Indices
#create loop 

fm  <- Flowmatrix(LIM, xs$X[1,])
#define: Import, Export, Dead
#Some of the indices only concern the organisms
#need to tell the computer which compartments can be used for the calculations

for (i in 1:nrow(xs$X)){
  # Specify which compartments are external to serve for import and export.
  # Specify which compartments comprise of dead material.
  Import <- "POC_W"
  Export <- c("EXP_S","EXP_B","DIC_W") 
  Dead <- "SED"
  # Get flow matrix
  fm  <- Flowmatrix(LIM, xs$X[i,])
  
  # Remove rows/cols that sum to zero,
  # and remove Imports from columns and Exports from rows.
  RemCol <- unique(which(colSums(fm)==0),
                   which(colnames(fm)%in% Import, arr.ind = TRUE))
  RemRow <- unique(which(rowSums(fm)==0),
                   which(rownames(fm)%in% Export, arr.ind = TRUE))
  if(length(RemRow) != 0) {fm <- fm[-RemRow,] }
  if(length(RemCol) != 0) {fm <- fm[,-RemCol] }
  
  # Redefine Import and Export if they were eliminated from fm
  Import <- Import[which(Import%in%rownames(fm))]
  Export <- Export[which(Export%in%colnames(fm))]      
  
  # Calculate indices
  NetInd[i, "T.."] <- GenInd (Flow=fm, Import=Import, Export=Export)$T..
  NetInd[i, "TST"] <- GenInd (Flow=fm, Import=Import, Export=Export)$TST
  NetInd[i, "Ltot"] <- GenInd (Flow=fm, Import=Import, Export=Export)$Ltot
  NetInd[i, "C"] <- GenInd (Flow=fm, Import=Import, Export=Export)$C
  NetInd[i, "Tij"] <- GenInd (Flow=fm, Import=Import, Export=Export)$Tij
}
NetInd
save(NetInd,file="GS1_Indices.Rdata")
# Calculate the mean and standard deviation of all indices
NI_mean <- colMeans(NetInd)
NI_stdev <- sqrt(diag(var(NetInd)))
NI_mean
NI_stdev
