rm(list = ls())
library(NetIndices)
#Description example food web
#food web:compartments and flows between these compartments. 
#a simple food web with 3 internal compartments (Plants, Animals and Detritus) 
                       #and 1 external compartment (CO2)
#CO2 is used by Plants for photosynthesis. 
#The Animals feed on Plants and Detritus. 
#The Plants and Animals also die, thereby turning into Detritus. 
#The Animals respire, an output flow to CO2. 
#The Animals also excrete faeces,a flow to Detritus.
#The Detritus mineralizes,  an output flow to CO2.

#Flow matrix
#The description of flows above->  a flow matrix. 
#The rows (i) are the source compartments,
#and the columns (j) are the destination compartments. 
#For now we assume all flows have a size of 1. The matrix we need looks like this:

#            #Plant         #Animal     #Detritus     #CO2
#Plant          0             1            1            0
#Animal         0             0            1            1
#Detritus       0             1            0            1         
#CO2            1             0            0            0

#create matrix
flowvalue<-c(0,1,1,0,
             0,0,1,1,
             0,1,0,1,
             1,0,0,0)
compartments <- c("Plants",
                  "Animals",
                  "Detritus",
                  "CO2")#order!
fm <- matrix(flowvalue,
             nrow = 4,ncol = 4,byrow = TRUE)
dimnames(fm) <- list(compartments, compartments)

#Specify external compartments
#The external compartments: are often big and vague compartments that are not quantified. 
                            #don’t want to use in calculations.

#specify “Import”, “Export” and “Dissipation”. 
#Export and Dissipation are both output flows to an external compartment,
#Export: the export of usable medium
#Dissipation:the export of unusable medium 
#(in food web this would be organic and inorganic material respectively). 
#However, this distinction does not have to be made 
          #and all export can be pooled under Export. 
#For our example food web we specify these compartments as follows, 
#not making a distinction between Export and Dissipation:
# Specify which compartments are external to serve for import and export.
# Specify which compartments comprise of dead material.
Import <- "CO2"
Export <- "CO2"
Dead <- "Detritus"
#Calculating indices

# You can report all indices belonging to a function.
# Example with General Indices (GenIND):
GenInd(Flow=fm, Import=Import, Export=Export)

# It's also possible to only report a specific index.
# In this case only report Link Density (LD)
GenInd(Flow=fm, Import=Import, Export=Export)$LD