rm(list=ls())
#-- Load the LIM (and limSolve) package 
library(LIM)
library(splus2R)
#MODEL SETUP####
#-- Define directory that contains the input file
DataDir <- "C:/Users/user/Downloads/labWei/Tung_thesis/GPSC_data/"
#GC1####
#-- Read the ascii files
File_GC<- paste(DataDir,"GC1_LIM_MR+BAC.input",sep="")  
LIM_GC<- Setup(file=File_GC) 
#Parsimonious####
# Find the solution range of each flow
flowSol_GC <- Xranges(LIM_GC)

# Find the parsimonious solution of each flow
pars_GC <- Lsei(LIM_GC, parsimonious = TRUE)
# Print your ranges and parsimonious solution on screen
SSA_GC<-data.frame(flowSol_GC, parsimonious=pars_GC$X)
?Xranges
#Likelihood####
#-- SSA is not based on ecological theory
xranges_GC<-Xranges(LIM_GC)
x0_GC <- lsei(E=LIM_GC$A,
           F=LIM_GC$B,
           A=diag(LIM_GC$NUnknowns),
           B=rowMeans(xranges_GC),
           G=LIM_GC$G,
           H=LIM_GC$H)$X
iter<-10000
jumpsize<-100
xs_GC <- xsample(E = LIM_GC$A,
                 F = LIM_GC$B,
                 G = LIM_GC$G,
                 H = LIM_GC$H,
                jmp= (xranges_GC[,2] - xranges_GC[,1])/jumpsize,
                 x0= x0_GC,
                 iter = iter)
LA_GC<-data.frame(flow=LIM_GC$Unknowns, 
               mean=colMeans(xs_GC$X),
               sd=sqrt(diag(var(xs_GC$X))))

#GS1####
#-- Read the ascii files
File_GS<- paste(DataDir,"GS1_LIM_MR+BAC.input",sep="")  
LIM_GS<- Setup(file=File_GS) 
#Parsimonious####
# Find the solution range of each flow
flowSol_GS <- Xranges(LIM_GS)
# Find the parsimonious solution of each flow
pars_GS <- Lsei(LIM_GS, parsimonious = TRUE)
# Print your ranges and parsimonious solution on screen
SSA_GS<-data.frame(flowSol_GS, parsimonious=pars_GS$X)

#Likelihood####
#-- SSA is not based on ecological theory
xranges_GS<-Xranges(LIM_GS)
x0_GS <- lsei(E=LIM_GS$A,
              F=LIM_GS$B,
              A=diag(LIM_GS$NUnknowns),
              B=rowMeans(xranges_GS),
              G=LIM_GS$G,
              H=LIM_GS$H)$X

xs_GS <- xsample(E = LIM_GS$A,
                 F = LIM_GS$B,
                 G = LIM_GS$G,
                 H = LIM_GS$H,
                 jmp= (xranges_GS[,2] - xranges_GS[,1])/jumpsize,
                 x0= x0_GS,
                 iter = iter)
LA_GS<-data.frame(flow=LIM_GS$Unknowns, 
                  mean=colMeans(xs_GS$X),
                  sd=sqrt(diag(var(xs_GS$X))))

library(ggplot2)
library(dplyr)
library(ggbreak)
LA_GC$Station<-"GC"
LA_GS$Station<-"GS"
LA<-rbind(LA_GC,LA_GS)
LA %>% filter(flow!="POC_W->SED") %>% 
ggplot(aes(x = flow, y = mean,
           fill=Station,
           ymin=mean-sd,ymax=mean+sd))+
  geom_bar(stat = "identity",position = "dodge",
           aes(fill=Station),width = 0.5)+
  ylab(expression(OC~(mg~C~m^-2~d^-1)))+
  geom_errorbar(stat = "identity", position = "dodge", width = 0.5) + 
  ylim(0, NA)+
  labs(title = "Carbon flow")+
  scale_fill_brewer(palette="Paired") + 
  theme_minimal()+
  coord_flip()+
  scale_y_break(c(20,35),scales = "free")
