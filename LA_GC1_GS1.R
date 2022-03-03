rm(list=ls())
#-- Load the LIM (and limSolve) package 
library(LIM)
library(splus2R)
#MODEL SETUP####
#-- Define directory that contains the input file
DataDir <- "C:/Users/user/Downloads/chueh/GPSC_data/"
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
LA_GC$Station<-"GC"
LA_GS$Station<-"GS"
LA<-rbind(LA_GC,LA_GS)

ggplot(data= ,aes(x = flow, y = mean))+
  geom_bar(stat = "identity",position = position_dodge(),
           aes(fill=Station))+
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), size=0.5,width=0.1,alpha=0.9, colour="black")+
  ylab(expression(OC~(mg~C~m^-2)))+
  ylim(0, NA)+
  facet_wrap(~Station,scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(title = "Meiofauna")+
  geom_point(data=MEI_point,aes(x=Cruise,y=total_biomass/area), color = "darkblue",size=0.5)
