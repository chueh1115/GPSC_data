rm(list = ls())
library(LIM)
library(tidyr)
#GC####
# Load your .Rdata file and view the iteration samples.
load("GC1_MR+BAC_10000_100.Rdata")
mat<-data.frame(flow=LIM$Unknowns,
                value=flow_mean)
compartment<-c("POC_W","SED",
               "BAC", "MEI", "MAC",
               "DIC_W","EXP_S","EXP_B")
mat_GC <- matrix(0,nrow=length(compartment),ncol=length(compartment))
colnames(mat_GC) <- compartment
rownames(mat_GC) <- compartment

flow<-data.frame(xs$X)
flow_mean<-as.numeric(colMeans(flow))
flow_name<-as.character(LIM$Unknowns)
flowdf<-as.data.frame(cbind(flow_name,flow_mean))
flowdf$flow_mean<-as.numeric(flowdf$flow_mean)
flowdf$j<-gsub(".*->","",flowdf$flow_name)
flowdf$i<-gsub("->.*","",flowdf$flow_name)
#####
mat_GC["POC_W","SED"]<-flowdf$flow_mean[flowdf$i=="POC_W"]
mat_GC["SED","BAC"]<-flowdf$flow_mean[flowdf$i=="SED"&
                                        flowdf$j=="BAC"]
mat_GC["SED","MEI"]<-flowdf$flow_mean[flowdf$i=="SED"&
                                        flowdf$j=="MEI"]
mat_GC["SED","MAC"]<-flowdf$flow_mean[flowdf$i=="SED"&
                                        flowdf$j=="MAC"]
mat_GC["SED","EXP_S"]<-flowdf$flow_mean[flowdf$i=="SED"&
                                        flowdf$j=="EXP_S"]
mat_GC["BAC","SED"]<-flowdf$flow_mean[flowdf$i=="BAC"&
                                        flowdf$j=="SED"]
mat_GC["BAC","MEI"]<-flowdf$flow_mean[flowdf$i=="BAC"&
                                        flowdf$j=="MEI"]
mat_GC["BAC","MAC"]<-flowdf$flow_mean[flowdf$i=="BAC"&
                                        flowdf$j=="MAC"]
mat_GC["BAC","DIC_W"]<-flowdf$flow_mean[flowdf$i=="BAC"&
                                        flowdf$j=="DIC_W"]
mat_GC["MEI","SED"]<-flowdf$flow_mean[flowdf$i=="MEI"&
                                        flowdf$j=="SED"]
mat_GC["MEI","MAC"]<-flowdf$flow_mean[flowdf$i=="MEI"&
                                        flowdf$j=="MAC"]
mat_GC["MEI","DIC_W"]<-flowdf$flow_mean[flowdf$i=="MEI"&
                                        flowdf$j=="DIC_W"]
mat_GC["MEI","EXP_B"]<-flowdf$flow_mean[flowdf$i=="MEI"&
                                        flowdf$j=="EXP_B"]
mat_GC["MAC","DIC_W"]<-flowdf$flow_mean[flowdf$i=="MAC"&
                                          flowdf$j=="DIC_W"]
mat_GC["MAC","SED"]<-flowdf$flow_mean[flowdf$i=="MAC"&
                                          flowdf$j=="SED"]
mat_GC["MAC","EXP_B"]<-flowdf$flow_mean[flowdf$i=="MAC"&
                                          flowdf$j=="EXP_B"]
#####
plotmat(mat_GC,
        name = colnames(mat_GC), lwd = 1, box.lwd = 2,
        cex.txt = 0.8, box.cex = 0.8, box.size = 0.08,
        arr.length = 0.5, box.type = "circle", box.prop = 1,
        shadow.size = 0.01, self.cex = 0.6, my = -0.075, mx = -0.01,
        relsize = 0.9, self.shiftx = c(0, 0, 0.125, -0.12, 0.125, 0),
        self.shifty = 0, main = "GC1")
library(diagram)
example<-as.data.frame(Teasel)
typeof(Teasel)
curves <- matrix(nrow = ncol(Teasel), ncol = ncol(Teasel), 0)
curves[3, 1] <- curves[1, 6] <- -0.35
curves[4, 6] <- curves[6, 4] <- curves[5, 6] <- curves[6, 5] <- 0.08
curves[3, 6] <- 0.35
plotmat(Teasel, pos = c(3, 2, 1), curve = curves,
          name = colnames(Teasel), lwd = 1, box.lwd = 2,
          cex.txt = 0.8, box.cex = 0.8, box.size = 0.08,
          arr.length = 0.5, box.type = "circle", box.prop = 1,
          shadow.size = 0.01, self.cex = 0.6, my = -0.075, mx = -0.01,
         relsize = 0.9, self.shiftx = c(0, 0, 0.125, -0.12, 0.125, 0),
          self.shifty = 0, main = "Teasel population model")

library(DiagrammeR)
grViz("
digraph dot {
graph[layout=dot,
  rankdir=TB]
  
  {
    rank=1
    node [shape=box style=filled fillcolor=lightsteelblue]
    POC_W [label=POC]
    EXP_S [label=Burial]
    DIC_W [label=DIC]
    EXP_B [label=Predation]
  }
  
  {
    rank=2
    node [style=filled fillcolor=Coral]
    DET [label=Sediment]
    BAC [label=Bacteria]
    MEI [label=Meiofauna]
    MAC [label=Macrofauna]
    
  }
  
  # POM input
  POC_W -> DET [color = tomato][label=53.85]
  # POM output
  DET -> EXP_S [color = tomato][label=42.44]
  
  # Deposit feeding
  DET -> BAC   [color = black][label=24.34]
  DET -> MEI   [color = black][label=0.15]
  DET -> MAC   [color = black][label=0.01]
  
  #Faeces
  BAC -> DET   [color = black][label=12.94]
  MEI -> DET   [color = black][label=0.13]
  MAC -> DET   [color = black][label=0.01]
  
  # Interaction
  # BAC
  BAC -> MEI   [color = black][label=0.15]
  BAC -> MAC   [color = black][label=0.01]

  # MEI
  MEI -> MAC   [color = black][label=0.01]
  
  # Predation
  MEI -> EXP_B   [color = gray][label=0.05]
  MAC -> EXP_B   [color = gray][label=0.01]
 
  # Respiration
  BAC -> DIC_W [color = gray][label=11.24]
  MEI -> DIC_W [color = gray][label=0.10]
  MAC -> DIC_W [color = gray][label=0.01]
  
}")
