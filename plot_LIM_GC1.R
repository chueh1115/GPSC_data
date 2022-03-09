rm(list = ls())
library(LIM)

#GC####
# Load your .Rdata file and view the iteration samples.
load("GC1_MR+BAC_10000_100.Rdata")
flow<-data.frame(xs$X)
flow_mean<-colMeans(flow)
mat<-data.frame(flow=LIM$Unknowns,
                value=flow_mean)
compartment<-c("POC_W", "SED", "BAC", "MEI", "MAC",
               "TSTC","FCI","APL",
               "AMI")