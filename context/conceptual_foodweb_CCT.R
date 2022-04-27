library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(rsvg)

concepural<-grViz("
digraph{
   graph[rankdir=TB]
  
  {rank=1
    node [shape=box style=filled fillcolor=lightsteelblue]
    POC_W [label=TOC]
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
  
  {
  # POM input
  POC_W -> DET [color = tomato fontcolor=steelblue3 ]
  # POM output
  DET -> EXP_S [color = tomato fontcolor=steelblue3 ] 
  
  # Deposit feeding
  DET -> BAC   [color = black  fontcolor=steelblue3 ]
  DET -> MEI   [color = black fontcolor=steelblue3 ]  
  DET -> MAC   [color = black  fontcolor=steelblue3 ]
  
  #Faeces
  BAC -> DET   [color = black fontcolor=steelblue3 ]  
  MEI -> DET   [color = black fontcolor=steelblue3 ]  
  MAC -> DET   [color = black  fontcolor=steelblue3]
  
  # Interaction
  # BAC
  BAC -> MEI   [color = black fontcolor=steelblue3]
  BAC -> MAC   [color = black fontcolor=steelblue3]

  # MEI
  MEI -> MAC   [color = black fontcolor=steelblue3]
  
  # Predation
  MEI -> EXP_B   [color = gray fontcolor=steelblue3]
  MAC -> EXP_B   [color = gray fontcolor=steelblue3]
 
  # Respiration
  BAC -> DIC_W [color = gray fontcolor=steelblue3]
  MEI -> DIC_W [color = gray fontcolor=steelblue3]
  MAC -> DIC_W [color = gray fontcolor=steelblue3]
}}
")
concepural%>%
  export_svg %>% charToRaw %>% rsvg_png("conceptual_graph.png")
