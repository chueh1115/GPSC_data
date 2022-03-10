
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
  POC_W -> DET [color = tomato]
  # POM output
  DET -> EXP_S [color = tomato]
  
  # Deposit feeding
  DET -> BAC   [color = black]
  DET -> MEI   [color = black]
  DET -> MAC   [color = black]
  
  #Faeces
  BAC -> DET   [color = black]
  MEI -> DET   [color = black]
  MAC -> DET   [color = black]
  
  # Interaction
  # BAC
  BAC -> MEI   [color = black]
  BAC -> MAC   [color = black]

  # MEI
  MEI -> MAC   [color = black]
  
  # Predation
  MEI -> EXP_B   [color = gray]
  MAC -> EXP_B   [color = gray]
 
  # Respiration
  BAC -> DIC_W [color = gray]
  MEI -> DIC_W [color = gray]
  MAC -> DIC_W [color = gray]
  
}")