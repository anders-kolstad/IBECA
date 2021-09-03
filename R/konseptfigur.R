library(DiagrammeR)



grViz("digraph{

     graph [compound = true, nodesep = .5, ranksep = .5,
         color = crimson, rankdir = TB]

  node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, fixedsize = true, width = 2,
        color = darkslategray]
        
  øko[label = 'Økosystem-\ntjenester']
  
  edge [color = black]
  
  subgraph cluster0 {
       label = Balanseregnskap;
       node [fixedsize = true, width = 2];
      'Tilstand'
      'Areal'
      
      edge[dir = both]
      Tilstand -> Areal
  }
       
       Tilstand -> øko [ltail = cluster0, headport = n, tailport = s, x= 1]
}")









     
