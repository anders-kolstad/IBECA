


grViz( 
 "digraph{

     graph [compound = true, nodesep = .5, ranksep = .5,
         color = crimson, rankdir = TB]

  node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, fixedsize = true, width = 2,
        color = darkslategray, style =filled, fillcolor = white]
        

  A[label = 'Påvirkningsindikatorer', shape = tab, fillcolor = cornsilk]

  
  B[label = 'Påvirkning A', shape = triangle, height = 1.8, fillcolor = orange]
  B2[label = 'Påvirkning B', shape = triangle, height = 1.8, fillcolor = orange]
  B3[label = 'Påvirkning C', shape = triangle, height = 1.8, fillcolor = orange]
  
  C[label = 'Tilstand A']
  C2[label = 'Tilstand B']
  
 
    D[label = 'Tilstandsindikator', shape = tab, fillcolor = cornsilk];
    D2[label = 'Tilstandsindikator', shape = tab, fillcolor = cornsilk]
  
  E[label = 'Egenskap A']
  E2[label = 'Egenskap B']
  E3[label = 'Egenskap C']

  F[label = 'Tilstanden til økosystemet', width = 5, height = 1, fillcolor = Peru, fontsize = 25, shape = box3d]
  
  
  edge [color = black]
  
  A -> B    [dir = back]
  A -> B2   [dir = back]
  A -> B3   [dir = back]
  
  B -> C
  B2 -> C
  B3 -> C2
  
  C-> D[dir=back]
  C2-> D2[dir=back]
  
  D -> E -> F
  D -> E2 -> F
  D2 -> E3 -> F

}")


