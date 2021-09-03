


grViz( 
  "digraph{

     graph [compound = true, nodesep = .5, ranksep = .5,
         color = crimson, rankdir = TB]

  node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, fixedsize = true, width = 4,
        color = darkslategray, style =filled, fillcolor = white]
        

  A[label = 'Jaktstatistikk', shape = tab, fillcolor = cornsilk, width = 2]
  A2[label = 'Årlig avvirket volum', shape = tab, fillcolor = cornsilk, width = 2]

  
  B[label = 'Beskatning', shape = triangle, height = 1.8, width = 2 fillcolor = orange]
  B2[label = 'Arealbruk/-inngrep', shape = triangle, height = 1.8, width = 2, fillcolor = orange]
 
  
  C[label = 'Antall rovdyr']
  C2[label = 'Mengden ROS']
  
 
  D[label = 'Bestandsestimater fra Rovdata', shape = tab, fillcolor = cornsilk]
  D2[label = 'ROS-arter i Landskog', shape = tab, fillcolor = cornsilk];
  
  
  E[label = 'Fordeling av biomasse mellom trofiske nivåer']
  E2[label = 'Funksjonelt viktige arter og strukturer']

  F[label = 'Tilstanden til økosystemet', width = 5, height = 1, fillcolor = Peru, fontsize = 25, shape = box3d]
  
  
  edge [color = black]
  
  A -> B    [dir = back]
  A2 -> B2   [dir = back]
  
  
  B -> C
  B2 -> C2
  
  
  C-> D[dir=back]
  C2-> D2[dir=back]
  
  D -> E -> F
  D2 -> E2 -> F
  

}")


