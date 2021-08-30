
# Start -------------------------------------------------------------------

library(readxl)
library(DiagrammeR)



# links ---------------------------------------------------------------
#https://github.com/rich-iannone/DiagrammeR



# Data-------------------------------------------------------------------------
dat <- read_excel("data/Indikatorer.xlsx",
                  sheet = "indikatorer")

dat$to <- paste(dat$paavirkning1, 
                dat$paavirkning2,
                dat$paavirkning3,
                dat$paavirkning4,
                sep = ";")

dat$egenskaper <- paste(dat$egenskap1, 
                dat$egenskap2,
                sep = ";")
dat <- dat[dat$utgår == "Nei",]


paavirkninger <-  read_excel("data/Indikatorer.xlsx",
                    sheet = "paavirkninger")

egenskaper <- read_excel("data/Indikatorer.xlsx",
                         sheet = "egenskaper")

datasett <- read_excel("data/Indikatorer.xlsx",
                       sheet = "datasett")

# Noder -------------------------------------------------------------------

allNodes <- c(dat$node, 
              egenskaper$Egenskaper,
              paavirkninger$paavirkninger)

allNodes <- data.frame(node = allNodes,
                       type = rep(c("A", "B", "C"), 
                                  times=c(nrow(dat),
                                          nrow(egenskaper),
                                          nrow(paavirkninger))))


# Edges1 -------------------------------------------------------------------


paavirkningTilTilstand <- data.frame(from = as.numeric(NULL),
                       to = as.numeric(NULL))

#tempEdge2 <- tempEdge
for(i in unique(dat$node)){
  print(i)
  from <- NULL
  to <- NULL

  from <- dat$node[dat$node==i]
  to <- stringr::str_split(dat$to[dat$node==i], pattern = ";", simplify = T)

  paavirkningTilTilstand <- rbind(paavirkningTilTilstand, data.frame(from, c(to)))
  
}

# remove non-complete cases
edges1 <- paavirkningTilTilstand[paavirkningTilTilstand$c.to!= "NA",]
colnames(edges1)[colnames(edges1) == "c.to."] <- "to"
rm(paavirkningTilTilstand, to, from)


# Edges2 -------------------------------------------------------------------


tilstandTilEgenskap <- data.frame(from = as.numeric(NULL),
                                     to = as.numeric(NULL))

#tempEdge2 <- tempEdge
for(i in unique(dat$node)){
  print(i)
  from <- NULL
  to <- NULL
  
  from <- dat$node[dat$node==i]
  to <- stringr::str_split(dat$egenskaper[dat$node==i], 
                           pattern = ";", simplify = T)
  
  tilstandTilEgenskap <- rbind(tilstandTilEgenskap, 
                               data.frame(from, c(to)))
  
}

# remove non-complete cases
edges2 <- tilstandTilEgenskap[tilstandTilEgenskap$c.to!= "NA",]
colnames(edges2)[colnames(edges2) == "c.to."] <- "to"
rm(tilstandTilEgenskap, to, from)


# Plot --------------------------------------------------------------------


myDAG <- create_graph(
             attr_theme = "tb") %>%   
  add_nodes_from_table(
    table = allNodes,
    type_col = type,
    label_col = node
  ) %>%
  add_edges_from_table(
    table = edges1,
    from_col = from,
    to_col = to,
    from_to_map = label    
  )  %>%
   set_node_attrs(node_attr = shape, values =  "box") %>%
   set_node_attrs(node_attr = width, values =  1.2) %>%
   set_node_attrs(node_attr = peripheries, value = 1.5) %>%
   set_node_attrs(node_attr = penwidth, value = 3) %>%
   set_edge_attrs(edge_attr = dir, values = "back") %>%
  add_edges_from_table(
    table = edges2,
    from_col = from,
    to_col = to,
    from_to_map = label    
  ) %>%
  select_nodes(conditions = type == "A") %>%
  set_node_attrs_ws(node_attr = fillcolor, value = "DarkOliveGreen") %>%
  clear_selection()%>%
  select_nodes(conditions = type == "B") %>%
  set_node_attrs_ws(node_attr = fillcolor, value = "Goldenrod") %>%
  clear_selection() %>%
  select_nodes(conditions = type == "C") %>%
  set_node_attrs_ws(node_attr = fillcolor, value = "grey") %>%
  clear_selection()
  render_graph(myDAG) 

 
             
#get_node_df(myDAG)
#get_edge_df(myDAG)



#get_node_df(myDAG)
