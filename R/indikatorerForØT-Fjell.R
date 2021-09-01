
# Start -------------------------------------------------------------------

library(readxl)
library(DiagrammeR)



# links ---------------------------------------------------------------
#https://github.com/rich-iannone/DiagrammeR



# Data-------------------------------------------------------------------------
dat <- read_excel("data/Indikatorer.xlsx",
                  sheet = "indikatorer")
getwd()

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

#egenskaper[3,] <- "Funksjonell sammensetning\ninnen trofiske nivåer"

datasett <- read_excel("data/Indikatorer.xlsx",
                       sheet = "datasett")

# Noder -------------------------------------------------------------------

allNodes <- c(dat$node, 
              egenskaper$Egenskaper,
              paavirkninger$paavirkninger)

myTimes <- c(nrow(dat),
           nrow(egenskaper),
           nrow(paavirkninger))

allNodes <- data.frame(node = allNodes,
                       type = rep(c("A", "B", "C"), 
                                  times=myTimes))

# ids for subsetting in the DAG
ids <- row.names(allNodes)[allNodes$type == "B"]
ids2 <- row.names(allNodes)[allNodes$type != "B"]


# field for tooltip


allNodes
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

edges1$rel <- "A"
edges2$rel <- "B"

edges <- rbind(edges1, edges2)


# Plot --------------------------------------------------------------------



myDAG <- create_graph( 
             attr_theme = "lr"
             ) %>%   
 #add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
 #                       value = c("dot", "LR", "false"),              # 
 #                       attr_type = c("graph", "graph", "graph", "edge"))%>%
   # all nodes
  add_nodes_from_table(
    table = allNodes,
    type_col = type,
    label_col = node
  ) %>%
  
   # global styling of nodes
  set_node_attrs(node_attr = shape, values =  "box") %>%
  set_node_attrs(node_attr = width, values =  3.1, nodes = ids) %>%  # width of egenskaper
  set_node_attrs(node_attr = width, values =  1.5, nodes = ids2) %>%  # width of the rest
  set_node_attrs(node_attr = peripheries, value = 1.5) %>%
  set_node_attrs(node_attr = penwidth, value = 3) %>%
  #set_node_position(1, x=1, y=1)%>%

  # node width
  #select_nodes(conditions = type == "B") %>%
  #set_node_attrs_ws(node_attr = width, values =  1) %>%
  #clear_selection()%>%
  
  # edges
  add_edges_from_table(
    table = edges,
    rel_col = rel,
    from_col = from,
    to_col = to,
    from_to_map = label    
  )  %>%
  
  select_edges(conditions = rel == "A") %>%
  rev_edge_dir_ws() %>%
  clear_selection() %>%
  
  
  # edge colour
  set_edge_attrs(edge_attr = color, values = "black") %>%
  
 
  
  # colouring the nodes
  select_nodes(conditions = type == "A") %>%
  set_node_attrs_ws(node_attr = fillcolor, value = "DarkOliveGreen") %>%
  clear_selection()%>%
  
  select_nodes(conditions = type == "B") %>%
  set_node_attrs_ws(node_attr = fillcolor, value = "Goldenrod") %>%
  clear_selection() %>%
  
  select_nodes(conditions = type == "C") %>%
  set_node_attrs_ws(node_attr = fillcolor, value = "grey40") %>%
  clear_selection() %>%
  
  # break names
  select_nodes(conditions = label == "Areal uten fremmede plantearter") %>%
  set_node_attrs_ws(node_attr = label, value = "Areal uten\nfremmede plantearter") %>%
  clear_selection() %>%
  
  select_nodes(conditions = label == "Areal uten tekniske inngrep") %>%
  set_node_attrs_ws(node_attr = label, value = "Areal uten\ntekniske inngrep") %>%
  clear_selection()

  
  
 # select_nodes(conditions = label == "Biomasse mellom trofiske nivåer") %>%
 # set_node_attrs_ws(node_attr = label, value = "Biomasse mellom\ntrofiske nivåer") %>%
 # clear_selection() %>%
   
 # select_nodes(conditions = label == "Funksjonell sammensetning innen trofiske nivåer") %>%
 # set_node_attrs_ws(node_attr = label, value = "Funksjonell\nsammensetning innen\ntrofiske nivåer") %>%
 # clear_selection() %>%
  
 # select_nodes(conditions = label == "Funksjonelt viktige arter og strukturer") %>%
 # set_node_attrs_ws(node_attr = label, value = "Funksjonelt viktige\narter og strukturer") %>%
 # clear_selection() %>%
 # 
 # select_nodes(conditions = label == "Landskapsøkologiske mønstre") %>%
 # set_node_attrs_ws(node_attr = label, value = "Landskapsøkologiske\nmønstre")%>%
 # clear_selection()
 


render_graph(myDAG) 

myDAG$nodes_df
allNodes$node

#subgraph_left <-
#  myDAG %>%
#  select_nodes(conditions = type ==  "C") %>%
#  transform_to_subgraph_ws()
#subgraph_right <-
#  myDAG %>%
#  select_nodes(conditions = type ==  "B") %>%
#  transform_to_subgraph_ws()
#subgraph_middle <-
#  myDAG %>%
#  select_nodes(conditions = type ==  "A") %>%
#  transform_to_subgraph_ws()



#myDAG$nodes_df$rank <-  unlist(lapply(myDAG$nodes_df$type,
#                                      function(type) switch(type,
#                                                            "A" = "1",
#                                                            "B" = "5",
#                                                            "C" = "9"
#                                      )))
#myDAG$global_attrs[myDAG$global_attrs == "layout", 2] <- "dot"
render_graph(subgraph_left)
render_graph(subgraph_right)
render_graph(subgraph_middle)

subgraphs <-  create_graph_series(subgraph_left,
                                  series_name = "subgraphs") %>%
  add_graph_to_graph_series(subgraph_middle) %>%
  add_graph_to_graph_series(subgraph_right)
             
render_graph_from_graph_series(subgraphs)

comb <- combine_graphs(subgraph_left, subgraph_middle)
render_graph(comb)

nodedf <- get_node_df(myDAG)
anyDuplicated(nodedf$label)
nodedf$label

get_edge_df(myDAG)

get_global_graph_attr_info(myDAG)

#get_node_df(myDAG)
