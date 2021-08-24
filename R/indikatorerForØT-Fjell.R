
# Start -------------------------------------------------------------------

library(readxl)
library(DiagrammeR)



# Vignettes ---------------------------------------------------------------

# This web page is not updated:
# http://rich-iannone.github.io/DiagrammeR/index.html

# Use the manual in the git hub readme file
#https://github.com/rich-iannone/DiagrammeR



# Data-------------------------------------------------------------------------
dat <- read_excel("data/Indikatorer-linkesTilVisio.xlsx")

dat$to <- paste(dat$toNavn1, dat$toNavn2, dat$toNavn3, dat$toNavn4, sep = ";")

# dat <- dat[dat$Utgår != "Ja",] Failed for some reason...
# Noder -------------------------------------------------------------------

allNodes <- dat[!is.na(dat$node),]

#nodes_tilstand <- allNodes[allNodes$Type == "Tilstandsindikator",]
#nodes_fenomen <- allNodes[allNodes$Type == "Fenomen",]
#nodes_påvirkning <- allNodes[allNodes$Type == "Påvirkning",]
#nodes_påvirkningindikator <- allNodes[allNodes$Type == "Påvirkningsindikator",]
#
#
#nodes_tilstand <- create_node_df(n = nrow(nodes_tilstand),
#                          label = nodes_tilstand$node,
#                         #type = "a",
#                         #node_aes = node_aes(
#                         #  color = "steelblue",
#                         #  fillcolor = "lightblue",
#                         #  fontcolor = "grey30"
#                         #),
#                          tooltip = "This is a tooltip")         # Jeg tror denne ikke parses i figuren
#
#nodes_fenomen <- create_node_df(n = nrow(nodes_fenomen),
#                                 label = nodes_fenomen$node,
#                                 type = "b",
#                                 tooltip = "This is a tooltip as well")        
#
#nodes_påvirkning <- create_node_df(n = nrow(nodes_påvirkning),
#                                label = nodes_påvirkning$node,
#                                type = "c",
#                                tooltip = "This is a tooltip as well") 
#
#nodes_påvirkningindikator <- create_node_df(n = nrow(nodes_påvirkningindikator),
#                                 label = nodes_påvirkningindikator$node,
#                                 type= "d",
#                                 tooltip = "This is a tooltip")
#
#allNodes <- combine_ndfs(nodes_tilstand, nodes_fenomen, nodes_påvirkning, nodes_påvirkningindikator)



# Edges -------------------------------------------------------------------


tempEdge <- data.frame(from = as.numeric(NULL),
                       to = as.numeric(NULL))

#tempEdge2 <- tempEdge
for(i in unique(allNodes$node)){
  print(i)
  from <- NULL
  to <- NULL

  from <- allNodes$node[allNodes$node==i]
  to <- stringr::str_split(allNodes$to[allNodes$node==i], pattern = ";", simplify = T)

  tempEdge <- rbind(tempEdge, data.frame(from, c(to)))
  
}

# remove non-complete cases
edges <- tempEdge[tempEdge$c.to!= "NA",]

#edges <-
#  create_edge_df(
#    from = tempEdge$from,
#    to = tempEdge$c.to.)




# Plot --------------------------------------------------------------------


myDAG <- create_graph(
             attr_theme = "lr") %>%        # left-right
  add_nodes_from_table(
    table = allNodes,
    label_col = node,
    type_col = Type
  ) %>%
  add_edges_from_table(
    table = edges,
    from_col = from,
    to_col = c.to.,
    from_to_map = label    # capital letters are my own ID's from excel
  )  %>%
  select_nodes(conditions = type == "Tilstandsindikator") %>%
  set_node_attrs_ws(node_attr = fillcolor, value = "orange") %>%
  clear_selection()
             
#get_node_df(myDAG)
#get_edge_df(myDAG)

render_graph(myDAG) 

#get_node_df(myDAG)
