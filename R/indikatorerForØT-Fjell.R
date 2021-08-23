
# Start -------------------------------------------------------------------

library(readxl)
library(DiagrammeR)



# Vignettes ---------------------------------------------------------------
# http://rich-iannone.github.io/DiagrammeR/index.html

# Data-------------------------------------------------------------------------
dat <- read_excel("data/Indikatorer-linkesTilVisio.xlsx")



# Noder -------------------------------------------------------------------

allNodes <- dat[!is.na(dat$node),]
nodes_tilstand <- allNodes[allNodes$Type == "Tilstandsindikator",]
nodes_fenomen <- allNodes[allNodes$Type == "Fenomen",]
nodes_påvirkning <- allNodes[allNodes$Type == "Påvirkning",]
nodes_påvirkningindikator <- allNodes[allNodes$Type == "Påvirkningsindikator",]


nodes_tilstand <- create_node_df(n = nrow(nodes_tilstand),
                          label = nodes_tilstand$node,
                          type = "a",                            # vet ikke hva denne gjør
                          style = "filled",
                          color = "aqua",
                          shape = "rectangle",
                          tooltip = "This is a tooltip")         # Jeg tror denne ikke parses i figuren

nodes_fenomen <- create_node_df(n = nrow(nodes_fenomen),
                                 label = nodes_fenomen$node,
                                 type = "b",                            # vet ikke hva denne gjør
                                 style = "filled",
                                 color = "green",
                                 shape = "circle",
                                 tooltip = "This is a tooltip as well")        
nodes_påvirkning <- create_node_df(n = nrow(nodes_påvirkning),
                                label = nodes_påvirkning$node,
                                type = "c",                            # vet ikke hva denne gjør
                                style = "filled",
                                color = "blue",
                                shape = "circle",
                                tooltip = "This is a tooltip as well") 
nodes_påvirkningindikator <- create_node_df(n = nrow(nodes_påvirkningindikator),
                                 label = nodes_påvirkningindikator$node,
                                 type = "a",                            # vet ikke hva denne gjør
                                 style = "filled",
                                 color = "aqua",
                                 shape = "rectangle",
                                 tooltip = "This is a tooltip")

allNodes2 <- combine_ndfs(nodes_tilstand, nodes_fenomen, nodes_påvirkning, nodes_påvirkningindikator)
# Edges -------------------------------------------------------------------


tempEdge <- data.frame(from = as.numeric(NULL),
                       to = as.numeric(NULL))

#tempEdge2 <- tempEdge
for(i in unique(dat$ID)){
  print(i)
  from <- NULL
  to <- NULL

  from <- dat$ID[dat$ID==i]
  to <- stringr::str_split(dat$to[dat$ID==i], pattern = ";", simplify = T)

  tempEdge <- rbind(tempEdge, data.frame(from, c(to)))
  
}
tempEdge2 <- tempEdge[!is.na(tempEdge$c.to.),]
edges <-
  create_edge_df(
    from = tempEdge2$from,
    to = tempEdge2$c.to.,
    rel = "requires",                #?
    color = "green")

myPlot <- create_graph(allNodes2, edges)
render_graph(myPlot)
