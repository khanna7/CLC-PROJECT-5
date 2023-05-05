# Visualize networks


## Clear the workspace ----------

rm(list=ls())




## Import packages ----------

library(dplyr)
library(data.table)
library(igraph)
library(ggplot2)
library(ggraph)


## Data location ----------
data_loc <- "/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT"



### extract egos (i.e. participants) consenting to give SNS data, 
### and their attributes

network_env <- readRDS(paste0(data_loc, "/network_objects.rds"))
sns_consenting_dt <- as.data.table(network_env$sns_consenting_dt)

### extract attributes of adult alters (i.e., network members)
merged_network_participant_env <- readRDS(paste0(data_loc, "/merged_network_participant_objects.rds"))
sns_dt_long_merged_ego_characteristics <- merged_network_participant_env$sns_dt_long_merged_ego_characteristics


## Create a network object ----------

edge_list <- as.matrix(sns_dt_long_merged_ego_characteristics[, c("MTURKID", "alterID")])
g <- igraph::graph_from_edgelist(edge_list)


### investigate the structure of g
igraph::vertex_attr(g)
head(V(g)$name, 20)


### count the number of clusters in g
clusters <- clusters(g)
num_clusters <- length(clusters$membership)


## Visualize network objects (vaccine outcomes are of interest) ----------

## preliminary visualization

ggraph(g, layout = 'kk') +
  geom_edge_link() +
  geom_node_point() +
  theme_void()


## color vertices by participant vs network member  labels:

### define vertex attribute
V(g)$label <- ifelse(grepl("_\\d+$", V(g)$name), "Network Member", "Participant")

### create a color palette for the labels
color_palette <- c("Participant" = "blue", "Network Member" = "red")

### plot the graph with labeled vertices

p <- 
  ggraph(g, layout = "kk") + 
  geom_edge_link() + 
  geom_node_point(aes(color = label)) +
  scale_color_manual(values = color_palette) +
  #geom_node_text(aes(label = ifelse(grepl("_\\d+$", name), "Network Member", "Participant"))) +
  theme_void()

p

ggsave("network_plot.png", width = 8, height = 8, dpi = 300)

## Visualize a cluster instead of the whole network ----------

### extract the clusters from the graph object
clusters <- clusters(g)


### extract the vertices belonging to the cluster
sub_vertices <- V(g)[clusters$membership == 2] #specifies which cluster to extract
  
### extract the edges belonging to the cluster
sub_edges <- E(g)[.from(sub_vertices) %in% sub_vertices & .to(sub_vertices) %in% sub_vertices]

### create the subgraph
sub_g <- induced_subgraph(g, sub_vertices)

### set the label attribute for the subgraph vertices
V(sub_g)$label <- ifelse(grepl("_\\d+$", V(sub_g)$name), "Network Member", "Participant")

### create the plot
ggraph(sub_g, layout = "kk") +
  geom_edge_link() +
  geom_node_point(aes(color = label)) +
  scale_color_manual(values = c("Participant" = "blue", "Network Member" = "red")) +
  labs(title = paste0("Cluster "))




## Visualize clustering on one "trait" of interest ----------
### cdc_avg_out_adequate in the egos vs 
setnames(sns_consenting_dt, "MTURK1", "MTURKID")
cols_of_interest <- "cdc_avg_out_adequate"
selected_rows <- sns_consenting_dt[MTURKID %in% V(g)$name, ..cols_of_interest]
attr_values <- rep(NA, vcount(g))
attr_values[match(selected_rows$MTURKID, V(g)$name)] <- selected_rows[[cols_of_interest]]

g <- set_vertex_attr(g, name = cols_of_interest, value = attr_values)
g <- set_vertex_attr(g, name = "cdc_avg_out_adequate", value = NA)

matching_nodes <- V(g)[name %in% sns_consenting_dt$MTURKID]
g <- set_vertex_attr(g, name = "cdc_avg_out_adequate", 
                     value = ifelse(V(g)$name %in% matching_nodes$name, 
                                    sns_consenting_dt[MTURKID %in% matching_nodes$name, "cdc_avg_out_adequate"],
                                    NA))



