# Visualize networks


## Clear the workspace ----------
rm(list=ls())


## Set working directory ----------

setwd("/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")


## Import packages ----------

library(dplyr)
library(data.table)
library(igraph)
library(ggplot2)
library(ggraph)


## Load data ----------

network_env <- readRDS("network_objects.rds")
sns_dt_long_wide_no_minors <- network_env$sns_dt_long_wide_no_minors

eda_env <- readRDS("eda_objects.rds")
tab2_dt <- as.data.table(eda_env$tab2_dt)
cdc_scores <- eda_env$cdc_scores
vh_scores <- eda_env$vh_info_scores

tab2_dt$MTURKID <- as.factor(tab2_dt$MTURK1)

merged_network_participant_env <- readRDS("merged_network_participant_objects.rds")
sns_dt_long_merged_ego_characteristics <- merged_network_participant_env$sns_dt_long_merged_ego_characteristics

## Create a network object ----------

edge_list <- as.matrix(sns_dt_long_merged_ego_characteristics[, c("MTURKID", "alterID")])
g <- igraph::graph_from_edgelist(edge_list)


### investigate the structure of g
igraph::vertex_attr(g)
head(V(g)$name)


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
### vh1 in the egos vs 
