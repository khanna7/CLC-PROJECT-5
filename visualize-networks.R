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



## Merge individual data in to the network object ----------

edge_list <- as.matrix(sns_dt_long_wide_no_minors[, c("MTURKID", "alterID")])
g <- igraph::graph_from_edgelist(edge_list)


### get the MTURKID values from the data frame
mturk_ids <- tab2_dt$MTURKID

### create a named list of vertex attributes
vertex_attrs <- 
  list(
  age = tab2_dt$age
  # attribute2 = tab2_dt$attribute2,
  # Add more attributes here as needed
)


### subset the graph based on MTURKID values
sub_g <- induced_subgraph(g, V(g)$name %in% mturk_ids)


### investigate the structure of g
igraph::vertex_attr(g)
head(V(g)$name)


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

ggsave("network_plot.png", width = 8, height = 8, dpi = 300)


