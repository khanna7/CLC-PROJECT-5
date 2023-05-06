# Visualize networks


# Clear the workspace ----------

rm(list=ls())


# Set knitr chunk options ----------


library(haven)
library(dplyr)
library(data.table)
library(igraph)
library(ggplot2)
library(ggraph)


# Data location ----------
data_loc <- "/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT"



## extract egos (i.e. participants) consenting to give SNS data, 
## and their attributes

network_env <- readRDS(paste0(data_loc, "/network_objects.rds"))
sns_consenting_dt <- as.data.table(network_env$sns_consenting_dt)

## extract attributes of adult alters (i.e., network members)
merged_network_participant_env <- readRDS(paste0(data_loc, "/merged_network_participant_objects.rds"))
sns_dt_long_merged_ego_characteristics <- merged_network_participant_env$sns_dt_long_merged_ego_characteristics


# Create a network object ----------

edge_list <- as.matrix(sns_dt_long_merged_ego_characteristics[, c("MTURKID", "alterID")])
g <- igraph::graph_from_edgelist(edge_list)


## investigate the structure of g
#head(igraph::vertex_attr(g))
head(V(g)$name, 20)


## count the number of clusters in g
clusters <- clusters(g)
num_clusters <- length(clusters$membership)


# Visualize network objects (vaccine outcomes are of interest) ----------

## preliminary visualization

ggraph(g, layout = 'kk') +
  geom_edge_link() +
  geom_node_point() +
  theme_void()

## plot 4 randomly selected clusters to check the shape 
## (i.e., one central ego, 4-5 alters connecting to the ego but not to each other)


### ---------- 
set.seed(42) # Setting seed for reproducibility

# Find the clusters in the network
cluster_info <- clusters(g)

# Extract cluster IDs
unique_clusters <- unique(cluster_info$membership)

# Randomly select 4 cluster IDs
selected_clusters <- sample(unique_clusters, size = 4)

# Create a subgraph with only the selected clusters
selected_nodes <- V(g)[cluster_info$membership %in% selected_clusters]
subgraph_g <- induced_subgraph(g, selected_nodes)

# Visualize the selected clusters using the Kamada-Kawai layout
ggraph(subgraph_g, layout = 'kk') +
  geom_edge_link() +
  geom_node_point() +
  theme_void()

### ---------- 

# Color vertices by participant vs network member  labels ----------

## define vertex attribute
V(g)$label <- ifelse(grepl("_\\d+$", V(g)$name), "Network Member", "Participant")

## create a color palette for the labels
color_palette <- c("Participant" = "blue", "Network Member" = "red")

## plot the graph with labeled vertices

p <- 
  ggraph(g, layout = "kk") + 
  geom_edge_link() + 
  geom_node_point(aes(color = label)) +
  scale_color_manual(values = color_palette) +
  #geom_node_text(aes(label = ifelse(grepl("_\\d+$", name), "Network Member", "Participant"))) +
  theme_void()

p

ggsave(paste0(data_loc, "/network_plot.png"), width = 8, height = 8, dpi = 300)

## plot 4 randomly selected clusters to check the shape 
## (i.e., one central ego, 4-5 alters connecting to the ego but not to each other)


### ---------- 
set.seed(42) # Setting seed for reproducibility

# Find the clusters in the network
cluster_info <- clusters(g)

# Extract cluster IDs
unique_clusters <- unique(cluster_info$membership)

# Randomly select 4 cluster IDs
selected_clusters <- sample(unique_clusters, size = 4)

# Create a subgraph with only the selected clusters
selected_nodes <- V(g)[cluster_info$membership %in% selected_clusters]
subgraph_g <- induced_subgraph(g, selected_nodes)

# Define vertex attribute
V(subgraph_g)$label <- ifelse(grepl("_\\d+$", V(subgraph_g)$name), "Network Member", "Participant")

# Create a color palette for the labels
color_palette <- c("Participant" = "blue", "Network Member" = "red")

# Visualize the selected clusters using the Kamada-Kawai layout
ggraph(subgraph_g, layout = 'kk') +
  geom_edge_link() +
  geom_node_point(aes(color = label)) +
  scale_color_manual(values = color_palette) +
  theme_void()


### ---------- 


# Visualize clustering on one "trait" of interest ----------

## cdc_avg_out_adequate in the egos vs 
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




## Create data table with egos, alters, and traits of interest

# Extract vertex names
vertex_names <- V(g)$name

# Create a data.table with vertex names
vertex_dt <- data.table(name = vertex_names)

# Merge ego attributes from sns_consenting_dt
vertex_dt <- vertex_dt %>%
  merge(sns_consenting_dt, by.x = "name", by.y = "MTURKID", all.x = TRUE)

# Merge alter attributes from sns_dt_long_merged_ego_characteristics
vertex_dt <- vertex_dt %>%
  merge(sns_dt_long_merged_ego_characteristics, by.x = "name", by.y = "alterID", all.x = TRUE)

# Display the merged data.table
head(vertex_dt)

### test ego characteristics ----------

# Check specific ego attributes
ego_id <- sample(sns_consenting_dt$MTURKID, 1)
print(ego_id)
ego_original <- sns_consenting_dt[sns_consenting_dt$MTURKID == ego_id, ]
ego_merged <- vertex_dt[vertex_dt$name == ego_id, ]

# Choose specific columns to display
columns_to_show <- c("MTURKID", "DEMO1", "DEMO2", "DEMO3_1")

# Filter the original ego data
ego_original_filtered <- ego_original %>% select(columns_to_show)

# Filter the merged ego data
ego_merged_filtered <- ego_merged %>% select(columns_to_show)
ego_merged_filtered$MTURKID <- ego_id

# Compare the original ego data with the merged data
print("Original ego data:")
print(ego_original_filtered)

print("Merged ego data:")
print(ego_merged_filtered)


### test alter characteristics -----------
ego_id <- sample(sns_consenting_dt$MTURKID, 1)
print(ego_id)

edge_dt <- sns_dt_long_merged_ego_characteristics[, c("MTURKID", "alterID")]
alter_ids <- edge_dt[edge_dt$MTURKID == ego_id,]$alterID
print(alter_ids)

alter_original <- sns_dt_long_merged_ego_characteristics[alterID %in% alter_ids, ]
alter_merged <- vertex_dt[name %in% alter_ids, ]
columns_to_show <- c("MTURKID", "SN11", "SN12", "SN13", "SN25") #just pick a few that are mostly populated across the columns

alter_original_filtered <- alter_original %>% select(columns_to_show)
alter_merged_filtered <- alter_merged %>% select(columns_to_show)
alter_merged_filtered$MTURKID <- alter_ids

print("Original alter data:")
print(alter_original_filtered)
print("Merged alter data:")
print(alter_merged_filtered)

# identical(
#   alter_original_filtered %>% select(-MTURKID),
#   alter_merged_filtered %>% select(-MTURKID)
# )

# Prepare data to plot traits ----------

# Set vertex attributes
V(g)$is_ego <- !grepl("_\\d+$", V(g)$name)
V(g)$cdc_avg_out <- vertex_dt$cdc_avg_out.x[match(V(g)$name, vertex_dt$name)]
V(g)$SN9 <- vertex_dt$SN9[match(V(g)$name, vertex_dt$name)]
V(g)$SN9_numeric <- as.numeric(as.character(V(g)$SN9))

# Set vertex colors based on attributes
V(g)$color <- ifelse(V(g)$is_ego, "red", "blue")

# Set vertex sizes based on cdc_avg_out for egos and SN9 for alters
V(g)$size <- ifelse(V(g)$is_ego, V(g)$cdc_avg_out * 2, V(g)$SN9 * 2)

# Replace infinite or missing values with a default size (e.g., 5)
V(g)$size[is.infinite(V(g)$size) | is.na(V(g)$size)] <- 5

# Try plotting the graph again
layout <- layout_with_fr(g)
plot(g, vertex.label = NA, layout = layout)

# Plot the graph
plot(g, vertex.label = NA)

# Color based on ego/alter behaviors.

assign_alter_color <- function(is_ego, sn9_value) {
  if (is_ego) {
    return(NA)
  } else {
    if (is.na(sn9_value)) {
      return("gray")
    } else if (sn9_value == 1) {
      return("red")
    } else if (sn9_value == 2) {
      return("blue")
    } else if (sn9_value == 3) {
      return("orange")
    } else if (sn9_value %in% c(4, 5, 6, 7, 8)) {
      return("purple")
    } else {
      return("gray")
    }
  }
}

threshold <- 2

V(g)$color <- mapply(assign_alter_color, V(g)$is_ego, V(g)$SN9_numeric)
V(g)$color[V(g)$is_ego & V(g)$cdc_avg_out > threshold] <- "blue"
  V(g)$color[V(g)$is_ego & V(g)$cdc_avg_out <= threshold] <- "red"
    V(g)$size <- ifelse(V(g)$is_ego, V(g)$cdc_avg_out * 2, 5)
    plot(g, vertex.label = NA)
    

      

# Select a few clusters to visualize
set.seed(Sys.time())
clusters_to_show <- sample(cluster_info$no, 3) #sample X out of the total number of clusters 
subgraphs <- lapply(clusters_to_show, function(x) { induced_subgraph(g, which(cluster_info$membership == x)) })

# Plot the selected clusters
par(mfrow = c(1, length(clusters_to_show)))
for (i in seq_along(subgraphs)) {
  plot(subgraphs[[i]], vertex.label = NA, main = paste0("Cluster ", clusters_to_show[i]))
}


# Legend for study participants (egos)
legend(
  "bottomleft",
  title = "Study Participants",
  legend = c("Score > 2", "Score <= 2"),
  col = c("blue", "red"),
  pch = 21,
  pt.bg = c("blue", "red"),
  bty = "n", # No box around the legend
  cex = 0.8,
  y.intersp = 1.5,
  ncol = 1
)

# Legend for network members (alters)
legend(
  "bottomright",
  title = "Network Members",
  legend = c("Republican", "Democrat", "Independent", "Others"),
  col = c("red", "blue", "orange", "purple"),
  pch = 21,
  pt.bg = c("red", "blue", "orange", "purple"),
  bty = "n", # No box around the legend
  cex = 0.8,
  y.intersp = 1.5,
  ncol = 1
)

# Compute distributions of alter political party affiliations ---------------
## for the adequately/in- protected group at different thresholds 

ego_alter_stats <- lapply(1:length(unique_clusters), function(cluster_id) {
  ego_score <- vertex_dt$cdc_avg_out.x[match(V(g)$name, vertex_dt$name)][cluster_info$membership == cluster_id][1]
  
  # Handle missing values in the SN3 columns
  alter_party_data <- vertex_dt$SN9[match(V(g)$name, vertex_dt$name)][cluster_info$membership == cluster_id][-1]
  alter_party_data_clean <- na.omit(alter_party_data)
  
  alter_party_counts <- table(alter_party_data_clean)
  all_parties <- 1:8
  missing_parties <- setdiff(all_parties, as.integer(names(alter_party_counts)))
  alter_party_counts_full <- c(alter_party_counts, setNames(as.list(rep(0, length(missing_parties))), as.character(missing_parties)))
  alter_party_counts_full <- alter_party_counts_full[order(as.integer(names(alter_party_counts_full)))]
  alter_party_counts_full <- unlist(alter_party_counts_full) # Convert to numeric vector
  
  alter_party_proportions <- alter_party_counts_full / sum(alter_party_counts_full)
  
  list(score = ego_score, proportions = alter_party_proportions)
})

compute_mean_alter_party_distribution <- function(ego_alter_stats, threshold) {
  high_score_egos <- which(sapply(ego_alter_stats, function(x) x$score) > threshold)
  low_score_egos <- which(sapply(ego_alter_stats, function(x) x$score) <= threshold)
  
  # Filter out entries with NaN values in proportions
  high_score_egos_filtered <- high_score_egos[sapply(ego_alter_stats[high_score_egos], function(x) !any(is.nan(x$proportions)))]
  
  # Filter out entries with NaN values in proportions
  low_score_egos_filtered <- low_score_egos[sapply(ego_alter_stats[low_score_egos], function(x) !any(is.nan(x$proportions)))]
  
  mean_proportions_high_score <- colMeans(do.call(rbind, lapply(ego_alter_stats[high_score_egos_filtered], function(x) as.numeric(x$proportions))))
  mean_proportions_low_score <- colMeans(do.call(rbind, lapply(ego_alter_stats[low_score_egos_filtered], function(x) as.numeric(x$proportions))))
  
  return(list(mean_proportions_high_score = mean_proportions_high_score,
              mean_proportions_low_score = mean_proportions_low_score))
}

# Apply the functions
threshold <- seq(0.5, 2.75, by=0.25)
alter_pol_dist<- lapply(threshold, function(x) compute_mean_alter_party_distribution(ego_alter_stats, x))

alter_pol_dist

# end ---------------
