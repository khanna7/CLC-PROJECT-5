# Load necessary libraries

rm(list=ls())

library(haven)
library(dplyr)
library(data.table)
library(igraph)
library(ggplot2)
library(ggraph)
library(psych)

# Read Data ----------

## Location
data_loc <- "/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT/"
network_env <- readRDS(paste0(data_loc, "network_objects.rds"))
names(network_env)

## Ego Data
sns_consenting_dt <- as.data.table(network_env$sns_consenting_dt)

## Network Data
sns_dt_long_wide_no_minors <- as.data.table(network_env$sns_dt_long_wide_no_minors)


# Inspect data ---------

## Ego Data
dim(sns_consenting_dt)
colnames(sns_consenting_dt)
table(sns_consenting_dt$FUVA3, exclude = NULL) #vaccination status of egos

## Network Data

dim(sns_dt_long_wide_no_minors)
table(sns_dt_long_wide_no_minors$SN37, exclude = NULL) ## vaccination status of alters

  #### RECEIVED AT LEAST ONE VACCINE DOSE (SN37)
    # if the network member has received at least one dose of the COVID-19
    # vaccine (SN37),
    # o	Yes  (1) 
    # o	No  (2) 
    # o	Don't know  (3) 


# Matching --------- 

unique_network_mturk_ids <- unique(sns_dt_long_wide_no_minors$MTURKID)
unique_ego_mturk_ids <- unique(sns_consenting_dt$MTURK1)

length(which(unique_ego_mturk_ids %in% unique_network_mturk_ids))

# Rename the datasets  more meaningfully --------- 

ego_dt <- sns_consenting_dt
network_member_dt <- sns_dt_long_wide_no_minors


# Plot the data -----------

# Create the edge list
edge_list <- as.matrix(network_member_dt[, c("MTURKID", "alterID")])
g <- igraph::graph_from_edgelist(edge_list, directed = FALSE)

# Assign names to vertices
V(g)$name <- as.character(V(g)$name)

# Separate ego and alter IDs
ego_ids <- unique(ego_dt$MTURKID)
alter_ids <- unique(network_member_dt$alterID)

# Create named vectors for vaccination status
ego_vax_status <- setNames(ego_dt$FUVA3, ego_dt$MTURK1)
alter_vax_status <- setNames(network_member_dt$SN37, network_member_dt$alterID)

table(ego_vax_status, exclude = NULL)
table(alter_vax_status, exclude = NULL)

# Combine the vectors
combined_vax_status <- c(ego_vax_status, alter_vax_status)
combined_vax_status <- combined_vax_status[!is.na(names(combined_vax_status))]

# Check the names of combined_vax_status
combined_vax_status_names <- names(combined_vax_status)
head(combined_vax_status_names)

# Extract base IDs from vertex names
base_ids_from_vertices <- unique(sub("_\\d+$", "", V(g)$name))

# Check if all base IDs exist in combined_vax_status
missing_base_ids <- base_ids_from_vertices[!base_ids_from_vertices %in% combined_vax_status_names]

# Print missing base IDs, if any
if (length(missing_base_ids) > 0) {
  print(missing_base_ids)
} else {
  print("All base IDs are present in combined_vax_status.")
}

# Create a new named vector to store the vaccination status with the correct suffixes
vaccine_status_with_suffix <- setNames(vector("character", length(V(g))), V(g)$name)

# Loop through each vertex name and assign the correct vaccination status
for (vertex_name in V(g)$name) {
  base_id <- sub("_\\d+$", "", vertex_name)
  if (base_id %in% names(combined_vax_status)) {
    vaccine_status_with_suffix[vertex_name] <- combined_vax_status[base_id]
  } else {
    vaccine_status_with_suffix[vertex_name] <- NA
  }
}

# Assign the vaccination status to vertices
V(g)$vaccine_status <- vaccine_status_with_suffix[V(g)$name]

# Check the assignment
table(V(g)$vaccine_status, exclude = NULL)

# Define color palette for the vaccination status
color_palette_vax <- c("1" = "green", "2" = "red", "NA" = "white")

# Plot the network with vaccination status
ggraph(g, layout = 'kk') +
  geom_edge_link() +
  geom_node_point(aes(color = factor(vaccine_status)), size = 3) +
  scale_color_manual(values = color_palette_vax, na.translate = TRUE) +
  theme_void() +
  labs(color = "Vaccination Status")

# Plot the network with vaccination status using layout_with_fr
ggraph(g, layout = 'fr') +  # Using the Fruchterman-Reingold layout
  geom_edge_link() +
  geom_node_point(aes(color = factor(vaccine_status)), size = 3) +
  scale_color_manual(values = color_palette_vax, na.translate = TRUE) +
  theme_void() +
  labs(color = "Vaccination Status")

# Plot the network using the layout_with_graphopt layout
p <- ggraph(g, layout = "graphopt") + 
  geom_edge_link() + 
  geom_node_point(aes(color = factor(vaccine_status)), size = 3) +
  scale_color_manual(values = color_palette_vax, na.translate = TRUE) +
  theme_void() +
  labs(color = "Vaccination Status")

p1 <- ggraph(g, layout = "graphopt") + 
  geom_edge_link() + 
  geom_node_point(aes(color = factor(vaccine_status)), size = 3) +
  scale_color_manual(values = color_palette_vax,
                     labels = c("1" = "At least one dose", "2" = "No doses", "NA" = "Unknown")) +
  theme_void() +
  labs(color = "Vaccination Status")+
  theme(legend.position = "right", 
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))


# Print the plot
print(p1)


