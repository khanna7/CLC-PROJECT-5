# Visualize networks and analyze ego/alter behaviors of interest


# Clear the workspace ----------

rm(list=ls())


# Set knitr chunk options ----------


library(haven)
library(dplyr)
library(data.table)
library(igraph)
library(ggplot2)
library(ggraph)


# Read Data ----------
  
  ## specify data location 
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


  set.seed(Sys.time()) # Setting seed for reproducibility
  
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
 
  set.seed(Sys.time()) # Setting seed for reproducibility
  
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




  
# Visualize clustering on one "trait" of interest ----------
  ## cdc_avg_out_adequate in the egos vs alters
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
  



# Create data table with egos, alters, and traits of interest ----------
  
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
  

  ## test alter characteristics 
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




  ## prepare data to plot traits 

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
      
  
        

  ## Select a few clusters to visualize
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




# Analyze alter-party distributions  ----------

  ## Compute alter distributions of  the adequately vs. not- protected group at different thresholds 
  
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

  ## Apply the functionsand summarize the distributions
  threshold <- seq(0.5, 2.75, by=0.25)
  alter_pol_dist<- lapply(threshold, function(x) compute_mean_alter_party_distribution(ego_alter_stats, x))
  
  alter_pol_dist

  ## compute homophily-like measure for the ego score and the alter party distribution 
  
  ## we'll compute correlations
    ego_party_proportions <- data.frame(
      ego_score = sapply(ego_alter_stats, function(x) x$score),
      proportion_republicans = sapply(ego_alter_stats, function(x) x$proportions[1]),
      proportion_democrats = sapply(ego_alter_stats, function(x) x$proportions[2]),
      proportion_independents = sapply(ego_alter_stats, function(x) x$proportions[3])
    )
  
  correlations <- cor(ego_party_proportions, use = "complete.obs")
  correlations
  
  
   
  
  
  
# Analyze alter knowing anyone who died of COVID-19  ----------
  
  compute_ego_alter_attribute_proportions_SN29 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN29 <- compute_ego_alter_attribute_proportions_SN29(vertex_dt, edge_dt, "SN29")
  
  ## compute correlations with %alters reporting knowing someone who died of COVID-19
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN29 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN29),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN29, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN29, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN29, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN29 <- cor(ego_proportions_df_SN29$cdc_avg_out, ego_proportions_df_SN29$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN29 <- cor(ego_proportions_df_SN29$cdc_avg_out, ego_proportions_df_SN29$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN29 <- cor(ego_proportions_df_SN29$cdc_avg_out, ego_proportions_df_SN29$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN29 = cor_cdc_yes_SN29, cor_cdc_no_SN29 = cor_cdc_no_SN29, cor_cdc_missing_SN29 = cor_cdc_missing_SN29)

  
  
# Analyze alter encouraged  testing SN32----------
  
  compute_ego_alter_attribute_proportions_SN32 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN32 <- compute_ego_alter_attribute_proportions_SN32(vertex_dt, edge_dt, "SN32")
  
  ## compute correlations with %alters 
  options(warn=1) #for correlations with constants, such as proportion_missing below
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN32 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN32),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN32, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN32, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN32, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN32 <- cor(ego_proportions_df_SN32$cdc_avg_out, ego_proportions_df_SN32$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN32 <- cor(ego_proportions_df_SN32$cdc_avg_out, ego_proportions_df_SN32$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN32 <- cor(ego_proportions_df_SN32$cdc_avg_out, ego_proportions_df_SN32$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN32 = cor_cdc_yes_SN32, cor_cdc_no_SN32 = cor_cdc_no_SN32, cor_cdc_missing_SN32 = cor_cdc_missing_SN32)
  
  
# Analyze alter encouraged  following social distancing guidelines SN34----------
  
  compute_ego_alter_attribute_proportions_SN34 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN34 <- compute_ego_alter_attribute_proportions_SN34(vertex_dt, edge_dt, "SN34")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN34 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN32),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN34, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN34, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN34, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN34 <- cor(ego_proportions_df_SN34$cdc_avg_out, ego_proportions_df_SN34$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN34 <- cor(ego_proportions_df_SN34$cdc_avg_out, ego_proportions_df_SN34$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN34 <- cor(ego_proportions_df_SN34$cdc_avg_out, ego_proportions_df_SN34$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN34 = cor_cdc_yes_SN34, cor_cdc_no_SN34 = cor_cdc_no_SN34, cor_cdc_missing_SN34 = cor_cdc_missing_SN34)
  
  

# Analyze alter encouraged  mask wearing SN36----------
  
  compute_ego_alter_attribute_proportions_SN36 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN36 <- compute_ego_alter_attribute_proportions_SN36(vertex_dt, edge_dt, "SN36")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN36 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN36),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN36, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN36, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN36, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes <- cor(ego_proportions_df_SN36$cdc_avg_out, ego_proportions_df_SN36$proportion_yes, use = "complete.obs")
  cor_cdc_no <- cor(ego_proportions_df_SN36$cdc_avg_out, ego_proportions_df_SN36$proportion_no, use = "complete.obs")
  cor_cdc_missing <- cor(ego_proportions_df_SN36$cdc_avg_out, ego_proportions_df_SN36$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes = cor_cdc_yes, cor_cdc_no = cor_cdc_no, cor_cdc_missing = cor_cdc_missing)
  
  
  
# Analyze alter follows social distancing guidelines SN33----------
  
  compute_ego_alter_attribute_proportions_SN33 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN33 <- compute_ego_alter_attribute_proportions_SN33(vertex_dt, edge_dt, "SN33")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN33 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN33),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN33, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN33, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN33, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes <- cor(ego_proportions_df_SN33$cdc_avg_out, ego_proportions_df_SN33$proportion_yes, use = "complete.obs")
  cor_cdc_no <- cor(ego_proportions_df_SN33$cdc_avg_out, ego_proportions_df_SN33$proportion_no, use = "complete.obs")
  cor_cdc_missing <- cor(ego_proportions_df_SN33$cdc_avg_out, ego_proportions_df_SN33$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes = cor_cdc_yes, cor_cdc_no = cor_cdc_no, cor_cdc_missing = cor_cdc_missing)
  
  
  
  
# Analyze alter received at least one dose of the vaccine SN37 ------------
  
  compute_ego_alter_attribute_proportions_SN37 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN37 <- compute_ego_alter_attribute_proportions_SN37(vertex_dt, edge_dt, "SN37")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN37 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN37),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN37, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN37, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN37, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN37 <- cor(ego_proportions_df_SN37$cdc_avg_out, ego_proportions_df_SN37$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN37 <- cor(ego_proportions_df_SN37$cdc_avg_out, ego_proportions_df_SN37$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN37 <- cor(ego_proportions_df_SN37$cdc_avg_out, ego_proportions_df_SN37$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN37 = cor_cdc_yes_SN37, cor_cdc_no_SN37 = cor_cdc_no_SN37, cor_cdc_missing_SN37 = cor_cdc_missing_SN37)
  
  

  
# Analyze alter encouraged receiving vaccine SN38 ------ ------------
  
  compute_ego_alter_attribute_proportions_SN38 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN38 <- compute_ego_alter_attribute_proportions_SN38(vertex_dt, edge_dt, "SN38")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN38 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN38),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN38, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN38, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN38, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN38 <- cor(ego_proportions_df_SN38$cdc_avg_out, ego_proportions_df_SN38$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN38 <- cor(ego_proportions_df_SN38$cdc_avg_out, ego_proportions_df_SN38$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN38 <- cor(ego_proportions_df_SN38$cdc_avg_out, ego_proportions_df_SN38$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN38 = cor_cdc_yes_SN38, cor_cdc_no_SN38 = cor_cdc_no_SN38, cor_cdc_missing_SN38 = cor_cdc_missing_SN38)
  
  
  
  
  
  
# Analyze alter discouraged receiving vaccine SN39 ------ ------------
  
  compute_ego_alter_attribute_proportions_SN39 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN39 <- compute_ego_alter_attribute_proportions_SN39(vertex_dt, edge_dt, "SN39")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN39 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN39),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN39, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN39, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN39, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN39 <- cor(ego_proportions_df_SN39$cdc_avg_out, ego_proportions_df_SN39$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN39 <- cor(ego_proportions_df_SN39$cdc_avg_out, ego_proportions_df_SN39$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN39 <- cor(ego_proportions_df_SN39$cdc_avg_out, ego_proportions_df_SN39$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN39 = cor_cdc_yes_SN39, cor_cdc_no_SN39 = cor_cdc_no_SN39, cor_cdc_missing_SN39 = cor_cdc_missing_SN39)
  
  
  
  
  
  
  
# Analyze alter knows someone hospitalized because of COVID-19 SN28 ------ 
  
  compute_ego_alter_attribute_proportions_SN28 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN28 <- compute_ego_alter_attribute_proportions_SN28(vertex_dt, edge_dt, "SN28")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN28 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN28),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN28, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN28, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN28, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN28 <- cor(ego_proportions_df_SN28$cdc_avg_out, ego_proportions_df_SN28$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN28 <- cor(ego_proportions_df_SN28$cdc_avg_out, ego_proportions_df_SN28$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN28 <- cor(ego_proportions_df_SN28$cdc_avg_out, ego_proportions_df_SN28$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN28 = cor_cdc_yes_SN28, cor_cdc_no_SN28 = cor_cdc_no_SN28, cor_cdc_missing_SN28 = cor_cdc_missing_SN28)
  
  
  
  
  
  
# Analyze alter how often wears mask in public SN 35 ------ 
  
  # Analyze the discrete SN35 variable
  ## we consider 1 (always) or 2 (usally) as "success" and 3 (sometimes) or 4 (rarely/never) as failure
  compute_ego_alter_attribute_proportions_SN35_disc <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] %in% c(1, 2), na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] %in% c(3,4), na.rm = TRUE) 
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN35 <- compute_ego_alter_attribute_proportions_SN35_disc(vertex_dt, edge_dt, "SN35")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN35 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN35),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN35, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN35, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN35, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN35 <- cor(ego_proportions_df_SN35$cdc_avg_out, ego_proportions_df_SN35$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN35 <- cor(ego_proportions_df_SN35$cdc_avg_out, ego_proportions_df_SN35$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN35 <- cor(ego_proportions_df_SN35$cdc_avg_out, ego_proportions_df_SN35$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN35 = cor_cdc_yes_SN35, cor_cdc_no_SN35 = cor_cdc_no_SN35, cor_cdc_missing_SN35 = cor_cdc_missing_SN35)
  
  
  
  
  
  
  
# Analyze alter encouraged drinking in public SN24 ------ 
  
  compute_ego_alter_attribute_proportions_SN24 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN24 <- compute_ego_alter_attribute_proportions_SN24(vertex_dt, edge_dt, "SN24")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN24 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN24),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN24, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN24, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN24, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN24 <- cor(ego_proportions_df_SN24$cdc_avg_out, ego_proportions_df_SN24$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN24 <- cor(ego_proportions_df_SN24$cdc_avg_out, ego_proportions_df_SN24$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN24 <- cor(ego_proportions_df_SN24$cdc_avg_out, ego_proportions_df_SN24$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN24 = cor_cdc_yes_SN24, cor_cdc_no_SN24 = cor_cdc_no_SN24, cor_cdc_missing_SN24 = cor_cdc_missing_SN24)
  
  
  
  
  

  
# Analyze alter tested for COVID-19 SN27 ------ 
  
  compute_ego_alter_attribute_proportions_SN27 <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN27 <- compute_ego_alter_attribute_proportions_SN27(vertex_dt, edge_dt, "SN27")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN27 <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN27),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN27, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN27, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN27, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN27 <- cor(ego_proportions_df_SN27$cdc_avg_out, ego_proportions_df_SN27$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN27 <- cor(ego_proportions_df_SN27$cdc_avg_out, ego_proportions_df_SN27$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN27 <- cor(ego_proportions_df_SN27$cdc_avg_out, ego_proportions_df_SN27$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN27 = cor_cdc_yes_SN27, cor_cdc_no_SN27 = cor_cdc_no_SN27, cor_cdc_missing_SN27 = cor_cdc_missing_SN27)
  
  
  
  
  
  
  
# Analyze alter tested positive for COVID-19 SN27a ------ 
  
  compute_ego_alter_attribute_proportions_SN27a <- function(vertex_dt, edge_dt, attribute_name) {
    egos <- unique(edge_dt$MTURKID)
    ego_alter_attribute_proportions <- list()
    
    for (ego in egos) {
      alter_ids <- edge_dt[edge_dt$MTURKID == ego,]$alterID
      alter_data <- vertex_dt[vertex_dt$name %in% alter_ids, c("name", attribute_name), with = FALSE]
      
      yes_count <- sum(alter_data[[attribute_name]] == 1, na.rm = TRUE)
      no_count <- sum(alter_data[[attribute_name]] == 2, na.rm = TRUE)
      missing_count <- sum(is.na(alter_data[[attribute_name]]))
      total_count <- yes_count + no_count + missing_count
      
      proportion_yes <- yes_count / total_count
      proportion_no <- no_count / total_count
      proportion_missing <- missing_count / total_count
      
      ego_alter_attribute_proportions[[as.character(ego)]] <- list(
        proportion_yes = proportion_yes,
        proportion_no = proportion_no,
        proportion_missing = proportion_missing
      )
    }
    
    return(ego_alter_attribute_proportions)
  }
  
  ego_alter_stats_SN27a <- compute_ego_alter_attribute_proportions_SN27a(vertex_dt, edge_dt, "SN27a")
  
  ## compute correlations with %alters 
  ego_data <- vertex_dt[!grepl("_", vertex_dt$name),]
  
  ego_proportions_df_SN27a <- data.frame(# create data frame
    MTURKID = names(ego_alter_stats_SN27a),
    cdc_avg_out = ego_data$cdc_avg_out.x,
    proportion_yes = sapply(ego_alter_stats_SN27a, function(x) x$proportion_yes),
    proportion_no = sapply(ego_alter_stats_SN27a, function(x) x$proportion_no),
    proportion_missing = sapply(ego_alter_stats_SN27a, function(x) x$proportion_missing)
  )
  
  cor_cdc_yes_SN27a <- cor(ego_proportions_df_SN27a$cdc_avg_out, ego_proportions_df_SN27a$proportion_yes, use = "complete.obs")
  cor_cdc_no_SN27a <- cor(ego_proportions_df_SN27a$cdc_avg_out, ego_proportions_df_SN27a$proportion_no, use = "complete.obs")
  cor_cdc_missing_SN27a <- cor(ego_proportions_df_SN27a$cdc_avg_out, ego_proportions_df_SN27a$proportion_missing, use = "complete.obs")
  
  list(cor_cdc_yes_SN27a = cor_cdc_yes_SN27a, cor_cdc_no_SN27a = cor_cdc_no_SN27a, cor_cdc_missing_SN27a = cor_cdc_missing_SN27a)
  
  
  
  
  
  