library(readxl)
library(dplyr)
library(igraph)

getwd()

  col_palette <- c("Seed_specialist" = "#1b9e77", 
                   "Farmer"   = "#d95f02", 
                   "Custodian"  = "#7570b3", 
                   "Trader"        = "#e7298a",
                   "Other" = "gold",
                   "Public" = "darkred")
  
  ties  <- read_excel("./Data/TieData_social_network.xlsx", 
                      sheet = "stress_ties")
  nodes <- read_excel("./Data/NodeData_social_network.xlsx", 
                      sheet = "stress_nodes")
  
  # Aggregate transaction volumes per directed pair
  edge_list <- ties |>
    mutate(weight = as.numeric(`Transaction_vol (kg)`)) %>%
    group_by(`Source (from)`, `Sink (to)`) |>
    summarise(weight = sum(weight), .groups = "drop") |>
    rename(from = `Source (from)`, to = `Sink (to)`) |>
    filter(weight > 0)
  
  # Build node attribute table (all unique node IDs in edge list)
  all_ids <- unique(c(edge_list$from, edge_list$to))
  idx     <- match(all_ids, nodes$Node_ID)
  
  node_table <- data.frame(
    name     = all_ids,
    type     = nodes$Node_type[idx],
    region   = nodes$Node_region[idx],
    location = nodes$Node_location[idx],
    gender   = nodes$Node_gender[idx],
    stringsAsFactors = FALSE
  )
  
  node_table[node_table == c("Farm_assoc")] <- c("Farmer")
  node_table[node_table == c("Government")] <- c("Public")
  node_table[node_table == c("NGO")] <- c("Public")
  node_table[node_table == c("Company")] <- c("Trader")
  node_table[node_table == c("Market")] <- c("Trader")
  
  ### ASSESSMENT OF LINK TYPE ABSENCE OR RARITY
  # 1. Clean up and isolate the node types from your node table
  node_types <- node_table %>% 
    select(name, type)
  
  # 2. Count OUTGOING links by Sender Type (Who sends the seed?)
  outgoing_counts <- edge_list %>%
    filter(weight > 0) %>%                        # Only look at active trades
    left_join(node_types, by = c("from" = "name")) %>%
    group_by(type) %>%
    summarise(
      outgoing_links = n(),
      total_volume_sent = sum(weight)
    ) %>%
    rename(sender_type = type)
  
  # 3. Count INCOMING links by Receiver Type (Who buys/gets the seed?)
  incoming_counts <- edge_list %>%
    filter(weight > 0) %>%                        # Only look at active trades
    left_join(node_types, by = c("to" = "name")) %>%
    group_by(type) %>%
    summarise(
      incoming_links = n(),
      total_volume_received = sum(weight)
    ) %>%
    rename(receiver_type = type)
  
  ## BUILD THE GEOGRAPHIC COVARIATE MATRIX
  node_table <- node_table %>%
    mutate(geoloc = paste(paste0(location,","), region))
  #  geolocation <- data.frame(location = unique(node_table$geoloc),
  #                            latitude = "",
  #                            longitude = "")
  #  write.csv(geolocation, "geolocations.csv")
  geolocation <- read_excel("geolocations.xlsx", sheet = "stress")
  geolocation$latitude <- as.numeric(geolocation$latitude)
  geolocation$longitude <- as.numeric(geolocation$longitude)

  uni.geo <- unique(node_table$geoloc)
  nonas <- which(!uni.geo %in% geolocation$geoloc)
  uni.geo[nonas]
    
  node_table <- merge(node_table, geolocation, by = "geoloc") 
  
  nodes_geo <- node_table |>
    filter(name %in% all_ids) %>%
    arrange(match(name, all_ids))
  
  library(sf)
  nodes_sf <- st_as_sf(nodes_geo, coords = c("longitude", "latitude"), crs = 4326)
  dist_matrix <- st_distance(nodes_sf, nodes_sf)
  dist_matrix <- matrix(as.numeric(dist_matrix), 
                        nrow = nrow(dist_matrix), 
                        ncol = ncol(dist_matrix)) / 1000
  rownames(dist_matrix) <- nodes_geo$name
  colnames(dist_matrix) <- nodes_geo$name
  dist_matrix <- log(dist_matrix+1)
   
  # START THE BINARY NETWORK
  library(network)
  library(ergm)
  
  # Creating a network
  net_binary <- network(edge_list[, c("from", "to")],
                        vertex.attr = list(vertex.names = nodes_geo$name), 
                        directed = TRUE)
  
  set.vertex.attribute(net_binary, "type", nodes_geo$type)
  set.vertex.attribute(net_binary, "gender", nodes_geo$gender)
  set.vertex.attribute(net_binary, "region", nodes_geo$region)
  
  summary(net_binary)
  plot(net_binary)
  
  # FIT THE BINARY ERGM
  binary_model <- ergm(net_binary ~ edges +
                         mutual +
                         nodeifactor("type") +
                         nodeofactor("type") +
                         nodematch("gender") +
                         edgecov(dist_matrix),
                       control = control.ergm(
                         MCMLE.maxit = 60,
                         MCMC.burnin = 200000,
                         MCMC.samplesize = 5000,
                         MCMC.interval = 8000,
                         MCMLE.density.guard = 100
                       ))
  
  summary(binary_model)
  
  mcmc.diagnostics(binary_model)
  
  gof_binary <- gof(binary_model,
                    GOF = ~idegree + odegree + espartners + distance)
  par(mfrow = c(2,2))
  plot(gof_binary)
  
  ### NETWORK PREDICTIONS WITH SAME NETWORK SIZE
  n <- 2500
  simulated_networks <- simulate(binary_model, 
                                 nsim = n, 
                                 output = "network")
  
  n_nodes <- network.size(net_binary)
  node_names <- network.vertex.names(net_binary)
  
  # Blank matrix to accumulate simulated weights
  sum_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes, 
                       dimnames = list(node_names, node_names))
  
  # 3. Sum the weight matrices across all 100 simulations
  for(i in 1:2500) {
    sim_mat <- as.matrix(simulated_networks[[i]])
    sum_matrix <- sum_matrix + sim_mat
  }
  
  # 4. Calculate the final weight matrix and thresholded if needed
  sum_matrix[sum_matrix < 35] <- 0
  
  predicted_net <- network(sum_matrix, directed = TRUE, 
                           matrix.type = "adjacency", 
                           ignore.eval = FALSE, 
                           names.eval = "weight")
  set.vertex.attribute(predicted_net, "type", 
                       get.vertex.attribute(net_binary, "type"))
  hist(get.edge.attribute(predicted_net, "weight"))
  
  par(mfrow = c(1,2))
  colors_observed <- col_palette[get.vertex.attribute(net_binary, "type")]
  colors_predicted <- col_palette[get.vertex.attribute(predicted_net, "type")]
  
  plot(net_binary, #main = "Observed",
       vertex.col = colors_observed)
  plot(predicted_net, main = "Predicted",
       vertex.col = colors_predicted)
  
  # Saving network
  weighted.net <- network(edge_list[, c("from", "to")],
                        vertex.attr = list(vertex.names = nodes_geo$name), 
                        directed = TRUE)
  set.edge.value(weighted.net, "weight", edge_list$weight^0.5)
  weighted.mat <- as.matrix(weighted.net, matrix.type = "adjacency", 
                            attrname = "weight")

  combined.mat <- (weighted.mat + sum_matrix)/2
  combined.net <- network(combined.mat, directed =  TRUE,
                          matrix.type = "adjacency",
                          ignore.eval = FALSE,
                          names.eval = "weight")
  set.vertex.attribute(combined.net, "type", 
                       get.vertex.attribute(net_binary, "type"))
  set.vertex.attribute(combined.net, "region",
                       get.vertex.attribute(net_binary, "region"))
  set.vertex.attribute(combined.net, "gender",
                       get.vertex.attribute(net_binary, "gender"))
  par(mfrow = c(1,1))
  plot(combined.net, vertex.col = colors_observed)
  
  library(igraph)
  small_net <- graph_from_adjacency_matrix(
    combined.mat, mode = "directed", 
    weighted = TRUE, diag = FALSE)
  V(small_net)$type <- get.vertex.attribute(combined.net, "type")
  V(small_net)$region <- get.vertex.attribute(combined.net, "region")
  V(small_net)$gender <- get.vertex.attribute(combined.net, "gender")
  V(small_net)$betweenness  <- betweenness(small_net, normalized = TRUE)
  V(small_net)$in_strength  <- strength(small_net, mode = "in",  
                                        weights = E(small_net)$weight)
  V(small_net)$out_strength <- strength(small_net, mode = "out", 
                                        weights = E(small_net)$weight)
  V(small_net)$in_degree    <- degree(small_net, mode = "in")
  V(small_net)$out_degree   <- degree(small_net, mode = "out")
  saveRDS(small_net, "small_net_stress.RDS")
  
  
  
  ### NETWORK PREDICTIONS WITH LARGER NETWORK SIZE
  
  library(terra)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(geodata)
  library(viridis)
  
  dark.palette<-viridis_pal(option = "magma", begin = 0, end = 1)
  countries <- vect(ne_countries(scale = "medium", returnclass = "sf"))
  peru.ext<-ext(countries[countries$admin == "Peru", ])
  peru.units<-gadm(country = "PER", level = 2, path = ".")
  
  # Potato data from https://www.mapspam.info/
  potato <- rast("spam2020_V2r2_global_H_POTA_A.tif")
  potato <- crop(potato, peru.ext)
  potato <- mask(potato, countries[countries$admin == "Peru", ])
  
  par(mfrow = c(1,1))
  plot(peru.units, col = "grey25", border = "grey", add = FALSE,
       lwd = 0.75, box = FALSE, axes = FALSE)
  plot(potato^0.25, col = dark.palette(100),
       add = TRUE, legend = "left")
  
  potato.df <- as.data.frame(potato, xy = TRUE)
  colnames(potato.df) <- c("longitude", "latitude", "harv.area")
  hist(potato.df$harv.area)
  
  # Assigning regions
  peru_regions <- gadm(country = "PER", level = 1, path = tempdir())
  peru_regions <- st_as_sf(peru_regions)
  
  potato_sf <- st_as_sf(potato.df, 
                        coords = c("longitude", "latitude"), 
                        crs = st_crs(peru_regions))
  potato_sf <- st_join(potato_sf, peru_regions)
  potato.df <- data.frame(
    st_coordinates(potato_sf),
    harv.area = potato_sf$harv.area,
    region = potato_sf$NAME_1)
  potato.df <- potato.df[!is.na(potato.df$region), ]
  potato.df <- potato.df[potato.df$harv.area > 0,]
  colnames(potato.df) <- c("longitude","latitude","harv.area","region")
  
  n.nodes <- dim(potato.df)[1]
  
  # 1. Defining parameters
  N_old <- network.size(net_binary) # Original network size
  N_new <- n.nodes                    # Target simulation size

  # 2. Generating a new empty network of N nodes
  net_large <- network.initialize(N_new, directed = TRUE)
  table(get.vertex.attribute(net_binary, "type"))
  prop.type <- prop.table(table(get.vertex.attribute(net_binary, "type")))
  
  # 3. Assigning attributes based on specifiec proportions
  # Proportions: Trader (10%), Public (20%), Seed Specialist (20%), Farmer (25%), Other (25%)
  types_pool <- c(
    rep("Custodian",       0.945 * N_new), #prop.type["Custodian"]
    rep("Trader",          0.098 * N_new), #prop.type["Trader"]
    rep("Public",          0.098 * N_new), #prop.type["Public"]
    rep("Seed_specialist", 0.099 * N_new), #prop.type["Seed_specialist"]
    rep("Farmer",          98.662 * N_new), #prop.type["Farmer"]
    rep("Other",           0.098 * N_new)
  )
  set.vertex.attribute(net_large, "type", sample(types_pool))
  
  prop.gender <- prop.table(table(get.vertex.attribute(net_binary,"gender")))
  genders_pool <- c(
    rep("Female",   prop.gender["Female"]*N_new),
    rep("Male",     prop.gender["Male"]*N_new),
    rep("NA",       prop.gender["NA"]*N_new)
  )
  set.vertex.attribute(net_large, "gender", sample(genders_pool))

  # 4. Calculating the Krivitsky size adjustment offset
  size_offset <- -log(N_new / N_old)  

  # 5. Generating a distance matrix
  farmers_sf <- st_as_sf(potato.df, coords = c("longitude", "latitude"), crs = 4326)
  new_distmat <- st_distance(farmers_sf, farmers_sf)
  new_distmat <- matrix(as.numeric(new_distmat), 
                        nrow = n.nodes, ncol = n.nodes) / 1000
  new_distmat <- log(new_distmat + 1)
  dist_matrix <- new_distmat
  
  # Check Memory allocation footprint to ensure system stability
  print(object.size(new_distmat), units = "Mb")
  
  # 5. Siimulating the large-scale network
  # We pass the coefficients extracted from your 'binary_model' directly into the simulation.
  simulated_lnet <- simulate(
    net_large ~ 
      edges + 
      offset(edges) + # This injects our scaling correction
      mutual + 
      nodeifactor("type") + 
      nodeofactor("type") + 
      nodematch("gender") + 
      edgecov(dist_matrix),
    coef = c(coef(binary_model), size_offset), # Append the calculated offset value here
    nsim = 1000
  )  
  
  summary(simulated_lnet[[2]] ~ edges + mutual)
  summary(net_binary ~ edges + mutual)
  
  # Blank matrix to accumulate simulated weights
  new.names <- network.vertex.names(net_large)
  large_matrix <- matrix(0, nrow = N_new, ncol = N_new, 
                         dimnames = list(new.names, new.names))
  
  # Sum the weight matrices across all 100 simulations
  for(i in 1:1000) {
    sim_mat <- as.matrix(simulated_lnet[[i]])
    large_matrix <- large_matrix + sim_mat
  }
  
  # 4. Calculate the final weight matrix using a threshold if needed
  large_matrix[large_matrix < 12] <- 0
  
  predicted_large <- network(large_matrix, directed = TRUE, 
                           matrix.type = "adjacency", 
                           ignore.eval = FALSE, 
                           names.eval = "weight")
  set.vertex.attribute(predicted_large, "type", 
                       get.vertex.attribute(net_large, "type"))
  set.vertex.attribute(predicted_large, "region",
                       potato.df$region)
  set.vertex.attribute(predicted_large, "harvarea",
                       potato.df$harv.area)
  set.vertex.attribute(predicted_large, "gender",
                       get.vertex.attribute(net_large, "gender"))
  hist(get.edge.attribute(predicted_large, "weight"))
  
  par(mfrow = c(1,1))
  colors_large <- col_palette[get.vertex.attribute(predicted_large, "type")]
  plot(predicted_large, vertex.cex = 0.5,
       vertex.col = colors_large)
  
  # Saving the large network
  large_net <- as.matrix(predicted_large, matrix.type = "adjacency", 
                         attrname = "weight")
  large_net <- graph_from_adjacency_matrix(
    large_net, mode = "directed", 
    weighted = TRUE, diag = FALSE)
  V(large_net)$type <- get.vertex.attribute(net_large, "type")
  V(large_net)$gender <- get.vertex.attribute(net_large, "gender")
  V(large_net)$region <- get.vertex.attribute(predicted_large, "region")
  V(large_net)$harvarea <- get.vertex.attribute(predicted_large, "harvarea")
  
  V(large_net)$betweenness  <- igraph::betweenness(large_net, normalized = TRUE)
  V(large_net)$in_strength  <- igraph::strength(large_net, mode = "in",  
                                                weights = E(large_net)$weight)
  V(large_net)$out_strength <- igraph::strength(large_net, mode = "out", 
                                                weights = E(large_net)$weight)
  V(large_net)$in_degree    <- igraph::degree(large_net, mode = "in")
  V(large_net)$out_degree   <- igraph::degree(large_net, mode = "out")
  saveRDS(large_net, "large_net_stress.RDS")
  large_net <- readRDS("large_net_stress.RDS")

  plot(V(large_net)$betweenness, V(large_net)$out_strength)  
  
  potato.df$betweenness <- V(large_net)$betweenness
  potato.df$in_strength <- V(large_net)$in_strength
  potato.df$out_strength <- V(large_net)$out_strength
  potato.df$in_degree <- V(large_net)$in_degree
  potato.df$out_degree <- V(large_net)$out_degree
  
  library(ggplot2)
  ggplot(data = potato.df, aes(x = longitude, y = latitude)) +
    geom_tile(aes(fill = betweenness)) +
    scale_fill_viridis(option = "inferno") +
    theme_void()
  ggplot(data = potato.df, aes(x = longitude, y = latitude)) +
    geom_tile(aes(fill = out_strength)) +
    scale_fill_viridis(option = "inferno") +
    theme_void()
  ggplot(data = potato.df, aes(x = longitude, y = latitude)) +
    geom_tile(aes(fill = in_strength)) +
    scale_fill_viridis(option = "inferno") +
    theme_void()  
  ggplot(data = potato.df, aes(x = longitude, y = latitude)) +
    geom_tile(aes(fill = in_degree)) +
    scale_fill_viridis(option = "inferno") +
    theme_void()  
  
  