#Authors: Patricia Breen & Louise Allcock
#Note that this code was written to run on a local system prior to publication of
#data layers. File paths will need amending throughout this code but filenames
#remain unchanged.
#Parts of code not relevant to the scenario herein are commented out.

# Libraries
library(raster)
library(sp)
library(prioritizr)
library(sf)
library(gurobi)
library(dplyr)
library(tibble)
library(scales)
library(ggplot2)
library(topsis)
library(withr)
library(purrr)
library(ggspatial)
library(terra)
library(Matrix)



# ---------------------------------
# 1.0 Get data
# ---------------------------------
# ---------------------------------
# 1.1 Make a raster stack of available feature layers
# ---------------------------------

# setwd("H:/mpa_ecological_sensitivity2/mpa_ecological_sensitivity/Celtic Sea/ProcessedFeatures/FinalFeatures")
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedFeatures/FinalFeatures")

feature_tifs <- list.files(pattern = "\\.tif$")
features <- lapply(feature_tifs, raster)  # code for raster package
names(features) <- feature_tifs
feat_stack_1km <- stack(features)  # code for raster package
names(feat_stack_1km)

# ---------------------------------
# 1.2 Drop front layer and replace with two split layers (into CelticSea and Coastal fronts)
# ---------------------------------

front_layer <- grep("Fronts_temp_1km.tif", names(feat_stack_1km))
feat_stack_1km <- dropLayer(feat_stack_1km, unlist(front_layer)) # note to drop from rasterstack use dropLayer(stack_name, unlist(index_variable))
names(feat_stack_1km)

setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedFeatures/CelticSeaFront/split")
CelticSeaFront <- raster("CelticSeaFront.tif")
CoastalFront <- raster("coastal_front.tif")
feat_stack_1km <- stack(feat_stack_1km, CelticSeaFront, CoastalFront)
names(feat_stack_1km)

# ---------------------------------
# 1.3 Make a raster stack of available cost layers
# ---------------------------------

#setwd("../../ProcessedCostLayers") # USE IF 1.2 SKIPPED
setwd("../../../ProcessedCostLayers")

cost_tifs <- list.files(pattern = "\\.tif$")
costs <- lapply(cost_tifs, raster) # code for raster package
# costs <- lapply(cost_tifs, rast)  # code for terra package
names(costs) <- cost_tifs
cost_stack_1km <- stack(costs)  # code for raster package
# cost_stack_1km <- c(costs)  # code for terra package
names(cost_stack_1km)

# ---------------------------------
# 1.3 Get planning units and count number of cells
# ---------------------------------

setwd("../PlanningUnits")
pu_1km <- raster("Planning_units_1km.tif") # code for raster package
# pu_1km <- rast("Planning_units_1km.tif") # code for terra package

# can also count cells with:
#ncell(cost_stack_3km[[1]])-cellStats(cost_stack_3km[[1]], stat = "countNA", na.rm=TRUE)


# ---------------------------------
# 1.4 Get ORE layer for checking overlap later
# ---------------------------------

setwd("../ProcessedCostLayers/IntermediateLayers")
ORE_1km <- raster("ORE_even_1km.tif")

# ---------------------------------
# 1.5 Get SAC & SPA rasters and combine to single presence absence layer
# ---------------------------------
# 
# setwd("../../")
# 
# SAC <- raster("Data/Special_areas_of_conservation/Special_areas_of_conservation_p_a.tif")
# SPA <- raster("Data/Special_protection_areas/Special_protection_areas_p_a.tif")
# 
# ExistProt <- calc(stack(SAC, SPA), sum) # Combine the two layers
# ExistProt[ExistProt >0] <- 1 # Make binary
# names(ExistProt) <- "Existing_Protected_Areas"
# plot(ExistProt, main = "SACs and SPAs combined", axes = FALSE, legend = FALSE)
# 
# # For interest only, calculate current proportion occupied by SAC
# cellno_1km <- sum(!is.na(getValues(ExistProt)))
# ExistProt_cells <- cellStats(ExistProt, stat = sum, na.rm = TRUE)
# 
# print(paste0("Existing protection covers ", round(ExistProt_cells/cellno_1km*100, 1), " % of AOI"))
# ---------------------------------
# 2.0 Aggregate to 3km scale
# ---------------------------------

# # Aggregation method averages values across aggregated cells
# feat_stack_3km <-aggregate(feat_stack_1km, fact=3)
# # Aggregation method sums values across aggregated cells to maintain total costs of layer
# cost_stack_3km <-aggregate(cost_stack_1km, fact=3, fun = sum)
# # Aggregation of pu layer to 3km
# pu_3km <- aggregate(pu_1km, fact=3, fun = sum)
# ORE_3km <- aggregate(ORE_1km, fact =3)




# ---------------------------------
# 3.0 Set up scenario and solve
# ---------------------------------
# CHANGE TO cost_stack_3km IF WORKING 3KM SCALE
cellno <- sum(!is.na(getValues(cost_stack_1km[[2]])))
# cellno <- sum(!is.na(getValues(cost_stack_3km[[1]]))) # code for raster package
# cellno <- sum(!is.na(as.matrix(cost_stack_3km[[1]]))) # code for terra package

# ---------------------------------
# 3.1 Set up cost layer, features and targets 
# ---------------------------------

setwd("../../")
setwd("Solutions")

scenario_name <- "1km_threatened_UNWcost_LA" #Each scenario needs a unique name
scenario_group <- "THREATENED"

date <- as.character(Sys.time())

# Choose cost layer to apply and adjust slice accordingly.  If no costs, use "pu_3km"
names(cost_stack_1km)
cost <- cost_stack_1km[[6]] # Choose cost layer
cost_name <- names(cost)


# # FOR LOCKED OUT SAC SCENARIOS ONLY
# # Apply SAC/SPA mask to the current layer
# mask <- ExistProt < 0.5
# cost <- mask * cost
# plot(cost, main="selected cost - protected areas")
# # Add SAC/SPA layer to feature staxk
# feat_stack_1km <- addLayer(feat_stack_1km, ExistProt)
# # END OF LOCKED OUT SAC SCENARIOS ONLY


# rescale cost, when cost values too high - e.g., for actual value layers
cellStats(cost, 'max')
cost <- cost / 100

dir.create(scenario_name)
setwd(paste0("", scenario_name))

writeLines(c("Cost layer used: ",cost_name), "costlayer.txt")
cost_value <- cellStats(cost, sum, na.rm = TRUE)


# List features and store for later tracking
feature_names <- names(feat_stack_1km)
feature_list <- as.list(feature_names)
included_features <- paste(feature_names, collapse = "; ")

# Set targets and store for later tracking file, also 

# # INITIAL RUN TARGETS
# targets <- (rep(0.28, length(feature_names))) #adjust to meet overall 30% target.
# targets[c(8, 26, 33, 34)] <- 0.56 #target X2 other targets for the fronts,and the salmon and eel estuaries

# targets[35] <- 0.9

# # THREATENED SPECIES TARGETS
targets <- (rep(0.18, length(feature_names))) #adjust to meet overall 30% target. 
targets[c(1,2,8,13,14,15,17,18,24,25,26,28,29,32)] <- 0.36

# # ECOLOGICAL IMPORTANCE TARGETS
# targets <- (rep(0.16, length(feature_names))) #adjust to meet overall 30% target.
# targets[c(7,14,16,25,27,28,30,31,32,33,34)] <- 0.32

# # PRAGMATIC SOLUTIONS TARGETS
# # NEED TO CHECK IF YOU CAN SET TARGETS TO ZERO....
# targets <- (rep(0.16, length(feature_names))) #adjust to meet overall 30% target.
# targets[c(7,8,16,26,27,30,31,33,34)] <- 0.32
# targets[c(1,2,13,14,15,17,18,24,25,28,29,32)] <- 0

targets_used <- paste(targets, collapse = "; ")

#write target for each feature to text file
featurextarget <- cbind(feature_list, targets)
write.table(featurextarget, file = paste0(scenario_name,"_feature_targets.txt"), sep = ",", row.names = FALSE)

# ---------------------------------
# 3.2 Generate boundary length data for the planning units
# ---------------------------------
# CHANGE TO BLD_3KM IF WORKING 3KM SCALE
bld_1km <- boundary_matrix(pu_1km)
# bld_3km@x <-rescale(bld_3km@x, to = c(0.001,10)) # Deprecated in PrioritizR 8.0.2.7
bld_1km <-rescale_matrix(bld_1km, max = 10)  # Use for all versions after PrioritizR 8.0.2.7


# ---------------------------------
# 3.3 Define a range of penalty values
# ---------------------------------
prelim_lower <- -3   # change this for your own data
prelim_upper <- -1  # change this for your own data
prelim_penalty <- round(10^seq(prelim_lower, prelim_upper, length.out = 9), 5)

# print penalty values
print(prelim_penalty)

# ---------------------------------
# 3.4 Pose a 1km/3KM-scale PrioritizR problem with boundary penalties
# ---------------------------------

# CHANGE TO FEAT_STACK_3KM IF WORKING 3KM SCALE
p1 <- problem(cost, feat_stack_1km) %>%
  add_min_set_objective() %>%  # minimize the cost of the solution whilst ensuring that all targets are met
  add_relative_targets(targets) %>%
  add_binary_decisions()

save(p1, file = paste0(scenario_name, "_problem.RData"))

# ---------------------------------
# 3.5 Generate Prelims 
# ---------------------------------
# generate preliminary prioritizations based on each penalty
# note that we specify a relaxed gap and time limit for the solver
# adjust solver as required and according to licence constraints

# CHANGE TO BLD_3KM IF WORKING 3KM SCALE
prelim <- lapply(prelim_penalty, function(x) {
  s <-
    p1 %>%
    add_boundary_penalties(penalty = x, data = bld_1km) %>%
    add_neighbor_constraints(k=2) %>%
    #add_cbc_solver(threads = parallel::detectCores(TRUE) - 1, gap = 0.2, time_limit = 180, verbose = TRUE) %>%
    add_gurobi_solver(threads = parallel::detectCores(TRUE) - 1, gap = 0.2, time_limit = 600, verbose = TRUE) %>%
    solve()
  s <- stack(s)
  names(s) <- paste0("bp_", round(x,5))
  return(s)
}) 

save(prelim, file = paste0(scenario_name, "_prelim.RData")) 

png(paste0(scenario_name, "_prelims.png"), width = 1200, height = 1200, res = 300)
plot(stack(prelim), axes = FALSE, box = FALSE, legend = FALSE)
dev.off()

# ---------------------------------
# 3.6 Generate Solutions
# ---------------------------------

# define a new set of penalty values that ranges below the two preferred prelim values
# This time a linear scale is applied
penalty <- round(seq(prelim_penalty[6], prelim_penalty[8], length.out = 9), 5)

# CHANGE TO BLD_3KM IF WORKING 3KM SCALE
solution <- lapply(penalty, function(x) {
  s <-
    p1 %>%
    add_boundary_penalties(penalty = x, data = bld_1km) %>%
    add_neighbor_constraints(k = 2) %>%
    #add_cbc_solver(threads = parallel::detectCores(TRUE) - 1, gap = 0.1, time_limit = 1200, verbose = TRUE) %>%
    add_gurobi_solver(threads = parallel::detectCores(TRUE) - 1, gap = 0.05, time_limit = 3600, verbose = TRUE) %>%
    solve()
  s <- stack(s)
  names(s) <- paste0("bp_", round(x,5))
  return(s)
})

save(solution, file = paste0(scenario_name, "_solution.RData")) 
solutions <-stack(solution)

#double check working directory
png(paste0(scenario_name, "_solutions.png"), width = 1200, height = 1200, res = 300)
plot(solutions, axes = FALSE, box = FALSE, legend = FALSE)
dev.off()

for (i in 1:nlayers(solutions)) {  
  # Write files  
  soln <- solutions[[i]]  
  writeRaster(soln, paste0(scenario_name, "_bp_", penalty[[i]], ".tif"), format = "GTiff", overwrite = TRUE)}

# for (i in 1:nlayers(solutions)) {
#   # Write files
#   soln <- solutions[[i]]
#   writeRaster(soln, paste0(scenario_name, "_bp_", penalty[[i]]), format = "GTiff")
# }

# ---------------------------------
# 4.0 Evaluate solutions
# ---------------------------------

# ---------------------------------
# 4.1.1 Evaluate feature representations and export to CSV
# ---------------------------------

# Evaluate feature representation in the list of raster solutions
featurereps <- lapply(solution, function(x) {
  eval_feature_representation_summary(p1, x)
})

# Name the tibbles in the list according to the penalty applied
names(featurereps) <- paste0(scenario_name, "_bp_", penalty)

# Examine them in the console
print(featurereps)

# stack the fifth column of all tibbles and bind the data
feat_df <- featurereps %>% 
  map_dfc(~select(.x, relative_held)) %>% 
  set_names(nm = names(featurereps)) %>% 
  mutate(feature = c(featurereps[[1]][[2]]))

# convert to dataframe and assign feature as row names
feat_df <- as.data.frame(feat_df)
rownames(feat_df) <- feat_df$feature
feat_df <- feat_df[, -10]
# reduce to 3 decimal places
feat_df <- round(feat_df,3)
# print the resulting table
feat_df
# Export the summary to CSV
write.csv(feat_df, file=paste0(scenario_name, "_solutions_feature_reps.csv"))

# ---------------------------------
# 4.1.2 Evaluate whether any features have missed their target
# ---------------------------------

# Initialize an empty list to hold the results
OK_solns <- list()

# Loop over each column in soln_df
for (i in 1:ncol(feat_df)) {
  # Extract the values in the current column
  column_values <- feat_df[[i]]
  # Check if all values in the current column are greater than or equal to the corresponding values in targets
  is_OK <- all(column_values >= targets)
  # Append the result to the OK_solns list
  OK_solns[[i]] <- is_OK
}

# Print the resulting list of TRUE/FALSE values
OK_solns

# Iterate through the list to identify those items which are false:

# Initialize an empty list to hold the indices of any FALSE values
false_indices <- list()
# Loop over each element in OK_solns
for (i in seq_along(OK_solns)) {
  # Check if the current element is FALSE
  if (!OK_solns[[i]]) {
    # If it is, append its index to the false_indices list
    false_indices[[length(false_indices) + 1]] <- i
  }
}

# Print the resulting list of false indices
false_indices



# ---------------------------------
# 4.1.3 Check the overlap with ORE
# ---------------------------------

#CHANGE 1KM TO 3KM IF NECESSARY
overlaps <- solution

for (i in seq_along(solution)) {
  summed_raster <- overlay(solution[[i]], ORE_1km, fun = sum)
  overlaps[[i]] <- summed_raster
}
names(overlaps) <- paste0("bp_", round(penalty,5))
plot(stack(overlaps))

png(paste0(scenario_name, "_ORE_overlap.png"), width = 1200, height = 1200, res = 300)
plot(stack(overlaps), axes = FALSE, box = FALSE)
dev.off()



# ---------------------------------
# 4.2 Pick best solution
# ---------------------------------


# ---------------------------------
# 4.2.1 Convert 'solutions' (a list of raster stacks) to a list of rasters
# ---------------------------------

# Create an empty list to store the raster layers
solution_layers <- list()
# Loop over each raster stack in the list and extract the first layer
for (i in seq_along(solution)) {
  solution_layers[[i]] <- solution[[i]][[1]]
}
names(solution_layers) <- names(solutions)

# ---------------------------------
# 4.2.2 Remove layers which did not hit targets
# ---------------------------------

# Get the indices of the elements to keep (i.e., the complement of false_indices)
keep_indices <- setdiff(seq_along(solution_layers), unlist(false_indices))
# Subset solution_layers using the keep_indices
OK_layers <- solution_layers[keep_indices]
# Print the resulting filtered list
OK_layers

# ---------------------------------
# 4.2.3 Generate dataframe of relevant parameters with lapply
# ---------------------------------

soln_summary <- lapply(OK_layers, function(x) {   # To apply to ALL layers then use <lapply(soln_layers, function(x)> instead
  clumps <- clump(x)
  clump_sizes <- freq(clumps)
  total_clumps <- length(unique(clumps))
  orphans <- which(clump_sizes[, 2] <37)
  soln_cost = eval_cost_summary(p1, x)$cost
  boundary <- eval_boundary_summary(p1, x)$boundary
  pu <- eval_n_summary(p1, x)$n
  data.frame(clumps = total_clumps, orphans = length(orphans),
    cost = soln_cost, pu = pu, boundary = boundary, area = pu/cellno)
})

# Subset the penalty list using keep_indices (so as to correctly apply names)
penalty_filtered <- penalty[keep_indices]
penalty_filtered
names(soln_summary) <- paste0("bp_", penalty_filtered)

# Combine the tables into a single dataframe
sum_df<- bind_rows(soln_summary)
sum_df <- as.data.frame(sum_df)
rownames(sum_df) <- names(soln_summary)
sum_df <- round(sum_df,3)
sum_df

# Export the summary to CSV
write.csv(sum_df, file=paste0(scenario_name, "_solution_comparisons_1km.csv"))


# ---------------------------------
# 4.2.4 Pick best solution (based on minimizing orphans and cost)
# ---------------------------------

# Manually drop any layer with unacceptable overlap with ORE
# Change index to any solution that needs dropping
# sum_df <- sum_df %>% slice(-9)
# sum_df

# Get the minimum value of 'orphans' in the 'sum_df'
min_orphans <- min(sum_df$orphans)
# Create a new dataframe 'low_orph_df' that only includes rows where 'orphans' is equal to the minimum or the minimum plus 1
low_orph_df <- subset(sum_df, orphans == min_orphans | orphans == min_orphans + 1)
# Print the resulting dataframe
low_orph_df

# Find the lowest cost in 'low_orph_df' (regardless of the value of 'orphans')
min_cost_all <- min(low_orph_df$cost)
# Calculate the ratio of the lowest cost to 'cost_value'
ratio_all <- min_cost_all / cost_value
ratio_all
# Find the lowest cost in 'low_orph_df' where 'orphans' equals 'min_orphans'
min_cost_min_orphans <- min(low_orph_df$cost[low_orph_df$orphans == min_orphans])
# Calculate the ratio of the lowest cost for 'min_orphans' to 'cost_value' minus 0.02
ratio_min_orphans <- (min_cost_min_orphans / cost_value) - 0.02
ratio_min_orphans

# Determine which ratio is lower and select the corresponding row into 'best_soln_df'
if (ratio_all < ratio_min_orphans) {
  best_soln_df <- low_orph_df[low_orph_df$cost == min_cost_all,]
} else {
  best_soln_df <- low_orph_df[low_orph_df$cost == min_cost_min_orphans & low_orph_df$orphans == min_orphans,]
}

# Print the resulting dataframe and write to a text file
best_soln_df
write.csv(best_soln_df, file = paste0(scenario_name, "_bestsolution.csv"))

# -------------------------------
# 4.2.5 Plot best solution
# -------------------------------
best_soln_index <- which(rownames(sum_df) == rownames(best_soln_df))
best_soln_index
best_solution <- solution_layers[[best_soln_index]]  # Can change manually - just replace best_soln_index with a number

sol_df <- rasterToPoints(best_solution)
sol_df <- data.frame(sol_df)
sol_df[ ,3] <- as.factor(sol_df[ ,3]) 


land <- st_read("../../GIS_layers/IREUK_poly_ITM.shp") #in github folder GIS_layers
sa <- st_read("../../GIS_layers/celtic_sea_AOI.shp") #in github folder GIS_layers

SPA_CelticSeaMarine <- st_read("../../GIS_layers/SPA_CelticSeaMarine.shp") #in github folder GIS_layers
SAC_ITM_2024_CelticSea <- st_read("../../GIS_layers/SAC_ITM_2024_CelticSea.shp") #in github folder GIS_layers

OREmap <- st_read("../../GIS_layers/FinalDmap/Draft_SC_DMAP_Maritime_Areas_WGS84.shp")
extent <- st_bbox(sa)


# Extract the minimum and maximum x and y coordinates
xmin <- extent[1]
xmax <- extent[3]
ymin <- extent[2]
ymax <- extent[4]

# Use the extracted values in coord_sf() function
ggplot() +
  geom_tile(sol_df, mapping = aes(x = x, y = y, fill = sol_df[, 3])) +
  layer_spatial(sa, fill = NA, colour = "grey80") +
  layer_spatial(land, fill = "grey85", colour = NA) +
  layer_spatial(OREmap, fill=NA,colour ="blue") +
  layer_spatial(SAC_ITM_2024_CelticSea, fill=NA, colour ="red") +
  layer_spatial(SPA_CelticSeaMarine, fill=NA, colour = "orange") +
  scale_fill_manual(
    name = "Status",
    values = c("white", "green4"),
    na.value = NA,
    labels = c("Not selected", "Selected")
  ) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank(),
    legend.position = c(0.2, 0.2),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
  )

ggsave(paste0(scenario_name, "_bestsolution.png"), width = 8, height = 6, dpi = 300)

writeRaster(best_solution, paste0(scenario_name, "_bestsolution.tif"), format = "GTiff", overwrite = TRUE)

# ---------------------------------
# 5.0 Evaluate solution costs on a sectoral basis UPDATE THIS CODE TO INCLUDE JUST THE SECTOR LAYERS AND NOT THE COMBINED LAYERS
# ---------------------------------

# Pull in the fishing gear costs according to cost layer used
# setwd("../../ProcessedCostLayers/VesselNoGearCosts")
# setwd("../../ProcessedCostLayers/LandingsGearCosts")
setwd("../../ProcessedCostLayers/UnweightedGearCosts")
gear_tifs <- list.files(pattern = "\\.tif$")
gears <- lapply(gear_tifs, raster)
names(gears) <-gear_tifs
gear_stack <- stack(gears)
names(gear_stack)

# CHANGE IF WORKING AT A 3KM SCALE
#gear_stack <-aggregate(gear_stack, fact=3, fun = sum)


# Initialize a matrix to store the results
result_matrix <- matrix(0, nrow = nlayers(solutions) * nlayers(gear_stack), ncol = 3, 
                        dimnames = list(NULL, c("Solution Layer", "Cost Layer", "Cost Sum")))

setwd("../../Solutions")
setwd(scenario_name)

# Loop through each layer in the "solutions" stack
count <- 0
for (i in 1:nlayers(solutions)) {
  
  # Extract the current "solution" layer and its name
  current_solution <- solutions[[i]]
  current_solution_name <- names(solutions[[i]])
  
  # Loop through each layer in the "costs" stack
  for (j in 1:nlayers(gear_stack)) {
    # Extract the current "cost" layer and its name
    current_cost <- gear_stack[[j]]
    current_cost_name <- names(gear_stack[[j]])
    # multiply rasters to get costs of proposed protected area (areas outside protection are multiplied by zero)
    soln_sect_cost <- current_cost * current_solution
    # Sum the values of the selected cells
    tot_soln_sect_cost <- sum(soln_sect_cost[!is.na(soln_sect_cost)])
    # Get the total of the current cost layer
    cost_lyr_total <- cellStats(gear_stack[[j]], 'sum')
    # calculate cost as a percent of total
    percent_cost <- tot_soln_sect_cost/cost_lyr_total*100
    # Update the result matrix with the current results
    count <- count + 1
    result_matrix[count, ] <- c(current_solution_name, current_cost_name, percent_cost)
  }
}

write.csv(result_matrix, file=paste0(scenario_name, "_sector_cost_eval.csv"))

# Convert data matrix to a data frame
result_df <- data.frame(result_matrix) 
#save.image("ToDate.RData")
result_df$Cost.Sum <- as.numeric(result_df$Cost.Sum)

# Remove "X1km_bp_" and ".tif" from Solution.Layer
result_df$Solution.Layer <- gsub("bp_", "", result_df$Solution.Layer) 

# Convert Solution.Layer to numeric
result_df$Solution.Layer <- as.numeric(result_df$Solution.Layer)

# Plot using ggplot
ggplot(result_df, aes(x = Solution.Layer, y = Cost.Sum, color = Cost.Layer, group = Cost.Layer)) +
  geom_line() + 
  geom_point() +
  labs(x = "Boundary penalty", y = "Cost to Sectors", fill = "Sector") +
  theme_classic()

# Save the plot as a PNG file
ggsave(paste0(scenario_name, "_sector_cost_eval.png"), width = 8, height = 6, dpi = 300)

# ---------------------------------
# 6.0 Write to Scenarios.csv
# ---------------------------------

setwd("../")
final_penalty <- rownames(best_soln_df)
final_penalty <- gsub("bp_", "", final_penalty)

# Write details to CSV file for tracking purposes
tracking <- read.csv(file = "Scenarios_LA.csv")    

tracking[nrow(tracking)+1,] <- list (scenario_group, scenario_name, included_features, cost_name,
                                     targets_used, date, final_penalty, best_soln_df$boundary,
                                     best_soln_df$cost, round(best_soln_df$cost/cost_value, 3),
                                     best_soln_df$clumps, best_soln_df$orphans, best_soln_df$pu, best_soln_df$area) 
tracking

write.csv(tracking, file = "Scenarios_LA.csv", row.names = FALSE)

#END

