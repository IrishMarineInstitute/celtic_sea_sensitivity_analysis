# ---------------------------------------------------------------------------- #
# 1.0 Script to scale cost layers
# Authors: Louise Allcock & Patricia Breen
# 
# Note that this script was written to run locally.  Stakeholders will need
# to change pathways to run. However, all input files are provided with the same file
# names as used herein, and an effort has been made to place these in logical 
# folders such that anyone with basic knowledge of R should be able to follow the 
# script, and generate intermediate and final layers.  The eight final cost layers
# used in the report, as listed in Appendix 5E table A5e.1, are also provided 
# separately in a folder. Thus this script and its input layers is primarily for 
# those who want to do a deep dive into cost layers.
# This script includes some preliminary analyses of data distribution within 
# cost layers which was a step in deciding how best to combine different costs.
# ------------------------------------------------------------------------------

# Libraries
library(raster)
library(sp)
library(viridis)

# Pull in the planning unit reference file
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/PlanningUnits/")
pu <- raster("Planning_units_1km.tif")

# ------------------------------------------------------------------------------
# 2.0 Get Sector layers and standardize
# 
# 2.1 SECTOR: FISHING
# 
# 2.1.1 OFFSHORE INTERNATIONAL FISHING EFFORT LAYERS
# ------------------------------------------------------------------------------ 

setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/Data")
setwd("VMS_Fishing_Effort_International/Combined_International_Effort")

# Import all TIFs from selected directory and make a raster stack
tif_files <- list.files(pattern = "\\.tif$")
int_fishing_effort_layers <- lapply(tif_files, raster)

# Sort the names out
names(int_fishing_effort_layers) <- tif_files
pattern_start <- "^2018_2022_InternationalEffort"
patternXstart <- "^X2018_2022_InternationalEffort"
pattern_end <- "_1km.tif$"
patternXend <- "_1km$"
new_names <- gsub(pattern_start, "", names(int_fishing_effort_layers))
new_names <- gsub(pattern_end, "", new_names)
names(int_fishing_effort_layers) <- new_names
print(names(int_fishing_effort_layers))

for (i in seq_along(int_fishing_effort_layers)) {
  new_name <- gsub(patternXstart, "", names(int_fishing_effort_layers[[i]]))
  new_name <- gsub(patternXend, "", new_name)
  names(int_fishing_effort_layers[[i]]) <- new_name
}
names(int_fishing_effort_layers[[1]])

# # Check difference between 'bottom mobile' and the sum of 'beam' 'otter' and 'dredge'
# bottom_mobile_substack <- stack(int_fishing_effort_layers[[1]], int_fishing_effort_layers[[3]], int_fishing_effort_layers[[4]])
# bottom_mobile <- calc(bottom_mobile_substack, sum, na.rm = FALSE)
# names(bottom_mobile) <- "AggregatedDredgeBeamOtter"
# pairs(stack(bottom_mobile, int_fishing_effort_layers[[2]]))

# Find the index of the BottomMobile Layer and remove that layer from the stack
bottom_mobile_layer <- grep("BottomMobile", names(int_fishing_effort_layers))
int_fishing_effort_layers <- int_fishing_effort_layers[-bottom_mobile_layer] # note to drop from rasterstack use dropLayer(stack_name, unlist(index_variable))
names(int_fishing_effort_layers)

# Find the index of the Longine Layer and remove that layer from the stack (as v limited activity in Celtic Sea)
longline_layer <- grep("LongLines", names(int_fishing_effort_layers))
int_fishing_effort_layers <- int_fishing_effort_layers[-longline_layer] # note to drop from rasterstack use dropLayer(stack_name, unlist(index_variable))
names(int_fishing_effort_layers)

# Create empty dataframe called int_effort_summary with 6 columns
int_effort_summary <- data.frame(
  Gear_type = character(0),
  Max_cell_value = numeric(0),
  Number_nonzero_cells = numeric(0),
  Sum_cell_values = numeric(0),
  Mean_cell_values = numeric(0),
  SD_cell_values = numeric(0)
)

# Loop through layers generating cell stats and png plots saving them locally for further thought/discussion

setwd("/Users/louise/Documents/MPA_Advisory_MarkII/DataChecking/CostLayers")

for (j in seq_along(int_fishing_effort_layers)) {
  layer <- int_fishing_effort_layers[[j]]
  
  hist_obj <- hist(layer[layer != 0], main = "", xlab="Effort", ylab="Frequency")
  print(hist_obj)
  dev.print(png, paste0("int_fishing_effort_", names(layer), "_hist.png"), width = 1200, height = 1200, res = 100)
  dev.off()
  
  print(spplot(layer, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
  dev.print(png, paste0("int_fishing_effort_", names(layer), ".png"), width = 1200, height = 1200, res = 100)
  dev.off()
  
  CellMax <- cellStats(layer, 'max')
  CellNo <- sum(!is.na(getValues(layer)) & getValues(layer) > 0)
  CellSum <- cellStats(layer, 'sum')
  CellMean <- cellStats(layer, 'mean')
  CellSD <- cellStats(layer, 'sd')
  
  new_row <- data.frame(
    Gear_type = names(layer),        
    Max_cell_value = round(CellMax, 2),
    Number_nonzero_cells = CellNo,
    Sum_cell_values = round(CellSum, 0),
    Mean_cell_values = round(CellMean, 3),
    SD_cell_values = round(CellSD, 3)             
  )
  
  # Append the new row of data to the dataframe
  int_effort_summary <- rbind(int_effort_summary, new_row)

  # Tidy layers
  layer[is.na(layer)] <- 0
  layer <- mask(layer, pu)
  int_fishing_effort_layers[[j]] <- layer
  }  

# ------------------------------------------------------------------------------
# Clean VMS line out of the data
# ------------------------------------------------------------------------------
# Layers containing the line
# ottertrawls
# Beamtrawls
# dredges

# Get index of bottomottertrawl layer, and extract layer
ottertrawl_index <- grep("BottomOtterTrawls", names(int_fishing_effort_layers))
ottertrawl_layer <- int_fishing_effort_layers[[ottertrawl_index]]
rows <- 100:200
cols <- 238
ottertrawl_layer[rows, cols] <- ottertrawl_layer[rows, cols] / 2
spplot(ottertrawl_layer, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(ottertrawl_layer, col.regions = viridis_pal(direction = -1), at = seq(0, 10, length.out = 256),
       colorkey = list(at = seq(0, 10, length.out = 10), space = "right"),
       scales = list(draw = TRUE))

# Get index of beamtrawl layer, and extract layer
beamtrawl_index <- grep("BeamTrawls", names(int_fishing_effort_layers))
beamtrawl_layer <- int_fishing_effort_layers[[beamtrawl_index]]
beamtrawl_layer[rows, cols] <- beamtrawl_layer[rows, cols] / 2
rows <- 125:130
beamtrawl_layer[rows, cols] <- beamtrawl_layer[rows, cols] / 2

spplot(beamtrawl_layer, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(beamtrawl_layer, col.regions = viridis_pal(direction = -1), at = seq(0, 10, length.out = 256),
       colorkey = list(at = seq(0, 10, length.out = 10), space = "right"),
       scales = list(draw = TRUE))

# Get index of dredge layer, and extract layer
dredge_index <- grep("Dredges", names(int_fishing_effort_layers))
dredge_layer <- int_fishing_effort_layers[[dredge_index]]
rows <- 67:85
cols <- 237:238
dredge_layer[rows, cols] <- dredge_layer[rows, cols] / 2
spplot(dredge_layer, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(dredge_layer, col.regions = viridis_pal(direction = -1), at = seq(0, 10, length.out = 256),
       colorkey = list(at = seq(0, 10, length.out = 10), space = "right"),
       scales = list(draw = TRUE))

# Write the modified rasters back to the list
int_fishing_effort_layers[[ottertrawl_index]] <- ottertrawl_layer
int_fishing_effort_layers[[beamtrawl_index]] <- beamtrawl_layer
int_fishing_effort_layers[[dredge_index]] <- dredge_layer

# -----------------END OF CLEANING VMS LINE FROM DATA---------------------------

# Create a raster stack from the list of rasters, plot and save image
int_fishing_effort_stack <- stack(int_fishing_effort_layers)
print(pairs(int_fishing_effort_stack))
dev.print(png, "int_fishing_effort_stack_correlations.png", width = 2400, height = 1200, res = 100)
dev.off()
print(spplot(int_fishing_effort_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
dev.print(png, "int_fishing_effort_stack.png", width = 1200, height = 1200, res = 100)
dev.off()

# Save the dataframe to a csv file
write.csv(int_effort_summary, "int_effort_summary.csv", row.names = FALSE)

# Save the cleaned layers for later analysis of solution costs
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/UnweightedGearCosts")

for (i in seq_along(int_fishing_effort_layers)) {
  writeRaster(int_fishing_effort_layers[[i]], paste0("unw_int_",names(int_fishing_effort_layers[[i]]),".tif"), format = "GTiff", overwrite =TRUE)
}


# ------------------------------------------------------------------------------
# Sum offshore international fishing effort layers directly by effort
# ------------------------------------------------------------------------------

unweighted_int_fishing_effort <- calc(int_fishing_effort_stack, sum, na.rm = FALSE)
print(spplot(unweighted_int_fishing_effort, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
dev.print(png, "unweighted_int_fishing_effort.png", width = 1200, height = 1200, res = 100)
dev.off()

# Set path to Github Processed Layer folder
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/IntermediateLayers")
# Write the raster to file
writeRaster(unweighted_int_fishing_effort, "unweighted_int_fishing_effort_1km", format="GTiff", overwrite=TRUE)

# Produce a plot that highlights low values for discussion purposes
maxCell <- cellStats(unweighted_int_fishing_effort, 'max')
maxCell
demo_plot <- unweighted_int_fishing_effort
demo_plot[demo_plot > 30] <- 30

spplot(demo_plot, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)




# ------------------------------------------------------------------------------
#Spread same cost value across each layer, such that each fishing 
# metier is considered of equal value to the country
# ------------------------------------------------------------------------------

# Create an empty raster stack with the same extent and resolution to
# receive standardized cost layers
std_int_fishing_effort_stack <- stack(lapply(1:nlayers(int_fishing_effort_stack), function(i) setValues(unweighted_int_fishing_effort,0)))
names(std_int_fishing_effort_stack) <- names(int_fishing_effort_stack)
spplot(std_int_fishing_effort_stack)

# Iterate through cost stack, standardize each layer, and add to standardized cost stack
for (i in 1:nlayers(int_fishing_effort_stack)) {
  # get the cell values as a vector
  cost_layer <- int_fishing_effort_stack[[i]]
  values <- getValues(cost_layer)
  # calculate the sum of non-NA values
  cellTotals <- sum(values, na.rm=TRUE)
  #scale raster so that total cost = 1000
  values[!is.na(values)] <- values[!is.na(values)] / cellTotals * 1000
  std_int_fishing_effort_stack[[i]] <- setValues(std_int_fishing_effort_stack[[i]], values)
}

# Check rescaling as expected
spplot(std_int_fishing_effort_stack)
cellStats(std_int_fishing_effort_stack, 'sum')

# Sum across the stack to produce a single layer 
std_int_fishing_effort <- calc(std_int_fishing_effort_stack, sum, na.rm = FALSE)
writeRaster(std_int_fishing_effort, "std_int_fishing_effort_7000_1km", format="GTiff", overwrite=TRUE)
# and restandardize to 1000 units
std_int_fishing_effort <- std_int_fishing_effort / nlayers(std_int_fishing_effort_stack)
cellStats(std_int_fishing_effort, 'sum')

# Save a png of the layer locally for further thought/discussion
setwd("/Users/louise/Documents/MPA_Advisory_MarkII/DataChecking/CostLayers")
print(spplot(std_int_fishing_effort, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
dev.print(png, "std_int_fishing_effort.png", width = 1200, height = 1200, res = 100)
dev.off()

# Produce a plot that highlights low values for discussion purposes
demo_plot <- std_int_fishing_effort
demo_plot[demo_plot > 0.2] <- 0.2
spplot(demo_plot, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
maxCell <- cellStats(std_int_fishing_effort, 'max')
maxCell

# Set path to Github Processed Layer folder
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/IntermediateLayers")
# Write the raster to file
writeRaster(std_int_fishing_effort, "std_int_fishing_effort_1000_1km", format="GTiff", overwrite=TRUE)


# --------------------------ENDS---------------------------------------------- #


# ------------------------------------------------------------------------------
# 2.1.2 INSHORE FISHING EFFORT LAYERS
# ------------------------------------------------------------------------------

# Vessel count used as proxy for effort
# Cell value is number of vessels per polygon divided by the number of raster
# cells intersecting the polygon. 

setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/Data/inshore_fishing_footprints/Effort")

nets <- raster("Inshore_net_fishing_effort_1km.tif")
pots <- raster("Inshore_pots_fishing_effort_1km.tif")

names(nets) <- "nets"
names(pots) <- "pots"

inshore_fishing_effort_layers <- c(nets, pots)
names(inshore_fishing_effort_layers[[1]]) <- "nets"
names(inshore_fishing_effort_layers[[2]]) <- "pots"
names(inshore_fishing_effort_layers) <- c("nets", "pots")
print(inshore_fishing_effort_layers)

# Write these to folder for later solution costs analysis
writeRaster(nets, "../../../ProcessedCostLayers/UnweightedGearCosts/inshore_nets.tif", format = "GTiff", overwrite = TRUE)
writeRaster(pots, "../../../ProcessedCostLayers/UnweightedGearCosts/inshore_pots.tif", format = "GTiff", overwrite = TRUE)

# Create empty dataframe called int_effort_summary with 6 columns
inshore_effort_summary <- data.frame(
  Gear_type = character(0),
  Max_cell_value = numeric(0),
  Number_nonzero_cells = numeric(0),
  Sum_cell_values = numeric(0),
  Mean_cell_values = numeric(0),
  SD_cell_values = numeric(0)
)

# Loop through layers generating cell stats and png plots saving them locally for further thought/discussion
setwd("/Users/louise/Documents/MPA_Advisory_MarkII/DataChecking/CostLayers")

for (j in seq_along(inshore_fishing_effort_layers)) {
  layer <- inshore_fishing_effort_layers[[j]]
  
  hist_obj <- hist(layer[layer != 0], main = "", xlab="Effort", ylab="Frequency")
  print(hist_obj)
  dev.print(png, paste0("inshore_fishing_effort_", names(layer), "_hist.png"), width = 1200, height = 1200, res = 100)
  dev.off()
  
  print(spplot(layer, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
  dev.print(png, paste0("inshore_fishing_effort_", names(layer), ".png"), width = 1200, height = 1200, res = 100)
  dev.off()
  
  CellMax <- cellStats(layer, 'max')
  CellNo <- sum(!is.na(getValues(layer)) & getValues(layer) > 0)
  CellSum <- cellStats(layer, 'sum')
  CellMean <- cellStats(layer, 'mean')
  CellSD <- cellStats(layer, 'sd')
  
  new_row <- data.frame(
    Gear_type = names(layer),        
    Max_cell_value = round(CellMax, 2),
    Number_nonzero_cells = CellNo,
    Sum_cell_values = round(CellSum, 0),
    Mean_cell_values = round(CellMean, 3),
    SD_cell_values = round(CellSD, 3)             
  )
  
  # Append the new row of data to the dataframe
  inshore_effort_summary <- rbind(inshore_effort_summary, new_row)
  
  # Tidy layers
  layer[is.na(layer)] <- 0
  layer <- mask(layer, pu)
  inshore_fishing_effort_layers[[j]] <- layer
}  

# Create a raster stack from the list of rasters, plot and save image
inshore_fishing_effort_stack <- stack(inshore_fishing_effort_layers)
print(pairs(inshore_fishing_effort_stack))
dev.print(png, "inshore_fishing_effort_stack_correlations.png", width = 2400, height = 1200, res = 100)
dev.off()
print(spplot(inshore_fishing_effort_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
dev.print(png, "inshore_fishing_effort_stack.png", width = 1200, height = 1200, res = 100)
dev.off()

# Save the dataframe to a csv file
write.csv(inshore_effort_summary, "inshore_effort_summary.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Sum inshore fishing effort layers directly by effort
# ------------------------------------------------------------------------------

unweighted_inshore_fishing_effort <- calc(inshore_fishing_effort_stack, sum, na.rm = FALSE)
print(spplot(unweighted_inshore_fishing_effort, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
dev.print(png, "unweighted_inshore_fishing_effort.png", width = 1200, height = 1200, res = 100)
dev.off()

# Set path to Github Processed Layer folder
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/IntermediateLayers")
# Write the raster to file
writeRaster(unweighted_inshore_fishing_effort, "unweighted_inshore_fishing_effort_1km", format="GTiff", overwrite=TRUE)

# ------------------------------------------------------------------------------
# Spread same cost value across each layer, such that each fishing 
# metier is considered of equal value to the country
# ------------------------------------------------------------------------------

# Create an empty raster stack with the same extent and resolution to
# receive standardized cost layers
std_inshore_fishing_effort_stack <- stack(lapply(1:nlayers(inshore_fishing_effort_stack), function(i) setValues(unweighted_inshore_fishing_effort,0)))
names(std_inshore_fishing_effort_stack) <- names(inshore_fishing_effort_stack)
spplot(std_inshore_fishing_effort_stack)

# Iterate through cost stack, standardize each layer, and add to standardized cost stack
for (i in 1:nlayers(inshore_fishing_effort_stack)) {
  # get the cell values as a vector
  cost_layer <- inshore_fishing_effort_stack[[i]]
  values <- getValues(cost_layer)
  # calculate the sum of non-NA values
  cellTotals <- sum(values, na.rm=TRUE)
  #scale raster so that total cost = 1000
  values[!is.na(values)] <- values[!is.na(values)] / cellTotals * 1000
  std_inshore_fishing_effort_stack[[i]] <- setValues(std_inshore_fishing_effort_stack[[i]], values)
}

# Check rescaling as expected
spplot(std_inshore_fishing_effort_stack)
cellStats(std_inshore_fishing_effort_stack, 'sum')

# Sum across the stack to produce a single layer
std_inshore_fishing_effort <- calc(std_inshore_fishing_effort_stack, sum, na.rm = FALSE)
writeRaster(std_inshore_fishing_effort, "std_inshore_fishing_effort_2000_1km", format="GTiff", overwrite=TRUE)
# and restandardize to 1000 units
std_inshore_fishing_effort <- std_inshore_fishing_effort / nlayers(std_inshore_fishing_effort_stack)
cellStats(std_inshore_fishing_effort, 'sum')

# Save a png of the layer locally for further thought/discussion
setwd("/Users/louise/Documents/MPA_Advisory_MarkII/DataChecking/CostLayers")
print(spplot(std_inshore_fishing_effort, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
dev.print(png, "std_inshore_fishing_effort.png", width = 1200, height = 1200, res = 100)
dev.off()

# Set path to Github Processed Layer folder
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/IntermediateLayers")
# Write the raster to file
writeRaster(std_inshore_fishing_effort, "std_inshore_fishing_effort_1000_1km", format="GTiff", overwrite=TRUE)


# ------------------------------------------------------------------------------
# 2.1.3 OFFSHORE IRISH LANDINGS VALUE
# ------------------------------------------------------------------------------

# Raster value is euro (all species combined; irish primarily) -  value will  
# generally only be for Irish vessels as we usually do not have logbook data
# for foreign vessels

# Can simply pull in all layer and sum.
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/Data/VMS_Fishing_Value_Irish_primarily")

# Import all TIFs from selected directory and make a raster stack
tif_files <- list.files(pattern = "\\.tif$")
fishing_value_layers <- lapply(tif_files, raster)

for (i in seq_along(fishing_value_layers)) {
  layer <- fishing_value_layers[[i]]
  layer[is.na(layer)] <- 0
  layer[is.na(pu)] <- NA
  fishing_value_layers[[i]] <- layer
}

fishing_value_stack <- stack(fishing_value_layers)
names(fishing_value_stack) <- tif_files
spplot(fishing_value_stack, col.regions = viridis_pal(direction = -1), main = "Fishing value", axes = FALSE, box = FALSE)

fishing_value <- calc(fishing_value_stack, sum, rm.na=FALSE)
fishing_value <- fishing_value / 5 #Because data are summed from 5 years
spplot(fishing_value, col.regions = viridis_pal(direction = -1), main = "Fishing value", axes = FALSE, box = FALSE)
demo_plot <- fishing_value
demo_plot[demo_plot > 10000] <- 10000
spplot(demo_plot, col.regions = viridis_pal(direction = -1), main = "Fishing value, diff scale", axes = FALSE, box = FALSE)

# Write raster to file
# Set path to Github Processed Layer folder
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/IntermediateLayers")
# Write the raster to file
writeRaster(fishing_value, "offshore_fishing_value_1km", format="GTiff", overwrite=TRUE)

# ------------------------------------------------------------------------------
# 2.1.4 OFFSHORE IRISH LANDINGS VALUE - PREPPING BY GEAR FOR GEAR COST ANALYSES
# ------------------------------------------------------------------------------

setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/Data/VMS_Fishing_Value_Irish_primarily")

# Import all TIFs from selected directory and make a raster stack

gear_list <- c("BeamTrawls", "Dredges", "PelagicTrawls", "Seines", "BottomOtterTrawls", "Pots", "GillNets")

for (j in seq_along(gear_list)) {
  tif_files <- list.files(pattern = paste0("\\", gear_list[[j]], ".tif$"))
  fishing_value_layers <- lapply(tif_files, raster)
  for (i in seq_along(fishing_value_layers)) {
    layer <- fishing_value_layers[[i]]
    layer[is.na(layer)] <- 0
    layer[is.na(pu)] <- NA
    fishing_value_layers[[i]] <- layer
}
  fishing_value_stack <- stack(fishing_value_layers)
  fishing_value <- calc(fishing_value_stack, sum, rm.na=FALSE)
  fishing_value <- fishing_value / 5 #Because data are summed from 5 years
  writeRaster(fishing_value, paste0("../../ProcessedCostLayers/LandingsGearCosts/Landings",gear_list[[j]]), format="GTiff", overwrite=TRUE)
}

# Check TIFs
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/LandingsGearCosts/")
tif_files <- list.files(pattern = "\\.tif$")
landing_layers <- lapply(tif_files, raster)
plot(stack(landing_layers))
# ------------------------------------------------------------------------------
# 2.1.4 OFFSHORE VESSEL NUMBERS
# ------------------------------------------------------------------------------

# Can simply pull in all layer and sum.
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/Data/VMS_Fishing_NumberOfVessels")

# Import all TIFs from selected directory and make a raster stack
tif_files <- list.files(pattern = "\\.tif$")
fishing_vessel_layers <- lapply(tif_files, raster)

for (i in seq_along(fishing_vessel_layers)) {
  layer <- fishing_vessel_layers[[i]]
  layer[is.na(layer)] <- 0
  layer[is.na(pu)] <- NA
  fishing_vessel_layers[[i]] <- layer
}

# There is something strange about two of the otter trawl layers (2018 & 2019)
# Kellie reports null values showing as 128 so need to fix that.
fishing_vessel_layers[[2]][fishing_vessel_layers[[2]]==128]<-0
fishing_vessel_layers[[9]][fishing_vessel_layers[[9]]==128]<-0
fishing_vessel_layers[[2]][is.na(pu)]<-NA
fishing_vessel_layers[[9]][is.na(pu)]<-NA

fishing_vessel_stack <- stack(fishing_vessel_layers)
names(fishing_vessel_stack) <- tif_files
spplot(fishing_vessel_stack, col.regions = viridis_pal(direction = -1), main = "Fishing vessel numbers", axes = FALSE, box = FALSE)

fishing_vessel <- calc(fishing_vessel_stack, sum, rm.na=FALSE)
fishing_vessel <- fishing_vessel / 5 # to give average number of vessels per year
spplot(fishing_vessel, col.regions = viridis_pal(direction = -1), main = "Fishing vessel numbers", axes = FALSE, box = FALSE)
demo_plot <- fishing_vessel
demo_plot[demo_plot > 15] <- 15
spplot(demo_plot, col.regions = viridis_pal(direction = -1), main = "Fishing vessel numbers, diff scale", axes = FALSE, box = FALSE)


# Write raster to file
# Set path to Github Processed Layer folder
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/IntermediateLayers")
# Write the raster to file
writeRaster(fishing_vessel, "offshore_fishing_vesselNo_1km", format="GTiff", overwrite=TRUE)



# ------------------------------------------------------------------------------
# 2.1.5 OFFSHORE VESSEL NUMBERS - PREPPING BY GEAR FOR GEAR COST ANALYSES
# ------------------------------------------------------------------------------


setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/Data/VMS_Fishing_NumberOfVessels")

# Import all TIFs from selected directory and make a raster stack

gear_list <- c("BeamTrawls", "Dredges", "PelagicTrawls", "Seines", "BottomOtterTrawls", "Pots", "GillNets")

for (j in seq_along(gear_list)) {
  tif_files <- list.files(pattern = paste0("\\", gear_list[[j]], ".tif$"))
  fishing_value_layers <- lapply(tif_files, raster)
  for (i in seq_along(fishing_value_layers)) {
    layer <- fishing_value_layers[[i]]
    layer[is.na(layer)] <- 0
    layer[is.na(pu)] <- NA
    fishing_value_layers[[i]] <- layer
  }
  fishing_value_stack <- stack(fishing_value_layers)
  fishing_value <- calc(fishing_value_stack, sum, rm.na=FALSE)
  fishing_value <- fishing_value / 5 #Because data are summed from 5 years
  writeRaster(fishing_value, paste0("../../ProcessedCostLayers/VesselNoGearCosts/VesselNo",gear_list[[j]]), format="GTiff", overwrite=TRUE)
}

# Check TIFs
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/VesselNoGearCosts")
tif_files <- list.files(pattern = "\\.tif$")
landing_layers <- lapply(tif_files, raster)
plot(stack(landing_layers))

# ------------------------------------------------------------------------------
# 2.2 SECTOR: SHIPPING CELTIC SEA 2024
# 
# Notes: MPA Advisory Group decision to use 4 layers available from 
# EMODnet giving vessel density for different sectors
# In folders as follows
# Tanker
# Passenger
# Military_and_law
# Cargo
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 2.2.1 Pull in shipping cost layers by section
# ------------------------------------------------------------------------------

folder <- list("Tanker", "Passenger", "Military_and_law", "Cargo")

setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/Data/Vessel_density")
setwd("Tanker")
# Raster value is mean vessel density within each 1km grid cell
# Import all TIFs from selected directory and make a raster stack
tif_files <- list.files(pattern = "\\.tif$")
tanker_layers <- lapply(tif_files, raster)
# Set the names of the list elements to the file names
tanker_stack <- stack(tanker_layers)
tanker <- calc(tanker_stack, sum, na.rm = FALSE)
spplot(tanker, col.regions = viridis_pal(direction = -1)(100), axes = FALSE, box = FALSE)
CellMax <- cellStats(tanker, 'max')

setwd("../Passenger")
# Raster value is mean vessel density within each 1km grid cell
# Import all TIFs from selected directory and make a raster stack
tif_files <- list.files(pattern = "\\.tif$")
passenger_layers <- lapply(tif_files, raster)
# Set the names of the list elements to the file names
passenger_stack <- stack(passenger_layers)
passenger <- calc(passenger_stack, sum, na.rm = FALSE)
spplot(passenger, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
CellMax <- cellStats(passenger, 'max')

setwd("../Military_and_law")
# Raster value is mean vessel density within each 1km grid cell
# Import all TIFs from selected directory and make a raster stack
tif_files <- list.files(pattern = "\\.tif$")
military_layers <- lapply(tif_files, raster)
# Set the names of the list elements to the file names
military_stack <- stack(military_layers)
military <- calc(military_stack, sum, na.rm = FALSE)
spplot(military, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
CellMax <- cellStats(military, 'max')

setwd("../Cargo")
# Raster value is mean vessel density within each 1km grid cell
# Import all TIFs from selected directory and make a raster stack
tif_files <- list.files(pattern = "\\.tif$")
cargo_layers <- lapply(tif_files, raster)
# Set the names of the list elements to the file names
cargo_stack <- stack(cargo_layers)
cargo <- calc(cargo_stack, sum, na.rm = FALSE)
spplot(cargo, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
CellMax <- cellStats(cargo, 'max')

# Create stack of layers, sum across stack, check basic stack parameters
shipping_cost_stack <- stack(tanker, passenger, military, cargo)
names(shipping_cost_stack) <- folder
spplot(shipping_cost_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
shipping_cost <- calc(shipping_cost_stack, sum, na.rm = FALSE)
names(shipping_cost) <- "Shipping_costs"


# ------------------------------------------------------------------------------
# 2.2.2 Log the shipping cost layer to deal with high values around ports
# ------------------------------------------------------------------------------
shipping_cost_nozero <- shipping_cost + 1
log10_function <- function(x) {
  log10(x)
}
shipping_cost_log10 <- calc(shipping_cost_nozero, log10_function)

# ------------------------------------------------------------------------------
# 2.2.2 Standardize logged and unlogged layers to 1000 units
# ------------------------------------------------------------------------------
cellTotals <- cellStats(shipping_cost, sum)
shipping_cost[!is.na(shipping_cost)] <- shipping_cost[!is.na(shipping_cost)] / cellTotals * 1000
cellStats(shipping_cost, "sum")
cellStats(shipping_cost, "max")
hist_obj <- hist(shipping_cost[shipping_cost >=1], main = "shipping cost", xlab="Cell value", ylab="Frequency")
spplot(shipping_cost, col.regions = viridis_pal(direction = -1), main = "shipping cost", axes = FALSE, box = FALSE)


cellTotals <- cellStats(shipping_cost_log10, sum)
shipping_cost_log10[!is.na(shipping_cost_log10)] <- shipping_cost_log10[!is.na(shipping_cost_log10)] / cellTotals * 1000
cellStats(shipping_cost_log10, "sum")
cellStats(shipping_cost_log10, "max")
hist_obj <- hist(shipping_cost_log10[shipping_cost_log10 >0], main = "shipping cost Log10", xlab="Cell value", ylab="Frequency")
spplot(shipping_cost_log10, col.regions = viridis_pal(direction = -1), main = "shipping cost Log10", axes = FALSE, box = FALSE)

# ------------------------------------------------------------------------------
# 2.2.3 Write standardized cost layers to file
# ------------------------------------------------------------------------------

# Save png locally for further thought/discussion
setwd("/Users/louise/Documents/MPA_Advisory_MarkII/DataChecking/CostLayers")
print(spplot(shipping_cost, col.regions = viridis_pal(direction = -1), main = "shipping cost", axes = FALSE, box = FALSE))
dev.print(png, "shipping_1000_1km.png", width = 1200, height = 1200, res = 100)
dev.off()

print(spplot(shipping_cost_log10, col.regions = viridis_pal(direction = -1), main = "shipping cost Log10", axes = FALSE, box = FALSE))
dev.print(png, "shipping_log10_1000_1km.png", width = 1200, height = 1200, res = 100)
dev.off()

# Set path to Github Processed Layer folder
setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/IntermediateLayers")
# Write the raster to file
writeRaster(shipping_cost, "shipping_1000_1km", format="GTiff", overwrite=TRUE)
writeRaster(shipping_cost_log10, "shipping_log10_1000_1km", format="GTiff", overwrite=TRUE)

# ------------------------------------------------------------------------------
# 2.3 SECTOR: Offshore Renewal Energy (ORE)
# ------------------------------------------------------------------------------
# 
# Notes: Dmap senario2
# Note that Area A is the area for the first ORESS auction and as such is highest priority
# to DECC.  Area D is least priority given its position furthest offshore and noting its 
# closeness to the SPA 
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 2.3.2 MAKE VARIABLE ORE LAYER WITH NEW DMAP
# ------------------------------------------------------------------------------

setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/IntermediateLayers")

ORE_even_1km <- raster("../../Data/DMAP/FINAL_SC_DMAP_Maritime_Areas_pa.tif")
names(ORE_even_1km) <- "ORE_even_1km"

ORE_var_1km <- clump(ORE_even_1km)
spplot(ORE_var_1km)
ORE_var_1km[ORE_var_1km==1] <- 5
ORE_var_1km[ORE_var_1km==2] <- 1
ORE_var_1km[ORE_var_1km==4] <- 2
ORE_var_1km[ORE_var_1km==5] <- 4
spplot(ORE_var_1km)

ORE_var_1km[ORE_even_1km==0] <- 0
spplot(ORE_var_1km)

writeRaster(ORE_even_1km, "ORE_even_1km", format = "GTiff")
writeRaster(ORE_var_1km, "ORE_variable_1km", format = "GTiff")



# ------------------------------------------------------------------------------
# 3.0 Create Combined Layers
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 3.1 Pull in the saved prepped layers and generate some stats
# ------------------------------------------------------------------------------

setwd("/Users/Louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/ProcessedCostLayers/IntermediateLayers")

unweighted_int_fishing_effort <- raster("unweighted_int_fishing_effort_1km.tif")
unweighted_inshore_fishing_effort <- raster("unweighted_inshore_fishing_effort_1km.tif")
offshore_fishing_value <- raster("offshore_fishing_value_1km.tif")
offshore_fishing_vesselNo <- raster("offshore_fishing_vesselNo_1km.tif")
std_int_fishing_effort_7000 <- raster("std_int_fishing_effort_7000_1km.tif")
std_inshore_fishing_effort_2000 <- raster("std_inshore_fishing_effort_2000_1km.tif")
shipping_1000 <- raster("shipping_1000_1km.tif")
shipping_log10_1000 <- raster("shipping_log10_1000_1km.tif")
ORE_variable <- raster("ORE_variable_1km.tif")
ORE_even <- raster("ORE_even_1km.tif")

cost_list <- c(unweighted_int_fishing_effort, unweighted_inshore_fishing_effort,
               offshore_fishing_value, offshore_fishing_vesselNo,
               std_int_fishing_effort_7000, std_inshore_fishing_effort_2000, 
               shipping_1000, shipping_log10_1000, ORE_variable, ORE_even)
names(cost_list) <- c("unweighted_int_fishing_effort", "unweighted_inshore_fishing_effort", 
					  "offshore_fishing_value", "offshore_fishing_vesselNo", 
                      "std_int_fishing_effort_7000", "std_inshore_fishing_effort_2000", 
                      "shipping_1000", "shipping_log10_1000", "ORE_variable", "ORE_even")

# Create empty dataframe called costs_summary with 6 columns
costs_summary <- data.frame(
  cost_type = character(0),
  Max_cell_value = numeric(0),
  Sum_cell_values = numeric(0),
  Median_cell_values = numeric(0),
  Mean_cell_values = numeric(0),
  SD_cell_values = numeric(0)
)

# Loop through layers generating cell stats and png plots saving them locally for further thought/discussion

setwd("/Users/louise/Documents/MPA_Advisory_MarkII/DataChecking/CostLayers/Combining")

for (j in seq_along(cost_list)) {
  layer <- cost_list[[j]]
  
  hist_obj <- hist(layer[layer != 0], main = "", xlab="Effort", ylab="Frequency")
  print(hist_obj)
  dev.print(png, paste0(names(layer), "_hist.png"), width = 1200, height = 1200, res = 100)
  dev.off()
  
  print(spplot(layer, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
  dev.print(png, paste0(names(layer), ".png"), width = 1200, height = 1200, res = 100)
  dev.off()
  
  cell_values <- as.vector(getValues(layer))
  cell_values <- cell_values[!is.na(cell_values)]
  cell_values <- cell_values[cell_values != 0]
  CellMedian <- median(cell_values)
  
  CellMax <- cellStats(layer, 'max')
  CellNo <- sum(!is.na(getValues(layer)) & getValues(layer) > 0)
  CellSum <- cellStats(layer, 'sum')
  CellMean <- cellStats(layer, 'mean')
  CellSD <- cellStats(layer, 'sd')
  
  new_row <- data.frame(
    Gear_type = names(layer),        
    Max_cell_value = round(CellMax, 3),
    Sum_cell_values = round(CellSum, 0),
    Median_cell_values = round(CellMedian, 3),
    Mean_cell_values = round(CellMean, 3),
    SD_cell_values = round(CellSD, 3)             
  )
  
  # Append the new row of data to the dataframe
  costs_summary <- rbind(costs_summary, new_row)
  
  # Tidy layers
  layer[is.na(layer)] <- 0
  layer <- mask(layer, pu)
  cost_list[[j]] <- layer
}  

# Save the dataframe to a csv file
write.csv(costs_summary, "costs_summary.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 3.2 Unweighted EFFORT layers
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 3.2.1 Unweighted Fishing
# ------------------------------------------------------------------------------

# Multiplying by 96 to bring median cell value of inshore fishing equivalent
# to median cell value of international offshore fishing t
unweighted_inshore_fishing_effortx96 <- unweighted_inshore_fishing_effort * 96

unweighted_fishing_international <- stack(unweighted_inshore_fishing_effortx96, unweighted_int_fishing_effort)
unweighted_fishing_international <- calc(unweighted_fishing_international, sum, na.rm = FALSE)
names(unweighted_fishing_international) <- "fishing_effort_allfleet"

print(spplot(unweighted_fishing_international, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE))
dev.print(png, "Unweighted_fishing_international_1km.png", width = 1200, height = 1200, res = 100)
dev.off()

writeRaster(unweighted_fishing_international, "unweighted_fishing_international_1km", format="GTiff", overwrite=TRUE)
writeRaster(unweighted_fishing_ireland, "unweighted_fishing_ireland_1km", format="GTiff", overwrite=TRUE)

# ------------------------------------------------------------------------------
# 3.2.2 Add shipping and ORE to Unweighted fishing
# ------------------------------------------------------------------------------

# Based on summary stats generated shipping max values similar to those of 
# other layers, such that the areas important to shipping i.e., ports will be
# maximally protected by the cost layer.  Thus can add directly.

# For the ORE layers, it is reasonable to set median values similar to 
# other layers.  

# Calculate median quantiles for unweighted fishing layer (used inshore
# as layer is flatter and so distribution of cell values more similar)

cell_values <- as.vector(getValues(unweighted_fishing_international))
cell_values <- cell_values[!is.na(cell_values)]
cell_values <- cell_values[cell_values != 0]
CellMedian <- round(median(cell_values), 3)

# choose quantiles and apply to ORE layers
ORE1_quantile <- 0.5
ORE2_quantile <- 0.7
ORE3_quantile <- 0.9

ORE1 <- round(quantile(cell_values, ORE1_quantile), 3)
ORE2 <- round(quantile(cell_values, ORE2_quantile), 3)
ORE3 <- round(quantile(cell_values, ORE3_quantile), 3)

OREvar_weighted <- ORE_variable
OREeven_weighted <- ORE_even
# Reset ORE_variable layer such that sectors B & C of median value, while
# sector A upper quartile value, and D lower quartile value
OREvar_weighted[OREvar_weighted == 1] <- ORE1
OREvar_weighted[OREvar_weighted == 2] <- ORE2
OREvar_weighted[OREvar_weighted == 3] <- ORE2
OREvar_weighted[OREvar_weighted == 4] <- ORE3

OREeven_weighted[OREeven_weighted == 1] <- ORE2

# weight shipping footprint to balance max and median

adj_shipping_log10_1000 <- shipping_log10_1000 * 50

spplot(adj_shipping_log10_1000, col.regions = viridis_pal(direction = -1), main = 'adjusted logged shipping',axes = FALSE, box = FALSE)
spplot(OREeven_weighted, col.regions = viridis_pal(direction = -1), main = 'OREeven_weighted',axes = FALSE, box = FALSE)
spplot(OREvar_weighted, col.regions = viridis_pal(direction = -1), main = 'OREvar_weighted',axes = FALSE, box = FALSE)
spplot(unweighted_fishing_international, col.regions = viridis_pal(direction = -1), main = 'unweighted_fishing_international',axes = FALSE, box = FALSE)

# cell_values <- as.vector(getValues(unweighted_fishing_ireland))
# cell_values <- cell_values[!is.na(cell_values)]
# cell_values <- cell_values[cell_values != 0]
# CellMedian <- median(cell_values)
# CellMedian

# Add shipping and ORE layer to international unweighted fishin

unweighted_international_varORE_stack <- stack(OREvar_weighted, adj_shipping_log10_1000, unweighted_fishing_international)
unweighted_international_varORE_cost <- calc(unweighted_international_varORE_stack, sum, na.rm = FALSE)

unweighted_international_evenORE_stack <- stack(OREeven_weighted, adj_shipping_log10_1000, unweighted_fishing_international)
unweighted_international_evenORE_cost <- calc(unweighted_international_evenORE_stack, sum, na.rm = FALSE)

unweighted_international_noORE_stack <- stack(adj_shipping_log10_1000, unweighted_fishing_international)
unweighted_international_noORE_cost <- calc(unweighted_international_noORE_stack, sum, na.rm = FALSE)

spplot(unweighted_international_varORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(unweighted_international_varORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "effort_international_varORE_cost")
spplot(unweighted_international_evenORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(unweighted_international_evenORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "effort_international_evenORE_cost")
spplot(unweighted_international_noORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(unweighted_international_noORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "effort_international_noORE_cost")

# Produce plots that highlights low values for discussion purposes
demo_plot1 <- unweighted_international_varORE_cost
demo_plot2 <- unweighted_international_evenORE_cost
demo_plot3 <- unweighted_international_noORE_cost

demo_plot1[demo_plot1 >45] <- 45
demo_plot2[demo_plot2 >45] <- 45
demo_plot3[demo_plot3 >45] <- 45

spplot(demo_plot1, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "effort_international_varORE_cost (adjusted axes)")
spplot(demo_plot2, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "effort_international_evenORE_cost (adjusted axes)")
spplot(demo_plot3, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "effort_international_noORE_cost (adjusted axes)")

writeRaster(unweighted_international_varORE_cost, "unw_int_varORE_cost_1km", format="GTiff", overwrite=TRUE)
writeRaster(unweighted_international_evenORE_cost, "unw_int_evenORE_cost_1km", format="GTiff", overwrite=TRUE)
writeRaster(unweighted_international_noORE_cost, "unw_int_noORE_cost_1km", format="GTiff", overwrite=TRUE)


# ------------------------------------------------------------------------------
# 3.3 FISHING VALUES
# ------------------------------------------------------------------------------
# Need to make combine fishing layer
# Divide inshore effott layer by total sum of cell values and multiply by
# 13,000,000 which is the estimated value of The Celtic Sea inshore catch
# from these fisheries
# total sum of inshore fishing effort layer is 283 (from CSV)

inshore_fishing_value <- unweighted_inshore_fishing_effort / 283 * 13000000
spplot(inshore_fishing_value, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "inshore_fishing_value")
spplot(offshore_fishing_value, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "offshore_fishing_value")

fishing_value_stack <- stack(inshore_fishing_value, offshore_fishing_value)
fishing_value <- calc(fishing_value_stack, sum)

spplot(fishing_value, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "combined_fishing_value")

demo_plot1 <- fishing_value
demo_plot1[demo_plot1 >10000] <- 10000
spplot(demo_plot1, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "combined_fishing_value")

# Calculate cell median and quantiles for the inshore fishing layer (because it is flat
# and therefore has the most similar distribution to the ORE layer)
cell_values <- as.vector(getValues(inshore_fishing_value))
cell_values <- cell_values[!is.na(cell_values)]
cell_values <- cell_values[cell_values != 0]
CellMedian <- round(median(cell_values), 3)

# choose quantiles and apply to ORE layers
ORE1_quantile <- 0.5
ORE2_quantile <- 0.7
ORE3_quantile <- 0.9

ORE1 <- round(quantile(cell_values, ORE1_quantile), 3)
ORE2 <- round(quantile(cell_values, ORE2_quantile), 3)
ORE3 <- round(quantile(cell_values, ORE3_quantile), 3)

OREvar_weighted <- ORE_variable
OREeven_weighted <- ORE_even
# Reset ORE_variable layer such that sectors B & C of median value, while
# sector A upper quartile value, and D lower quartile value
OREvar_weighted[OREvar_weighted == 1] <- ORE1
OREvar_weighted[OREvar_weighted == 2] <- ORE2
OREvar_weighted[OREvar_weighted == 3] <- ORE2
OREvar_weighted[OREvar_weighted == 4] <- ORE3

OREeven_weighted[OREeven_weighted == 1] <- ORE2

# weight shipping footprint to balance max and median

adj_shipping_log10_1000 <- shipping_log10_1000 * 40000


spplot(adj_shipping_log10_1000, col.regions = viridis_pal(direction = -1), main = 'adjusted logged shipping',axes = FALSE, box = FALSE)
spplot(OREeven_weighted, col.regions = viridis_pal(direction = -1), main = 'OREeven_weighted',axes = FALSE, box = FALSE)
spplot(OREvar_weighted, col.regions = viridis_pal(direction = -1), main = 'OREvar_weighted',axes = FALSE, box = FALSE)
spplot(fishing_value, col.regions = viridis_pal(direction = -1), main = 'fishing_value',axes = FALSE, box = FALSE)


# Add shipping and ORE layer to fishing value

value_varORE_stack <- stack(OREvar_weighted, adj_shipping_log10_1000, fishing_value)
value_varORE_cost <- calc(value_varORE_stack, sum, na.rm = FALSE)

value_evenORE_stack <- stack(OREeven_weighted, adj_shipping_log10_1000, fishing_value)
value_evenORE_cost <- calc(value_evenORE_stack, sum, na.rm = FALSE)

value_noORE_stack <- stack(adj_shipping_log10_1000, fishing_value)
value_noORE_cost <- calc(value_noORE_stack, sum, na.rm = FALSE)

spplot(value_varORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(value_varORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "value_varORE_cost")
spplot(value_evenORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(value_evenORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "value_evenORE_cost")
spplot(value_noORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(value_noORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "value_noORE_cost")

# Produce plots that highlights low values for discussion purposes
demo_plot1 <- value_varORE_cost
demo_plot2 <- value_evenORE_cost
demo_plot3 <- value_noORE_cost

demo_plot1[demo_plot1 >10000] <- 10000
demo_plot2[demo_plot2 >10000] <- 10000
demo_plot3[demo_plot3 >10000] <- 10000

spplot(demo_plot1, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "value_varORE_cost (adjusted axes)")
spplot(demo_plot2, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "value_evenORE_cost (adjusted axes)")
spplot(demo_plot3, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "value_noORE_cost (adjusted axes)")

writeRaster(value_varORE_cost, "value_varORE_cost_1km", format="GTiff", overwrite=TRUE)
writeRaster(value_evenORE_cost, "value_evenORE_cost_1km", format="GTiff", overwrite=TRUE)
writeRaster(value_noORE_cost, "value_noORE_cost_1km", format="GTiff", overwrite=TRUE)


# ------------------------------------------------------------------------------
# 3.4 FISHING VESSELS (NUMBER OF)
# ------------------------------------------------------------------------------

# 41 and 242 nets and pots fishing inshore (calculated from those layers)
# offshore fleet includes
# BeamTrawls 52
# BottomOtterTrawls 285
# Dredges 18
# GillNets 71
# Pelagic 74
# Pots 13
# Seines 20
# Total 533
# From CSV produced earlier: 0.047 is median of inshore effort layer
# From CSV produced earlier: 2 is median of offshore_vesselNo layer


# Set fleet sizes
inshoreFleetSize <- 283
offshoreFleetSize <- 533

# Examine available layers
spplot(offshore_fishing_vesselNo, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "offshore_fishing_vesselNo")
hist_obj <- hist(offshore_fishing_vesselNo[offshore_fishing_vesselNo != 0], xlab="Number of vessels", ylab="Frequency", main = "offshore_fishing_vesselNo")
print(hist_obj)

spplot(unweighted_inshore_fishing_effort, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "inshore_fishing_effort")
hist_obj <- hist(unweighted_inshore_fishing_effort[unweighted_inshore_fishing_effort != 0], xlab="Number of vessels", ylab="Frequency", main = "inshore_fishing_effort")
print(hist_obj)

# Adjust inshore effort layer according to medians and number of boats
inshore_fishing_vesselNo <- unweighted_inshore_fishing_effort * 2/0.047 * inshoreFleetSize/offshoreFleetSize

# Examine new layer
spplot(inshore_fishing_vesselNo, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "inshore_fishing_vesselNo")
hist_obj <- hist(inshore_fishing_vesselNo[offshore_fishing_vesselNo != 0], xlab="Number of vessels", ylab="Frequency", main = "inshore_fishing_vesselNo")
print(hist_obj)

# Stack and combine layers and check visually
vesselNo_stack <- stack(inshore_fishing_vesselNo, offshore_fishing_vesselNo)
vesselNo <- calc(vesselNo_stack, sum, na.rm = FALSE)

spplot(vesselNo_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(vesselNo, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "Vessel No")

# Get median and quantiles of vesselNo
cell_values <- as.vector(getValues(inshore_fishing_vesselNo))
cell_values <- cell_values[!is.na(cell_values)]
cell_values <- cell_values[cell_values != 0]
CellMedian <- round(median(cell_values), 3)

# choose quantiles and apply to ORE layers
ORE1_quantile <- 0.5
ORE2_quantile <- 0.7
ORE3_quantile <- 0.9

ORE1 <- round(quantile(cell_values, ORE1_quantile), 3)
ORE2 <- round(quantile(cell_values, ORE2_quantile), 3)
ORE3 <- round(quantile(cell_values, ORE3_quantile), 3)

OREvar_weighted <- ORE_variable
OREeven_weighted <- ORE_even
# Reset ORE_variable layer such that sectors B & C of median value, while
# sector A upper quartile value, and D lower quartile value
OREvar_weighted[OREvar_weighted == 1] <- ORE1
OREvar_weighted[OREvar_weighted == 2] <- ORE2
OREvar_weighted[OREvar_weighted == 3] <- ORE2
OREvar_weighted[OREvar_weighted == 4] <- ORE3

OREeven_weighted[OREeven_weighted == 1] <- ORE2

# weight shipping footprint to balance max and median

adj_shipping_log10_1000 <- shipping_log10_1000 * 20


# Check layers look correct
spplot(adj_shipping_log10_1000, col.regions = viridis_pal(direction = -1), main = 'adjusted logged shipping',axes = FALSE, box = FALSE)
spplot(OREeven_weighted, col.regions = viridis_pal(direction = -1), main = 'OREeven_weighted',axes = FALSE, box = FALSE)
spplot(OREvar_weighted, col.regions = viridis_pal(direction = -1), main = 'OREvar_weighted',axes = FALSE, box = FALSE)
spplot(vesselNo, col.regions = viridis_pal(direction = -1), main = 'fishing_vesselNo',axes = FALSE, box = FALSE)

# Add shipping and ORE layer to fishing value

vesselNo_varORE_stack <- stack(OREvar_weighted, adj_shipping_log10_1000, vesselNo)
vesselNo_varORE_cost <- calc(vesselNo_varORE_stack, sum, na.rm = FALSE)

vesselNo_evenORE_stack <- stack(OREeven_weighted, adj_shipping_log10_1000, vesselNo)
vesselNo_evenORE_cost <- calc(vesselNo_evenORE_stack, sum, na.rm = FALSE)

vesselNo_noORE_stack <- stack(adj_shipping_log10_1000, vesselNo)
vesselNo_noORE_cost <- calc(vesselNo_noORE_stack, sum, na.rm = FALSE)

spplot(vesselNo_varORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(vesselNo_varORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "vesselNo_varORE_cost")
spplot(vesselNo_evenORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(vesselNo_evenORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "vesselNo_evenORE_cost")
spplot(vesselNo_noORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(vesselNo_noORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "vesselNo_noORE_cost")

# Produce plots that highlights low values for discussion purposes
demo_plot1 <- vesselNo_varORE_cost
demo_plot2 <- vesselNo_evenORE_cost
demo_plot3 <- vesselNo_noORE_cost

demo_plot1[demo_plot1 >15] <- 15
demo_plot2[demo_plot2 >15] <- 15
demo_plot3[demo_plot3 >15] <- 15

spplot(demo_plot1, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "vesselNo_varORE_cost (adjusted axes)")
spplot(demo_plot2, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "vesselNo_evenORE_cost (adjusted axes)")
spplot(demo_plot3, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "vesselNo_noORE_cost (adjusted axes)")

writeRaster(vesselNo_varORE_cost, "vesselNo_varORE_cost_1km", format="GTiff", overwrite=TRUE)
writeRaster(vesselNo_evenORE_cost, "vesselNo_evenORE_cost_1km", format="GTiff", overwrite=TRUE)
writeRaster(vesselNo_noORE_cost, "vesselNo_noORE_cost_1km", format="GTiff", overwrite=TRUE)



# ------------------------------------------------------------------------------
# 3.5 Standardized Layers
# ------------------------------------------------------------------------------

# Treat each gear and ORE and shipping as equally valuable

# Standardize ORE layers to 1000 units
ORE_variable_1000 <- OREvar_weighted
ORE_even_1000 <- OREeven_weighted

cellTotals <- cellStats(ORE_variable_1000, sum)
ORE_variable_1000[!is.na(ORE_variable_1000)] <- ORE_variable_1000[!is.na(ORE_variable_1000)] / cellTotals * 1000
cellStats(ORE_variable_1000, "sum")
cellStats(ORE_variable_1000, "max")
cellTotals <- cellStats(ORE_even_1000, sum)
ORE_even_1000[!is.na(ORE_even_1000)] <- ORE_even_1000[!is.na(ORE_even_1000)] / cellTotals * 1000
cellStats(ORE_even_1000, "sum")
cellStats(ORE_even_1000, "max")

spplot(ORE_variable_1000, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(ORE_even_1000, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)

# Add everything together
std_international_varORE_stack <- stack(ORE_variable_1000, shipping_log10_1000, std_int_fishing_effort_7000, 
                                        std_inshore_fishing_effort_2000)
std_international_varORE_cost <- calc(std_international_varORE_stack, sum, na.rm = FALSE)

std_international_evenORE_stack <- stack(ORE_even_1000, shipping_log10_1000, std_int_fishing_effort_7000, 
                                         std_inshore_fishing_effort_2000)
std_international_evenORE_cost <- calc(std_international_evenORE_stack, sum, na.rm = FALSE)

std_international_noORE_stack <- stack(shipping_log10_1000, std_int_fishing_effort_7000, std_inshore_fishing_effort_2000)
std_international_noORE_cost <- calc(std_international_noORE_stack, sum, na.rm = FALSE)

# Plot it as a sanity check
spplot(std_international_varORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(std_international_varORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "std_international_varORE_cost")
spplot(std_international_evenORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(std_international_evenORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "std_international_evenORE_cost")
spplot(std_international_noORE_stack, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE)
spplot(std_international_noORE_cost, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "std_international_noORE_cost")

# Produce plots that highlights low values for discussion purposes
demo_plot1 <- std_international_varORE_cost
demo_plot2 <- std_international_evenORE_cost
demo_plot3 <- std_international_noORE_cost

demo_plot1[demo_plot1 >3] <- 3
demo_plot2[demo_plot2 >3] <- 3
demo_plot3[demo_plot3 >3] <- 3

spplot(demo_plot1, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "std_international_varORE_cost (adjusted axes)")
spplot(demo_plot2, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "std_international_evenORE_cost (adjusted axes)")
spplot(demo_plot3, col.regions = viridis_pal(direction = -1), axes = FALSE, box = FALSE, main = "std_international_noORE_cost (adjusted axes)")

writeRaster(std_international_varORE_cost, "std_int_varORE_cost_1km", format="GTiff", overwrite=TRUE)
writeRaster(std_international_evenORE_cost, "std_int_evenORE_cost_1km", format="GTiff", overwrite=TRUE)
writeRaster(std_international_noORE_cost, "std_int_noORE_cost_1km", format="GTiff", overwrite=TRUE)




