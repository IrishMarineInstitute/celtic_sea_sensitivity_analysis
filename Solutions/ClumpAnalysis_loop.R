#Authors: Patricia Breen & Louise Allcock
#Note that this code was written to run on a local system prior to publication of
#data layers. File paths will need amending throughout this code but filenames
#remain unchanged.
# Ignore 1.1.1 since data supplied to stakeholders in a single folder

library(raster)
library(sp)
library(prioritizr)
library(sf)
library(dplyr)
library(tibble)
library(scales)
library(ggplot2)
library(topsis)
library(withr)
library(purrr)
library(ggspatial)
library(scales)
library(ggnewscale)

# ---------------------------------
# 1.0 Get data
# ---------------------------------

# setwd("H:/mpa_git/Celtic Sea/Analysis")
setwd("/Users/louise/Documents/GitHub/mpa_ecological_sensitivity/Celtic Sea/Analysis")

# ---------------------------------
# 1.1 Make a raster stack of available feature layers
# ---------------------------------

setwd("../ProcessedFeatures/FinalFeatures")

feature_tifs <- list.files(pattern = "\\.tif$")
features <- lapply(feature_tifs, raster)
# Set the names of the list elements to the file names
names(features) <- feature_tifs
# Create a raster stack from the list of rasters
feat_stack_1km <- stack(features)
names(feat_stack_1km)

# ---------------------------------
# 1.1.1 Drop front layer and replace with two split layers (into CelticSea and Coastal fronts)
# ---------------------------------

front_layer <- grep("Fronts_temp_1km.tif", names(feat_stack_1km))
feat_stack_1km <- dropLayer(feat_stack_1km, unlist(front_layer)) # note to drop from rasterstack use dropLayer(stack_name, unlist(index_variable))
names(feat_stack_1km)


CelticSeaFront <- raster("../CelticSeaFront/split/CelticSeaFront.tif")
CoastalFront <- raster("../CelticSeaFront/split/coastal_front.tif")
feat_stack_1km <- stack(feat_stack_1km, CelticSeaFront, CoastalFront)
names(feat_stack_1km)

feat_stack_3km <-aggregate(feat_stack_1km, fact=3)

# ---------------------------------
# 1.2 Pull in the GIS files for the plot
# ---------------------------------

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



setwd("../../Analysis")

# ---------------------------------
# 1.3 3km analysis
# ---------------------------------


scenario_list <- c("3km_EcolImp_STDcost_PB", "3km_EcolImp_UNWcost_PB",
                   "3km_SACSPA_STDcost_LA", "3km_SACSPA_UNWcost_LA2",
                   "3km_INITIAL_STDcost_PB", "3km_INITIAL_UNWcost_PB",
                   "3km_pragmatic_STDcost_PB", "3km_pragmatic_UNWcost_PB",
                   "3km_threatened_STDcost_PB", "3km_threatened_UNWcost_PB")
                   
folder_list <- c("Run23_3km_EcolImp_STDcost_PB", "Run22_3km_EcolImp_UNWcost_PB",
                   "Run27_3km_SACSPA_STDcost_LA", "Run26_3km_SACSPA_UNWcost_LA2",
                   "Run19_3km_INITIAL_STDcost_PB", "Run18_3km_INITIAL_UNWcost_PB",
                   "Run25_3km_pragmatic_STDcost_PB", "Run24_3km_pragmatic_UNWcost_PB",
                   "Run21_3km_threatened_STDcost_PB", "Run20_3km_threatened_UNWcost_PB")

for (k in seq_along(scenario_list)) {
  analysed_layer <- raster(paste0("../Solutions/", folder_list[[k]], "/", scenario_list[[k]], "_bestsolution.tif"))
  clumps <- clump(analysed_layer)
  writeRaster(clumps, paste0(scenario_list[[k]],"_Reserves.tif"), format = "GTiff", overwrite = TRUE)
  NoClumps <- maxValue(clumps)
  prop_vals_list <- vector("list", NoClumps) # Initialize list with NoClumps elements

  # Loop over each layer in feat_stack_3km
  for (i in 1:nlayers(feat_stack_3km)) {
    current_layer <- feat_stack_3km[[i]]
    for (j in 1:NoClumps) {
      mask <- clumps == j
      masked_raster <- mask * current_layer
      mask_vals <- cellStats(masked_raster, sum, na.rm = TRUE)
      sum_vals <- cellStats(current_layer, sum, na.rm = TRUE)
      prop_vals <- mask_vals / sum_vals
      prop_vals_list[[j]][[i]] <- prop_vals
    }
  }

  prop_vals_list_by_clump <- lapply(prop_vals_list, unlist)
  prop_df <- data.frame(prop_vals_list_by_clump)
  colnames(prop_df) <- paste0("clump ", 1:NoClumps)
  row.names(prop_df) <- names(feat_stack_3km)
  prop_df$Total <- apply(prop_df, 1, sum)
  prop_df <- prop_df %>%
    mutate_all(~round(., 2))
  prop_df
  write.csv(prop_df, file = paste0(scenario_list[[k]], "_featurereps.csv"))

  color_pal <- scales::hue_pal()(NoClumps)
  clumps_df <- as.data.frame(clumps, xy = TRUE)
  colnames(clumps_df) <- c("x", "y", "value")
  clumps_df$value <- as.factor(clumps_df$value)

  ggplot() +
    geom_tile(data = clumps_df, aes(x = x, y = y, fill = value)) +
    scale_fill_manual(values = color_pal, na.value = NA, name = "Area") +
    # Add the first set of spatial layers with a different legend
    layer_spatial(sa, fill = NA, colour = "grey80") +
    layer_spatial(land, fill = "grey85", colour = NA) +
    new_scale("colour") + # Start a new colour scale
    layer_spatial(OREmap, aes(colour ="blue"), fill=NA) +
    layer_spatial(SAC_ITM_2024_CelticSea, aes(colour ="red"),fill=NA) +
    layer_spatial(SPA_CelticSeaMarine, aes(colour = "orange"), fill=NA) +
    scale_colour_manual(name = "", values = c("blue", "red", "orange"), labels = c("ORE", "SPAs", "SACs")) +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          rect = element_blank(),
          legend.direction="horizontal",
          legend.position = c(0.75,0.15),
          legend.title=element_text(size=10),
          legend.text=element_text(size=8),
          panel.background = element_rect(fill = "white", colour = "grey50"),
          plot.margin=grid::unit(c(0,0,0,0), "mm")
    )

  ggsave(paste0(scenario_list[[k]], ".png"), width = 8, height = 8, dpi = 300)
}

# ---------------------------------
# 1.3 1km analysis
# ---------------------------------

scenario_list <- c("1km_EcolImp_STDcost_LA", "1km_EcolImp_UNWcost_LA",
                   "1km_SACSPA_STDcost_LA", "1km_SACSPA_UNWcost_LA",
                   "1km_INITIAL_STDcost_LA", "1km_INITIAL_UNWcost_LA",
                   "1km_pragmatic_STDcost_LA", "1km_pragmatic1_UNWcost_LA",
                   "1km_threatened_STDcost_LA", "1km_threatened_UNWcost_LA")

folder_list <- c("Run12_1km_EcolImp_STDcost_LA", "Run11_1km_EcolImp_UNWcost_LA",
                   "Run16_1km_SACSPA_STDcost_LA", "Run15_1km_SACSPA_UNWcost_LA",
                   "Run6_1km_INITIAL_STDcost_LA", "Run5_1km_INITIAL_UNWcost_LA",
                   "Run14_1km_pragmatic_STDcost_LA", "Run13_1km_pragmatic1_UNWcost_LA",
                   "Run10_1km_threatened_STDcost_LA", "Run9_1km_threatened_UNWcost_LA")

for (k in seq_along(scenario_list)) {
  analysed_layer <- raster(paste0("../Solutions/", folder_list[[k]], "/", scenario_list[[k]], "_bestsolution.tif"))
  clumps <- clump(analysed_layer)
  writeRaster(clumps, paste0(scenario_list[[k]],"_Reserves.tif"), format = "GTiff", overwrite = TRUE)
  NoClumps <- maxValue(clumps)
  prop_vals_list <- vector("list", NoClumps) # Initialize list with NoClumps elements

  # Loop over each layer in feat_stack_3km
  for (i in 1:nlayers(feat_stack_1km)) {
    current_layer <- feat_stack_1km[[i]]
    for (j in 1:NoClumps) {
      mask <- clumps == j
      masked_raster <- mask * current_layer
      mask_vals <- cellStats(masked_raster, sum, na.rm = TRUE)
      sum_vals <- cellStats(current_layer, sum, na.rm = TRUE)
      prop_vals <- mask_vals / sum_vals
      prop_vals_list[[j]][[i]] <- prop_vals
    }
  }

  prop_vals_list_by_clump <- lapply(prop_vals_list, unlist)
  prop_df <- data.frame(prop_vals_list_by_clump)
  colnames(prop_df) <- paste0("clump ", 1:NoClumps)
  row.names(prop_df) <- names(feat_stack_1km)
  prop_df$Total <- apply(prop_df, 1, sum)
  prop_df <- prop_df %>%
    mutate_all(~round(., 2))
  prop_df
  write.csv(prop_df, file = paste0(scenario_list[[k]], "_featurereps.csv"))

  color_pal <- scales::hue_pal()(NoClumps)
  clumps_df <- as.data.frame(clumps, xy = TRUE)
  colnames(clumps_df) <- c("x", "y", "value")
  clumps_df$value <- as.factor(clumps_df$value)

  ggplot() +
    geom_tile(data = clumps_df, aes(x = x, y = y, fill = value)) +
    scale_fill_manual(values = color_pal, na.value = NA, name = "Area") +
    # Add the first set of spatial layers with a different legend
    layer_spatial(sa, fill = NA, colour = "grey80") +
    layer_spatial(land, fill = "grey85", colour = NA) +
    new_scale("colour") + # Start a new colour scale
    layer_spatial(OREmap, aes(colour ="blue"), fill=NA) +
    layer_spatial(SAC_ITM_2024_CelticSea, aes(colour ="red"),fill=NA) +
    layer_spatial(SPA_CelticSeaMarine, aes(colour = "orange"), fill=NA) +
    scale_colour_manual(name = "", values = c("blue", "red", "orange"), labels = c("ORE", "SPAs", "SACs")) +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          rect = element_blank(),
          legend.direction="horizontal",
          legend.position = c(0.75,0.15),
          legend.title=element_text(size=10),
          legend.text=element_text(size=8),
          panel.background = element_rect(fill = "white", colour = "grey50"),
          plot.margin=grid::unit(c(0,0,0,0), "mm")
    )

  ggsave(paste0(scenario_list[[k]], ".png"), width = 8, height = 8, dpi = 300)
}
