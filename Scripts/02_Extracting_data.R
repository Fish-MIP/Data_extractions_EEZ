## Summarising data per Exclusive Economic Zones
## Date: 2023-11-18
## Author: Denisse Fierro Arcos

#Loading relevant libraries
library(sf)
library(terra)
library(dplyr)
library(stringr)
library(readr)
library(raster)
library(tidyr)


# Biomass inputs ----------------------------------------------------------
#Location of files containing biomass
input_folder <- "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/raster_annual_sumSize"

#Getting list of biomass files
input_files <- list.files(input_folder, pattern = "global", full.names = T)

# Masks -------------------------------------------------------------------
mask_all <- rast("Outputs/EEZMasks/EEZ_mask_1deg.nc")


# Grid cell area ----------------------------------------------------------
area <- rast("ESM_Sample_Data/area_1deg.nc")


# Define functions --------------------------------------------------------
#Extract data by EEZ
sum_bio <- function(ras, area, eez_mask, meta){
  #Create empty data frame to store results
  summaries_biomass <- data.frame()
  #Getting years from biomass raster
  yrs <- str_remove(names(ras), "index_")
  #Reproject EEZ mask to match biomass raster if not the same
  if(crs(ras) != crs(eez_mask)){
    eez_mask <- project(eez_mask, ras)
  }
  #Get unique EEZ IDs in mask
  unique_id <- unique(eez_mask) |> 
    pull()
  #Extract each EEZ and apply mask
  for(i in unique_id){
    #Get single EEZ
    eez <- eez_mask == i
    eez[eez == 0] <- NA
    #Apply EEZ mask
    weighted_eez <- ras*eez
    #Transform into tabular form
    weighted_eez <- as.data.frame(weighted_eez) 


#Multiply raster by area and transform to terra
weight_mask <- function(ras_multi, weight){
  #Crop if extent is not the same
  if(crs(ras_multi) != crs(weight)){
    weight <- crop(weight, ext(ras_multi))
  }
  multi <- list()
  for(i in 1:dim(ras_multi)[3]){
    ras <- rast(ras_multi[[i]])
    multi[[i]] <- ras*weight
  }
  multi_ras <- rast(multi)
  return(multi_ras)
}

#Extract data by EEZ
sum_bio <- function(ras, eez_mask, meta){
  #Create empty data frame to store results
  summaries_biomass <- data.frame()
  #Getting years from biomass raster
  yrs <- str_remove(names(ras), "index_")
  #Crop EEZ to raster if not the same
  if(ext(ras) != ext(eez_mask)){
    eez_mask <- crop(eez_mask, ext(ras))
  }
  #Get unique EEZ IDs in mask
  unique_id <- unique(eez_mask) |> 
    pull()
  #Extract each EEZ and apply mask
  for(i in unique_id){
    #Get single EEZ
    eez <- eez_mask == i
    eez[eez == 0] <- NA
    #Apply EEZ mask
    weighted_eez <- ras*eez
    #Transform into tabular form
    weighted_eez <- as.data.frame(weighted_eez) 
    #Correct names in data frame - Use years
    names(weighted_eez) <- yrs
    #Make table longer to calculate sums per year
    w_e <- weighted_eez |> 
      pivot_longer(cols = everything(), names_to = "year", values_to = "biomass") |> 
      #Group by year
      group_by(year) |> 
      #Add all biomass values
      summarise(total_biomass_g_m2 = sum(biomass, na.rm = F),
                mean_weighted_biomass_G_m2 = mean(biomass, na.rm = F)) |> 
      #Add code for EEZ
      mutate(eez = i)
    summaries_biomass <- summaries_biomass |> 
      bind_rows(w_e)
  }
  #Adding metadata
  summaries_biomass <- summaries_biomass |> 
    mutate(model = meta[1, 1], 
           esm = meta[1, 2],
           scenario = meta[1, 3])
  
  return(summaries_biomass)
}


# Defining output folder --------------------------------------------------
output_folder <- "Outputs/biomass_summaries"
if(dir.exists(output_folder) == F){
  dir.create(output_folder, recursive = T)
}


# Applying functions to all files -----------------------------------------
for(f in input_files){
  #Read file
  brick <- readRDS(f)
  #Turn into terra
  brick <- brick[[names(brick)]]
  
  #Calculate area
  area <- cellSize(brick[[1]])
  
  #Define correct mask
  if(str_detect(f, "dbpm|zoomss_ipsl")){
    mask <- mask_dbpm
  }else{
    mask <- mask_all
  }
  
  #Metadata to be added to file
  meta <- str_extract(f, ".*sumSize_(.*)", group = 1) |> 
    str_split(pattern = "_", simplify = T)
  
  #Weighting data
  weighted_terra <- weight_mask(brick, area)
  #Calculating summaries
  summaries <- sum_bio(weighted_terra, mask, meta)
  
  #Getting output file name from original name
  out_name <- str_extract(f, ".*sumSize_(.*)", group = 1) |> 
    str_replace("rds", "csv")
  
  #Saving summaries
  summaries |> 
    write_csv(file.path(output_folder, out_name))
}











