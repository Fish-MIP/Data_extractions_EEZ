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
eez_mask <- rast("Outputs/EEZMasks/EEZ_mask_1deg.nc")
eez_df <- read_csv("Outputs/EEZMasks/EEZ_mask_1deg.csv") |> 
  rename(x = lon, y = lat)
#Load EEZ keys
keys <- read_csv("Outputs/EEZMasks/eez_key.csv") |> 
  select(!c(AREA_KM2, POL_TYPE)) |> 
  rename(eez = MRGID)


# Grid cell area ----------------------------------------------------------
#Loading the area prevent us from having to calculate the area every time a new
#file is loaded. This way we reduce computational resources needed to run script
area_df <- rast("ESM_Sample_Data/area_1deg.nc") |> 
  as.data.frame(xy = T) |> 
  #Keeping only grid cells inside EEZs
  right_join(eez_df, by = c("x", "y"))


# Define functions --------------------------------------------------------
#Extract data by EEZ
summarise_bio <- function(ras, area_df, eez_mask, meta, keys){
  #if CRS does not match, reproject data
  #Crop EEZ to raster if not the same
  if(crs(ras) != crs(eez_mask)){
    eez_mask <- project(eez_mask, ras)
  }
  #Applying EEZ binary mask to biomass data
  masked_bio <- ras*eez_mask
  #Transforming masked biomass data into data frame
  masked_bio_df <- masked_bio |> 
    as.data.frame(xy = T)
  #Merge with data frame with area and EEZ codes
  summaries_biomass <- masked_bio_df |> 
    left_join(area_df, by = c("x", "y")) |> 
    #Group by EEZ
    group_by(eez) |> 
    #Add area of grid cells per EEZ
    mutate(area_tot = sum(area_m, na.rm = T), 
           #Calculate weights (area grid cell/total area EEZ)
           weight = area_m/area_tot) |> 
    #Calculating total and weighted means of biomass per EEZ and year
    pivot_longer(cols = starts_with("index_"),
                 names_to = "year", values_to = "biomass") |>
    #Remove prefix from year column
    mutate(year = str_remove(year, "index_"),
           #Calculate total biomass per grid cell
           biomass_grid = biomass*area_tot,
           #Apply weight to biomass
           biomass_weighted = biomass*weight) |> 
    #Calculations per year and EEZ
    group_by(year, eez) |> 
    #Calculate total biomass
    summarise(sum_biomass = sum(biomass_grid, na.rm = T),
              #Calculate weighted means
              mean_biomass = sum(biomass_weighted, na.rm = T)) |> 
    #Add metadata - Model, ESM and scenario
    mutate(model = meta[1, 1], 
           esm = meta[1, 2],
           scenario = meta[1, 3]) |> 
    #Adding information about EEZs
    left_join(keys, by = "eez")
  return(summaries_biomass)
  }
  

# Defining output folder --------------------------------------------------
output_folder <- "Outputs/biomass_summaries"
if(dir.exists(output_folder) == F){
  dir.create(output_folder, recursive = T)
}


# Applying functions to all files -----------------------------------------
for(f in input_files){
  # # CN trial - issue explained in email
  # # problem: 
  # f = "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/raster_annual_sumSize/2023-11-15_01_raster_annual_sumSize_apecosm_ipsl_historical_global.rds"
  # # OK - saved as terra instead of raster 
  # f = "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/raster_annual_sumSize/2023-11-20_01_raster_annual_sumSize_zoomss_gfdl_historical_global.rds"
  
  #Read file
  brick <- readRDS(f)

  #Metadata to be added to file
  meta <- str_extract(f, ".*sumSize_(.*)", group = 1) |> 
    str_split(pattern = "_", simplify = T)
  
  #Calculating summaries
  summaries <- summarise_bio(brick, area_df, eez_mask, meta, keys)
  
  #Getting output file name from original name
  out_name <- str_extract(f, ".*sumSize_(.*)", group = 1) |>
    str_replace("rds", "csv")

  #Saving summaries
  summaries |>
    write_csv(file.path(output_folder, out_name))
}

