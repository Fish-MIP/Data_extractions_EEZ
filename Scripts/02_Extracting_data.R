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
### CN 
input_files <- list.files(input_folder ,full.names = T) #pattern = "2023-11-16.+global",

# Masks -------------------------------------------------------------------
mask_all <- rast("Outputs/EEZMasks/EEZ_mask_1deg.nc")
# mask_dbpm <- rast("Outputs/EEZMasks/EEZ_mask_1deg_DBPM.nc")

# Grid cell area ----------------------------------------------------------
# area_all <- rast("ESM_Sample_Data/area_1deg.nc")
# area_dbpm <- rast("ESM_Sample_Data/area_1deg_DBPM.nc")


# Define functions --------------------------------------------------------
#Multiply raster by area and transform to terra
weight_mask <- function(ras_multi, weight){
  
  # # trial CN
  # ras_multi = brick
  # weight = area
  
  #Crop if extent is not the same
  if(ext(ras_multi) != ext(weight)){
    weight <- crop(weight, ext(ras_multi))
  }
  multi <- list()
  for(i in 1:dim(ras_multi)[3]){
    
    ## CN changed if this was already transformed into terra object 
    # ras <- rast(ras_multi[[i]]), instead: 
    ras<-ras_multi
    multi[[i]] <- ras*weight
  }
  multi_ras <- rast(multi)
  
  # ## CN or simply?? 
  # trial<-ras_multi*weight
  # plot(multi_ras)
  # plot(trial)
  # 
  # plot(multi_ras[[50]])
  # plot(trial[[50]])
  # plot(weight)
  
  return(multi_ras)
}

#Extract data by EEZ
sum_bio <- function(ras, eez_mask, meta){
  
  # # CN trial 
  # ras = weighted_terra # this is values * grid cell area 
  # eez_mask = mask
  
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
    
    # CN trial 
    # i = unique_id[10]
    
    #Get single EEZ
    eez <- eez_mask == i
    eez[eez == 0] <- NA
    # CN: 
    # terra::plot(eez)
    
    #Apply EEZ mask
    weighted_eez <- ras*eez
    ## CN 
    # terra::plot(weighted_eez[[50]])
    
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
                #### CN should this be weighted.mean instead? I cannot understand where the /sumOfWeights part is...  
                mean_weighted_biomass_G_m2 = mean(biomass, na.rm = F)) |> 
      #Add code for EEZ
      mutate(eez = i)
    ## CN not sure what bind_rows does in this case - should it be outside the loop? 
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
  
  # CN trial
  # problem: 
  f = "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/raster_annual_sumSize/2023-11-15_01_raster_annual_sumSize_apecosm_ipsl_historical_global.rds"
  # OK - saved as terra instead of raster 
  f = "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/raster_annual_sumSize/2023-11-20_01_raster_annual_sumSize_zoomss_gfdl_historical_global.rds"
  
  #Read file
  brick <- readRDS(f)
  #Turn into terra
  brick <- brick[[names(brick)]]
  
  #Locate correct mask and area
  
  #### CN change this  
  # if(str_detect(f, "dbpm|zoomss_ipsl")){
    # mask <- mask_dbpm
    # area <- area_dbpm
  # }else{
    mask <- mask_all
    # area <- area_all
    
    # If rds are saved as raster object - though sometimes this is not working (with RasterLayer) 
    # forArea<-as(brick, "SpatRaster")
    # forArea<-rast(brick)
    # area <-cellSize(forArea[[1]], unit = "m")
    # If rds are saved as terra objects - though warning message when transforming from raster to terra in original code
    area <-cellSize(brick[[1]], unit = "m")
    terra::plot(area)
  # }
  
  #Metadata to be added to file
  meta <- str_extract(f, ".*sumSize_(.*)", group = 1) |> 
    str_split(pattern = "_", simplify = T)
  
  #Weighting data
  # CN this is values * gridcell area 
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











