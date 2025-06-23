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
# terra objects saved as rds files - working with this 
#input_folder <- "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/terra_annual_sumSize"
input_folder <- "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/sumSize_annual/sizeConsidered10g_10kg"


#Getting list of biomass files
#input_files <- list.files(input_folder, pattern = "2023-11-21.+global", full.names = T)
input_files <- list.files(input_folder, pattern = "2023-11-23.+global", full.names = T)

# or raster objects saved as grd (+gri) files- not working 
#input_folderB <- "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/raster_annual_sumSize"

#Getting list of biomass files
#input_filesB <- list.files(input_folderB, pattern = "2023-11-21.+global", full.names = T)

# plot terra object 
# plot same but raster objects
# seems the same 
#plot(rast(input_files[4])) # "2023-11-21_01_raster_annual_sumSize_boats_gfdl_historical_global.rds" 
#plot(brick(input_filesB[7])) # 2023-11-21_01_raster_annual_sumSize_boats_gfdl_historical_global.grd"

# Masks -------------------------------------------------------------------
eez_mask <- rast("Outputs/EEZMasks/EEZ_mask_1deg.nc")
eez_df <- read_csv("Outputs/EEZMasks/EEZ_mask_1deg.csv") |> 
  rename(x = lon, y = lat)
#Load EEZ keys
keys <- read_csv("Outputs/EEZMasks/eez_key.csv") |> 
  dplyr::select(!c(AREA_KM2, POL_TYPE)) |> 
  dplyr::rename(eez = MRGID)


# Grid cell area ----------------------------------------------------------
#Loading the area prevent us from having to calculate the area every time a new
#file is loaded. This way we reduce computational resources needed to run script

## CN adding line to explore 
area <- rast("ESM_Sample_Data/area_1deg.nc")
area_df <- area |> 
  as.data.frame(xy = T) |> 
  #Keeping only grid cells inside EEZs
  right_join(eez_df, by = c("x", "y"))


# Define functions --------------------------------------------------------
#Extract data by EEZ
#summarise_bio <- function(ras, area_df, eez_mask, meta, keys){
summarise_bio <- function(ras, area_df, eez_mask, keys){
  
  # # CN trial 
  # ras = brick
  
  #if CRS does not match, reproject data
  #Crop EEZ to raster if not the same
  # if(crs(ras) != crs(eez_mask)){
  #   eez_mask <- project(eez_mask, ras)
  # }
  
  # ## CN NO! here you are multiplying the biomass by the EEZ number
  # #Applying EEZ binary mask to biomass data
  # masked_bio <- ras*eez_mask
  masked_bio<-ras
  #Transforming masked biomass data into data frame
  # masked_bio_df <- masked_bio |> 
  #   as.data.frame(xy = T)
  masked_bio_df<-masked_bio
  
  # ## CN checks and understanding this 
  # trial<-masked_bio_df |> 
  #   left_join(area_df, by = c("x", "y"))
  # 
  # dplyr::filter(area_df, x == -179.5, y == 89.5)
  # sort(unique(area_df$x))
  # sort(unique(area_df$y)) # df_area includes values only if grid cell is in EEZ - so OK for some values to be NA  
  # 
  # # check other approach with area calculation on brick 
  # area2 <-cellSize(brick[[1]], unit = "m")
  # area_df2<-area2 |>
  #   as.data.frame(xy = T) |> 
  #   #Keeping only grid cells inside EEZs
  #   right_join(eez_df, by = c("x", "y"))
  # 
  # # check if same values 
  # # slightly different
  # plot(rast("ESM_Sample_Data/area_1deg.nc"))
  # plot(area2)
  # 
  # dplyr::filter(area_df, x == -1.5, y == 1.5)
  # dplyr::filter(area_df2, x == -1.5, y == 1.5)
  # 
  # dplyr::filter(area_df, x == 50.5, y == -10.5)
  # dplyr::filter(area_df2, x == 50.5, y == -10.5)
  
  #Merge with data frame with area and EEZ codes
  ## CN approach: 
  summaries_biomass <- masked_bio_df |> 
    left_join(area_df, by = c("x", "y")) |> 
    # #Calculating total and weighted means of biomass per EEZ and year
    # # pivot_longer(cols = starts_with("index_"),
    # #              names_to = "year", values_to = "biomass") |>
    # #Remove prefix from year column & calculate biomass in each grid cell given area  
    # mutate(#year = str_remove(year, "index_"),
    #        biomass_grid = biomass*area_m) |>
    # group_by(year, eez,mem,esm,scenario) %>% 
    # summarise(sum_biomass = sum(biomass_grid, na.rm = TRUE), 
    #           mean_biomass = weighted.mean(biomass, area_m, na.rm = TRUE)) |>
    # ungroup() |>
    # #Add metadata - Model, ESM and scenario
    # # mutate(model = meta[1, 1], 
    # #       esm = meta[1, 2],
    # #       scenario = meta[1, 3]) |> 
    # #Adding information about EEZs
     left_join(keys, by = "eez")
  
  # #Merge with data frame with area and EEZ codes
  # # Denisse's appraoch
  # summaries_biomass <- masked_bio_df |> 
  #   left_join(area_df, by = c("x", "y")) |> 
  #   #Group by EEZ
  #   group_by(eez) |> 
  #   #Add area of grid cells per EEZ
  #   mutate(area_tot = sum(area_m, na.rm = T), 
  #          #Calculate weights (area grid cell/total area EEZ)
  #          weight = area_m/area_tot) |> 
  #   #Calculating total and weighted means of biomass per EEZ and year
  #   pivot_longer(cols = starts_with("index_"),
  #                names_to = "year", values_to = "biomass") |>
  #   #Remove prefix from year column
  #   mutate(year = str_remove(year, "index_"),
  #          #Calculate total biomass per grid cell
  #          biomass_grid = biomass*area_tot,
  #          #Apply weight to biomass
  #          biomass_weighted = biomass*weight) |> 
  #   #Calculations per year and EEZ
  #   group_by(year, eez) |> 
  #   #Calculate total biomass
  #   summarise(sum_biomass = sum(biomass_grid, na.rm = T),
  #             #Calculate weighted means
  #             mean_biomass = sum(biomass_weighted, na.rm = T)) |> 
  #   #Add metadata - Model, ESM and scenario
  #   mutate(model = meta[1, 1], 
  #          esm = meta[1, 2],
  #          scenario = meta[1, 3]) |> 
  #   #Adding information about EEZs
  #   left_join(keys, by = "eez")
  
  # ### WARNING - sum biomass is different, mean biomass is the same.
  # # understanding weighted mean both approaches: OK
  # biomass = c(1,2,4,6,7)
  # area_grid = c(3,4,2,3,4)
  # area_tot = sum(area_grid)
  # # sum(biomass*(area_grid/area_tot))
  # # sum(biomass*area_grid)/area_tot
  # # understanding sum of biomass: of course different
  # sum(biomass*area_tot)
  # sum(biomass*area_grid)
  
  return(summaries_biomass)
  }
  

# Defining output folder --------------------------------------------------
# output_folder <- "Outputs/biomass_summaries"
# if(dir.exists(output_folder) == F){
#   dir.create(output_folder, recursive = T)
# }

#output_folder<-"/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/terra_annual_sumSize/EEZsummaries"

output_folder<-"/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/sumSize_annual/sizeConsidered10g_10kg/EEZsummaries"

# Applying functions to all files -----------------------------------------
for(f in input_files){ # or input_filesB if using raster objects
  
  # # CN trial - issue explained in email
  # # problem: 
  # f = "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/raster_annual_sumSize/2023-11-15_01_raster_annual_sumSize_apecosm_ipsl_historical_global.rds"
  # # OK - saved as terra instead of raster 
  # f = "/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/raster_annual_sumSize/2023-11-20_01_raster_annual_sumSize_zoomss_gfdl_historical_global.rds"
  # f = input_files[1]
  
  #Read file
  brick0 <- readRDS(f)
  brick = brick0[[1]]
  # plot(brick)
  # if using raster .grd object (input_filesB) - ERROR: [rast] cannot open this file as a SpatRaster
  # brick <-rast(f)
  # but this is ok - just not working with terra coe following 
  # brick <-brick(f)

  #Metadata to be added to file
  # meta <- str_extract(f, ".*sumSize_(.*)", group = 1) |> 
  #   str_split(pattern = "_", simplify = T)
  
  #Calculating summaries
 # summaries <- summarise_bio(brick, area_df, eez_mask, meta, keys)
  summaries <- summarise_bio(brick, area_df, eez_mask, keys)
  
  #Getting output file name from original name
 # out_name <- str_extract(f, ".*sumSize_(.*)", group = 1) |>
 #   str_replace("rds", "csv")
  out_name = paste(names(brick0),"_global_gridded",".csv",sep="")
  #Saving summaries
  summaries |>
    write_csv(file.path(output_folder, out_name))
  
  
}


# combine all files into a df

input_folder<-"/rd/gem/private/users/camillan/Extract_tcblog10_Data/Output/sumSize_annual/sizeConsidered10g_10kg/EEZsummaries"
input_files <- list.files(input_folder, full.names = T)
tables <- lapply(input_files, read.csv, header = TRUE)
df <- do.call(rbind , tables)
head(df)


