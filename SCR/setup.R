#### Import packages, global functions, set themes ------------------------------------------------
library(tidyverse)
library(dggridR)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gridExtra)
library(tricolore)
library(ggtern)
library(raster)
library(ggmap)
library(caret)

# Global ggplot theme
theme_set(theme_bw()+
            theme(
              panel.grid = element_blank()
            ))

# Function to read multiple CSV files that are output from GEE
readMultiFiles <- function(directory){
  
  files <- list.files(directory, pattern='*.csv', full.names=TRUE)
  raw <- files %>% 
    map_df(~read_csv(.))
  return (raw)
  
}

# Function to recode land cover classes from numeric to character
recodeLcClasses <- function(lc){
  output <-  recode_factor(factor(lc), `1` = 'Built area',
                           `2` = 'Crops',
                           `3` = 'Bare ground',
                           `4` = 'Grass',
                           `5` = 'Shrub & scrub',
                           `6` = 'Trees',
                           `7` = 'Flooded vegetation',
                           `8` = 'Water',
                           `9` = 'Snow & ice')
  return (output)
}

# Function to recode Biomes to a simplified typology
recodeBiomes <- function(biome){
  output <- recode_factor(factor(biome), 
                          "Temperate Broadleaf & Mixed Forests" = 'Temp. bor. Forests',
                          "Tropical & Subtropical Grasslands, Savannas & Shrublands" = 'Trop. subtrop. Forests',
                          "Deserts & Xeric Shrublands" = 'Deserts',
                          "Tropical & Subtropical Coniferous Forests" = 'Trop. subtrop. Forests',
                          "Tropical & Subtropical Dry Broadleaf Forests" = 'Trop. subtrop. Forests',
                          "Temperate Conifer Forests" = 'Temp. bor. Forests',
                          "Boreal Forests/Taiga" = 'Temp. bor. Forests',
                          "Mediterranean Forests, Woodlands & Scrub" = 'Mediterranean scrub',
                          "Mangroves" = 'Wetland',
                          "Tundra" = 'Tundra',
                          "Tropical & Subtropical Moist Broadleaf Forests" = 'Trop. subtrop. Forests',
                          "Flooded Grasslands & Savannas" = 'Wetland',
                          "Montane Grasslands & Shrublands" = 'Savannas',
                          "Temperate Grasslands, Savannas & Shrublands" = 'Savannas')
  return (output)
}

# Visualization lookup for land cover maps
lcVizLookup <- tibble(
  colors = c('#C4281B','#E49635', '#A59B8F','#88B053', '#DFC35A','#397D49','#f2bac1','#0002f6','#6fdbde'),
  lcLabs = c("Built area", #1
             "Crops", #2
             "Bare ground", #3
             "Grass", #4
             "Shrub & scrub", #5
             "Trees", #6
             "Flooded vegetation", #7
             "Water", #8
             "Snow & ice" ))

#### Create hex grid for GEE -------------------------------------------------------------------
# This is an equal area hexagonal grid that will be uploaded to GEE and used to calculate 
# land cover areas for the three global LULC products
# After creating this in R I realized that you can do it in GEE. See this blogpost if you are interested:
# https://gorelick.medium.com/more-buffered-samples-with-hex-cells-b9a9bd36120d
dggs  <- dgconstruct(area=75000, metric=FALSE, resround='down')
class(dggs)

maxcell <- dgmaxcell(dggs)
cells   <- seq(1,maxcell, 1)
grid    <- dgcellstogrid(dggs,cells) #Get grid

bounds <- st_as_sfc(st_bbox(grid))

countries <- map_data("world")

ggplot() + 
  geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black", size=0.1) +
  geom_sf(data=grid %>%
            mutate(long = unlist(map(st_centroid(grid)$geometry,1)),
                   lat = unlist(map(st_centroid(grid)$geometry,2))) %>%
            filter(long > -170 & long < 175)%>%
            filter(lat > -65 & lat < 80), fill=NA) +
  geom_sf(data = bounds, fill=NA, color='red') +
  coord_sf()

# Write out grid to shapefile for upload to GEE asset
grid %>%
  mutate(long = unlist(map(st_centroid(grid)$geometry,1)),
         lat = unlist(map(st_centroid(grid)$geometry,2))) %>%
  filter(long > -170 & long < 175)%>%
  filter(lat > -65 & lat < 80) %>%
  st_write('./DATA/For_GEE/equal_area_grid.shp')


