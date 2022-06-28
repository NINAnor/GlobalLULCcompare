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

theme_set(theme_bw()+
            theme(
              panel.grid = element_blank()
            ))

readMultiFiles <- function(directory){
  
  files <- list.files(directory, pattern='*.csv', full.names=TRUE)
  raw <- files %>% 
    map_df(~read_csv(.))
  return (raw)
  
}
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

pal <- c( "#2E214D",
          "#4B3B66",
          "#6E5480",
          "#926390",
          "#B26795",
          "#D17BA5",
          "#D495B8",
          "#D4ADC9",
          "#DBC9DC",
          "#E6E6F0")
pal <- c("#05598C",
         "#296284",
         "#4A7283",
         "#6F878D",
         "#929C96",
         "#ABAD96",
         "#BAB98D",
         "#C7C684",
         "#E0E08E",
         "#FEFEB2")

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

# Write out grid to shapefile for upload
grid %>%
  mutate(long = unlist(map(st_centroid(grid)$geometry,1)),
         lat = unlist(map(st_centroid(grid)$geometry,2))) %>%
  filter(long > -170 & long < 175)%>%
  filter(lat > -65 & lat < 80) %>%
  st_write('./DATA/For_GEE/equal_area_grid.shp')


