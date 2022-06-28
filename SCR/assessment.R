#### Import datasets -------------------------------------------------------------------------------

continents <- ne_countries(scale = "medium", returnclass = "sf")   %>%
  dplyr::select(iso_a3, economy, continent) 

countries <- ne_countries(scale = "medium", returnclass = "sf") 

hexGrid <- st_read('./DATA/For_GEE/equal_area_grid.shp')

val_global_metadata <- read_csv('./DATA/From_GEE/continents_global_val.csv') %>% dplyr::select(-`system:index`,-'.geo') %>%
  left_join(read_csv('./DATA/From_GEE/ecoregion_global_val.csv') %>% dplyr::select(-`system:index`,-'.geo')) %>%
  left_join(read_csv('./DATA/From_GEE/hsl_global_val.csv') %>% dplyr::select(-`system:index`,-'.geo')) %>%
  mutate(hsl = ifelse(hsl == 0, 'Uninhabited', 
                      ifelse(hsl == 1, 'Rural', 'Urban'))) %>%
  mutate(Lon = as.numeric(str_split(dw_id, '_') %>% map_chr(., 3)),
         Lat = str_split(dw_id, '_') %>% map_chr(., 4),
         Lat = as.numeric(substring(Lat,1, nchar(Lat)-9))) %>%
  mutate(BIOME_NAME = recodeBiomes(BIOME_NAME)) %>%
  drop_na(BIOME_NAME)
unique(val_global_metadata$BIOME_NAME)

val_global <- read_csv('./DATA/From_GEE/validation_global_DW_tiles.csv') %>%
  mutate(dw_id = str_replace_all(`system:index`, 'p', '.'))%>% 
  mutate(dw_id = substring(dw_id,
                           1, 
                           nchar(dw_id) - (nchar(str_split(dw_id, '_') %>% map_chr(., 5)) + 1))) %>%
  dplyr::select(-`system:index`,-'.geo')%>%
  mutate_at(vars(dw, wc, esri, groundTruth), recodeLcClasses)
val_global


val_regional_metadata <- read_csv('./DATA/From_GEE/hsl_regional_val.csv') %>% 
  left_join(read_csv('./DATA/From_GEE/ecoregion_regional_val.csv') %>% dplyr::select(-`system:index`,-'.geo', -lc1)) %>%
  mutate(Lat = as.numeric(str_split(str_split(str_split(`.geo`,'\\[') %>% map_chr(., 2), ',') %>% map_chr(., 2), '\\]') %>% map_chr(., 1)),
         Lon = as.numeric(str_split(str_split(`.geo`,'\\[') %>% map_chr(., 2), ',') %>% map_chr(., 1))) %>%
  dplyr::select(-`system:index`,-'.geo') %>%
  mutate(hsl = ifelse(hsl == 0, 'Uninhabited', 
                      ifelse(hsl == 1, 'Rural', 'Urban'))) %>%
  mutate(BIOME_NAME = recodeBiomes(BIOME_NAME)) %>%
  drop_na(BIOME_NAME)

val_regional <- readMultiFiles('./DATA/From_GEE/validation_european_LUCAS_pts/') %>%
  mutate(groundTruth = lcNum) %>%
  dplyr::select(-`system:index`,-'.geo', -lcNum, -lc1)%>%
  mutate_at(vars(dw, wc, esri, groundTruth), recodeLcClasses)

lc_areas <- readMultiFiles('./DATA/From_GEE/lc_areas_grid/')%>% 
  distinct(.keep_all=T) %>%
  dplyr::select(-`system:index`,-'.geo', -first) %>%
  gather(key, val, BG:W) %>%
  group_by( lc, seqnum) %>%
  mutate(tot = sum(val, na.rm=T),
         percCov = val/tot*100) %>%
  dplyr::select(lc,seqnum, key, percCov) %>%
  pivot_wider(names_from=key, values_from=percCov) %>%
  ungroup()

dwGlobal <- raster('./DATA/From_GEE/dynamicworld_global.tif')
dwLocal <- raster('./DATA/From_GEE/dynamicworld_local.tif')
dwLocal2 <- raster('./DATA/From_GEE/dynamicworld_local_2.tif')

wcGlobal <- raster('./DATA/From_GEE/worldcover_global.tif')
wcLocal <- raster('./DATA/From_GEE/worldcover_local.tif')
wcLocal2 <- raster('./DATA/From_GEE/worldcover_local_2.tif')

esriGlobal <- raster('./DATA/From_GEE/esri_global.tif')
esriLocal <- raster('./DATA/From_GEE/esri_local.tif')
esriLocal2 <- raster('./DATA/From_GEE/esri_local_2.tif')

dwLocal_eg1 <- raster('./DATA/From_GEE/dwLabelMosaic_local_eg.tif')
dwLocal_eg2 <- raster('./DATA/From_GEE/dwTreeCovExagg_local_eg.tif')
dwLocal_eg3 <- raster('./DATA/From_GEE/dwReclassImg_local_eg.tif')

#### Figure 1 - land cover maps ------------------------------------------------------------------------

rastGlobal <- dwGlobal
rastLocal <- dwLocal
rastLocal2 <- dwLocal2

# Get Google Maps API key registered. 
# Need to save this in  a txt file in a directory called Private
key <- read.delim('./Private/key.txt', header=F, sep=' ')
your_gmaps_API_key <- key$V1
register_google(key = your_gmaps_API_key)
getCent <- function(rast){
  cent <- c((extent(rast)[1] + extent(rast)[2])/2, (extent(rast)[3] + extent(rast)[4])/2)
  return (cent)
}
gmap <- get_googlemap(center = getCent(rastLocal), zoom = 15, maptype = "satellite")
ggmap(gmap)
gmap2 <- get_googlemap(center = getCent(rastLocal2), zoom = 13, maptype = "satellite")
ggmap(gmap2)

rasterToDf <- function(raster){
  names(raster) <- 'lc'
  lcSpat <- as.data.frame(raster, xy = TRUE) %>%
    filter(lc != 0) %>%
    na.omit() %>%
    as_tibble() %>%
    mutate(lc = factor(round(lc)),
           lc = recode_factor(lc, `1` = 'Built area',
                              `2` = 'Crops',
                              `3` = 'Bare ground',
                              `4` = 'Grass',
                              `5` = 'Shrub & scrub',
                              `6` = 'Trees',
                              `7` = 'Flooded vegetation',
                              `8` = 'Water',
                              `9` = 'Snow & ice')) %>%
    drop_na()
  return (lcSpat)
}

theme_1 <- theme(
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  plot.margin=grid::unit(c(0,0,0,0), "mm")
)

theme_legend_1 <- theme(legend.key.size = unit(0.25, 'cm'), #change legend key size
                        legend.key.height = unit(0.25, 'cm'), #change legend key height
                        legend.key.width = unit(0.25, 'cm'), #change legend key width
                        legend.title = element_text(size=6), #change legend title font size
                        legend.text = element_text(size=6)) #change legend text font size
label <- 'test'

makeGlobLocLCmap <- function(rastGlobal, rastLocal, rastLocal2, label, leg){
  
  lcSpatLocal <- rasterToDf(rastLocal)
  
  localMap <- ggplot() +
    geom_raster(data = lcSpatLocal , aes(x = x, y = y, fill = lc)) +
    scale_fill_manual(values = lcVizLookup[lcVizLookup$lcLabs %in% unique(lcSpatLocal$lc),]$colors,
                      name="")+
    theme(legend.position = 'none',
          axis.title = element_blank()) +
    theme_1 + 
    coord_cartesian(xlim = c(extent(rastLocal)[1],extent(rastLocal)[2]),
                    ylim = c(extent(rastLocal)[3],extent(rastLocal)[4]), expand = c(0, 0))
  
  localGmap <- ggmap(gmap)+
    theme_1+
    coord_cartesian(xlim = c(extent(rastLocal)[1],extent(rastLocal)[2]),
                    ylim = c(extent(rastLocal)[3],extent(rastLocal)[4]), expand = c(0, 0))
  
  localGrid <- grid.arrange(localGmap,localMap, nrow=1, widths=c(1,1), padding = unit(0, "line"), newpage = T) 
  
  lcSpatLocal2 <- rasterToDf(rastLocal2)
  
  localMap2 <- ggplot() +
    geom_raster(data = lcSpatLocal2 , aes(x = x, y = y, fill = lc)) +
    scale_fill_manual(values = lcVizLookup[lcVizLookup$lcLabs %in% unique(lcSpatLocal2$lc),]$colors,
                      name="")+
    theme(legend.position = 'none',
          axis.title = element_blank()) +
    theme_1 + 
    coord_cartesian(xlim = c(extent(rastLocal2)[1],extent(rastLocal2)[2]),
                    ylim = c(extent(rastLocal2)[3],extent(rastLocal2)[4]), expand = c(0, 0))
  
  localGmap2 <- ggmap(gmap2)+
    theme_1+
    coord_cartesian(xlim = c(extent(rastLocal2)[1],extent(rastLocal2)[2]),
                    ylim = c(extent(rastLocal2)[3],extent(rastLocal2)[4]), expand = c(0, 0))
  
  localGrid2 <- grid.arrange(localGmap2,localMap2, nrow=1, widths=c(1,1), padding = unit(0, "line"), newpage = T) 
  
  
  lcSpatGlobal <- rasterToDf(rastGlobal)
  globalMap <- ggplot() +
    geom_raster(data = lcSpatGlobal , aes(x = x, y = y, fill = lc)) +
    scale_fill_manual(values =lcVizLookup$colors,
                      name="")+
    geom_sf(data = countries, fill=NA, inherit.aes=F, size=0.1) + 
    geom_point(data = tibble(x = 18.88825, y=-33.93723), aes(x=x, y=y))+
    geom_point(data = tibble(x = -55.43433, y=-7.06479), aes(x=x, y=y))+
    #coord_sf( expand = FALSE)+
    xlim(-170, 170) +
    ylim(-75,75)+ 
    annotate("text", -Inf, Inf, label = label, hjust = -0.5, vjust = 2)+
    theme_1 + 
    theme(legend.position = c(0.47, 0.35),
          legend.title = element_blank(),
          legend.key=element_blank()) +
    theme(legend.background = element_rect(fill="transparent", color="transparent")) +
    theme_legend_1 +
    annotation_custom(
      localGrid ,
      xmin = 35, xmax = 120, ymin = -80, ymax = -40)+
    annotation_custom(
      localGrid2 ,
      xmin = -180, xmax = -95, ymin = -50, ymax = -5)
  #globalMap
  
  if (!leg){
    globalMap <- globalMap + theme(legend.position = 'none')
  }
  
  return (globalMap)
}
dev.off()

gm1 <- makeGlobLocLCmap(dwGlobal, dwLocal,dwLocal2, 'A)', TRUE)
gm2 <- makeGlobLocLCmap(wcGlobal, wcLocal,wcLocal2,  'B', FALSE)
gm3 <- makeGlobLocLCmap(esriGlobal, esriLocal,esriLocal2, 'C)', FALSE)

fig1 <- grid.arrange(gm1, gm2, gm3, nrow=3, heights=c(1,1,1), padding = unit(0, "line"), newpage = T) 
dev.off()

ggsave("fig1.png", fig1, width = 20, height=28, units='cm')

#### Figure 2 - validation data maps -------------------------------------------------------------------
val_regional_metadata
val_global_metadata

theme_2 <- theme(
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  plot.margin=grid::unit(c(0,0,0,0), "mm")
)

val_global_metadata_spat <- val_global_metadata %>%
  st_as_sf(coords=c('Lon','Lat'), crs=4326) 
val_global_metadata_spat_trans <-val_global_metadata_spat %>%
  st_transform(4326) %>%
  mutate(Lon = unlist(map(.$geometry,1)),
         Lat = unlist(map(.$geometry,2)))


m1 <- val_global_metadata_spat %>%
  ggplot()  +
  geom_sf(data = countries , 
          fill = '#e8e8e8', 
          color = '#000000',
          size = 0.09,
          alpha=0.9) +
  geom_sf(shape=21, stroke=0.8, size=1, color='#f88077')+
  coord_sf( expand = FALSE,crs = 4326)+
  xlim(min(val_global_metadata_spat_trans$Lon),max(val_global_metadata_spat_trans$Lon)) +
  ylim(min(val_global_metadata_spat_trans$Lat),max(val_global_metadata_spat_trans$Lat)) +
  labs(title = 'A) Global validation tiles') + 
  theme_2
m1
m2 <- val_regional_metadata %>%
  sample_n(50000) %>%
  ggplot()  +
  geom_sf(data = countries, 
          fill = '#e8e8e8', 
          color = '#000000',
          size = 0.09,
          alpha=0.9) +
  geom_point(aes(x = Lon, y = Lat), shape='.', color='#f88077')+
  coord_sf( expand = FALSE)+
  xlim(min(val_regional_metadata$Lon),max(val_regional_metadata$Lon)) +
  ylim(min(val_regional_metadata$Lat),max(val_regional_metadata$Lat)) +
  labs(title = 'B) Regional validation points') + 
  theme_2
m2
fig2 <- grid.arrange(m1,m2, nrow=1, widths=c(3,1), padding = unit(0, "line"), newpage = T) 
ggsave("fig2.png", fig1, width = 35, height=10, units='cm')


#### Figure 3 - spatial correspondence -----------------------------------------------------------------
lc_areas 

makeSpatCorrSubPlot <- function(varName, title){
  
  datSub <- lc_areas %>%
    mutate(select = lc_areas[[varName]]) %>%
    dplyr::select(select, lc, seqnum) %>%
    pivot_wider(names_from=lc, values_from=select) %>%
    drop_na()
  
  datSubSum <- datSub %>%
    summarise_at(vars(dw, wc, esri), mean) %>%
    gather(product, perc)
  
  subBarPlot <- datSubSum %>%
    ggplot(aes(y=product, x=perc, fill=product)) +
    geom_bar(stat='identity') +
    scale_fill_manual(values = c('#efc500', '#ff6fff', '#00f5f3')) +
    labs(x = 'Global percentage cover (%)') + 
    xlim(0,30) +
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size=4),
          axis.title.x = element_text(size=4),
          legend.position = 'none',
          axis.ticks = element_line(size=0.1),
          axis.line = element_line(colour = "black", size = 0.1),
          panel.background = element_rect(fill = 'transparent'),
          panel.border = element_rect(size = 0.1,fill = NA),
          plot.background=element_rect(fill = 'transparent', color= 'transparent'))
  
  
  tric <- Tricolore(datSub, p1 = 'dw', p2 = 'wc', p3 = 'esri',
                    spread = 1.5,
                    show_data = FALSE,
                    breaks = Inf)
  #tric$key +
  #  theme(text = element_text(size=7))
  
  datSub$rgb <- tric$rgb
  DemoTricolore()
  datMainToMap <- hexGrid %>%
    left_join(datSub, by = 'seqnum') %>%
    filter(!is.na(dw)) %>%
    mutate(meanLc = (dw+wc+esri)/3,
           p99 = quantile(meanLc, c(0.99))) %>%
    mutate(meanLc = ifelse(meanLc > p99, p99, meanLc),
           meanLc= BBmisc::normalize(meanLc, method='range'))
  
  theme_3 <- theme(
    axis.text = element_blank(),
    plot.title = element_text(size=10),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.margin=grid::unit(c(0,0,0,0), "mm")
  )
  
  mapSub <- datMainToMap %>%
    ggplot() +
    geom_sf(color=NA, aes(fill=rgb, alpha=meanLc)) +
    geom_sf(data = countries, fill=NA, inherit.aes=F, size=0.1) + 
    scale_fill_identity()+
    coord_sf( expand = FALSE)+
    xlim(-175, 175) +
    ylim(-55,80) +
    labs(title = title) +
    theme_3 +
    theme(legend.position = 'none') +
    annotation_custom(
      ggplotGrob(tric$key +
                   theme(plot.background = element_rect(fill = NA, color = NA),
                         text = element_text(size=5)) ),
      xmin = -170, xmax = -80, ymin = -55, ymax = 5)+
    annotation_custom(
      ggplotGrob(subBarPlot ),
      xmin = 50, xmax = 100, ymin = -55, ymax = 0)
  #mapSub
  
  return (mapSub)
}
dev.off()
cm1 <- makeSpatCorrSubPlot("BU", "A) Built area")
cm2 <- makeSpatCorrSubPlot("C", "B) Crops")
cm3 <- makeSpatCorrSubPlot("BG", "C) Bare ground")
cm4 <- makeSpatCorrSubPlot("G", "D) Grass")
cm5 <- makeSpatCorrSubPlot("SS", "E) Shrub & scrub")
cm6 <- makeSpatCorrSubPlot("T", "F) Trees")
cm7 <- makeSpatCorrSubPlot("F", "G) Flooded vegetation")
cm8 <- makeSpatCorrSubPlot("W", "H) Water")
cm9 <- makeSpatCorrSubPlot("SI", "I) Snow & ice")

fig3 <- grid.arrange(cm1, cm2, cm3,
                     cm4, cm5, cm6,
                     cm7, cm8, cm9, nrow=3, heights=c(1,1,1), padding = unit(0, "line"), newpage = T) 
ggsave("fig3.png", fig3, width = 35, height=17, units='cm')
dev.off()



#### Figure 4&5 - accuracy global -----------------------------------------------------------------
val_global 
val_regional
colSums(is.na(val_global))
colSums(is.na(val_regional))

val_global_metadata %>% ggplot(aes(x=BIOME_NAME)) + geom_bar() + coord_flip()
val_regional_metadata %>% ggplot(aes(x=BIOME_NAME)) + geom_bar() + coord_flip()
val_global_metadata %>% ggplot(aes(x=continent)) + geom_bar() + coord_flip()

getDataAcc <- function(dataPred, dataTrue, name){
  cm <- confusionMatrix(dataPred,dataTrue)
  
  acc <- as.data.frame(cm$byClass)
  acc$LC <- str_replace(rownames(acc), 'Class: ', '')
  overall <- tibble(LC = 'Overall', Recall=NA, Precision=NA, Overall = cm$overall[[1]])
  acc <- acc %>% as_tibble() %>%
    dplyr::select(LC, Recall, Precision) %>%
    bind_rows(overall) %>%
    mutate_at(vars(Recall, Precision, Overall), function(x)x*100)
  return (acc)
}
dev.off()

valOut <- getDataAcc(val_global$dw, val_global$groundTruth) %>% mutate(product = 'Dynamic World') %>%
  bind_rows(getDataAcc(val_global$wc, val_global$groundTruth) %>% mutate(product = 'World Cover') )%>%
  bind_rows(getDataAcc(val_global$esri, val_global$groundTruth) %>% mutate(product = 'Esri Land Cover') ) %>%
  mutate(type = 'Global',
         typen = length(val_global$dw),
         cat = 'Global') 

for (i in unique(val_global_metadata$continent)){
  if (i %in% c('Oceania')) next
  
  subVal <- val_global %>%
    left_join(val_global_metadata) %>%
    drop_na(continent)%>%
    filter(continent == i) 
  
  subValOut <- getDataAcc(subVal$dw, subVal$groundTruth) %>% mutate(product = 'Dynamic World') %>%
    bind_rows(getDataAcc(subVal$wc, subVal$groundTruth) %>% mutate(product = 'World Cover') )%>%
    bind_rows(getDataAcc(subVal$esri, subVal$groundTruth) %>% mutate(product = 'Esri Land Cover') ) %>%
    mutate(type = i,
           typen = length(subVal$dw),
           cat = 'Continents') 
  
  valOut <- valOut %>%
    bind_rows(subValOut)
}

val_global %>%
  left_join(val_global_metadata)  %>%
  group_by(BIOME_NAME) %>%
  summarise(n=n())

for (i in unique(val_global_metadata$BIOME_NAME)){
  
  subVal <- val_global %>%
    left_join(val_global_metadata) %>%
    drop_na(BIOME_NAME)%>%
    filter(BIOME_NAME == i)
  subValOut <- getDataAcc(subVal$dw, subVal$groundTruth) %>% mutate(product = 'Dynamic World') %>%
    bind_rows(getDataAcc(subVal$wc, subVal$groundTruth) %>% mutate(product = 'World Cover') )%>%
    bind_rows(getDataAcc(subVal$esri, subVal$groundTruth) %>% mutate(product = 'Esri Land Cover') ) %>%
    mutate(type = i,
           typen = length(subVal$dw),
           cat = 'Biomes') 
  
  valOut <- valOut %>%
    bind_rows(subValOut)
}

for (i in unique(val_global_metadata$hsl)){
  
  subVal <- val_global %>%
    left_join(val_global_metadata) %>%
    drop_na(hsl)%>%
    filter(hsl == i)
  subValOut <- getDataAcc(subVal$dw, subVal$groundTruth) %>% mutate(product = 'Dynamic World') %>%
    bind_rows(getDataAcc(subVal$wc, subVal$groundTruth) %>% mutate(product = 'World Cover') )%>%
    bind_rows(getDataAcc(subVal$esri, subVal$groundTruth) %>% mutate(product = 'Esri Land Cover') ) %>%
    mutate(type = i,
           typen = length(subVal$dw),
           cat = 'Settlement') 
  
  valOut <- valOut %>%
    bind_rows(subValOut)
}

hist(valOut$typen)

width <- 0.6
textSize <- 1.75

dummyData <- tibble(type=c('Global (n=72MM)','Global (n=72MM)','Global (n=72MM)'),
                    cat=c('Global','Global','Global'),
                    x=c(-1,-1,-1),
                    shape = c('Recall','Precision','Overall')) %>%
  mutate(cat = factor(cat, levels = c('Biomes', 'Settlement', 'Global')))

fig4 <- valOut %>%
  mutate(type = paste0(type, ' (n=', round(typen/1000000), 'MM)')) %>%
  filter(cat != 'Continents')  %>%
  mutate(Rpos = ifelse(Recall < Precision, Recall-12, Recall+12),
         Ppos = ifelse(Precision < Recall, Precision-12, Precision+12)) %>%
  mutate(LC = factor(LC, levels = c(lcVizLookup$lcLabs, 'Overall'))) %>%
  mutate(cat = factor(cat, levels = c('Global', 'Biomes', 'Settlement' )))%>%
  ggplot(aes(y=type)) +
  geom_linerange(aes(xmin=Precision, xmax=Recall, color=product), 
                 position=position_dodge(width=width)) +
  geom_point(aes(x = Recall, color=product), shape=1, position=position_dodge(width=width)) +
  geom_point(aes(x=Precision, color=product), shape=4, position=position_dodge(width=width)) +
  geom_point(aes(x=Overall,color=product), shape=16, position=position_dodge(width=width)) +
  geom_text(aes(x=Rpos, label=round(Recall,0), group=product), color='black', size=textSize, position=position_dodge(width=width))+
  geom_text(aes(x=Ppos, label=round(Precision,0), group=product), color='black', size=textSize, position=position_dodge(width=width))+
  geom_text(aes(x=Overall-12, label=round(Overall,0), group=product), color='black', size=textSize, position=position_dodge(width=width))+
  labs(x = 'Accuracy (%)') +
  geom_point(data=dummyData, aes(x = x, shape=shape)) +
  scale_shape_manual(values = c(16,4,1), name = 'Accuracy') +
  scale_color_manual(values = c('#efc500', '#ff6fff', '#00f5f3'), name='') +
  facet_grid(cat~LC,space='free_y', scales='free_y') +
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=7),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=8),
        panel.grid.major.x = element_line( size=.1, color="#000000" ),
        legend.position="top", 
        legend.box = "horizontal",
        axis.title.y=element_blank(),
        axis.text = element_text(size=7)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 16)) +
  scale_x_continuous(breaks=c(0,30,60,90), limits = c(0,120))

fig4

ggsave("fig4.png", fig4, width = 26, height=16, units='cm')


dummyData <- tibble(type=c('Africa (n=14MM)','Africa (n=14MM)','Africa (n=14MM)'),
                    cat=c('Continents','Continents','Continents'),
                    x=c(-1,-1,-1),
                    shape = c('Recall','Precision','Overall')) 

fig5 <- valOut %>%
  mutate(type = paste0(type, ' (n=', round(typen/1000000), 'MM)')) %>%
  filter(cat == 'Continents')  %>%
  mutate(Rpos = ifelse(Recall < Precision, Recall-12, Recall+12),
         Ppos = ifelse(Precision < Recall, Precision-12, Precision+12)) %>%
  mutate(LC = factor(LC, levels = c(lcVizLookup$lcLabs, 'Overall'))) %>%
  ggplot(aes(y=type)) +
  geom_linerange(aes(xmin=Precision, xmax=Recall, color=product), 
                 position=position_dodge(width=width)) +
  geom_point(aes(x = Recall, color=product), shape=1, position=position_dodge(width=width)) +
  geom_point(aes(x=Precision, color=product), shape=4, position=position_dodge(width=width)) +
  geom_point(aes(x=Overall,color=product), shape=16, position=position_dodge(width=width)) +
  geom_text(aes(x=Rpos, label=round(Recall,0), group=product), color='black', size=textSize, position=position_dodge(width=width))+
  geom_text(aes(x=Ppos, label=round(Precision,0), group=product), color='black', size=textSize, position=position_dodge(width=width))+
  geom_text(aes(x=Overall-12, label=round(Overall,0), group=product), color='black', size=textSize, position=position_dodge(width=width))+
  labs(x = 'Accuracy (%)') +
  geom_point(data=dummyData, aes(x = x, shape=shape)) +
  scale_shape_manual(values = c(16,4,1), name = 'Accuracy') +
  scale_color_manual(values = c('#efc500', '#ff6fff', '#00f5f3'), name='') +
  facet_grid(cat~LC,space='free_y', scales='free_y') +
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=7),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=8),
        panel.grid.major.x = element_line( size=.1, color="#000000" ),
        legend.position="top", 
        legend.box = "horizontal",
        axis.title.y=element_blank(),
        axis.text = element_text(size=7)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(breaks=c(0,30,60,90), limits = c(0,120))

fig5

ggsave("fig5.png", fig5, width = 26, height=10, units='cm')


#### Figure 6 - accuracy regional - Europe -------------------------------------------------------

valOut <- getDataAcc(val_regional$dw, val_regional$groundTruth) %>% mutate(product = 'Dynamic World') %>%
  bind_rows(getDataAcc(val_regional$wc, val_regional$groundTruth) %>% mutate(product = 'World Cover') )%>%
  bind_rows(getDataAcc(val_regional$esri, val_regional$groundTruth) %>% mutate(product = 'Esri Land Cover') ) %>%
  mutate(type = 'European',
         typen = length(val_regional$dw),
         cat = 'European') 

for (i in unique(val_regional_metadata$BIOME_NAME)){
  
  subVal <- val_regional %>%
    left_join(val_regional_metadata ) %>%
    filter(!is.na(BIOME_NAME)) %>%
    filter(BIOME_NAME == i)
  subValOut <- getDataAcc(subVal$dw, subVal$groundTruth) %>% mutate(product = 'Dynamic World') %>%
    bind_rows(getDataAcc(subVal$wc, subVal$groundTruth) %>% mutate(product = 'World Cover') )%>%
    bind_rows(getDataAcc(subVal$esri, subVal$groundTruth) %>% mutate(product = 'Esri Land Cover') ) %>%
    mutate(type = i,
           typen = length(subVal$dw),
           cat = 'Biomes') 
  
  valOut <- valOut %>%
    bind_rows(subValOut)
}

for (i in unique(val_regional_metadata$hsl)){
  
  subVal <- val_regional %>%
    left_join(val_regional_metadata) %>%
    drop_na(hsl)%>%
    filter(hsl == i)
  subValOut <- getDataAcc(subVal$dw, subVal$groundTruth) %>% mutate(product = 'Dynamic World') %>%
    bind_rows(getDataAcc(subVal$wc, subVal$groundTruth) %>% mutate(product = 'World Cover') )%>%
    bind_rows(getDataAcc(subVal$esri, subVal$groundTruth) %>% mutate(product = 'Esri Land Cover') ) %>%
    mutate(type = i,
           typen = length(subVal$dw),
           cat = 'Settlement') 
  
  valOut <- valOut %>%
    bind_rows(subValOut)
}


width <- 0.6
textSize <- 1.75

dummyData <- tibble(type=c('European (n=338k)','European (n=338k)','European (n=338k)'),
                    cat=c('European','European','European'),
                    x=c(-1,-1,-1),
                    shape = c('Recall','Precision','Overall')) %>%
  mutate(cat = factor(cat, levels = c('Biomes', 'Settlement', 'European')))

fig6 <- valOut %>%
  mutate(type = paste0(type, ' (n=', round(typen/1000), 'k)')) %>%
  mutate(Rpos = ifelse(Recall < Precision, Recall-12, Recall+12),
         Ppos = ifelse(Precision < Recall, Precision-12, Precision+12)) %>%
  mutate(LC = factor(LC, levels = c(lcVizLookup$lcLabs, 'Overall'))) %>%
  mutate(cat = factor(cat, levels = c('European', 'Biomes', 'Settlement' )))%>%
  ggplot(aes(y=type)) +
  geom_linerange(aes(xmin=Precision, xmax=Recall, color=product), 
                 position=position_dodge(width=width)) +
  geom_point(aes(x = Recall, color=product), shape=1, position=position_dodge(width=width)) +
  geom_point(aes(x=Precision, color=product), shape=4, position=position_dodge(width=width)) +
  geom_point(aes(x=Overall,color=product), shape=16, position=position_dodge(width=width)) +
  geom_text(aes(x=Rpos, label=round(Recall,0), group=product), color='black', size=textSize, position=position_dodge(width=width))+
  geom_text(aes(x=Ppos, label=round(Precision,0), group=product), color='black', size=textSize, position=position_dodge(width=width))+
  geom_text(aes(x=Overall-12, label=round(Overall,0), group=product), color='black', size=textSize, position=position_dodge(width=width))+
  labs(x = 'Accuracy (%)') +
  geom_point(data=dummyData, aes(x = x, shape=shape)) +
  scale_shape_manual(values = c(16,4,1), name = 'Accuracy') +
  scale_color_manual(values = c('#efc500', '#ff6fff', '#00f5f3'), name='') +
  facet_grid(cat~LC,space='free_y', scales='free_y') +
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=7),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=8),
        panel.grid.major.x = element_line( size=.1, color="#000000" ),
        legend.position="top", 
        legend.box = "horizontal",
        axis.title.y=element_blank(),
        axis.text = element_text(size=7)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 16)) +
  scale_x_continuous(breaks=c(0,30,60,90), limits = c(0,120))

fig6

ggsave("fig6.png", fig6, width = 26, height=14, units='cm')





#### Figure 7 - Dynamic World capabilities ------------------------------------------------------
plot(dwLocal_eg1)
plot(dwLocal_eg2)
plot(dwLocal_eg3)

theme_legend_2 <- theme(legend.key.size = unit(0.25, 'cm'), #change legend key size
                        legend.key.height = unit(0.25, 'cm'), #change legend key height
                        legend.key.width = unit(0.25, 'cm'), #change legend key width
                        legend.title = element_text(size=6), #change legend title font size
                        legend.text = element_text(size=6))


gmap3 <- get_googlemap(center = getCent(dwLocal_eg1), zoom = 15, maptype = "satellite")
ggmap(gmap3)

lcSpatLocal4 <- rasterToDf(dwLocal_eg1)
lcSpatLocal5 <- rasterToDf(dwLocal_eg3)

localGmap3 <- ggmap(gmap3)+
  scale_x_continuous(n.breaks=4) +
  scale_y_continuous(n.breaks=4) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size=5),
        axis.ticks = element_blank())+
  coord_cartesian(xlim = c(extent(dwLocal_eg1)[1],extent(dwLocal_eg1)[2]),
                  ylim = c(extent(dwLocal_eg1)[3],extent(dwLocal_eg1)[4]), expand = c(0, 0)) +
  annotate("text", -Inf, Inf, label = "A", hjust = -0.5, vjust = 2, color='white', size=6)
localGmap3
dev.off()
localDWorig <- ggplot() +
  geom_raster(data = lcSpatLocal4 , aes(x = x, y = y, fill = lc)) +
  scale_fill_manual(values = lcVizLookup[lcVizLookup$lcLabs %in% unique(lcSpatLocal4$lc),]$colors)+
  theme(legend.position = 'none',
        axis.title = element_blank()) +
  theme_1 + 
  coord_cartesian(xlim = c(extent(dwLocal_eg1)[1],extent(dwLocal_eg1)[2]),
                  ylim = c(extent(dwLocal_eg1)[3],extent(dwLocal_eg1)[4]), expand = c(0, 0)) +
  annotate("text", -Inf, Inf, label = "B", hjust = -0.5, vjust = 2, color='white', size=6)

localDWreclass <- ggplot() +
  geom_raster(data = lcSpatLocal5 , aes(x = x, y = y, fill = lc)) +
  scale_fill_manual(values = lcVizLookup[lcVizLookup$lcLabs %in% unique(lcSpatLocal5$lc),]$colors)+
  theme_1 + 
  theme_legend_2 +
  theme(legend.position = c(0.2, 0.2),
        legend.background = element_rect(fill=alpha('white',0.7)),
        legend.title = element_blank(),
        axis.title = element_blank()) +
  coord_cartesian(xlim = c(extent(dwLocal_eg1)[1],extent(dwLocal_eg1)[2]),
                  ylim = c(extent(dwLocal_eg1)[3],extent(dwLocal_eg1)[4]), expand = c(0, 0)) +
  annotate("text", -Inf, Inf, label = "D", hjust = -0.5, vjust = 2, color='white', size=6)


names(dwLocal_eg2) <- 'treeprob'
treeRastDF <- as.data.frame(dwLocal_eg2, xy = TRUE) %>%
  na.omit() %>%
  as_tibble() 

localDWtree <- ggplot()+
  geom_raster(data = treeRastDF , aes(x = x, y = y, fill = treeprob)) +
  scale_fill_gradientn(colours = c('black','white'), 
                       limits = c(0.2,0.8),
                       name = 'Tree prob.',
                       oob = scales::squish) +
  theme_1 +
  theme(legend.position = c(0.1, 0.2),
        axis.title = element_blank()) + 
  theme_legend_2 +
  coord_cartesian(xlim = c(extent(dwLocal_eg1)[1],extent(dwLocal_eg1)[2]),
                  ylim = c(extent(dwLocal_eg1)[3],extent(dwLocal_eg1)[4]), expand = c(0, 0)) +
  annotate("text", -Inf, Inf, label = "C", hjust = -0.5, vjust = 2, color='black', size=6)



fig7 <- grid.arrange(localGmap3,localDWorig, localDWtree, localDWreclass, nrow=2, widths=c(1,1), padding = unit(0, "line"), newpage = T) 
dev.off()

ggsave("fig7.png", fig7, width = 15, height=15, units='cm')
