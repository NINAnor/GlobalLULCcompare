var geometry = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[23.16838006397435, 50.24329062955167],
          [23.16838006397435, 43.50228942242464],
          [32.506758970224354, 43.50228942242464],
          [32.506758970224354, 50.24329062955167]]], null, false),
    geometry2 = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[18.881128124059114, -33.93183438425864],
          [18.881128124059114, -33.94337025730755],
          [18.895891002477082, -33.94337025730755],
          [18.895891002477082, -33.93183438425864]]], null, false),
    geometry3 = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-13.582928709575972, 70.93095515747594],
          [-13.582928709575972, 30.97748214423164],
          [38.448321290424026, 30.97748214423164],
          [38.448321290424026, 70.93095515747594]]], null, false),
    geometry4 = 
    /* color: #98ff00 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-55.464199777573945, -7.037106333152468],
          [-55.464199777573945, -7.089747131181427],
          [-55.408753152818086, -7.089747131181427],
          [-55.408753152818086, -7.037106333152468]]], null, false);


// Author: Zander Venter - zander.venter@nina.no

// Workflow in this script:
  // 1. Import global LULC datasets (Dynamic World pre-exported)
  // 2. Import data for validation site metadata
  // 3. Export tables for generating maps and figures in R
  // 4. Export images for maps in R


/*
  // Setup global environment ///////////////////////////////////////////////////////////////////////////////
*/

Map.setOptions('HYBRID');

var globalBounds = ee.Geometry.Polygon([-180, 90, 0, 90, 180, 90, 180, -90, 10, -90, -180, -90], null, false);

var globalSquareGrid = ee.Image.random(123).multiply(1e6).toInt()
    .reduceToVectors({
      reducer: ee.Reducer.countEvery(),
      geometry: globalBounds,
      geometryType: 'bb' ,
      eightConnected: false,
      scale: 2500000,
      crs: 'EPSG:4326'
    });
Map.addLayer(globalSquareGrid, {}, 'globalSquareGrid', 0)

var lcDict = {
  labels: [
    "Build area", //1
    "Crops", //2
    "Bare ground", //3
    "Grass", //4
    "Shrub & scrub", //5
    "Trees", //6
    "Flooded vegetation", //7
    "Water", //8
    "Snow & ice" //9
    ],
  colors: [
    '#C4281B',
    '#E49635',
    '#A59B8F',
    '#88B053',
    '#DFC35A',
    '#397D49',
    '#f2bac1', // '#7A87C6'
    '#0002f6', // '#419BDF'
    '#6fdbde' // #B39FE1'
  ]
}

var lcVizParams = {
  min: 1,
  max: 9,
  palette: lcDict['colors']
};

/*
  // Import, transform and visualize datasets ///////////////////////////////////////////////////////////////////////////////
*/

//// Global hex grid --------------------------------------------------------------
// Shapefile created in the "setu.R" script
var globalHexGrid = ee.FeatureCollection('users/zandersamuel/Global_misc/global_hex_grid_75kkm2');
Map.addLayer(globalHexGrid, {}, 'globalGrid', 0)


//// Validation data -------------------------------------------------------------------

// Global reference data
// Data downloaded from here: https://zenodo.org/record/4766508#.YrsDtnZBzIU
var validation = ee.ImageCollection('projects/nina/GIS_synergy/Extent/DW_global_validation_tiles')
  .select([1], ['grouhdtruth']);

// Reclassify typology
validation = validation.map(function(i){
  return i.updateMask(i.neq(0)).subtract(1).remap({
    from:[0,1,2,3,4,5,6,7,8],
    to:  [8,6,4,7,2,5,1,3,9]
  }).copyProperties(i)
})

//Map.centerObject(validation.first())
Map.addLayer(validation.mean(), lcVizParams, 'validation', 0)

var validationCentroids = validation.map(function(i){
  return ee.Feature(i.geometry().centroid(100), {
    S2_GEE_ID: i.get('S2_GEE_ID'), dw_id: i.get('dw_id')
  })
})
validationCentroids = ee.FeatureCollection(validationCentroids)
Map.addLayer(validationCentroids, {}, 'validationCentroids',0)

// Regional reference data for EU from here: https://www.nature.com/articles/s41597-020-00675-z
  // in GEE data catalogue
var lucas = ee.FeatureCollection("JRC/LUCAS_HARMO/THLOC/V1")
  .filter(ee.Filter.eq('year', 2018))
  .select(['lc1', 'point_id']);
print(lucas.size(), 'lucas size')
Map.addLayer(lucas, {}, 'lucas',0);


//// ESRI 2020 ------------------------------------------------------------------------
// From here: https://samapriya.github.io/awesome-gee-community-datasets/projects/esrilc2020/
var esri = ee.ImageCollection("projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m").mosaic();
esri = esri.remap({
  from: [1,2,3,4,5,6,7,8,9,10],
  to:   [8,6,4,7,2,5,1,3,9,0]
})
Map.addLayer(esri, lcVizParams, 'esri',0)


//// World cover ------------------------------------------------------------------
var worldcover = ee.ImageCollection("ESA/WorldCover/v100").first().rename('wc');
worldcover = worldcover.divide(10).remap({
  from: [1,2,3,4,5,6,7,8,9,9.5,10],
  to:   [6,5,4,2,1,3,9,8,7,7,3]
});
worldcover = worldcover.updateMask(esri)
Map.addLayer(worldcover, lcVizParams, 'worldcover',0)


//// Dynamic World ------------------------------------------------------------------------
// Pre-exported annual mosaic for 2020 from script "DW_global_mosaic_2020.js"
var dynamicWorld = ee.ImageCollection('projects/nina/GIS_synergy/Extent/DW_global_2020')
  .merge(ee.ImageCollection('users/rangelandee/DW_global_2020'))
  .merge(ee.ImageCollection('users/grazingresearch/DW_global_2020'))
  .map(function(i){
    var imgLabel = i.remap({
      from:[0,1,2,3,4,5,6,7,8],
      to:  [8,6,4,7,2,5,1,3,9]
    })
    return imgLabel
  });
dynamicWorld = dynamicWorld.mosaic()
dynamicWorld = dynamicWorld.updateMask(esri)
Map.addLayer(dynamicWorld, lcVizParams, 'dynamicWorld preExport', 0);

//// Continents -------------------------------------------------------------------------
var continents = ee.FeatureCollection('users/zandersamuel/Global_misc/continents');
Map.addLayer(continents, {}, 'continents',0)

//// Biomes -------------------------------------------------------------------------
var ecoRegions = ee.FeatureCollection("RESOLVE/ECOREGIONS/2017")
  .select(['BIOME_NAME', 'ECO_NAME']);
Map.addLayer(ecoRegions, {}, 'ecoRegions',0)

//// Human settlement --------------------------------------------------------------
var hsl = ee.ImageCollection("JRC/GHSL/P2016/SMOD_POP_GLOBE_V1")
  .filter(ee.Filter.calendarRange(2015,2015,'year')).mean();
Map.addLayer(hsl, {min:0, max:3}, 'hsl',0)

/*
  // Export tables ///////////////////////////////////////////////////////////////////////////////
*/

//// Land cover proportions per grid ------------------------------------------------------------------
globalHexGrid = worldcover.reduceRegions(globalHexGrid, ee.Reducer.firstNonNull(), 5000)
  .filter(ee.Filter.notNull(['first']));
Map.addLayer(globalHexGrid, {}, 'global grid filtered',0)
print(globalHexGrid.size(),'globalHexGrid.size()')

var globalHexGridCentroids = globalHexGrid.map(function(ft){
  return ft.centroid(100)
})

globalSquareGrid = worldcover.reduceRegions(globalSquareGrid, ee.Reducer.firstNonNull(), 5000)
  .filter(ee.Filter.notNull(['first']));
Map.addLayer(globalSquareGrid, {}, 'global suare grid filtered',0)
print(globalSquareGrid.size(), 'globalSquareGrid.size()')

var gridList = globalSquareGrid 
  //.filterBounds(geometry) //// !!!!!! use for de-bugging and testing
  .toList(1000);

// Change to length of list when ready to run all the exports
for (var i = 0; i<1; i++){
  var aoi = ee.Feature(gridList.get(i)).geometry()
  //Map.addLayer(aoi)
  var hexSelectNames = globalHexGridCentroids.filterBounds(aoi)
    .reduceColumns(ee.Reducer.toList(), ['seqnum']).get('list');
  var hexSelect = globalHexGrid.filter(ee.Filter.inList('seqnum', hexSelectNames));
  
  var lcImgCol = ee.ImageCollection([
    dynamicWorld.set('lc','dw'),
    worldcover.set('lc','wc'),
    esri.set('lc','esri')
    ]);
  lcImgCol = lcImgCol.map(function(x){
    return x
      .eq([1,2,3,4,5,6,7,8,9])
      .selfMask()
      .rename(['BU','C','BG','G','SS','T','F','W','SI'])
      .copyProperties(x)
  });
  
  var tableOut = lcImgCol.map(function(y){
    var tableInner = y.reduceRegions({
      collection: hexSelect, 
      reducer: ee.Reducer.count(), 
      scale: 100,
      tileScale: 2
    });
    tableInner = tableInner.map(function(ft){ 
      return ft.setGeometry(null).set('lc', y.get('lc'))
    })
    return tableInner
  }).flatten()
  
  print(tableOut.limit(2))
  
  Export.table.toDrive({
    collection: tableOut, 
    description: 'lc_areas_' + String(i) , 
    fileFormat: 'CSV'
  });
  
}

// After running the script, you will have a whole lot of export tasks ready to run
// Instead of clicking on each one individually, you can:
  // Copy the two functions below
  // Click F12 on your computer
  // In the "Console" tab paste the two functions and hit Enter
  // Then type 'runTaskList()' in the Console and hit Enter
  // Wait a few seconds - roughly 30 seconds wait per 100 pts
  // Then type 'confirmAll()' and hit Enter
  // All the tasks should start running at once.

//var runTaskList = () => document.querySelector("#task-pane").shadowRoot.querySelectorAll(".task .run-button").forEach(x => x.click());
//var confirmAll = () => document.querySelectorAll("ee-table-config-dialog").forEach(x => x.shadowRoot.querySelector("ee-dialog").shadowRoot.querySelector(".ok-button").click());


//// Site context - global validation sites ----------------------------------------------------------

var continentsToExport = validationCentroids.filterBounds(continents)
  .map(function(ft){
    var val = continents.filterBounds(ft.geometry()).first();
    return ft.set('continent', val.get('CONTINENT'))
  });
print(continentsToExport.limit(10), 'continentsToExport global')
Export.table.toDrive({
  collection: continentsToExport,
  description: 'continents_global_val',
  fileFormat: 'CSV'
})

var ecoRegionsToExport = validationCentroids
  .map(function(ft){
    var val = ecoRegions.filterBounds(ft.geometry()).first();
    return ft.set('BIOME_NAME', val.get('BIOME_NAME'), 'ECO_NAME', val.get('ECO_NAME'))
  });
print(ecoRegionsToExport.limit(10), 'ecoRegionsToExport global')
Export.table.toDrive({
  collection: ecoRegionsToExport,
  description: 'ecoregion_global_val',
  fileFormat: 'CSV'
})

var hslToExport = hsl.reduceRegions({
  collection: validationCentroids, 
  reducer: ee.Reducer.first(), 
  scale: 1000
});
hslToExport = hslToExport.select([ 'dw_id', 'first'], [ 'dw_id', 'hsl'])
print(hslToExport.limit(10), 'hslToExport global')
Export.table.toDrive({
  collection: hslToExport,
  description: 'hsl_global_val',
  fileFormat: 'CSV'
})
//// Site context - regional validation sites ----------------------------------------------------------

var ecoRegionsToExport = lucas.filterBounds(ecoRegions)
  .map(function(ft){
    var val = ecoRegions.filterBounds(ft.geometry()).first();
    return ft.set('BIOME_NAME', val.get('BIOME_NAME'), 'ECO_NAME', val.get('ECO_NAME'))
  });
print(ecoRegionsToExport.limit(10), 'ecoRegionsToExport regional')
Export.table.toDrive({
  collection: ecoRegionsToExport,
  description: 'ecoregion_regional_val',
  fileFormat: 'CSV'
})

var hslToExport = hsl.reduceRegions({
  collection: lucas, 
  reducer: ee.Reducer.first(), 
  scale: 1000
});
hslToExport = hslToExport.select([ 'point_id', 'first'], [ 'point_id', 'hsl'])
print(hslToExport.limit(10), 'hslToExport regional')
Export.table.toDrive({
  collection: hslToExport,
  description: 'hsl_regional_val',
  fileFormat: 'CSV'
})


//// Global validation pixel values ------------------------------------------------------------------

//validation = validation
//  .filterBounds(geometry) //////// !!!!!  use for de-bugging and testing

var valList = validation.toList(1000);
print(valList)

var pixelVals = validation.map(function(i){
  var lcStack = i.rename('groundTruth')
    .addBands(dynamicWorld.rename('dw'))
    .addBands(worldcover.rename('wc'))
    .addBands(esri.rename('esri'));
  
  var outTable = lcStack.sample({
    region: i.geometry(), 
    scale: 10, 
    //factor: 1, 
    //seed: 123
    })
  outTable = outTable.filter(ee.Filter.notNull(['groundTruth','dw']));
  //outTable = outTable.map(function(x){ return x.set('dw_id', i.get('dw_id'))})
  
  return outTable
}).flatten();
print(pixelVals.limit(10), 'dw val tiles out')

Export.table.toDrive({
  collection: pixelVals, 
  description: 'validation_global_DW_tiles' , 
  fileFormat: 'CSV'
});

//// European LUCAS pixel values -------------------------------------------

var lucasExportGrid = ee.Image.random(123).multiply(1e6).toInt()
    .reduceToVectors({
      reducer: ee.Reducer.countEvery(),
      geometry: geometry3,
      geometryType: 'bb' ,
      eightConnected: false,
      scale: 1000000,
      crs: 'EPSG:4326'
    });

var lucasIce = lucas.filter(ee.Filter.eq('lc1', 'G50'));
lucasIce = lucasIce.map(function(ft){
  return ft.set('lcLab', 'G50', 'lcNum', 9)
})

lucas = lucas.filter(ee.Filter.neq('lc1', 'G50'));
lucas = lucas.map(function(ft){
  var lcSimp = ee.String(ft.get('lc1')).slice(0,1)
  return ft.set('lcLab', lcSimp, 'lcNum', lcSimp)
})
lucas = lucas.filter(ee.Filter.neq('lcLab', ''));

// Lookup can be found here: https://data.jrc.ec.europa.eu/dataset/f85907ae-d123-471f-a44a-8cca993485a2#dataaccess
lucas = lucas.remap(
  ["A", "B", "C", "D", "E", "F", "G", "H"], 
  [1,2,6,5,4,3,8,7], 
  'lcNum');

lucas = lucas.merge(lucasIce)
//lucas = lucas
//  .filterBounds(geometry) //////// !!!!!  use for de-bugging and testing

var lcStack = dynamicWorld.rename('dw')
  .addBands(worldcover.rename('wc'))
  .addBands(esri.rename('esri'));

var lucasGridList = lucasExportGrid.toList(1000);

// Change to length of list when ready to run all the exports
for (var i = 0; i<2; i++){
  var aoi = ee.Feature(lucasGridList.get(i)).geometry();
  
  var lucasSel = lucas.filterBounds(aoi);
  
  var lucasOut = lcStack.sampleRegions({
    collection: lucasSel, 
    scale: 10,
    geometries: true,
    tileScale: 2
  });
  //print(lucasOut.limit(10), 'lucas out')
  
  Export.table.toDrive({
    collection: lucasOut, 
    description: 'validation_european_LUCAS_pts_' + String(i) , 
    fileFormat: 'CSV'
  });
}



/*
  // Export images ///////////////////////////////////////////////////////////////////////////////
*/

//// Global extent exports --------------------------------------------------------------

var dwToExport = dynamicWorld
  .reduceResolution(ee.Reducer.mode(), true, 100)
  .reproject(worldcover.projection().atScale(2000));

Export.image.toDrive({
  image: dynamicWorld,
  description: 'dynamicworld_global',
  scale: 2000,
  region: globalBounds,
  crs: 'EPSG:4326',
  maxPixels: 1e11
})

var wcToExport = worldcover
  .reduceResolution(ee.Reducer.mode(), true, 100)
  .reproject(worldcover.projection().atScale(2000));

Export.image.toDrive({
  image: wcToExport,
  description: 'worldcover_global',
  scale: 2000,
  region: globalBounds,
  crs: 'EPSG:4326',
  maxPixels: 1e11
})

var esriToExport = esri
  .reduceResolution(ee.Reducer.mode(), true, 100)
  .reproject(worldcover.projection().atScale(2000));

Export.image.toDrive({
  image: esri,
  description: 'esri_global',
  scale: 2000,
  region: globalBounds,
  crs: 'EPSG:4326',
  maxPixels: 1e11
})


//// Local extent exports --------------------------------------------------------------
Export.image.toDrive({
  image: worldcover,
  description: 'worldcover_local_1',
  scale: 10,
  region: geometry2,
  crs: 'EPSG:4326'
})
Export.image.toDrive({
  image: dynamicWorld,
  description: 'dynamicworld_local_1',
  scale: 10,
  region: geometry2,
  crs: 'EPSG:4326'
})
Export.image.toDrive({
  image: esri,
  description: 'esri_local_1',
  scale: 10,
  region: geometry2,
  crs: 'EPSG:4326'
})

Export.image.toDrive({
  image: worldcover,
  description: 'worldcover_local_2',
  scale: 10,
  region: geometry4,
  crs: 'EPSG:4326'
})
Export.image.toDrive({
  image: dynamicWorld,
  description: 'dynamicworld_local_2',
  scale: 10,
  region: geometry4,
  crs: 'EPSG:4326'
})
Export.image.toDrive({
  image: esri,
  description: 'esri_local_2',
  scale: 10,
  region: geometry4,
  crs: 'EPSG:4326'
})