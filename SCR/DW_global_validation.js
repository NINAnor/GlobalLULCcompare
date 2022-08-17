var geometry = 
    /* color: #d63000 */
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

// Set the satellite imagery with labels as the default basemap
Map.setOptions('HYBRID');

// Create a single polygon with a global extent
var globalBounds = ee.Geometry.Polygon([-180, 90, 0, 90, 180, 90, 180, -90, 10, -90, -180, -90], null, false);

// Create a random image that is then converted to a (square) grid
var globalSquareGrid = ee.Image.random(123).multiply(1e6).toInt()
    .reduceToVectors({
      reducer: ee.Reducer.countEvery(),
      geometry: globalBounds,
      geometryType: 'bb' ,
      eightConnected: false,
      scale: 2500000,
      crs: 'EPSG:4326'
    });
// Visualise Global grid
Map.addLayer(globalSquareGrid, {}, 'globalSquareGrid', 0)

// Defining a single color palette (as a dictionary) for visualising the three global lulc products.
// Keys correspond to 9 lulc classes and values correspond to hex colour codes (used as palette later)
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
    '#f2bac1', 
    '#0002f6',
    '#6fdbde'
  ]
}

// Defines vis params in a format expected by GEE when using map.addlayer.
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

// Reclassify typology for dynamic world validation data (above)
validation = validation.map(function(i){
  return i.updateMask(i.neq(0)).subtract(1).remap({
    from:[0,1,2,3,4,5,6,7,8],
    to:  [8,6,4,7,2,5,1,3,9]
  }).copyProperties(i)//copy metadata from each image
})

//Map.centerObject(validation.first())
Map.addLayer(validation.mean(), lcVizParams, 'validation', 0)

//get validation centroids for dynamic world validation tiles. set unique id for each centroid
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
// load and reclassify esri global lulc data to common typology
var esri = ee.ImageCollection("projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m").mosaic();
esri = esri.remap({
  from: [1,2,3,4,5,6,7,8,9,10],
  to:   [8,6,4,7,2,5,1,3,9,0]
})
Map.addLayer(esri, lcVizParams, 'esri',0)


//// World cover ------------------------------------------------------------------
// load and reclassify esri global lulc data to common typology
var worldcover = ee.ImageCollection("ESA/WorldCover/v100").first().rename('wc');
worldcover = worldcover.divide(10).remap({
  from: [1,2,3,4,5,6,7,8,9,9.5,10],
  to:   [6,5,4,2,1,3,9,8,7,7,3]
});
Map.addLayer(worldcover, lcVizParams, 'worldcover',0)


//// Dynamic World ------------------------------------------------------------------------
// Pre-exported annual mosaic for 2020 from script "DW_global_mosaic_2020.js"
var dynamicWorld = ee.ImageCollection('projects/nina/GIS_synergy/Extent/DW_global_2020_mode')
  .map(function(i){
    var imgLabel = i.remap({
      from:[0,1,2,3,4,5,6,7,8],
      to:  [8,6,4,7,2,5,1,3,9]
    })
    return imgLabel
  });
dynamicWorld = dynamicWorld.mosaic()
Map.addLayer(dynamicWorld, lcVizParams, 'dynamicWorld preExport', 0);

// updatemasks of LULC products to one another so that area calculations are comparable
worldcover = worldcover.updateMask(esri).updateMask(dynamicWorld);
dynamicWorld = dynamicWorld.updateMask(esri).updateMask(worldcover);
esri = esri.updateMask(dynamicWorld).updateMask(worldcover);

// Load other variables that serve for calculating accuracy per group
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
// Remove (filter out) grid cells with no data (nulls)
globalHexGrid = worldcover.reduceRegions(globalHexGrid, ee.Reducer.firstNonNull(), 5000)
  .filter(ee.Filter.notNull(['first']));
Map.addLayer(globalHexGrid, {}, 'global grid filtered',0)//visualise hex grid
print(globalHexGrid.size(),'globalHexGrid.size()')//get number of valid cells (cells with data)

// Get centroids for each grid cell
var globalHexGridCentroids = globalHexGrid.map(function(ft){
  return ft.centroid(100)
})

// Remove (filter out) grid cells with no data (nulls)
globalSquareGrid = worldcover.reduceRegions(globalSquareGrid, ee.Reducer.firstNonNull(), 5000)
  .filter(ee.Filter.notNull(['first']));
Map.addLayer(globalSquareGrid, {}, 'global suare grid filtered',0)
print(globalSquareGrid.size(), 'globalSquareGrid.size()')

// global square grid is iterated through in for loop to extract data. By converting 
// the square grid to a list an iterable is created.
var gridList = globalSquareGrid 
  //.filterBounds(geometry) //// !!!!!! use for de-bugging and testing
  .toList(1000);

// Change to length of list (to size of square features (+1) in the aquare grid) 
// when ready to run all the exports
for (var i = 0; i<96; i++){
  var aoi = ee.Feature(gridList.get(i)).geometry()
  //Map.addLayer(aoi)
  var hexSelectNames = globalHexGridCentroids.filterBounds(aoi)
    .reduceColumns(ee.Reducer.toList(), ['seqnum']).get('list');
  var hexSelect = globalHexGrid.filter(ee.Filter.inList('seqnum', hexSelectNames));
  
  //Create an imagecollection with the three global lc products. set a lc name property
  var lcImgCol = ee.ImageCollection([
    dynamicWorld.set('lc','dw'),
    worldcover.set('lc','wc'),
    esri.set('lc','esri')
    ]);
  // for each lc product (image) create a band for each lc and rename to class name
  lcImgCol = lcImgCol.map(function(x){
    return x
      .eq([1,2,3,4,5,6,7,8,9])
      .selfMask()
      .rename(['BU','C','BG','G','SS','T','F','W','SI'])
      .copyProperties(x)
  });
  // for each of three lulc datasets (images, get the count of each of 9 lc (band) categories.
  var tableOut = lcImgCol.map(function(y){
    var tableInner = y.reduceRegions({
      collection: hexSelect, 
      reducer: ee.Reducer.count(), 
      scale: 10,
      tileScale: 4
    });
    tableInner = tableInner.map(function(ft){ 
      return ft.setGeometry(null).set('lc', y.get('lc'))//for each output create a dummy feature (no geometry) with lulc product name
    })
    return tableInner
  }).flatten()//format results for readability
  
  //print(tableOut.limit(2))
  
  //create a unique csv export for each square grid cell
  Export.table.toDrive({
    collection: tableOut, 
    description: 'lc_areas_' + String(i) , 
    fileFormat: 'CSV'
  });
  
}
throw('s')
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
//set country attribute for validation centroids
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

//set ecoregion attribute for validation centroids
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

//set human settlement attribute for validation centroids
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

// Repeat for LUCAS points
//// Site context - regional validation sites ----------------------------------------------------------
//set ecoregion attribute for regional validation points
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
//set human settlement attribute for regional validation points
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


//// Global validation pixel values - main --------------------------------------------------------------

//validation = validation
//  .filterBounds(geometry) //////// !!!!!  use for de-bugging and testing

var valList = validation.toList(1000);
print(valList)

// create a multiband image containing dynamic world validation images and 3 lulc datasets
var pixelVals = validation.map(function(i){
  var lcStack = i.rename('groundTruth')
    .addBands(dynamicWorld.rename('dw'))
    .addBands(worldcover.rename('wc'))
    .addBands(esri.rename('esri'));
  // For each dw lc pixel create a point to sample three lulc datasets 
  
  var outTable = lcStack.sample({
    region: i.geometry(), 
    scale: 10, 
    //factor: 1, 
    //seed: 123
    })
  outTable = outTable.filter(ee.Filter.notNull(['groundTruth','dw']));//remove null dw points
  //outTable = outTable.map(function(x){ return x.set('dw_id', i.get('dw_id'))})
  
  return outTable
}).flatten();
print(pixelVals.limit(10), 'dw val tiles out')

Export.table.toDrive({
  collection: pixelVals, 
  description: 'validation_global_DW_tiles' , 
  fileFormat: 'CSV'
});

//// Global validation pixel values - supplementary --------------------------------------------------------------
// This repeats the above, except here Dynamic World composites are made in two different ways
function probToCat(img){
  var imgMax = img.reduce(ee.Reducer.max());
  var imgCat = ee.Image(0)
    .where(img.select('built').eq(imgMax), 1)
    .where(img.select('crops').eq(imgMax), 2)
    .where(img.select('bare').eq(imgMax), 3)
    .where(img.select('grass').eq(imgMax), 4)
    .where(img.select('shrub_and_scrub').eq(imgMax), 5)
    .where(img.select('trees').eq(imgMax), 6)
    .where(img.select('flooded_vegetation').eq(imgMax), 7)
    .where(img.select('water').eq(imgMax), 8)
    .where(img.select('snow_and_ice').eq(imgMax), 9)
  imgCat = imgCat.selfMask()
  return imgCat
}
var CLASS_NAMES = [
    'water', 'trees', 'grass', 'flooded_vegetation', 'crops',
    'shrub_and_scrub', 'built', 'bare', 'snow_and_ice'];

//validation = validation
//  .filterBounds(geometry) //////// !!!!!  use for de-bugging and testing

var valList = validation.toList(1000);
print(valList)

var pixelVals = validation.map(function(i){
  
  var dwSupp = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
    .filterBounds(i.geometry())
    .filter(ee.Filter.calendarRange(2020,2020,'year'))
    .select(CLASS_NAMES);
  
  // Method 2: mean reduction on probabilities and max probability
  var dwMean = dwSupp.reduce(ee.Reducer.mean()).rename(CLASS_NAMES);
  var dwMeanCat = probToCat(dwMean).rename('dwMeanCat')
  
  // Method 3: median reduction on probabilities and max probability
  var dwMedian = dwSupp.reduce(ee.Reducer.median()).rename(CLASS_NAMES);
  var dwMedianCat = probToCat(dwMedian).rename('dwMedianCat')
  
  
  var lcStack = i.rename('groundTruth')
    .addBands(dwMeanCat)
    .addBands(dwMedianCat);
  
  var outTable = lcStack.sample({
    region: i.geometry(), 
    scale: 10, 
    //factor: 1, 
    //seed: 123
    })
  outTable = outTable.filter(ee.Filter.notNull(['groundTruth','dwMeanCat']));
  //outTable = outTable.map(function(x){ return x.set('dw_id', i.get('dw_id'))})
  
  return outTable
}).flatten();
print(pixelVals.limit(10), 'dwSupp val tiles out supplementary')

Export.table.toDrive({
  collection: pixelVals, 
  description: 'validation_global_DW_tiles_supplementary' , 
  fileFormat: 'CSV'
});

//// European LUCAS pixel values - main -------------------------------------------
// Create grid to iterate over and export data later
var lucasExportGrid = ee.Image.random(123).multiply(1e6).toInt()
    .reduceToVectors({
      reducer: ee.Reducer.countEvery(),
      geometry: geometry3,
      geometryType: 'bb' ,
      eightConnected: false,
      scale: 1000000,
      crs: 'EPSG:4326'
    });

// select ice classified points
var lucasIce = lucas.filter(ee.Filter.eq('lc1', 'G50'));
lucasIce = lucasIce.map(function(ft){
  return ft.set('lcLab', 'G50', 'lcNum', 9)
})
// filter out ice lc points
lucas = lucas.filter(ee.Filter.neq('lc1', 'G50'));
lucas = lucas.map(function(ft){
  var lcSimp = ee.String(ft.get('lc1')).slice(0,1)
  return ft.set('lcLab', lcSimp, 'lcNum', lcSimp)
})
lucas = lucas.filter(ee.Filter.neq('lcLab', ''));//select points with labels

// Reclassify lc names
// Lookup can be found here: https://data.jrc.ec.europa.eu/dataset/f85907ae-d123-471f-a44a-8cca993485a2#dataaccess
lucas = lucas.remap(
  ["A", "B", "C", "D", "E", "F", "G", "H"], 
  [1,2,6,5,4,3,8,7], 
  'lcNum');

lucas = lucas.merge(lucasIce)
//lucas = lucas
//  .filterBounds(geometry) //////// !!!!!  use for de-bugging and testing

// create lc image with a band for each global lc product
var lcStack = dynamicWorld.rename('dw')
  .addBands(worldcover.rename('wc'))
  .addBands(esri.rename('esri'));

// convert grid to list (iterable)
var lucasGridList = lucasExportGrid.toList(1000);
print(lucasExportGrid.size(), 'lucasGridList size');

// Change to length of list when ready to run all the exports
for (var i = 0; i<2; i++){
  var aoi = ee.Feature(lucasGridList.get(i)).geometry();
  
  var lucasSel = lucas.filterBounds(aoi);//select all points within grid cell
  
  var lucasOut = lcStack.sampleRegions({//get lc properties for each of three lc products at points(within grid)
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

//// European LUCAS pixel values - supplementary -------------------------------------------
// This repeats the above, except here Dynamic World composites are made in two different ways

var lucasGridList = lucasExportGrid.toList(1000);

// Change to length of list when ready to run all the exports
for (var i = 0; i<2; i++){
  var aoi = ee.Feature(lucasGridList.get(i)).geometry();
  
  var lucasSel = lucas.filterBounds(aoi);
  
  var dwSupp = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
    .filterBounds(aoi)
    .filter(ee.Filter.calendarRange(2020,2020,'year'))
    .select(CLASS_NAMES);
  
  // Method 2: mean reduction on probabilities and max probability
  var dwMean = dwSupp.reduce(ee.Reducer.mean()).rename(CLASS_NAMES);
  var dwMeanCat = probToCat(dwMean).rename('dwMeanCat')
  
  // Method 3: median reduction on probabilities and max probability
  var dwMedian = dwSupp.reduce(ee.Reducer.median()).rename(CLASS_NAMES);
  var dwMedianCat = probToCat(dwMedian).rename('dwMedianCat')
  
  var lcStack = dwMedianCat
    .addBands(dwMeanCat);
    
  var lucasOut = lcStack.sampleRegions({
    collection: lucasSel, 
    scale: 10,
    geometries: true,
    tileScale: 4
  });
  //print(lucasOut.limit(10), 'lucas out')
  
  Export.table.toDrive({
    collection: lucasOut, 
    description: 'validation_european_LUCAS_pts_supplementary_' + String(i) , 
    fileFormat: 'CSV'
  });
  
  
}


/*
  // Export images ///////////////////////////////////////////////////////////////////////////////
*/

//// Global extent exports --------------------------------------------------------------
// Export images at a reduced resolution
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
