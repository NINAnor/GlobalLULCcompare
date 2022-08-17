// Author: Zander Venter - zander.venter@nina.no

// Workflow in this script:
  // 1. Import Dynamic World data for 2020
  // 2. Define a global grid to stratify exports
  // 3. Loop through the global grid, mosaic Dynamic World and generate export tasks
  // 4. Run the export tasks 

// Define a visualisation palette for dynamic world
var dwVisParams = {
  min: 0,
  max: 8,
  palette: [
    '#419BDF', '#397D49', '#88B053', '#7A87C6', '#E49635', '#DFC35A',
    '#C4281B', '#A59B8F', '#B39FE1'
  ]
};

// Load dynamic world data, filter to the year 2020 and select the discrete class band
var dw = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
  .filter(ee.Filter.calendarRange(2020,2020,'year'))
  .select('label');

// Visualise the 2020 most common class per pixel (mode)
Map.addLayer(dw.mode(), dwVisParams, 'Classified Image', 0);

// Define a global bounding box
var globalBounds = ee.Geometry.Polygon([-180, 90, 0, 90, 180, 90, 180, -90, 10, -90, -180, -90], null, false);

// Create a vector grid that covers a global extent. This is used as the subunits to export data (and
// prevent Out Of Memory errors)
var globalGrid = ee.Image.random(123).multiply(1e6).toInt()
    .reduceToVectors({
      reducer: ee.Reducer.countEvery(),
      geometry: globalBounds,
      geometryType: 'bb' ,
      eightConnected: false,
      scale: 500000,
      crs: 'EPSG:4326'
    });

// Load the word cover dataset
var worldcover = ee.ImageCollection("ESA/WorldCover/v100").first().rename('wc');
// Remove grid cells that do not have any data. If grid cell has no data, null is returned.
// these are then filtered out.
globalGrid = worldcover.reduceRegions(globalGrid, ee.Reducer.firstNonNull(), 5000)
  .filter(ee.Filter.notNull(['first']));

// Select the label band (from dw) and cellcode band (unique value per grid cell)
globalGrid = globalGrid.select(['label'],['CellCode'])
// Visualise grid
Map.addLayer(globalGrid, {}, 'export grid',0)

// Print the number of unique grid cells
var gridNum = globalGrid.aggregate_count('CellCode').getInfo()
print(gridNum, 'number of grids')

// Convert global grid cells into a list to allow for looping/ synchronous export.
// Any number gte the number of grid cells (1359) can be used. Here, 2000 was arbtrarily chosen.
// Note this must be lte 3000 to avoid reaching the max. limit of jobs submitted to the server
var list= globalGrid.toList(2000);

// A for loop to allow for the export of data at each grid cell
// i<3 is used here as a sanity check to verify the processing upto this point and the outputs.
// to reproduce the analysis in the paper change to i<1360 
for (var i = 0; i<3; i++){
  // Get a grid cell
  var aoi = ee.Feature(list.get(i)).geometry();
  // Map.centerObject(aoi) //Optional
  // get unique grid cell ID
  var CellCode = ee.Feature(list.get(i)).get('CellCode')
  // get the crs of the cell
  var crs = dw.filterBounds(aoi).first().projection().crs();
  //print(crs)
  //for the selected grid cell, get the most common (mode) LC at each pixel for 2020
  
  var dwSel = dw.filterBounds(aoi).mode();
  
  // clip the mode dw LC
  dwSel = dwSel.clip(aoi);
  // set a unique ID property for LC that matches the unique grid ID
  dwSel = dwSel.set('CellCode', CellCode).rename('landcover');
  //Map.addLayer(dwSel, dwVisParams, 'classified', 0);
  
  //Finally, export the mode dw LC with name based on unique id ensuring unique names for each tile @10m
  Export.image.toAsset({
    image: dwSel, 
    description: 'dw_global_2020_' + String(i), 
    assetId: 'projects/nina/GIS_synergy/Extent/DW_global_2020_mode/dw_global_2020_'  + String(i), 
    region: aoi,
    pyramidingPolicy: {'landcover': 'mode'},
    scale: 10,
    maxPixels: 269652427200
  });

}

/*
// After running the script, you will have a whole lot of export tasks ready to run
// Instead of clicking on each one individually, you can:
  // Copy the two functions below
  // Click F12 on your computer
  // In the "Console" tab paste the two functions and hit Enter
  // Then type 'runTaskList()' in the Console and hit Enter
  // Wait a few seconds - roughly 30 seconds wait per 100 pts
  // Then type 'confirmAll()' and hit Enter
  // All the tasks should start running at once.


var runTaskList = () => document.querySelector("#task-pane").shadowRoot.querySelectorAll(".task .run-button").forEach(x => x.click());
var confirmAll = () => document.querySelectorAll("ee-image-config-dialog").forEach(x => x.shadowRoot.querySelector("ee-dialog").shadowRoot.querySelector(".ok-button").click());

*/
