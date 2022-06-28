// Author: Zander Venter - zander.venter@nina.no

// Workflow in this script:
  // 1. Import Dynamic World data for 2020
  // 2. Define a global grid to stratify exports
  // 3. Loop through the global grid, mosaic Dynamic World and generate export tasks
  // 4. Run the export tasks 


var dwVisParams = {
  min: 0,
  max: 8,
  palette: [
    '#419BDF', '#397D49', '#88B053', '#7A87C6', '#E49635', '#DFC35A',
    '#C4281B', '#A59B8F', '#B39FE1'
  ]
};

var dw = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
  .filter(ee.Filter.calendarRange(2020,2020,'year'))
  .select('label');

Map.addLayer(dw.mode(), dwVisParams, 'Classified Image', 0);

var globalBounds = ee.Geometry.Polygon([-180, 90, 0, 90, 180, 90, 180, -90, 10, -90, -180, -90], null, false);

var globalGrid = ee.Image.random(123).multiply(1e6).toInt()
    .reduceToVectors({
      reducer: ee.Reducer.countEvery(),
      geometry: globalBounds,
      geometryType: 'bb' ,
      eightConnected: false,
      scale: 500000,
      crs: 'EPSG:4326'
    });
var worldcover = ee.ImageCollection("ESA/WorldCover/v100").first().rename('wc');
globalGrid = worldcover.reduceRegions(globalGrid, ee.Reducer.firstNonNull(), 5000)
  .filter(ee.Filter.notNull(['first']));
globalGrid = globalGrid.select(['label'],['CellCode'])
Map.addLayer(globalGrid, {}, 'export grid',0)



var gridNum = globalGrid.aggregate_count('CellCode').getInfo()
print(gridNum, 'number of grids')

var list= globalGrid.toList(2000);

for (var i = 0; i<3; i++){
  var aoi = ee.Feature(list.get(i)).geometry();
  //Map.centerObject(aoi)
  var CellCode = ee.Feature(list.get(i)).get('CellCode')
  
  var crs = dw.filterBounds(aoi).first().projection().crs();
  //print(crs)
  
  var dwSel = dw.filterBounds(aoi).mode();
  
  dwSel = dwSel.clip(aoi);
  dwSel = dwSel.set('CellCode', CellCode).rename('landcover');
  //Map.addLayer(dwSel, dwVisParams, 'classified', 0);
  
  Export.image.toAsset({
    image: dwSel, 
    description: 'dw_global_2020_' + String(i), 
    assetId: 'projects/nina/GIS_synergy/Extent/DW_global_2020/dw_global_2020_'  + String(i), 
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