var regionTrain = 
    /* color: #98ff00 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[13.808845415977036, 50.602593800183186],
          [13.808845415977036, 49.48079866891001],
          [15.621589556602036, 49.48079866891001],
          [15.621589556602036, 50.602593800183186]]], null, false),
    geometry = 
    /* color: #0b4a8b */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[14.404998144471275, 50.04756774103323],
          [14.404998144471275, 50.036791295321095],
          [14.423537573182212, 50.036791295321095],
          [14.423537573182212, 50.04756774103323]]], null, false);

// Author: Zander Venter - zander.venter@nina.no

// Workflow in this script:
  // 1. Extract woody plant cover probability scores from Dynamic World that have been adjusted
  // 2. Train a RF model using LUCAS reference data and Dynamic World probability scores as predictors
  // 3. Visualize output an export to Drive

//Define visualisation parameters for LC
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
    //"Snow & ice" //9
    ],
  colors: [
    '#C4281B',
    '#E49635',
    '#A59B8F',
    '#88B053',
    '#DFC35A',
    '#397D49',
    '#7A87C6',
    '#419BDF',
    //'#B39FE1'
  ]
}

var lcVizParams = {
  min: 1,
  max: 8, //9,
  palette: lcDict['colors']
};

var probabilityBands = [
  'water', 'trees', 'grass', 'flooded_vegetation', 'crops', 'shrub_and_scrub',
  'built', 'bare', //'snow_and_ice'
];

//function to merge dw categries
function changeTypology(img){
    var grey = ee.Image(img.select('built').add(img.select('bare'))).rename('grey');
    var tree = ee.Image(img.select('trees').add(img.select('shrub_and_scrub'))).rename('tree');
    var grass = ee.Image(img.select('grass').add(img.select('flooded_vegetation')).add(img.select('crops'))).rename('grass');
    return grey.addBands(tree).addBands(grass).copyProperties(img, img.propertyNames())
}

// prepare explantory variables- filter to aoi, select 2020 data and for may-august
var dw = ee.ImageCollection("GOOGLE/DYNAMICWORLD/V1")
  .filterBounds(regionTrain)
  .filter(ee.Filter.calendarRange(2020,2020,'year'))
  .filter(ee.Filter.calendarRange(5,9,'month'));

// get the mode image (used for comparison)-represents original dw
var dwLabelMosaic = dw.select('label').mode();
dwLabelMosaic = dwLabelMosaic.remap({
    from:[0,1,2,3,4,5,6,7,8],
    to:  [8,6,4,7,2,5,1,3,9]
  });

Map.addLayer(dwLabelMosaic, lcVizParams, 'Dynamic World original', 0);
// apply reclassification (combining dw lc categories) to mean probability values for 2020
var rgbProbMosaic = ee.Image(changeTypology(dw.select(probabilityBands).mean()));

Map.addLayer(rgbProbMosaic, {min:0, max:0.5}, 'rgbProbMosaic',0)

// get tree layer from reclassified dw image
var treeCovExagg = rgbProbMosaic.select('tree').unitScale(0,0.2).clamp(0,1)
Map.addLayer(treeCovExagg, {min:0, max:1}, 'tree cover exaggerated',0)

//prepare reference points (target)- load lucas points, filter to aoi, select lc property names
// Data from here: https://www.nature.com/articles/s41597-020-00675-z
  // in GEE data catalogue
var lucas = ee.FeatureCollection("JRC/LUCAS_HARMO/THLOC/V1")
  .filterBounds(regionTrain)
  .filter(ee.Filter.eq('year', 2018))
  .select(['lc1', 'point_id']);
print(lucas.size(), 'lucas size')
Map.addLayer(lucas.geometry(), {}, 'lucas',0);

//format lc names to strings (will be renamed later)
lucas = lucas.map(function(ft){
  var lcSimp = ee.String(ft.get('lc1')).slice(0,1)
  return ft.set('lcLab', lcSimp, 'lcNum', lcSimp)
})
lucas = lucas.filter(ee.Filter.neq('lcLab', ''));//remove points with no lc property

// reclassify lucas data lc names
// Lookup can be found here: https://data.jrc.ec.europa.eu/dataset/f85907ae-d123-471f-a44a-8cca993485a2#dataaccess
lucas = lucas.remap(
  ["A", "B", "C", "D", "E", "F", "G", "H"], 
  [1,2,6,5,4,3,8,7], 
  'lcNum');
print(lucas.limit(10))




// Define classification image
var modelStack = dw.select(probabilityBands).reduce(ee.Reducer.median());
print(modelStack, 'modelStack')

// Define bande to use or just adopt from image
var bands = modelStack.bandNames();
print(bands, 'bands');


// Make training data by sampling the image at the points
var training = modelStack.reduceRegions({
  collection: lucas,
  reducer: ee.Reducer.mean(),
  scale: 10
});
print(training.limit(10))

// Initialize a RandomForest classifier with 100 decision trees and train it.
var classifier = ee.Classifier.smileRandomForest(100).train({
  features: training, //collection with response and predictor data
  classProperty: 'lcNum', //response variable name
  inputProperties: bands //predictor names
});

// Classify the train and test image with RandomForest.
var classifiedImage = modelStack.classify(classifier, 'classification');

Map.addLayer(classifiedImage, lcVizParams, 'Dynamic World re-trained', 0);

Map.setOptions('HYBRID')
Map.centerObject(geometry)

Export.image.toDrive({
  image: dwLabelMosaic,
  description: 'dwLabelMosaic_local_eg',
  scale: 10,
  region: geometry,
  crs: 'EPSG:4326'
})
Export.image.toDrive({
  image: treeCovExagg,
  description: 'dwTreeCovExagg_local_eg',
  scale: 10,
  region: geometry,
  crs: 'EPSG:4326'
})
Export.image.toDrive({
  image: classifiedImage,
  description: 'dwReclassImg_local_eg',
  scale: 10,
  region: geometry,
  crs: 'EPSG:4326'
})


