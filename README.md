# GlobalLULCcompare
A comparison of global 10m land use land cover datasets: Google’s Dynamic World (DW), ESA’s World Cover 2020 (WC), Esri’s 2020 Land Cover (Esri) 

**Please refer to our manuscript for details on methods: <link to be inserted>**

This repository contains scripts to reproduce the findings in our paper. The workflow spans two programming platforms, namely Google Earth Engine (GEE) JavaScript API and RStudio. GEE is used to extract data on global LULC maps and spatial context, while R is used for statistical analyziz and data viz.

The ./DATA/ repository does not have the full dataset stored on GitHub due to file size limitations. Therefore you have to re-run the initial scripts to generate the data yourself before you can run the R vizualisation scripts. 

The file paths in the R scripts are relative file paths and should be stable to use as is. But please rename file paths in the JavaScript code which are currently hardcoded to a demo GEE Asset. You need to generate data within your own GEE Assets. The data in the GEE assets referenced in the code are shared publicly but we cannot assure their stability over time and they may be deleted.

The workflow is as follows:

- Run the 'DW_global_mosaic_2020.js' file in GEE.
  - Snapshot of code: https://code.earthengine.google.com/493a3ddcf0d5b1f1863ba3fdd61e2026
- *wait for exports tasks to complete running*
- Run the 'setup.R' file in R and upload the "equal_area_grid.shp" file that results to your GEE asset
- *wait for Asset import tasks to complete running*
- Run the 'DW_global_validation.js' in GEE
  - Snapshot of code: https://code.earthengine.google.com/880dd52f4ef5537cb6142b65e9542816
- Run the 'DW_probabilities_explore.js' script in GEE
  - Snapshot of code: https://code.earthengine.google.com/fa91b84a4d96f1231e903cca39f0f7c0
*wait for exports tasks to complete running and download from Google Drive into ./DATA/ respository*
- Run the 'assessment.R' script in R to generate all the manuscript figures.
