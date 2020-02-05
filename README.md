# Edge of Field analysis

This repo contains scripts for processin edge of field water quality data. 

## Compile processed data from all sites

Processed data will be archived in the Upper Midwest Water Science Center P drive
These data have already been processed using the `processing_run_file.R` scripts
These are now ready for across site comparisons
```
path_to_data <- "P:/0301"
path_to_data <- "M:/NonPoint Evaluation/GLRI Edge-of-field/R-analysis"
```



## Process raw data from each site individually 

Each center has the ability to process and analysis their own data using the following functions. Their is a workflow script titled: master_run_file.R in the 'scripts' folder. 


```
## Process raw data from each site ##
source('scripts/run_files/processing_run_file.R', echo = F)

## Analyze your data ##
source('scripts/run_files/analysis_run_file.R', echo = F)
```


## Disclaimer

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.