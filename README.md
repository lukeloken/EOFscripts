# Edge of Field analysis

This repo contains scripts for processin edge of field water quality data. 

## Compile processed data from all sites

### updated by Luke Loken February 2020

Data that have been processed and saved to the P: Drive will be compiled for across site comparisons. Currently, individual site data are located in the P:drive/analysis folder by states and by sites. The "site_mod.csv" files from each site are bound, summarized, and plotted. 

This processes uses the `comipilation_run_file.R`. It requires that a `path_to_data` object be set at before running. This will point to the main folder, which is currently the "P:/0301"

```
# Current data location
path_to_data <- "P:/0301"

# Prior data location
# path_to_data <- "M:/NonPoint Evaluation/GLRI Edge-of-field/R-analysis"
```



## Process raw data from each site individually 

### updated by Rebecca Carvin and Samantha Oliver February 2020


Each center has the ability to process and analysis their own data using the following functions. Their is a workflow script titled: `master_run_file.R` in the 'scripts' folder. Prior to running this script, user's must input site specific information in the `0_master_file.R` script. This includes information on site name, study type, time zones, dates, variable names, etc.. Basically all the meta information that goes along with the data file. 

After loading the site specific info and the data files, the script will process the data and analyze it. A number of figures, output data tables will be saved within the git/repo folder on your computer. 


```
## Process raw data from each site ##
source('scripts/run_files/processing_run_file.R', echo = F)

## Analyze your data ##
source('scripts/run_files/analysis_run_file.R', echo = F)
```


## Disclaimer

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.