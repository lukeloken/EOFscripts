

# Master file for running all cross-site comparisons

# path_to_data <- "M:/NonPoint Evaluation/GLRI Edge-of-field/R-analysis"
# This folder contains multiple sub-folders with 'analysis', 'soil' etc. 
path_to_data <- "P:/0301"

#Temporary place while teleworking
path_to_data <- "C:/copy of P0301/0301"


#Currently figures are saved here. This folder needs a subfolder titled "Figures"  
path_to_results <- "C:/Users/lloken/OneDrive - DOI/EOF_SoilHealth"

#load libraries
library(drake)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(ggfortify)
library(ggpubr)
library(RColorBrewer)
library(viridis)
library(gridExtra)
library(lubridate)
library(corrplot)
library(PerformanceAnalytics)
library(bestglm)

source('scripts/run_files/compilation_run_file.R')
