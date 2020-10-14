
# Master file for running all cross-site comparisons

# path_to_data <- "M:/NonPoint Evaluation/GLRI Edge-of-field/R-analysis"
# This folder contains multiple sub-folders with 'analysis', 'soil' etc. 
path_to_data <- "P:/0301"

#Currently figures are saved here. This folder needs a subfolder titled "Figures"  
path_to_results <- "C:/Users/lloken/OneDrive - DOI/EOF_SoilHealth"

#Path to data publication site table
path_to_site <- "C:/Users/lloken/DOI/GS-UMid GLRI EOF - Data Publication" 

#load libraries
# library(conflicted)
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
library(readxl)
library(openxlsx)
library(anytime)
library(ggrepel)

#load functions
source('scripts/functions/fxns_data_compilation.R')
source('scripts/functions/g_legend.R')
source('scripts/functions/not_all_na.R')
source('scripts/functions/ScaleYLog10Nice.R')

`%notin%` <- Negate(`%in%`)


# conflict_prefer("rename", "dplyr")

source('scripts/run_files/compilation_run_file.R')
