# Post_run

# Send results to a results folder within the site folder on the P: drive

# if results path doesn't exist, create it. If it already exists, there will be a warning.
State <- "WI"
Site <- "WI-SW4"
dir.create(file.path('P:/0301/analysis',State))
dir.create(file.path('P:/0301/analysis',State,Site))
dir.create(file.path('P:/0301/analysis',State,Site,'results'))
dir.create(file.path('P:/0301/analysis',State,Site,'approved_site_data'))

# build the results path
results_path <- file.path('P:/0301/analysis',State,Site,'results')

# create folders for the most recent run
Rundate <- format(Sys.time(),"%Y-%m-%d-%H%M")
#Rundate <- "2019-10"
dir.create(file.path(results_path,Rundate))
dir.create(file.path(results_path,Rundate,'figures'))
dir.create(file.path(results_path,Rundate,'figures','diagnostic'))
dir.create(file.path(results_path,Rundate,'tables'))
dir.create(file.path(results_path,Rundate,'cache'))

# copy site-specific results files to the site 'results' folder

# figures
# tables (useful .csv table(s))
# cache (less useful)

run_folder <- file.path('D:/EOF/EOFscripts')
#run_folder <- file.path('H:/Projects/GLRI EdgeOfField','OH-FM1')

# figures
copy.figures_1 <- list.files(file.path(run_folder, 'figures','diagnostic'), pattern = '', full.names=FALSE, recursive = TRUE)
file.copy(from = file.path(run_folder,'figures','diagnostic',copy.figures_1), to = file.path(results_path,Rundate,'figures','diagnostic'))
copy.figures_2 <- list.files(file.path(run_folder, 'figures'), pattern = 'pdf|png', full.names=FALSE, recursive = FALSE)
file.copy(from = file.path(run_folder,'figures',copy.figures_2), to = file.path(results_path,Rundate,'figures'))

# tables
copy.mod <- list.files(file.path(run_folder, 'data_cached'), pattern = 'mod_dat|mdc|percent|residual', full.names=FALSE, recursive = TRUE)
file.copy(from = file.path(run_folder,'data_cached',copy.mod), to = file.path(results_path,Rundate,'tables'))

# cache
copy.cache <- grep(list.files(file.path(run_folder, 'data_cached')), pattern = 'mod_dat|mdc|percent|residual', inv = TRUE, value = TRUE)
file.copy(from = file.path(run_folder,'data_cached',copy.cache), to = file.path(results_path,Rundate,'cache'))
# end

# # Copy old Raw_data folder to a 'run folder' in P: drive 'approved site data'
# State <- "WI"
# Site <- "WI-TL1"
# #dir.create(file.path('P:/0301/analysis',State,Site,'results',Rundate))
# run_folder <- file.path('H:/Projects/GLRI EdgeOfField',Site)
# #run_folder <- file.path('H:/Projects/GLRI EdgeOfField','EOF-PairedBasin_WI_SW4_SW5')
# results_path <- file.path('P:/0301/analysis',State,Site,'results')
# copy.data.raw <- list.files(file.path(run_folder), pattern = 'README', full.names=FALSE, recursive = TRUE)
# file.copy(from = file.path(run_folder,copy.data.raw), to = file.path(results_path,Rundate))

