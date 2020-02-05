# Pre-Run

# site_folder - point to the folder with site-specific data needed for this run

site_folder <- file.path('P:/0301/analysis/WI/WI-SW4')

# Temporarily copy raw data to D:EOF/EOFscripts/data_raw

  move.me <- list.files(file.path(site_folder, 'approved_site_data'), pattern = '', full.names=FALSE, recursive = FALSE)
  
  move.this <- file.path(site_folder,'approved_site_data',move.me)

  move.here <- file.path("D:/EOF/EOFscripts/data_raw")
  
  # remove any old files in the local 'data_raw' folder with the same name
  file.remove(file.path(move.here,move.me))
  
  # copy site-specific data_raw files to the local 'data_raw' folder
  file.copy(from = move.this, to = move.here)
  
# Paired Sites Only
  
  control_site <- file.path('P:/0301/analysis/WI/WI-SW4')
  treat_site <- file.path('P:/0301/analysis/WI/WI-SW5')
  paired_site <- file.path('P:/0301/analysis/WI/WI-SW4-SW5-pair')
  site_folder <- paired_site
  
  # Temporarily copy raw data to D:EOF/EOFscripts/data_raw
  
  move.me.c <- list.files(file.path(control_site, 'approved_site_data'), pattern = '.csv', full.names=FALSE, recursive = FALSE)
  move.this.c <- file.path(control_site,'approved_site_data',move.me.c)
  
  move.me.t <- list.files(file.path(treat_site, 'approved_site_data'), pattern = '.csv', full.names=FALSE, recursive = FALSE)
  move.this.t <- file.path(treat_site,'approved_site_data',move.me.t)
  
  move.me.p <- list.files(file.path(paired_site, 'approved_site_data'), pattern = '', full.names=FALSE, recursive = FALSE)
  move.this.p <- file.path(paired_site,'approved_site_data',move.me.p)
  
  move.me <- c(move.me.c,move.me.t,move.me.p)
  move.this <- c(move.this.c,move.this.t,move.this.p)
  
  move.here <- file.path("D:/EOF/EOFscripts/data_raw")
  
  # remove any old files in the local 'data_raw' folder with the same name
  file.remove(file.path(move.here,move.me))
  
  # copy site-specific data_raw files to the local 'data_raw' folder
  file.copy(from = move.this, to = move.here)
  