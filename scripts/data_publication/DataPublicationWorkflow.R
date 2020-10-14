

# ###########################################################################
# Load objects for data processing
# Load, concentration, yield, and flag columns need to remain in this order. 
# ###########################################################################

#Preferred load names
loadvars <- c('suspended_sediment_load_pounds', 
              'chloride_load_pounds',
              'no2_no3n_load_pounds', 
              'ammonium_n_load_pounds',
              'tkn_unfiltered_load_pounds', 
              'orthophosphate_load_pounds',
              'tp_unfiltered_load_pounds',
              'total_nitrogen_load_pounds',
              'organic_nitrogen_load_pounds',
              'doc_load_pounds', 
              'toc_load_pounds', 
              "tkn_filtered_load_pounds",  
              "tp_dissolved_filtered_load_pounds",
              "total_dissolved_solids_load_pounds",    
              "total_solids_load_pounds",     
              "total_suspended_solids_load_pounds",   
              "total_volatile_suspended_solids_load_pounds")

#Preferred concentration names
concvars <- c('suspended_sediment_conc_mgL', 
              'chloride_conc_mgL',
              'no2_no3_n_conc_mgL', 
              'ammonium_n_conc_mgL',
              'tkn_unfiltered_conc_mgL', 
              'orthophosphate_conc_mgL',
              'total_phosphorus_conc_mgL',
              'total_nitrogen_conc_mgL',
              'organic_nitrogen_conc_mgL',
              'doc_conc_mgL',
              'toc_conc_mgL',    
              "tkn_filtered_00623_mg_l",    
              "tp_dissolved_filtered_00666_mg_l",
              "total_dissolved_solids_70300_mg_l",   
              "total_solids_00500_mg_l",   
              "total_suspended_solids_00530_mg_l", 
              "total_volatile_suspended_solids_00535_mg_l")

#Preferred yield names
yieldvars <- c('suspended_sediment_yield_pounds_per_acre', 
               'chloride_yield_pounds_per_acre',
               'no2_no3n_yield_pounds_per_acre', 
               'ammonium_n_yield_pounds_per_acre',
               'tkn_unfiltered_yield_pounds_per_acre', 
               'orthophosphate_yield_pounds_per_acre',
               'tp_unfiltered_yield_pounds_per_acre',
               'total_nitrogen_yield_pounds_per_acre',
               'organic_nitrogen_yield_pounds_per_acre',
               'doc_yield_pounds_per_acre', 
               'toc_yield_pounds_per_acre',
               "tkn_filtered_yield_pounds_per_acre",
               "tp_dissolved_filtered_yield_pounds_per_acre",
               "total_dissolved_solids_yield_pounds_per_acre",    
               "total_solids_yield_pounds_per_acre",     
               "total_suspended_solids_yield_pounds_per_acre",   
               "total_volatile_suspended_solids_yield_pounds_per_acre")

#Preferred flag names
flagvars <- c("flag_suspended_sediment", "flag_chloride", "flag_no2_no3n", 
              "flag_ammonium_n", "flag_tkn_unfiltered", "flag_orthophosphate",
              "flag_tp_unfiltered", "flag_tn", "flag_orgN", "flag_doc", "flag_toc",
              "flag_tkn_filtered",    
              "flag_tp_dissolved_filtered",
              "flag_total_dissolved_solids",   
              "flag_total_solids",   
              "flag_total_suspended_solids", 
              "flag_total_volatile_suspended_solids")

#Variable names for data publication
common_vars <- c("site", "FieldName", "storm_start", "storm_end",
                 "peak_discharge", "runoff_volume")

approved_vars <- c("discrete", "estimated", "exclude", 
                   "frozen", "unique_storm_number", "storm")

rain_vars <- c("rain", "duration", "Ievent",
               "I5", "I10", "I15",
               "I30", "I60", "energy_m1",
               "erosivity_m1", "energy_m2", "erosivity_m2",
               "weq")

#Load site table
site_table <- read_excel(file.path(path_to_site, "EOF_Site_Table.xlsx")) %>%
  filter(!is.na(`USGS Site Number`))

names(site_table) <- gsub(" ", "", names(site_table))

site_table <- site_table %>%
  mutate(across(c(ApproxStartDate, ApproxEndDate), as.Date)) %>%
  mutate(USGSSiteNumber = gsub("[^0-9.-]", "", USGSSiteNumber)) %>%
  select(USGSSiteNumber, FieldName, everything())

# ###############################################################
# Compile data from individual sites, calculate yields and loads,
# Check loads/concentrations using 1:1 figures
# Output compiled table to be merged with other projects
# ###############################################################

source(file.path("scripts", "data_publication", "DiscoveryFarmDataCheck2.R"))
source(file.path("scripts", "data_publication", "MRBIDataCheck2.R"))
source(file.path("scripts", "data_publication", "PioneerFarmDataCheck2.R"))
source(file.path("scripts", "data_publication", "SandCountyDataCheck2.R"))

#Join above tables into a single file
source(file.path("scripts", "data_publication", "JoinEOFRunoffData.R"))

#Join rain data with runoff data
source(file.path("scripts", "data_publication", "CalculateRainEvents.R"))


