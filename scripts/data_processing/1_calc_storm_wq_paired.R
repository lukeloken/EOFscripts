# import storm-specific water quality data
con_wq <- read.csv(file.path('data_raw', control_site_file), na.strings = c("", "NA"), stringsAsFactors = F)
trt_wq <- read.csv(file.path('data_raw', treatment_site_file), na.strings = c("", "NA"), stringsAsFactors = F)

  
# check to see if all required columns are in data frame 
# use list in stickies to set this, not quite sure what the complete list is

# get conc/load vars
if (length(loads) == 1 & !is.na(loads)){
  loadvars <- grep(loads, names(trt_wq), ignore.case = TRUE, value = TRUE)
} else if (is.na(loads)) {
  loadvars <- NA
} else {
  loadvars <- loads
}

if (length(concentrations) == 1 & !is.na(concentrations)){
  concvars <- grep(concentrations, names(trt_wq), ignore.case = TRUE, value = TRUE)
} else if (is.na(concentrations)) {
  concvars <- NA
} else {
  concvars <- concentrations
}

if (length(flags) == 1 & !is.na(flags)) {
  flagvars <- grep(flags, names(trt_wq), ignore.case = TRUE, value = TRUE)
} else if (is.na(flags)) {
  flagvars <- NA
} else {
  flagvars <- flags
}

# output table of response vars and names to ensure proper pulling
responses <- c(concvars, loadvars, other_responses)
responses <- responses[!is.na(responses)]

response_table <- data.frame(responses = responses, 
                             responses_clean_names = clean_names)

write.csv(response_table, file.path('figures', 'diagnostic', paste0(site, '_response_var_check.csv')))




# make "<" values equal to half of the censored value

# first, find which variables have a "<"
# and replace with 0.5 * value


con_combined_events <- combine_sub_events(wq = con_wq, flagvars = flagvars, concvars = concvars, loadvars = loadvars)
trt_combined_events <- combine_sub_events(wq = trt_wq, flagvars = flagvars, concvars = concvars, loadvars = loadvars)

name_changer <- function(in_dat, prefix) {
  all_names <- names(in_dat)
  storm_id <- grepl('unique_storm_number', all_names)
  subset_names <- all_names[!storm_id]
  new_names <- paste0(prefix, subset_names)
  
  names(in_dat)[!storm_id] <- new_names
  return(in_dat)
}

con_combined_events <- name_changer(con_combined_events, 'con_')
trt_combined_events <- name_changer(trt_combined_events, 'trt_')

wq <- full_join(con_combined_events, trt_combined_events, by = 'unique_storm_number')

# remove rows with NA or 0 loads that occur due to unmatched pairs (event at one site did not occur at other)
#wq<- wq[wq==0] <- NA
wq <- wq[complete.cases(wq), ]																										  

# set dates to time zone
.origin <- as.POSIXct(ifelse(Sys.info()[['sysname']] == "Windows", "1899-12-30", "1904-01-01"))
tz(.origin) <- site_tz

date.vars <- grep('storm_start', names(wq), value = T)


for (i in 1:length(date.vars)) {
  wq[,date.vars[i]] <- as.POSIXct(wq[,date.vars[i]], origin = .origin, tz = site_tz, format = datetime_format)
}

# create a column for "period" -- before or after BMP implementation
start_col <- paste0('trt', '_storm_start')
wq <- wq %>%
  #mutate(frozen = as.logical(eof$frozen)) %>%
  mutate(period = ifelse(wq[,start_col] >= bmp_date, 'after', 'before'))



temp_filename <- file.path("data_cached", paste0(site, "_", site_paired, "_prepped_WQbystorm.csv"))
write.csv(wq, temp_filename, row.names = FALSE)

if (nrow(wq) > 0) {
  message(paste('Water quality data is now processed. See', temp_filename, 'to ensure correct processing.'))
} else {
  stop("Something went wrong with processing of water quality data. To debug, look through code in 'scipts/data_processing/1_calc_storm_wq_paired.R'.")
}

