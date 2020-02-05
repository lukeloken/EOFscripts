# import storm-specific water quality data
wq <- read.csv(file.path('data_raw', wq_file), na.strings = c("", "NA"), stringsAsFactors = F, strip.white = TRUE)

# check to see if all required columns are in data frame 
# use list in stickies to set this, not quite sure what the complete list is
must.haves <- c('storm_start', 'storm_end', 'storm_runoff_cubic_feet', 
                'unique_storm_number', 'unique_storm_id', 'peak_discharge', 'frozen')

# set concentration, load, and flag variables
if (!all(must.haves %in% names(wq))) {
  vars.missing <- must.haves[which(!must.haves %in% names(wq))]
  stop(paste0('The water quality data is missing column(s): ', paste0(vars.missing, collapse = ', ')), 
        call. = F)
}

# get conc/load vars
if (length(loads) == 1 & !is.na(loads)){
  loadvars <- grep(loads, names(wq), ignore.case = TRUE, value = TRUE)
} else if (is.na(loads)) {
  loadvars <- NA
} else {
  loadvars <- loads
}

if (length(concentrations) == 1 & !is.na(concentrations)){
  concvars <- grep(concentrations, names(wq), ignore.case = TRUE, value = TRUE)
} else if (is.na(concentrations)) {
  concvars <- NA
} else {
  concvars <- concentrations
}

if (length(flags) == 1 & !is.na(flags)) {
  flagvars <- grep(flags, names(wq), ignore.case = TRUE, value = TRUE)
} else if (is.na(flags)|is.na(concvars)) {
  flagvars <- NA
} else {
  flagvars <- flags
}

# output table of response vars and names to ensure proper pulling
responses <- c(concvars, loadvars, other_responses)
responses <- responses[!is.na(responses)]

response_table <- data.frame(responses = responses, 
                             responses_clean_names = clean_names)

write.csv(response_table, file.path('data_cached', paste0(site, '_response_var_check.csv')))

# set dates to time zone
.origin <- as.POSIXct(ifelse(Sys.info()[['sysname']] == "Windows", "1899-12-30", "1904-01-01"))
tz(.origin) <- site_tz

date.vars <- c('storm_start', 'storm_end')

for (i in 1:length(date.vars)) {
  wq[,date.vars[i]] <- as.POSIXct(wq[,date.vars[i]], origin = .origin, tz = site_tz, format = datetime_format)
  if(all(is.na(wq[,date.vars[i]]))) {
    stop(paste0("Column ", date.vars[i], " was not properly converted to date, and all values are NAs. Please check if the variable datetime_format in 0_master_file.R matches that of the imported columns."))
  }
}

# clean up the data to exclude estimated values, combine sub storm events, etc.
# exclude storms that have a 1 in exclude, are estimated, or are discrete samples
storms <- filter(wq, exclude == 0) %>%
  #filter(storm == 0) %>% #Event or Baseflow Toggle for Tile sites
  filter(estimated == 0) %>%
  filter(discrete == 0) #%>%
  #filter(frozen == 0))

# make "<" values equal to half of the censored value

# first, find which variables have a "<"
# and replace with 0.5 * value
if (!is.na(flagvars[1])){
  for (i in 1:length(flagvars)) {
  flags <- grep('<', storms[, flagvars[i]])
  storms[flags, concvars[i]] <- 0.5*storms[flags, concvars[i]]
  print(paste0(length(flags), ' observations below detection limit for ', concvars[i]))
  }
}

# combine sub storms
# add a column that will be used for weighting concentrations by total storm_runoff_volume
storm.vols <- wq[,c('unique_storm_number', 'storm_runoff_cubic_feet', 'unique_storm_id')]
storm.vols <- storm.vols %>%
  group_by(unique_storm_number) %>%
  summarise(sum_runoff = sum(storm_runoff_cubic_feet), 
            sub_storms = paste(unique_storm_id, collapse = ","))

storms <- merge(storms, storm.vols, by = 'unique_storm_number', all.x = TRUE)
storms <- mutate(storms, vol_weight = storm_runoff_cubic_feet/sum_runoff)

# get rid of any samples that do not have a volume weight, 
# which will give us NA values later on
storms <- filter(storms, !is.na(vol_weight))

# Handle sub storms
# when combining sub events, take:
# max of peak discharge to report as "peak discharge" for event
# min start date
# max end date
# load = sum of subs
# conc weighted by sum(load)/sum(runoff volume)


loadbystorm <- storms %>% 
  group_by(unique_storm_number) %>%
  summarise_at(vars(loadvars), sum, na.rm = TRUE) 

if (!is.na(concvars)) {
concbystorm <- storms[,c(concvars, 'unique_storm_number', 'vol_weight')]
concbystorm[, concvars] <- concbystorm[,concvars]*concbystorm[,'vol_weight']

concbystorm <- concbystorm %>%
  group_by(unique_storm_number) %>%
  summarise_at(vars(concvars), sum, na.rm = TRUE)

stormdesc <- storms %>%
  group_by(unique_storm_number) %>%
  summarise(
    #sample_start = min(sample_start, na.rm = TRUE),
    #sample_end = max(sample_end, na.rm = TRUE),
    storm_start = min(storm_start),
    storm_end = max(storm_end),
    peak_discharge = max(peak_discharge), 
    runoff_volume = sum(storm_runoff_cubic_feet), 
    frozen = paste0(unique(frozen), collapse = ', ')
  )

stormdesc$frozen <- as.numeric(ifelse(nchar(stormdesc$frozen) == 1, stormdesc$frozen, event_over_thaw))

flagsbystorm <- storms %>%
  group_by(unique_storm_number) %>%
  summarise_at(vars(flagvars), toString) 

wq.bystorm <- merge(concbystorm, loadbystorm)
wq.bystorm <- merge(wq.bystorm, storm.vols)
wq.bystorm <- merge(wq.bystorm, flagsbystorm)
wq.bystorm <- merge(wq.bystorm, unique(storms[,c('unique_storm_number', 'sub_storms')]), all.x = TRUE)
wq.bystorm <- merge(wq.bystorm, stormdesc)
} else {
  
  stormdesc <- storms %>%
    group_by(unique_storm_number) %>%
    summarise(
      #sample_start = min(sample_start, na.rm = TRUE),
      #sample_end = max(sample_end, na.rm = TRUE),
      storm_start = min(storm_start),
      storm_end = max(storm_end),
      peak_discharge = max(peak_discharge), 
      runoff_volume = sum(storm_runoff_cubic_feet), 
      frozen = paste0(unique(frozen), collapse = ', ')
    )
  
  stormdesc$frozen <- as.numeric(ifelse(nchar(stormdesc$frozen) == 1, stormdesc$frozen, event_over_thaw))
  
  wq.bystorm <- loadbystorm
  wq.bystorm <- merge(wq.bystorm, storm.vols)
  wq.bystorm <- merge(wq.bystorm, unique(storms[,c('unique_storm_number', 'sub_storms')]), all.x = TRUE)
  wq.bystorm <- merge(wq.bystorm, stormdesc)
}
# check if wq.bystorm has more than one row, and print message. Otherwise, stop process.

temp_filename <- file.path("data_cached", paste0(site, "_", "prepped_WQbystorm.csv"))


if (nrow(wq.bystorm) > 0) {
  message(paste('Water quality data is now processed. See', temp_filename, 'to ensure correct processing.'))
} else {
  stop("Something went wrong with processing of water quality data. To debug, look through code in 'scipts/data_processing/1_calc_storm_wq.R'.")
}

write.csv(wq.bystorm, temp_filename, row.names = FALSE)


