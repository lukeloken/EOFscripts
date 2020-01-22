# A collection of functions that are called throughout the 
# data processing steps

# function to run through Rainmaker and extract events, event characteristics
run.rainmaker <- function(precip_raw, wq.dat, ieHr = 2, rainthresh = 0.008, 
                          xmin = c(5,10,15,30,60), antecedentDays = c(1,3,7,14)) {
  
  # calculate events
  events <- RMevents_sample(df = precip_raw, ieHr = ieHr, rain = 'rain', time = 'pdate', 
                            dfsamples = wq.dat, bdate = 'storm_start', edate = 'storm_end')
  
  
  # extract data from events output
  events_list <- events$storms
  tipsbystorm <- events$tipsbystorm
  
  # calculate storm intensity at different time intervals
  StormSummary <- RMintensity(df=tipsbystorm, date="pdate", rain="rain", df.events=events_list,
                              sdate="StartDate", edate="EndDate", depth="rain", xmin=xmin)
  
  # calculate erosivity 
  # method 1
  StormSummary <- RMerosivity(df = tipsbystorm, ieHr=ieHr, rain='rain', StormSummary=StormSummary, method=1)
  StormSummary <- rename(StormSummary, 'erosivity_m1' = "erosivity", 'energy_m1' = 'energy')
  
  # method 2
  StormSummary <- RMerosivity(df= tipsbystorm, ieHr=ieHr, rain="rain", StormSummary=StormSummary, method=2)
  StormSummary <- rename(StormSummary, 'erosivity_m2' = "erosivity", 'energy_m2' = 'energy')
  
  
  # calculate antecedent rain
  StormSummary <- RMarf(df = precip_raw, date = 'pdate', rain = 'rain', df.events = StormSummary, 
                        sdate = "StartDate", days = antecedentDays, varnameout = "ARFdays")
  
  
  # give storms IDs
  StormSummary$unique_storm_number <- wq.dat$unique_storm_number
  
  return(StormSummary)
}

combine_sub_events <- function(wq, concvars, loadvars, flagvars) {
  storms <- filter(wq, exclude == 0) %>%
    filter(estimated == 0) %>%
    filter(discrete == 0)
  
  if (all(!is.na(flagvars)) & all(!is.na(concvars))){
    for (i in 1:length(flagvars)) {
      flags <- grep('<', storms[, flagvars[i]])
      storms[flags, concvars[i]] <- 0.5*storms[flags, concvars[i]]
      print(paste0(length(flags), ' observations below detection limit for ', concvars[i]))
    }
  }
  
  # function to sum sub events
  # combine sub storms
  # add a column that will be used for weighting concentrations by total runoff volume
  storm.vols <- storms[,c('unique_storm_number', 'runoff_volume', 'unique_storm_id')]
  storm.vols <- storm.vols %>%
    group_by(unique_storm_number) %>%
    summarise(sum_runoff = sum(runoff_volume), 
              sub_storms = paste(unique_storm_id, collapse = ","))
  
  storms <- merge(storms, storm.vols, by = 'unique_storm_number', all.x = TRUE)
  storms <- mutate(storms, vol_weight = runoff_volume/sum_runoff)
  
  # get rid of any samples that do not have a volume weight, 
  # which will give us NA values later on
  
  
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
  
  if (all(!is.na(concvars))) {
    
    storms <- filter(storms, !is.na(vol_weight))
    
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
        runoff_volume = sum(runoff_volume), 
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
        runoff_volume = sum(runoff_volume), 
        frozen = paste0(unique(frozen), collapse = ', ')
      )
    
    stormdesc$frozen <- as.numeric(ifelse(nchar(stormdesc$frozen) == 1, stormdesc$frozen, event_over_thaw))
    
    wq.bystorm <- loadbystorm
    wq.bystorm <- merge(wq.bystorm, storm.vols)
    wq.bystorm <- merge(wq.bystorm, unique(storms[,c('unique_storm_number', 'sub_storms')]), all.x = TRUE)
    wq.bystorm <- merge(wq.bystorm, stormdesc)
  }
  
  return(wq.bystorm)
}
