

RMevents_sample_LCL <- function(dfrain,
                                ieHr=6,
                                rain="rain",
                                time="pdate",
                                dfsamples,
                                bdate="bpdate",
                                edate="epdate"){
  
  # Filter rain data (rain > 0) and calculate time differences
  df <- dfrain %>%
    filter(rain > 0 | row_number() == 1) %>%
    filter(is.finite(!!sym(time))) %>%
    arrange(!!sym(time)) %>%
    mutate(timediff = difftime(!! sym(time), lag(!! sym(time), 1), units = "secs"),
           timediff_min = difftime(!! sym(time), lag(!! sym(time), 1), units = "mins"),
           across(contains("timediff"), as.numeric)
    ) 
  
  # State variables for filtering results
  rain_timezone <- lubridate::tz(df[,time])
  rain_first <- min(dfrain[,time], na.rm = TRUE)
  rain_last <- max(dfrain[,time], na.rm = TRUE)
  ieSec <- ieHr * 3600 # compute interevent period in seconds to use with POSIX
  
  # Objects to fill in per row
  rainDepth <- as.numeric(rep(NA, nrow(dfsamples)))
  startRainDates <- endRainDates <- as.POSIXct(rep(NA, nrow(dfsamples)), tz = rain_timezone)
  tipsbystorm_list <- list()
  
  i = 1
  for (i in 1:nrow(dfsamples)){
    # if sample dates are outside precip dates skip
    if (dfsamples[i, edate] <= rain_first |
        dfsamples[i, bdate] >= rain_last){
      startRainDates[i] <- endRainDates[i] <- NA
      rainDepth[i] <- NA
      next
    }
    
    # Identify first rain row after the start time and
    # the last rain row before the end time 
    beginRow <- min(which(df[, time] > dfsamples[i, bdate]))
    endRow <- max(which(df[,time] < dfsamples[i,edate]))
    
    # rain end time (ED). Note that ED can be before the sample time
    # if rain == 0 during the flow period
    ED <- df[endRow,time]
    
    # rain record preceding sample time 
    subdf <- df[c(1:(beginRow-1)),]
    
    # Find the start of the most recent rain event preceding sample time and resubset
    if (length(which(subdf$timediff > ieSec)) > 0) {
      startRainRow <- max(which(subdf$timediff > ieSec))
      subdf <- df[startRainRow:(beginRow-1),]
    } else {
      startRainRow = 1
      }
    
    # Identify rain start timedate
    # if end of last preceding storm is within ieSec from sample start time, 
    # include the preceding storm. Otherwise start with first rain in interval
    if (difftime(dfsamples[i,bdate], max(subdf[,time]), units = "secs") < ieSec) {
      BD <- subdf[1,time]
    } else {
      BD <- df[beginRow,time] 
    }
    
    # If first rain tip is after end of flow, set start and end times
    if (BD > dfsamples[i, edate]) {
      BD <- dfsamples[i, bdate]
      ED <- BD + 60 
    }
    
    # Final subset of data to include in rain totals
    subdf2 <- df %>%
      filter(!! sym(time) >= BD &
               !! sym(time) <= ED)
    
    if(ED < BD) {
      ED <- BD + 60
      startRainDates[i] <- BD
      endRainDates[i] <- ED
      rainDepth[i] <- NA
      next
      
    } 
    
    eventRows <- dfrain %>%
      filter(!! sym(time) >= BD &
               !! sym(time) <= ED)
    
    #Save times and rain total
    rainDepth[i] <- sum(eventRows[,rain])
    startRainDates[i] <- BD
    endRainDates[i] <- ED
    
    # save data frame of rain event, includes zeros, and add event id column
    tipsbystorm_list[[i]] <- df %>%
      filter(!! sym(time) >= BD & 
               !! sym(time) <= ED) %>%
      mutate(event = i)
    
  }
  
  # Objects to return
  df_out <- data.frame(stormnum = 1:nrow(dfsamples),
                       StartDate = startRainDates,
                       EndDate = endRainDates,
                       rain = rainDepth)
  
  # Bind all tipsbystorm
  tipsbystorm <- bind_rows(tipsbystorm_list, .id = NULL) %>%
    select(!! sym(rain), !! sym(time),
           dif_time = timediff_min,
           event)
  
  # Minimum time interval
  timeInterval <- min(tipsbystorm$dif_time, na.rm = T)
  
  out <- list(df_out, df_out, tipsbystorm, timeInterval)
  names(out) <- c('storms2', 'storms', 'tipsbystorm', 'timeInterval')
  
  return(out)
}


events <- RMevents_sample(df = precip_raw, ieHr = ieHr, rain = 'rain', time = 'pdate',
                          dfsamples = wq.dat, bdate = 'storm_start', edate = 'storm_end')

events_LCL <- RMevents_sample_LCL(dfrain = precip_raw, ieHr = ieHr, rain = 'rain', time = 'pdate',
                          dfsamples = wq.dat, bdate = 'storm_start', edate = 'storm_end')

identical(events, events_LCL)

tail(events$storms)
tail(events_LCL$storms)

