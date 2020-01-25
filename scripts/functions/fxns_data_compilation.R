
#Functions for data compilation and cross-site analysis

#Calculate water year from posixct or date object
getWY <- function(ts) {
  
  if (class(ts)[1] %in% c('POSIXct', 'POSIXt')) {
    ts <- as.Date(ts, tz=attributes(ts)$tzone)
  } else if (class(ts)[1] %in% c('Date')) {
    
  } else {
    stop("input term is not class Date or POSIXct")
  }
  
year <- year(ts)
add_one <- rep(0, length(year))
add_one[which(month(ts)>=10)] <- 1  

WY <- year +  add_one

return(WY)

}

#combine percent clay, silt, sand among multiple depths
#Need the percents (e.g., clay), Bulk density, and relative heights
CombineDepthPercent <- function (Per.top, BD.top, depth.top, Per.bot, BD.bot, depth.bot){

Per.combined <- (Per.top*BD.top*depth.top + Per.bot*BD.bot*depth.bot)/ (BD.top*depth.top + BD.bot*depth.bot)

return(Per.combined)

}

