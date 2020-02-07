

#Function to remove columns in a data.frame that contain all NAs

not_all_na <- function(x) {!all(is.na(x))}

