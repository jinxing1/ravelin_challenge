data_pre_processing <- function (path = "data.csv") {
  # This function allows collecting data from csv file in the given path and pre-process data
  #
  # Argus:
  # -path: path where the csv file is situated (by default: 'data.csv')
  #
  # Returns:
  # -data: data after pre-processing
  
  # load library
  library(data.table)
  library(lubridate)
  
  # load data from csv file
  data <- fread(path, header = TRUE)
  
  # data pre processing
  for (i in 1:nrow(data)) {
    # fill missing date by +1 day to its previous date
    if(data[i]$date == "") {
      data[i, date := as.character(as.Date(data[i-1, date]) + days(1), "%Y-%m-%d") ]
    }
    
    # fill missing order by taking the average of its previous order and next order
    #     in case that the next order is missing, only keep the same as the previous one
    if (is.na(data[i]$orders)) {
      if (!is.na(data[i+1, orders])){
        data[i, orders := as.integer(mean(c(data[i-1, orders], data[i+1, orders])))]
      } else {
        data[i, orders := data[i-1, orders]]
      }
      
    }
    
    # fill missing drivers_available by taking the average of its privious and next drivers_available
    #     in case that the next one is missing, only keep the same as the previous one
    if (is.na(data[i]$drivers_available)) {
      if (!is.na(data[i+1, drivers_available])){
        data[i, drivers_available := as.integer(mean(c(data[i-1, drivers_available], data[i+1, drivers_available])))]
      } else {
        data[i, drivers_available := data[i-1, drivers_available]]
      }
    }
  }
  
  # convert string to Date
  data[, date := as.Date(date)]
  
  # replace the incorrect value in the column 'circumstance'
  #     replace empty circumstance by 'dry' (just randomly picked, 1 wrong observation is tolerable)
  #     since there is only 3 observations, it's faster to do as follows than checking each row
  data[circumstance == "dr", circumstance := "dry"]
  data[circumstance == "RAINY", circumstance := "rainy"]
  data[circumstance == "", circumstance := "dry"]
  
  return(data)
}

########## call functions ##########
# data <- data_pre_processing()
