forecasting <- function(date = Sys.Date(), circ = "dry", drivers_available = 4000) {
  # This function allows to predict the orders with a given date, a giver circumstance and a given drivers_available
  #
  # Args:
  # -date: a given date (by defaultf: current date)
  # -circ: a given circumstance (by default: "dry"; it can by "rainy", "very_rainy", "strike")
  # -drivers_available: a given drivers_available (by default, 4000)
  
  # load function
  source("data_pre_processing.R")

  data <- data_pre_processing()
  
  lr <- lm(orders ~ as.numeric(date) + I((as.numeric(date))^2) +
             I((as.numeric(date))^3) + drivers_available +
             I(drivers_available^2) + I(drivers_available^3) +
             I(drivers_available^4) + I(drivers_available^5),
           data[circumstance==circ])
  
  prediction <- predict(lr, data.frame(as.numeric(date), as.numeric(date)^2,
                as.numeric(date)^3, drivers_available,
                drivers_available^2, drivers_available^3,
                drivers_available^4, drivers_available^5))
  return(prediction)
}

#######################" call function ########################
# date :2014-02-25
# circumstance : dry
# drivers: 7980
# forecasting(date = as.Date("2014-02-25"), circ = "dry", drivers_available = 7980)