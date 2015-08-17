# install packages
# install.packages("data.table")
# install.packages("lubridate")
# install.packages("ReporteRs")
# isntall.packages("ggplot2")

# load files
## generate the analysis report
source("data_analysing.R")

## forecasting function
source("forecasting.R")

# date :2014-02-25
# circumstance : dry
# drivers: 7980
forecasting(date = as.Date("2014-02-25"), circ = "dry", drivers_available = 7980)
