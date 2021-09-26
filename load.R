library(plyr)
library(astsa)
library(graphics)
.First <- function(){library(astsa)}
library(forecast)
library(timeSeries)
library(bizdays)
library(shiny)
library(plotly)
library(DT)
library(data.table)
library(quantmod)
library(xts)
library(TTS)
library(zoo)
library(Rcpp)
library(lubridate)
library(tsoutliers)
library(shinyjs)
library(shinydashboard)
require(stringr)

library(sparklyr)
library(dplyr)
library(loggit)
library(dbplyr)
library(shinyWidgets)
library(rugarch)
library(PerformanceAnalytics)
library(shinyalert)
library(promises)
library(future)
library(nnfor)
library(thief)
plan(multiprocess)
library(rmarkdown)


# sc <- spark_connect(master = "local")
# src_tbls(sc)
# options(DT.options = list(
#   lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
#   "dom" = 'lfrtip<"clear">B',
#   buttons = list(list(extend = "csv", filename = "Data"))
# ))
#setwd("D:\\Anvita\\CropPriceForecasting\\University Space\\Primary\\kapc-dashboard\\ForecastTuning")
setwd("//srv//shiny-server//kapc-dashboard")
source("DataAccessCache.R", local=TRUE)

bizdays.options$get("default.calendar")
create.calendar("actual", weekdays = c("saturday","sunday"))
colnames(data_tbl) <- c("maincomm_name","Comm_name", "market_name", "pricelist_date","Variety_name", "grade_name", "Num_arrivals", "Modal_price", "Max_price", "Min_price", "arrival_units", "modal_units", "MSP", "CostA1FL", "CostC2","TotalC3")

mmDates = getMaxAndMinDates()
minDate = mmDates$min
maxDate = mmDates$max

bigDataFile <- getDataWithGrades(minDate, maxDate)
colnames(bigDataFile) <- c("CommodityGroup", "Commodity", "Market", "Date", "Variety", "Grade", "Arrival", "Modal", "Max", "Min", "ArrivalUnits", "ModalUnits", "MSP", "CostA1FL","CostC2", "TotalC3")
#print("Got the bigData File")

trainingDurationStartDate <- maxDate %m-% years(5)
trainingDurationEndDate <- maxDate %m-% months(3) %m-% years(1)

validationStartDate <- trainingDurationEndDate + 1 
validationEndDate <- trainingDurationEndDate %m+% years(1)

referenceDateRangeStartDate <- maxDate %m-% years(1)
referenceDateRangeEndDate <- maxDate

forecastDateRangeStartDate <- maxDate - 15
forecastDateRangeEndDate <- maxDate %m+% months(3)

trainingDurationStartDate <- as.Date(paste(as.character(lubridate::mday(forecastDateRangeStartDate)),"-",lubridate::month(forecastDateRangeStartDate),"-", lubridate::year(trainingDurationStartDate)), format="%d - %m - %Y")
#trainingDurationEndDate <- maxDate %m-% months(3) 
trainingDurationEndDate <- maxDate


x <- list(
  title = "Date"
)

y <- list(
  title = "Modal Prices"
)

xa <- list(
  title = "Arrivals"
)

ya <- list(
  title = "Modal Prices"
)

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}