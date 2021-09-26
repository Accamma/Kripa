
library(plyr)
library(astsa)


library(data.table)


#setwd("C:\\Anvita\\CropPriceForecasting\\University Space\\Primary\\kapc-dashboard\\Primary")
setwd("//srv//shiny-server//kapc-dashboard")
source("DataAccess.R", local=TRUE)

  mmDates <- getMaxAndMinDatesDB()
  dataFile <- getDataWithGradesDB(mmDates$min, mmDates$max)
  data_tbl <- data.table::as.data.table(dataFile)
  save(data_tbl, file = "CachedData.RData")
