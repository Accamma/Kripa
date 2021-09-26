library(RCurl)
library(XML)
library(RPostgreSQL)
library(lubridate)

library("tictoc")
library(tsoutliers)
library(plyr)
library(astsa)

con <- NULL;

connectDB <- function()
{
  pw <- {
    "kapc"
  }
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "kapcdb",
                   host = "localhost", port = 5432,
                   user = "kapc", password = pw)
  return (con)
}
getMarketCodes<- function()
{
  if (is.null(con))
  { con <- connectDB()}
  
  
  market_codes <- dbGetQuery(con, paste0("select market_code, market_name from kapcschema.market_codes"));
  
  on.exit(dbDisconnect(con))
  return (market_codes)
}
getGrades<- function()
{
  if (is.null(con))
  { con <- connectDB()}
  
  
  grades <- dbGetQuery(con, paste0("select grade_code, grade_name from kapcschema.Grade"));
  
  on.exit(dbDisconnect(con))
  return (grades)
}

getUnits <- function()
{
  if (is.null(con))
  { con <- connectDB()}
  
  
  units <- dbGetQuery(con, paste0("select unit_code,unit_name from kapcschema.Unit"));
  
  on.exit(dbDisconnect(con))
  return (units)
}

getCommVariety <- function()
{
  if (is.null(con))
  { con <- connectDB()}
  
  
  comvars <- dbGetQuery(con, paste0("select commodity_variety_id,maincomm_code, maincomm_name, Comm_code, Comm_name, Variety_code, Variety_name from kapcschema.Commodity_Variety"));
  
  on.exit(dbDisconnect(con))
  return (comvars)
}

getMaxDateFromDB <- function(){
  if (is.null(con))
  { con <- connectDB()}
  
  
  maxDate <- dbGetQuery(con, paste0("select sl_no, pricelist_date from kapcschema.pricelist where sl_no=(select max(sl_no) from kapcschema.pricelist);"));
  
  on.exit(dbDisconnect(con))
  return (maxDate)
}

getcom_var_id <- function(comvars, Comm_code, Variety_code){
  for (i in 1:nrow(comvars)){
    if ((comvars$variety_code[i] == Variety_code) & (comvars$comm_code[i] == Comm_code)){
      return(comvars$commodity_variety_id[i])
    }
  }
}

getFromCosts <- function(){
  if (is.null(con))
  { con <- connectDB()}
  
  q <-  paste0("select Commodity_variety_id, start_date, MSP, CostA1FL, TotalC3, CostC2 from kapcschema.costs;")
  q
  cost <- dbGetQuery(con, q)
  
  cost[is.na(cost)] <- 0
  on.exit(dbDisconnect(con))
  return (cost)
}


getCostsForId <- function(com_var_id, pricelist_date, costsList) {
  
  costParams=data.frame(MSP=numeric(1),CostA1FL=numeric(1), TotalC3=numeric(1))
  for (i in 1:nrow(costsList)){
    if (costsList$commodity_variety_id[i] == com_var_id){
      if ((as.Date(pricelist_date, format="%d/%m/%Y") >= (as.Date(costsList$start_date[i]))) & (as.Date(pricelist_date, format="%d/%m/%Y") < (as.Date(costsList$start_date[i]) + 365))){
        if ((!is.null(costsList$msp[i])) & (!is.nan(costsList$msp[i]))){
          costParams$MSP[1]=costsList$msp[i]}
        
        if ((!is.null(costsList$costa1fl[i])) & (!is.nan(costsList$costa1fl[i]))){
          costParams$CostA1FL[1]=costsList$costa1fl[i]}
          
        if ((!is.null(costsList$totalc3[i])) & (!is.nan(costsList$totalc3[i]))){
          costParams$TotalC3[1]=costsList$totalc3[i]}
      }
    }
  }
  return (costParams)
}

getPriceList <- function(aDate, sl_no){
  
  if (is.null(con))
  { con <- connectDB()}
  
  #aDate='08/05/2017'
  
  headerFields =
    c(Accept = "text/xml",
      Accept = "multipart/*",
      'Content-Type' = "text/xml; charset=utf-8",
      SOAPAction="http://tempuri.org/GetMarketPrice")
  
  body = paste0('<?xml version="1.0" encoding="utf-8"?>
                <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                <soap:Body>
                <GetMarketPrice xmlns="http://tempuri.org/">
                <Username xsi:type="s:string">PriceCommission</Username>
                <Password xsi:type="s:string">Kapc$18</Password>
                <today xsi:type="s:string">',aDate,'</today>
                </GetMarketPrice>
                </soap:Body>
                </soap:Envelope>')
  
  
  reader = basicTextGatherer()
  
  handle = getCurlHandle()
  
  
  curlPerform(url= "https://www.krishimaratavahini.kar.nic.in/KAPC_Service.asmx?WSDL",
              httpheader = headerFields,
              postfields = body,
              writefunction = reader$update
  )
  
  data1 <- XML::xmlParse(reader$value())
  data <- XML::xmlToList(data1)
  dataList <- data$Body$GetMarketPriceResponse$GetMarketPriceResult$diffgram$NewDataSet
 
  print(paste0("Inserting into database:",length(dataList)," for date:",aDate))
  if (!is.null(dataList)){
    comvars <- getCommVariety()
    market_codes <- getMarketCodes()
    grades <- getGrades()
    units <- getUnits()
    costsList <- getFromCosts()
    
    for (i in 1:length(dataList)){
      tic("Inserting")
      sl_no=sl_no+1
      Market_code = as.integer(dataList[i]$PriceList$Market_code)
      Comm_code = as.integer(dataList[i]$PriceList$Comm_code)
      Variety_code = as.integer(dataList[i]$PriceList$Variety_code)
      Num_arrivals = as.integer(dataList[i]$PriceList$Num_arrivals)
      Min_price = as.integer(dataList[i]$PriceList$Min_price)
      Max_price = as.integer(dataList[i]$PriceList$Max_price)
      Modal_price = as.integer(dataList[i]$PriceList$Modal_price)
      Grade_code = as.integer(dataList[i]$PriceList$Grade_code)
      
      com_var_id <- getcom_var_id(comvars, Comm_code, Variety_code)
      print(paste0("Getting Cost for:Comm Code ",Comm_code,", Variety_code:",Variety_code," , com_var_id:",com_var_id))
      costs <- getCostsForId(com_var_id, aDate, costsList) 
    
      if (is.null(con))
      { con <- connectDB()}
      q<- paste0("insert into kapcschema.PriceList(sl_no, Pricelist_date,Market_code,commodity_variety_id,Grade_code,Num_arrivals,Min_price,Max_price,Modal_price, MSP, CostA1FL, TotalC3) values(",sl_no,",'",aDate,"',",Market_code,",",com_var_id,",",Grade_code,",",Num_arrivals,",",Min_price,",",Max_price,",",Modal_price,",",costs$MSP,",",costs$CostA1FL,",",costs$TotalC3," )")
      print(q)
     dbSendQuery(con, q)
     toc(log = TRUE)
    }
  }
  on.exit(dbDisconnect(con))
  return(sl_no)
}

deletePricelistFomDB <- function(startDate, endDate){
  if (is.null(con))
  { con <- connectDB()}
  q<- paste0("delete from kapcschema.PriceList where pricelist_date >= '",startDate,"' and pricelist_date <= '",endDate,"';")
  print(q)
  t <- dbSendQuery(con, q)
  print(t)
}


updateValueUptoNextMar <- function(dataFileMI) {
  #Get the last value
  
  
  minDate = min(dataFileMI$Date)
  yMinDate = year(minDate)
  maxDate = max(dataFileMI$Date)
  yMaxDate = year(maxDate)
  
  
  
  for (iy in yMinDate:yMaxDate){
    
    currentYear <- as.numeric(format(Sys.Date(), "%Y"))
    if(iy>currentYear+1)
      break;
    
    dPrev0 = as.Date(paste0(iy-1,"-04-01"), "%Y-%m-%d")
    dPrev = as.Date(paste0(iy,"-03-31"), "%Y-%m-%d")
    
    #Extending MSP
    m1 = mean(dataFileMI$MSP[which(dataFileMI$Date >=  dPrev0 & dataFileMI$Date <=  dPrev)])
    if (!is.na(m1) & (m1 > 0)){
      dNext = as.Date(paste0(iy,"-04-01"), "%Y-%m-%d")
      dLast = as.Date(paste0(iy+1,"-03-31"), "%Y-%m-%d")
      
      m2 = mean(dataFileMI$MSP[which(dataFileMI$Date >=  dNext & dataFileMI$Date <=  dLast)])
      
      if(!is.na(m2) & m2 == 0){
        index <- which(dataFileMI$Date >= dNext & dataFileMI$Date <= dLast)
        dataFileMI$MSP[index] = m1
      }
    }
    
    #Extending CostA1FL
    m1 = mean(dataFileMI$CostA1FL[which(dataFileMI$Date >=  dPrev0 & dataFileMI$Date <=  dPrev)])
    if (!is.na(m1) & (m1 > 0)){
      dNext = as.Date(paste0(iy,"-04-01"), "%Y-%m-%d")
      dLast = as.Date(paste0(iy+1,"-03-31"), "%Y-%m-%d")
      
      m2 = mean(dataFileMI$CostA1FL[which(dataFileMI$Date >=  dNext & dataFileMI$Date <=  dLast)])
      
      if(!is.na(m2) & m2 == 0){
        index <- which(dataFileMI$Date >= dNext & dataFileMI$Date <= dLast)
        dataFileMI$CostA1FL[index] = m1
      }
    }
    
    #Extending CostC2
    m1 = mean(dataFileMI$CostC2[which(dataFileMI$Date >=  dPrev0 & dataFileMI$Date <=  dPrev)])
    if (!is.na(m1) & (m1 > 0)){
      dNext = as.Date(paste0(iy,"-04-01"), "%Y-%m-%d")
      dLast = as.Date(paste0(iy+1,"-03-31"), "%Y-%m-%d")
      
      m2 = mean(dataFileMI$CostC2[which(dataFileMI$Date >=  dNext & dataFileMI$Date <=  dLast)])
      
      if(!is.na(m2) & m2 == 0){
        index <- which(dataFileMI$Date >= dNext & dataFileMI$Date <= dLast)
        dataFileMI$CostC2[index] = m1
      }
    }
    
    #Extending TotalC3
    m1 = mean(dataFileMI$TotalC3[which(dataFileMI$Date >=  dPrev0 & dataFileMI$Date <=  dPrev)])
    if (!is.na(m1) & (m1 > 0)){
      dNext = as.Date(paste0(iy,"-04-01"), "%Y-%m-%d")
      dLast = as.Date(paste0(iy+1,"-03-31"), "%Y-%m-%d")
      
      m2 = mean(dataFileMI$TotalC3[which(dataFileMI$Date >=  dNext & dataFileMI$Date <=  dLast)])
      
      if(!is.na(m2) & m2 == 0){
        index <- which(dataFileMI$Date >= dNext & dataFileMI$Date <= dLast)
        dataFileMI$TotalC3[index] = m1
      }
    }
    
  }
  return(dataFileMI)
}

endDate = with_tz(Sys.Date(), "Asia/Kolkata")
startDate = endDate %m-% months(1)
deletePricelistFomDB(startDate, endDate)
print("Deleted last 1 month from database")
maxRow = getMaxDateFromDB()

if (nrow(maxRow)>0){
  sl_no = maxRow$sl_no
  maxDate = as.Date(maxRow$pricelist_date)
  startDate = maxDate + 1
  #endDate = as.Date('31/01/2018', format="%d/%m/%Y")
  endDate = with_tz(Sys.Date(), "Asia/Kolkata")
# endDate =  endDate - 14
}else{
  sl_no=1
  startDate='01/01/2012'
  endDate ='31/12/2013'
}

if (startDate <= endDate){
  dlist <- seq(as.Date(startDate, format="%d/%m/%Y"), as.Date(endDate, format="%d/%m/%Y"), by="days")
  format="%d/%m/%Y"
  tic("total time") 
  
  for (i in 1:length(dlist)){
    sDate = format(dlist[i], format)
    sl_no = getPriceList(sDate, sl_no)
  }
  toc(log = TRUE)
}else{
  print(paste0("startDate:",startDate))
  print(paste0("endDate:",endDate))
  print("Nothing to update")
}

#Cache the Database
setwd("//srv//shiny-server//kapc-dashboard")
source("DataAccess.R", local=TRUE)
mmDates <- getMaxAndMinDatesDB()
dataFile <- getDataWithGradesDB(mmDates$min, mmDates$max)
colnames(dataFile) <- c("commodity_variety_id", "CommodityGroup", "Commodity", "Market", "Date", "Variety", "Grade", "Arrival", "Modal", "Max", "Min", "ArrivalUnits", "ModalUnits", "MSP", "CostA1FL", "CostC2", "TotalC3" )
idList = unique(dataFile$commodity_variety_id)
for (itr in 1:length(idList)){
  indList <- which(dataFile$commodity_variety_id == idList[itr])
  df <- dataFile[indList,]
  df <- updateValueUptoNextMar(df)
  dataFile[indList,] <- df
}
data_tbl <- data.table::as.data.table(dataFile[,2:17])
colnames(data_tbl) <- c("maincomm_name","Comm_name", "market_name", "pricelist_date","Variety_name", "grade_name", "Num_arrivals", "Modal_price", "Max_price", "Min_price", "arrival_units", "modal_units", "MSP", "CostA1FL", "CostC2", "TotalC3")
save(data_tbl, file = "CachedData.RData")
file.copy("CachedData.RData", paste("KSAMB_Reports//KSAMB_Backup//CachedData",endDate,".RData")) #####
