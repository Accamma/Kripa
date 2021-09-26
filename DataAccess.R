library(RCurl)
library(XML)

library(RPostgreSQL)
library("tictoc")
library(tsoutliers)
library(plyr)
library(astsa)
library(dplyr)
library(lubridate)

con <- NULL;

connectDB <- function()
{
  pw <- {
    "kapc"
  }
  
 
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), dbname = "kapcdb",
                   host = "localhost", port = 5432,
                   user = "kapc", password = pw)
  return (con)
}
#market_db <- tbl(con,"kapcschema.Grade")

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
  
  # on.exit(dbDisconnect(con))
  return (comvars)
}

getcom_var_id <- function(comvars, Comm_code, Variety_code){
  for (i in 1:nrow(comvars)){
    if ((comvars$variety_code[i] == Variety_code) & (comvars$comm_code[i] == Comm_code)){
      return(i)
    }
  }
  
}

getPriceList <- function(aDate){
  
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
      Market_code = as.integer(dataList[i]$PriceList$Market_code)
      Comm_code = as.integer(dataList[i]$PriceList$Comm_code)
      Variety_code = as.integer(dataList[i]$PriceList$Variety_code)
      Num_arrivals = as.integer(dataList[i]$PriceList$Num_arrivals)
      Min_price = as.integer(dataList[i]$PriceList$Min_price)
      Max_price = as.integer(dataList[i]$PriceList$Max_price)
      Modal_price = as.integer(dataList[i]$PriceList$Modal_price)
      Grade_code = as.integer(dataList[i]$PriceList$Grade_code)
      
      com_var_id <- getcom_var_id(comvars, Comm_code, Variety_code)
      costs <- getCostsForId(com_var_id, aDate, costsList) 
      
      if (is.null(con))
      { con <- connectDB()}
      q<- paste0("insert into kapcschema.PriceList(Pricelist_date,Market_code,commodity_variety_id,Grade_code,Num_arrivals,Min_price,Max_price,Modal_price, MSP, CostA1FL, TotalC3) values('",aDate,"',",Market_code,",",com_var_id,",",Grade_code,",",Num_arrivals,",",Min_price,",",Max_price,",",Modal_price,",",costs$MSP,",",costs$CostA1FL,",",costs$TotalC3," )")
      q
      dbSendQuery(con, q)
    }
  }
  on.exit(dbDisconnect(con))
}

updateDB <- function(startDate, endDate){
  
  #startDate = "11/02/2018"
  #endDate = "17/02/2018"
  
  dlist <- seq(as.Date(startDate, format="%d/%m/%Y"), as.Date(endDate, format="%d/%m/%Y"), by="days")
  format="%d/%m/%Y"
  tic("total time") 
  
  for (i in 1:length(dlist)){
    sDate = format(dlist[i], format)
    getPriceList(sDate)
  }
  toc(log = TRUE)
}


updateCosts <- function()
{
  costs <- read.csv("costs.csv", header = TRUE)
  
  for (i in 1: nrow(costs)){
    maincomm_code = costs$maincomm_code[i]
    Comm_code = costs$Comm_code[i]
    MSP = costs$MSP[i]
    CostA1FL = costs$CostA1FL[i]
    TotalC3 = costs$TotalC3[i]       	
    
    comvars <- getCommVariety()
    com_var_idList = get_var_ids(comvars, Comm_code, maincomm_code)				
    
    if (!is.null(com_var_idList)){
      for (j in 1: length(com_var_idList)){
        if (is.null(con))
        { con <- connectDB()}
        q<- paste0("insert into kapcschema.costs(commodity_variety_id,MSP,CostA1FL, TotalC3) values('",com_var_idList[j],"',",MSP,",",CostA1FL,",",TotalC3,")")
        q                                          
        dbSendQuery(con, q)
      }
    }
  }
}	  

updateCostsNew <- function()
{
  costs <- read.csv("/srv/shiny-server/kapc-dashboard/Costs_201819_C2.csv", header = TRUE)
  #costs <- read.csv("D:\\Anvita\\CropPriceForecasting\\University Space\\Primary\\kapc-dashboard\\Primary\\Database\\Costs_201819_C2.csv", header = TRUE)
  comvars <- getCommVariety()
  
  for (i in 1: nrow(costs)){
    maincomm_code = costs$maincomm_code[i]
    Comm_code = costs$Comm_code[i]
    Variety_code = costs$Variety_code[i]
    Start_date = costs$Start_date[i]
    MSP = costs$MSP[i]
    CostA1FL = costs$CostA1FL[i]
    TotalC3 = costs$TotalC3[i]  
    CostC2 = costs$CostC2[i]
    
    if(is.null(MSP) || is.na(MSP) || MSP==''){
      MSP=0
    }
    if(is.null(CostA1FL) || is.na(CostA1FL) || CostA1FL==''){
      CostA1FL=0
    }
    if(is.null(TotalC3) || is.na(TotalC3) || TotalC3==''){
      TotalC3=0
    }
    if(is.null(CostC2) || is.na(CostC2) || CostC2==''){
      CostC2=0
    }
    
    #Get all varieties
    com_var_idList = get_var_ids(comvars, Comm_code, maincomm_code,Variety_code)			
    
    if (!is.null(com_var_idList)){
      for (j in 1: length(com_var_idList)){
        if (is.null(con))
        { con <- connectDB()}
        #q<- paste0("insert into kapcschema.costs(commodity_variety_id,start_date,MSP,CostA1FL, TotalC3) values('",com_var_idList[j],"','",Start_date,"','",MSP,"','",CostA1FL,"','",TotalC3,"')")
        #print(q)                                          
        #dbSendQuery(con, q)
        
        #Upsert query
        q<- paste0("insert into kapcschema.costs(commodity_variety_id, start_date, msp, costc2, costa1fl) values ('",com_var_idList[j],"','",Start_date,"','",MSP,"','",CostC2,"','",CostA1FL,"')
                   ON CONFLICT ON CONSTRAINT ux_cvi_sd do update set msp='",MSP,"',costc2='",CostC2,"', costa1fl='",CostA1FL,"';")
        
        write(q,file="myfile.txt",append=TRUE)
      }
    }
  }
}	  


get_var_ids <- function(comvars, Comm_code, maincomm_code, Variety_code){
  com_var_idList=NULL
  count=1
  
  if (is.na(Variety_code)){
    for (i in 1:nrow(comvars)){
      if ((comvars$maincomm_code[i] == maincomm_code) & (comvars$comm_code[i] == Comm_code)){
        com_var_idList[count] =  comvars$commodity_variety_id[i]
        count=count+1
      }
    }
  }else{
    for (i in 1:nrow(comvars)){
    if ((comvars$maincomm_code[i] == maincomm_code) & (comvars$comm_code[i] == Comm_code) &(comvars$variety_code[i] == Variety_code)){
      com_var_idList[count] =  comvars$commodity_variety_id[i]
    }
    }
  }
  return (com_var_idList)
}

getCommStartCosts <- function()
{
  if (is.null(con))
  { con <- connectDB()}
  
  q <- paste0("select commodity_variety_id, start_date from costs;")
  data <- dbGetQuery(con, q)
  
  on.exit(dbDisconnect(con))
  return (data)
}

updateEntirePriceListWithCosts <- function(){

  if (is.null(con))
  { con <- connectDB()}
  
  q <- paste0("update kapcschema.PriceList set MSP=c.MSP, CostA1FL=c.costa1fl, Costc2 = c.costc2, totalc3=c.totalc3 from costs c, pricelist p where p.commodity_variety_id=c.commodity_variety_id and p.pricelist_date>c.start_date and p.pricelist_date<(c.start_date + interval '1 year');")
  dbSendQuery(con, q)
  on.exit(dbDisconnect(con))
}


getData <- function(startDate, endDate){
  
  if (is.null(con))
  { con <- connectDB()}
  
  q <- paste0("select c.maincomm_name,c.Comm_name, m.market_name, p.Pricelist_date,c.Variety_name, p.Num_arrivals, p.Modal_price, p.Max_price, p.Min_price, c.arrival_units, c.modal_units from kapcschema.PriceList p, kapcschema.Market_codes m, kapcschema.Commodity_Variety c
              where p.Pricelist_date >= '",startDate,"' AND p.Pricelist_date <= '",endDate,"' AND c.Commodity_variety_id = p.Commodity_variety_id AND p.Market_code = m.Market_code;")
  
  q
  data <- dbGetQuery(con, q)
  
  on.exit(dbDisconnect(con))
  return (data)
}

getMaxAndMinDatesDB <- function(){
  if (is.null(con))
  { con <- connectDB()}

  q <- paste0("select max(pricelist_date), min(pricelist_date) from kapcschema.pricelist;")

  print(q)
  mmdates <- dbGetQuery(con, q)
  on.exit(dbDisconnect(con))
  return (mmdates)
}

getDataWithGradesDB <- function(startDate, endDate){

  if (is.null(con))
  { con <- connectDB()}

  print(paste0("Fetching Data from ", startDate, " to ", endDate))
  q <- paste0("select c.commodity_variety_id, c.maincomm_name,c.Comm_name, m.market_name, p.Pricelist_date,c.Variety_name, g.grade_name, p.Num_arrivals, p.Modal_price, p.Max_price, p.Min_price, c.arrival_units, c.modal_units from kapcschema.PriceList p, kapcschema.Market_codes m, kapcschema.Commodity_Variety c, kapcschema.grade g
              where p.Pricelist_date >= '",startDate,"' AND p.Pricelist_date <= '",endDate,"' AND c.Commodity_variety_id = p.Commodity_variety_id AND p.Market_code = m.Market_code AND p.grade_code= g.grade_code;")
  
  print(q)
  
  data <- dbGetQuery(con, q)
  print(nrow(data))
  
  costs = getFullCostsDB()
  
  #Merge Cost and other data
  
  costs$end_date = costs$start_date %m+% years(1)-1
  
  data1 <- left_join(data, costs, all.x = TRUE, by="commodity_variety_id") %>%
    filter(is.na(start_date) | (pricelist_date >= start_date & pricelist_date <= end_date))
  
  difference = setdiff(data,data1[,1:13])
  
  data1$start_date = NULL
  data1$end_date = NULL
  
  difference$msp =0
  difference$costa1fl = 0
  difference$totalc3 = 0
  difference$costc2 = 0
  
  data = rbind(data1,difference)
  
  on.exit(dbDisconnect(con))
  return (data)
}

getFullCostsDB <- function(){
  if (is.null(con))
  { con <- connectDB()}
  
  q <-  paste0("select commodity_variety_id,start_date,msp,costa1fl,costc2,totalc3 from kapcschema.costs;")
  q
  cost <- dbGetQuery(con, q)
  
  on.exit(dbDisconnect(con))
  return (cost)
}

getFromCosts <- function(){
  if (is.null(con))
  { con <- connectDB()}
  
  q <-  paste0("select Commodity_variety_id, start_date, MSP, CostA1FL, TotalC3 from kapcschema.costs;")
  q
  cost <- dbGetQuery(con, q)
  
  on.exit(dbDisconnect(con))
  return (cost)
}


getCostsForId <- function(com_var_id, pricelist_date, costsList) {
  
  costParams=data.frame(MSP=numeric(1),CostA1FL=numeric(1), TotalC3=numeric(1))
  for (i in 1:nrow(costsList)){
    if (costsList$com_var_di[i] == com_var_id){
      if ((pricelist_date >= (as.Date(costsList$start_date[i]))) & (pricelist_date < (as.Date(costsList$start_date[i]) + 365))){
        costParams$MSP=costList$MSP[i]
        costParams$CostA1FL=costList$CostA1FL[i]
        costParams$TotalC3=costList$TotalC3[i]
      }
    }
  }
  return (costParams)
}

getCost <- function(){
  
  
  if (is.null(con))
  { con <- connectDB()}
  
  q <-  paste0("select distinct c.maincomm_name,c.Comm_name, c.Variety_name, co.start_date, co.MSP, co.CostA1FL, co.TotalC3 from kapcschema.Commodity_Variety c, kapcschema.Costs co 
               where c.Commodity_variety_id = co.Commodity_variety_id;")
  
  q
  cost <- dbGetQuery(con, q)
  
  on.exit(dbDisconnect(con))
  return (cost)
  
}

outlierKD <- function(dt, var) 
{
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow = c(2, 2), oma = c(0,0,3,0))
  #boxplot(var_name, main = "With outliers")
  #hist(var_name, main = "With outliers", xlab = NA, ylab = NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  #boxplot(var_name, main = "Without outliers")
  #hist(var_name, main = "Without outliers", xlab = NA, ylab = NA)
  #title("Outlier Check", outer = TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  cat("Outliers successfully removed", "n")
  return(invisible(dt))
}

cleanData <- function(dataFile){
  dataFile$Modal <- tsoutliers::tso(chicken,types = c("AO","LS","TC"),maxit.iloop=10)
}


build_fields <- function(entries_to_add) {
  
    lapply(1:entries_to_add, function(entry) {
      list(
        column(12, dateRangeInput(paste0("period", entry), label = paste0("Period ", entry), start = ActualDurationStartDate, end = ActualDurationEndDate, format = "dd-mm-yyyy", min=minDate, max=maxDate ))
      )
    })
}



  

