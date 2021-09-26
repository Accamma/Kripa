library(RCurl)
library(XML)

library(RPostgreSQL)
library("tictoc")
library(tsoutliers)
library(plyr)
library(astsa)
library(dplyr)
require(data.table)


load("CachedData.RData")

getMaxAndMinDates <- function(){
  mm=NULL
 mm$max <- data_tbl[,data_tbl[which.max(pricelist_date)]]$pricelist_date
 mm$min <- data_tbl[,data_tbl[which.min(pricelist_date)]]$pricelist_date
 return (mm)
}

getDataWithGrades <- function(startDate, endDate){
  filteredDate <- data_tbl[,data_tbl[which(pricelist_date>=startDate & pricelist_date <= endDate)]]
  return (filteredDate)
}

getPeriod <- function(dataFile){
  
  per = 0
  dataFile1 = dataFile
  
  if (nrow(dataFile) > 2){
    dfD = data.frame(as.Date(sort(dataFile$Date)))
    dfD$day <- weekdays(as.Date(dfD$as.Date.sort.dataFile.Date..))
    
    T = length(which(dfD$day == 'Sunday'))
    if (T>0)
      per=per+1
    
    
    T = length(which(dfD$day == 'Monday'))
    if (T>0)
      per=per+1
    
    T = length(which(dfD$day  == 'Tuesday'))
    if (T>0)
      per=per+1
    
    T = length(which(dfD$day  == 'Wednesday'))
    if (T>0)
      per=per+1
    
    T = length(which(dfD$day  == 'Thursday'))
    if (T>0)
      per=per+1
    
    T = length(which(dfD$day == 'Friday'))
    if (T>0)
      per=per+1
    
    T = length(which(dfD$day == 'Saturday'))
    if (T>0)
      per=per+1
  }
  return (per)
}


calcPDQpdq <- function(dataFile, testingData)
{
  
  df = dataFile
  per = getPeriod(dataFile)
  
  paramsDataFrame <- data.frame(p=0,q=0,d=1,P=0,Q=0,D=0, arimaPeriod=per)
  paramsDataFrameNew <- data.frame(p=0,q=0,d=1,P=0,Q=0,D=1, arimaPeriod=per)
  mape1=0
  mape2=1
  
  paramsDataFrame <- rbind(paramsDataFrame,paramsDataFrameNew)
  #paramsDataFrame$arimaPeriod=per
  
  
  
  #First calculate p,q,P,Q with D=0
  
  
  if (nrow(dataFile) > 6){
    
    #Calculate p,q,P,Q for D=0
    
    #Identify q
    acf1 <- acf(diff(dataFile$Modal))#q=3
    acf1$acf1 <- abs(acf1$acf)
    names(acf1)
    nacf1 <- length(acf1$acf)
    acf1$n.used
    ncommodity <- length(dataFile$Modal)
    acf1$x <- 2 * sqrt((1/ acf1$n.used) * ((acf1$n.used - acf1$lag)/(acf1$n.used + 2)))
    acf1$qacf1 <- 0
    
    names(acf1)
    
    #Identification q based on StandardError of acf1
    
    for(i in 1:nacf1)
    {
      if (acf1$acf1[i] >= acf1$x[i])
      {
        (acf1$qacf1[i] <- 1)
      }
      else
        (acf1$qacf1[i] <- 0)
    }
    
    acf1$qaacf1 <- ave(acf1$qacf1, data.table::rleid(acf1$qacf1 != 0), FUN=cumsum)
    acf1$qaacf1[acf1$qaacf1 == 0] <- NA
    qaacf1.rle <- rle(is.na(acf1$qaacf1))
    if ((qaacf1.rle$values[1] == TRUE)){
      q <- 0
    }else{
      q <- qaacf1.rle$lengths[1] - 1
    }
    paramsDataFrame$q[1] = q
    
    #Identification of p based on pacf1$x 
    pacf1 <- pacf(diff(dataFile$Modal))# p=3
    pacf1$pacf1 <- abs(pacf1$acf)
    names(pacf1)
    npacf1 <- length(pacf1$pacf1)
    pacf1$x <- 2 * sqrt((1/ pacf1$n.used) * ((pacf1$n.used - pacf1$lag)/(pacf1$n.used + 2)))
    pacf1$ppacf1 <- 0
    
    names(pacf1)
    
    for(i in 1:npacf1)
    {
      if (pacf1$pacf1[i] >= pacf1$x[i])
      {
        (pacf1$ppacf1[i] <- 1)
      }
      else
        (pacf1$ppacf1[i] <- 0)
    }
    
    pacf1$ppacf1 <- ave(pacf1$ppacf1, data.table::rleid(pacf1$ppacf1 != 0), FUN=cumsum)
    pacf1$ppacf1[pacf1$ppacf1 == 0] <- NA
    ppacf1.rle <- rle(is.na(pacf1$ppacf1))
    if ((ppacf1.rle$values[1] == TRUE)){
      p=0
    }else{
      p <- ppacf1.rle$lengths[1]
    }
    paramsDataFrame$p[1] = p
    
    nrow(dataFile$Modal)
    
    Pacf <- pacf(diff(dataFile$Modal))# change 7 to 6 for all others, p=3, P=2
    
    SeaQacf <- per #to be derived based on days per week
    
    
    #Identification of q and Q from ACF Series diff(diff)
    #Identification of q from ACF Series diff(diff)
    Qacf <- acf(diff(dataFile$Modal))# change 7 to 6 for all others, q=6, Q= 4
    Qacf$acf1 <- abs(Qacf$acf)
    names(Qacf)
    nQacf <- length(Qacf$acf1)
    Qacf$x <- 2 * sqrt((1/ Qacf$n.used) * ((Qacf$n.used - Qacf$lag)/(Qacf$n.used + 2)))
    Qacf$qacf1 <- 0
    
    names(Qacf)
    
    #Identification q based on StandardError of Qacf1
    
    for(i in 1:nQacf)
    {
      if (Qacf$acf1[i] >= Qacf$x[i])
      {
        (Qacf$qacf1[i] <- 1)
      }
      else
        (Qacf$qacf1[i] <- 0)
    }
    
    Qacf$qaacf1 <- ave(Qacf$qacf1, data.table::rleid(Qacf$qacf1 != 0), FUN=cumsum)
    Qacf$qaacf1[Qacf$qaacf1 == 0] <- NA
    Qqaacf.rle <- rle(is.na(Qacf$qaacf1))
    Qq <- Qqaacf.rle$lengths[1] - 1 #Qq is p value of QACF, minus 1 is to account for 0 lag
    
    if(Qq > (SeaQacf - 1)) {
      Qq = (SeaQacf - 1)
    } else {
      Qq = Qq
    }
    
    Qq
    
    #Identification of Q from ACF Series diff(diff)
    Qacf <- acf(diff(dataFile$Modal))# change 7 to 6 for all others, q=6, Q= 4
    Qacf$acf1 <- abs(Qacf$acf)
    names(Qacf)
    nQacf <- length(Qacf$acf1)
    Qacf$x <- 2 * sqrt((1/ Qacf$n.used) * ((Qacf$n.used - Qacf$lag)/(Qacf$n.used + 2)))
    Qacf$lag1 <- as.numeric(Qacf$lag)
    Qacf$lag1 <- c(1:(max(Qacf$lag1)+1))
    AQ <- (Qacf$lag1[which(Qacf$lag1%%SeaQacf==0)])
    AQ <- AQ + 1
    AQ
    
    QacfSea <- Qacf[c(AQ),]
    QacfSea$acf <- abs(QacfSea$acf)
    QacfSea$x <- Qacf$x[AQ]
    QacfSea$n <- 0
    Qacf1 <- length(QacfSea$acf)
    
    if (Qacf1 > 0){
      for(i in 1:Qacf1)
      {
        if ((!is.na(QacfSea$acf[i])) & (QacfSea$acf[i] >= QacfSea$x[i]))
        {
          (QacfSea$n[i] <- 1)
        }else{
          (QacfSea$n[i] <- 0)}
      }
      
      QacfSea$Q <- ave(QacfSea$n, data.table::rleid(QacfSea$n != 0), FUN=cumsum)
      QacfSea$n[QacfSea$n == 0] <- NA
      Q.rle <- rle(is.na(QacfSea$n))
      
      if ((Q.rle$values[1] == TRUE)){
        Q <- 0
      }else{
        Q <- Q.rle$lengths[1]}
      
      paramsDataFrame$Q[1] = Q
    }else{
      Q=0
      paramsDataFrame$Q[1] = 0
    }
    
    
    #Identification of p and P from PACF Series diff(diff)
    #Identification of p from PACF Series diff(diff)
    
    Pacf <- pacf(diff(dataFile$Modal))# change 7 to 6 for all others, p=3, P=2
    Pacf$acf1 <- abs(Pacf$acf)
    names(Pacf)
    nPacf <- length(Pacf$acf1)
    Pacf$x <- 2 * sqrt((1/ Pacf$n.used) * ((Pacf$n.used - Pacf$lag)/(Pacf$n.used + 2)))
    Pacf$pacf1 <- 0
    
    names(Pacf)
    
    #Identification p based on StandardError of Pacf1
    
    for(i in 1:nPacf)
    {
      if (Pacf$acf1[i] >= Pacf$x[i])
      {
        (Pacf$pacf1[i] <- 1)
      }else{
        (Pacf$pacf1[i] <- 0)
      }
    }
    
    Pacf$paacf1 <- ave(Pacf$pacf1, data.table::rleid(Pacf$pacf1 != 0), FUN=cumsum)
    Pacf$paacf1[Pacf$paacf1 == 0] <- NA
    Ppaacf1.rle <- rle(is.na(Pacf$paacf1))
    Pp <- Ppaacf1.rle$lengths[1] #Pp is p value of PACF
    
    if(Pp > (SeaQacf - 1)) {
      Pp = (SeaQacf - 1)
    } else {
      Pp = Pp
    }
    
    Pp
    
    #Identification of P from PACF Series diff(diff)
    
    Pacf$acf1 <- abs(Pacf$acf)
    names(Pacf)
    nPacf <- length(Pacf$acf1)
    Pacf$x <- 2 * sqrt((1/ Pacf$n.used) * ((Pacf$n.used - Pacf$lag)/(Pacf$n.used + 2)))
    Pacf$lag1 <- as.numeric(Pacf$lag)
    Pacf$lag1 <- c(1:(max(Pacf$lag1)))
    AP <- (Pacf$lag1[which(Pacf$lag1%%SeaQacf==0)])
    AP
    
    PacfSea <- Pacf[c(AP),]
    PacfSea$acf <- abs(PacfSea$acf)
    PacfSea$x <- Pacf$x[AP]
    PacfSea$n <- 0
    Pacf1 <- length(PacfSea$acf)
    
    if (Pacf1 > 0){
      for(i in 1:Pacf1)
      {
        if (PacfSea$acf[i] >= PacfSea$x[i])
        {
          (PacfSea$n[i] <- 1)
        }
        else
          (PacfSea$n[i] <- 0)
      }
      
      PacfSea$P <- ave(PacfSea$n, data.table::rleid(PacfSea$n != 0), FUN=cumsum)
      PacfSea$n[PacfSea$n == 0] <- NA
      P.rle <- rle(is.na(PacfSea$n))
      
      if ((P.rle$values[1] == TRUE)){
        P <- 0
      }else{
        P <- P.rle$lengths[1]}
      
      
      paramsDataFrame$P[1] = P
    }else{
      P=0
      paramsDataFrame$P[1] = 0
    }
    
    #Calculate p,q,P,Q for D=1
    #Identify q
    acf1 <- acf(diff(diff(dataFile$Modal)))#q=3
    acf1$acf1 <- abs(acf1$acf)
    names(acf1)
    nacf1 <- length(acf1$acf)
    acf1$n.used
    ncommodity <- length(dataFile$Modal)
    acf1$x <- 2 * sqrt((1/ acf1$n.used) * ((acf1$n.used - acf1$lag)/(acf1$n.used + 2)))
    acf1$qacf1 <- 0
    
    names(acf1)
    
    #Identification q based on StandardError of acf1
    
    for(i in 1:nacf1)
    {
      if (acf1$acf1[i] >= acf1$x[i])
      {
        (acf1$qacf1[i] <- 1)
      }
      else
        (acf1$qacf1[i] <- 0)
    }
    
    acf1$qaacf1 <- ave(acf1$qacf1, data.table::rleid(acf1$qacf1 != 0), FUN=cumsum)
    acf1$qaacf1[acf1$qaacf1 == 0] <- NA
    qaacf1.rle <- rle(is.na(acf1$qaacf1))
    if ((qaacf1.rle$values[1] == TRUE)){
      q <- 0
    }else{
      q <- qaacf1.rle$lengths[1] - 1
    }
    paramsDataFrame$q[2] = q
    
    #Identification of p based on pacf1$x 
    pacf1 <- pacf(diff(diff(dataFile$Modal)))# p=3
    pacf1$pacf1 <- abs(pacf1$acf)
    names(pacf1)
    npacf1 <- length(pacf1$pacf1)
    pacf1$x <- 2 * sqrt((1/ pacf1$n.used) * ((pacf1$n.used - pacf1$lag)/(pacf1$n.used + 2)))
    pacf1$ppacf1 <- 0
    
    names(pacf1)
    
    for(i in 1:npacf1)
    {
      if (pacf1$pacf1[i] >= pacf1$x[i])
      {
        (pacf1$ppacf1[i] <- 1)
      }
      else
        (pacf1$ppacf1[i] <- 0)
    }
    
    pacf1$ppacf1 <- ave(pacf1$ppacf1, data.table::rleid(pacf1$ppacf1 != 0), FUN=cumsum)
    pacf1$ppacf1[pacf1$ppacf1 == 0] <- NA
    ppacf1.rle <- rle(is.na(pacf1$ppacf1))
    if ((ppacf1.rle$values[1] == TRUE)){
      p=0
    }else{
      p <- ppacf1.rle$lengths[1]
    }
    paramsDataFrame$p[2] = p
    
    nrow(dataFile$Modal)
    
    Pacf <- pacf(diff(diff(dataFile$Modal)))# change 7 to 6 for all others, p=3, P=2
    
    SeaQacf <- per #to be derived based on days per week
    
    
    #Identification of q and Q from ACF Series diff(diff)
    #Identification of q from ACF Series diff(diff)
    Qacf <- acf(diff(diff(dataFile$Modal)))# change 7 to 6 for all others, q=6, Q= 4
    Qacf$acf1 <- abs(Qacf$acf)
    names(Qacf)
    nQacf <- length(Qacf$acf1)
    Qacf$x <- 2 * sqrt((1/ Qacf$n.used) * ((Qacf$n.used - Qacf$lag)/(Qacf$n.used + 2)))
    Qacf$qacf1 <- 0
    
    names(Qacf)
    
    #Identification q based on StandardError of Qacf1
    
    for(i in 1:nQacf)
    {
      if (Qacf$acf1[i] >= Qacf$x[i])
      {
        (Qacf$qacf1[i] <- 1)
      }
      else
        (Qacf$qacf1[i] <- 0)
    }
    
    Qacf$qaacf1 <- ave(Qacf$qacf1, data.table::rleid(Qacf$qacf1 != 0), FUN=cumsum)
    Qacf$qaacf1[Qacf$qaacf1 == 0] <- NA
    Qqaacf.rle <- rle(is.na(Qacf$qaacf1))
    Qq <- Qqaacf.rle$lengths[1] - 1 #Qq is p value of QACF, minus 1 is to account for 0 lag
    
    if(Qq > (SeaQacf - 1)) {
      Qq = (SeaQacf - 1)
    } else {
      Qq = Qq
    }
    
    Qq
    
    #Identification of Q from ACF Series diff(diff)
    Qacf <- acf(diff(diff(dataFile$Modal)))# change 7 to 6 for all others, q=6, Q= 4
    Qacf$acf1 <- abs(Qacf$acf)
    names(Qacf)
    nQacf <- length(Qacf$acf1)
    Qacf$x <- 2 * sqrt((1/ Qacf$n.used) * ((Qacf$n.used - Qacf$lag)/(Qacf$n.used + 2)))
    Qacf$lag1 <- as.numeric(Qacf$lag)
    Qacf$lag1 <- c(1:(max(Qacf$lag1)+1))
    AQ <- (Qacf$lag1[which(Qacf$lag1%%SeaQacf==0)])
    AQ <- AQ + 1
    AQ
    
    QacfSea <- Qacf[c(AQ),]
    QacfSea$acf <- abs(QacfSea$acf)
    QacfSea$x <- Qacf$x[AQ]
    QacfSea$n <- 0
    Qacf1 <- length(QacfSea$acf)
    
    if (Qacf1 > 0){
      for(i in 1:Qacf1)
      {
        if ((!is.na(QacfSea$acf[i])) & (QacfSea$acf[i] >= QacfSea$x[i]))
        {
          (QacfSea$n[i] <- 1)
        }else{
          (QacfSea$n[i] <- 0)}
      }
      
      QacfSea$Q <- ave(QacfSea$n, data.table::rleid(QacfSea$n != 0), FUN=cumsum)
      QacfSea$n[QacfSea$n == 0] <- NA
      Q.rle <- rle(is.na(QacfSea$n))
      
      if ((Q.rle$values[1] == TRUE)){
        Q <- 0
      }else{
        Q <- Q.rle$lengths[1] - 1}
      
      paramsDataFrame$Q[2] = Q
    }else{
      Q=0
      paramsDataFrame$Q[2] = 0
    }
    
    
    #Identification of p and P from PACF Series diff(diff)
    #Identification of p from PACF Series diff(diff)
    
    Pacf <- pacf(diff(diff(dataFile$Modal)))# change 7 to 6 for all others, p=3, P=2
    Pacf$acf1 <- abs(Pacf$acf)
    names(Pacf)
    nPacf <- length(Pacf$acf1)
    Pacf$x <- 2 * sqrt((1/ Pacf$n.used) * ((Pacf$n.used - Pacf$lag)/(Pacf$n.used + 2)))
    Pacf$pacf1 <- 0
    
    names(Pacf)
    
    #Identification p based on StandardError of Pacf1
    
    for(i in 1:nPacf)
    {
      if (Pacf$acf1[i] >= Pacf$x[i])
      {
        (Pacf$pacf1[i] <- 1)
      }else{
        (Pacf$pacf1[i] <- 0)
      }
    }
    
    Pacf$paacf1 <- ave(Pacf$pacf1, data.table::rleid(Pacf$pacf1 != 0), FUN=cumsum)
    Pacf$paacf1[Pacf$paacf1 == 0] <- NA
    Ppaacf1.rle <- rle(is.na(Pacf$paacf1))
    Pp <- Ppaacf1.rle$lengths[1] #Pp is p value of PACF
    
    if(Pp > (SeaQacf - 1)) {
      Pp = (SeaQacf - 1)
    } else {
      Pp = Pp
    }
    
    Pp
    
    #Identification of P from PACF Series diff(diff)
    
    Pacf$acf1 <- abs(Pacf$acf)
    names(Pacf)
    nPacf <- length(Pacf$acf1)
    Pacf$x <- 2 * sqrt((1/ Pacf$n.used) * ((Pacf$n.used - Pacf$lag)/(Pacf$n.used + 2)))
    Pacf$lag1 <- as.numeric(Pacf$lag)
    Pacf$lag1 <- c(1:(max(Pacf$lag1)))
    AP <- (Pacf$lag1[which(Pacf$lag1%%SeaQacf==0)])
    AP
    
    PacfSea <- Pacf[c(AP),]
    PacfSea$acf <- abs(PacfSea$acf)
    PacfSea$x <- Pacf$x[AP]
    PacfSea$n <- 0
    Pacf1 <- length(PacfSea$acf)
    
    if (Pacf1 > 0){
      for(i in 1:Pacf1)
      {
        if (PacfSea$acf[i] >= PacfSea$x[i])
        {
          (PacfSea$n[i] <- 1)
        }
        else
          (PacfSea$n[i] <- 0)
      }
      
      PacfSea$P <- ave(PacfSea$n, data.table::rleid(PacfSea$n != 0), FUN=cumsum)
      PacfSea$n[PacfSea$n == 0] <- NA
      P.rle <- rle(is.na(PacfSea$n))
      
      if ((P.rle$values[1] == TRUE)){
        P <- 0
      }else{
        P <- P.rle$lengths[1]}
      
      
      paramsDataFrame$P[2] = P
    }else{
      P=0
      paramsDataFrame$P[2] = 0
    }
  
  trainingDataSarimaFor <-  Arima(dataFile$Modal,order = c(paramsDataFrame$p[1],paramsDataFrame$d[1],paramsDataFrame$q[1]),seasonal=list(order=c(paramsDataFrame$P[1],paramsDataFrame$D[1],paramsDataFrame$Q[1]), period=per),method = "CSS", optim.method = "BFGS")
  predictingDataArimaForecast <- forecast(trainingDataSarimaFor,h = nrow(testingData))
  predictingDataFrameArima <- data.frame(predictingDataArimaForecast)
  
  #a1 = accuracy(fitted(trainingDataSarimaFor),dataFile$Modal)
  a1 = accuracy(predictingDataFrameArima$Point.Forecast ,testingData$Modal)
  #print("First Model")
  #print(a1)
  
  trainingDataSarimaFor <-  Arima(dataFile$Modal,order = c(paramsDataFrame$p[2],paramsDataFrame$d[2],paramsDataFrame$q[2]),seasonal=list(order=c(paramsDataFrame$P[2],paramsDataFrame$D[2],paramsDataFrame$Q[2]), period=per),method = "CSS", optim.method = "BFGS")
  predictingDataArimaForecast <- forecast(trainingDataSarimaFor,h = nrow(testingData))
  predictingDataFrameArima <- data.frame(predictingDataArimaForecast)
  
  a2 = accuracy(predictingDataFrameArima$Point.Forecast ,testingData$Modal)
  #a2 = accuracy(fitted(trainingDataSarimaFor),dataFile$Modal)
  #print("Second Model")
  #print(a2)
  #print(paramsDataFrame)
  ##save(list = ls(all.names = TRUE), file = "pdqPDQ.RData")
  # if (a1[5]<a2[5])
  #   return(paramsDataFrame[1,])
  # else
  #   return(paramsDataFrame[2,])
  
  return(paramsDataFrame[2,])
  
  # t <- a1<a2
  # if (length(t[t==TRUE]) > length(t[t==FALSE]))
  #   return(paramsDataFrame[1,])
  # else
  #   return(paramsDataFrame[2,])
  
  }
 
  # #save(list = ls(all.names = TRUE), file = "pdqData.RData")
  # if (mape1<mape2)
  #   return(paramsDataFrame[1,])
  # else
  #   return(paramsDataFrame[2,])
}

updateValueUpto2019Mar <- function(dataFileMI) {
  #Get the last value
  
 
  minDate = min(dataFileMI$Date)
  yMinDate = year(minDate)
  maxDate = max(dataFileMI$Date)
  yMaxDate = year(maxDate)
  
  
  
  for (iy in yMinDate:yMaxDate){
    
    currentYear <- as.numeric(format(Sys.Date(), "%Y"))
    if(iy>(currentYear+1))
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
