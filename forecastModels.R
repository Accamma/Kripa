#setwd("D:\\Anvita\\CropPriceForecasting\\University Space\\Primary\\kapc-dashboard\\ForecastTuning")
setwd("//srv//shiny-server//kapc-dashboard")
bestFitTable <- read.csv("bestFitTableMar15.csv")

forecastLookup<- function(cg, c, v, m, g){
  bf <- bestFitTable[(bestFitTable$CommodityGroup == cg & bestFitTable$Commodity == c & bestFitTable$Market == m & bestFitTable$Variety == v & bestFitTable$Grade == g),]
  if(nrow(bf) >0)
    return(toString(bf$Model))
  else
    return ("Traditional")
}

forecastLookupAggregate<- function(cg, c, v, m, g){
  bf <- bestFitTable[(bestFitTable$CommodityGroup == cg & bestFitTable$Commodity == c & bestFitTable$Market == m & bestFitTable$Variety == v & bestFitTable$Grade == g),]
  if(nrow(bf) >0)
    return(toString(bf$Aggregate))
  else
    return ("No")
}

findMatchingPattern <-  function(trainingData, testingData){
  trSize <- nrow(trainingData)
  tsSize <- nrow(testingData)
  
  startIndex <- 1
  corL <- list()
  for(startIndex in 1:(trSize - tsSize)){
    if (startIndex+tsSize < trSize){
      trainingSubset <- trainingData[startIndex:(startIndex+tsSize-1),]
        ttsTrain <- ts(trainingSubset$Modal, frequency = 12)
        components.ts <- decompose(ttsTrain)
        trainTrend <- components.ts$seasonal
        
        ttsTest <- ts(testingData$Modal, frequency = 12)
        components.ts <- decompose(ttsTest)
        testTrend <- components.ts$seasonal
        
        corL[startIndex] <- cor(trainTrend, testTrend, use = "complete.obs", method = c("pearson"))
      }
  }
  bestPosition <- which.max(corL)
  return (bestPosition)
  }


Jasma1.1.ETSv1 <- function(trainingData, testingData, predictingDataDates){
  noOfPeriodsToForecast <- length(predictingDataDates)
  yy <- ets(trainingData$Modal)
  trainingDataNnetar<- nnetar(trainingData$Modal,decay=0,xreg=yy$residuals)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Jasma1.1.ETSv1"))
  return(output)
}

Jasma1.2.ETSv2 <- function(trainingData, testingData, predictingDataDates){
  noOfPeriodsToForecast <- length(predictingDataDates)
  etsFit <- ets(trainingData$Modal,lambda = "auto",opt.crit = "mae", ic = "aic", bounds = "both",biasadj = TRUE)
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=etsFit$residuals,lambda = "auto",biasadj = TRUE)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg,lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[((nrow(predictingDataFrameNnetarForecast) - noOfPeriodsToForecast+1): nrow(predictingDataFrameNnetarForecast)),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Jasma1.2.ETSv2"))
  return(output)
}

Jasma1.3.ETSv3 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  etsFit <- ets(trainingData$Modal,lambda = "auto",opt.crit = "amse", ic = "bic", bounds = "both",biasadj = TRUE, model = "ANN",nmse = 10)
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=scale(etsFit$residuals),lambda = "auto",biasadj = TRUE)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=scale(trainingDataNnetar$xreg),lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  print("Inside the function:")
  print(predictingDataFrameNnetarForecast$Point.Forecast)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Jasma1.3.ETSv3"))
  return(output)
}

Jasma1.4.ETSv4 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  etsFit <- ets(trainingData$Modal,lambda = "auto",opt.crit = "mae", ic = "bic", bounds = "usual",biasadj = TRUE, model = "ZZZ", nmse = 3)
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=etsFit$residuals,lambda = "auto",biasadj = TRUE)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, xreg=trainingDataNnetar$xreg,lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Jasma1.4.ETSv4"))
  return(output)
}

Jasma2.1BATSv1 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  
  trainingTS <- ts(trainingData$Modal)
  batsFit <- bats(trainingData$Modal, use.box.cox = NULL, use.trend = NULL, use.damped.trend = NULL, use.arma.errors = TRUE, use.parallel = TRUE, num.cores = NULL, biasadj = FALSE)
  #trainingDataNnetar <- elm.fast(trainingTS)
  #trainingDataNnetar <- elm.thief(trainingTS, h = noOfPeriodsToForecast)
  #predictingDataNnetarForecast <- forecast(trainingDataNnetar, h = noOfPeriodsToForecast, xreg=trainingDataNnetar$xreg, xreg.lags=NULL)
  
 
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=batsFit$errors)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  #bestPosition <- findMatchingPattern(trainingData, testingData)
  #predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(bestPosition:(bestPosition + noOfPeriodsToForecast-1)),])
  
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Jasma2.1BATSv1"))
  return(output)
}

Jasma3.1.ARFIMAv1 <- function(trainingData, testingData, predictingDataDates)
{
  print("Inside ARFIMA")
  noOfPeriodsToForecast <- length(predictingDataDates)
  arfimaFit <- arfima(trainingData$Modal, estim = "mle", lambda = "auto", drange = 0, biasadj = TRUE)
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=arfimaFit$residuals,lambda = "auto", scale.inputs = TRUE)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h=noOfPeriodsToForecast, robust = FALSE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg, lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Jasma3.1.ARFIMAv1"))
  return(output)
}


Jasma5.1.BaggedModelv1 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedModelFit <- baggedModel(trainingData$Modal)
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedModelFit$residuals,lambda = "auto", scale.inputs = TRUE)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h=noOfPeriodsToForecast, robust = FALSE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg, lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Jasma5.1.BaggedModelv1"))
  return(output)
}

Jasma6.1.BaggedETSv1 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedETSFit <- baggedETS(trainingData$Modal)
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedETSFit$residuals,lambda = "auto", scale.inputs = TRUE)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h=noOfPeriodsToForecast, robust = FALSE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg, lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Jasma6.1.BaggedETSv1"))
  return(output)
}
##########################################################################################################################################################

Accamma2.1.ETSv1 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  etsTrainFit <- ets(trainingData$Modal)
  etsTestFit <- ets(testingData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=etsTrainFit$residuals)
  nnetarTestFit <- nnetar(testingData$Modal,decay=0,xreg=etsTestFit$residuals)
  
  predictingDataNnetarForecast <- forecast(nnetarTestFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Accamma2.1.ETSv1"))
  return(output)
}

Accamma2.2.ETSv2 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  etsTrainFit <- ets(trainingData$Modal,lambda = "auto",opt.crit = "amse", ic = "bic", bounds = "both",biasadj = TRUE, model = "ANN",nmse = 10)
  etsTestFit <- ets(testingData$Modal,lambda = "auto",opt.crit = "amse", ic = "bic", bounds = "both",biasadj = TRUE, model = "ANN",nmse = 10)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=scale(etsTrainFit$residuals),lambda = "auto",biasadj = TRUE)
  nnetarTestFit <- nnetar(testingData$Modal,decay=0,xreg=scale(etsTestFit$residuals),lambda = "auto",biasadj = TRUE)
  
  predictingDataNnetarForecast <- forecast(nnetarTestFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=scale(trainingDataNnetar$xreg),lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Accamma2.2.ETSv2"))
  return(output)
}

Accamma3.1.BaggedModelv1 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedModelTrainFit <- baggedModel(trainingData$Modal)
  baggedModelTestFit <- baggedModel(testingData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedModelTrainFit$residuals,lambda = "auto", scale.inputs = TRUE)
  nnetarTestFit <- nnetar(testingData$Modal,decay=0,xreg=baggedModelTestFit$residuals,lambda = "auto", scale.inputs = TRUE)
  
  predictingDataNnetarForecast <- forecast(nnetarTestFit,h=noOfPeriodsToForecast, robust = FALSE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg, lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Accamma3.1.BaggedModelv1"))
  return(output)
}

Accamma4.1.BaggedETSv1 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedETSTrainFit <- baggedETS(trainingData$Modal)
  baggedETSTestFit <- baggedETS(testingData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedETSTrainFit$residuals,lambda = "auto", scale.inputs = TRUE)
  nnetarTestFit <- nnetar(testingData$Modal,decay=0,xreg=baggedETSTestFit$residuals,lambda = "auto", scale.inputs = TRUE)
  
  predictingDataNnetarForecast <- forecast(nnetarTestFit,h=noOfPeriodsToForecast, robust = FALSE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg, lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Accamma4.1.BaggedETSv1"))
  return(output)
}

Accamma5.1.StructTSv1 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  stsTrainFit <- StructTS(trainingData$Modal)
  stsTestFit <- StructTS(testingData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=stsTrainFit$residuals)
  nnetarTestFit <- nnetar(testingData$Modal,decay=0,xreg=stsTestFit$residuals)
  
  predictingDataNnetarForecast <- forecast(nnetarTestFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Accamma5.1.StructTSv1"))
  return(output)
}
##########################################################################################################################################################
vhbs1.BaggedETS.BaggedModelv1 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedETSTrainFit <- baggedETS(trainingData$Modal)
  baggedModelTestFit <- baggedModel(testingData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedETSTrainFit$residuals)
  nnetarTestFit <- nnetar(testingData$Modal,decay=0,xreg=baggedModelTestFit$residuals)
  
  predictingDataNnetarForecast <- forecast(nnetarTestFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("vhbs1.BaggedETS.BaggedModelv1"))
  return(output)
}

vhbs1.BaggedETS.BaggedETSv2 <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedModelTrainFit <- baggedModel(trainingData$Modal)
  baggedETSTestFit <- baggedETS(testingData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedModelTrainFit$residuals)
  nnetarTestFit <- nnetar(testingData$Modal,decay=0,xreg=baggedETSTestFit$residuals)
  
  predictingDataNnetarForecast <- forecast(nnetarTestFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("vhbs1.BaggedETS.BaggedETSv2"))
  return(output)
}

Traditional <- function(trainingData, testingData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  trainingDataNnetar <- nnetar(trainingData$Modal,  MaxNWts = 10000)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h = noOfPeriodsToForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Basic NN"))
  return(output)
}

Ashok1.1BATSv1 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  noOfValidationPoints <- nrow(validationData)
  
  trainingTS <- ts(trainingData$Modal)
  validationTS <- ts(validationData$Modal)
  #trainingBatsFit <- nnfor::elm(trainingTS)
  #validationBatsFit <- nnfor::elm(validationTS)
  
  trainingBatsFit <- bats(trainingData$Modal, use.box.cox = NULL, use.trend = NULL, use.damped.trend = NULL, use.arma.errors = TRUE, use.parallel = TRUE, num.cores = NULL, biasadj = FALSE)
  validationBatsFit <- bats(validationData$Modal, use.box.cox = NULL, use.trend = NULL, use.damped.trend = NULL, use.arma.errors = TRUE, use.parallel = TRUE, num.cores = NULL, biasadj = FALSE)
  #trainingDataNnetar <- elm.fast(trainingTS)
  #trainingDataNnetar <- elm.thief(trainingTS, h = noOfPeriodsToForecast)
  #predictingDataNnetarForecast <- forecast(trainingDataNnetar, h = noOfPeriodsToForecast, xreg=trainingDataNnetar$xreg, xreg.lags=NULL)
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=trainingBatsFit$residuals,lambda = "auto", scale.inputs = TRUE)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=validationBatsFit$residuals,lambda = "auto", scale.inputs = TRUE)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = FALSE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg, lambda = "auto")
 
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=trainingBatsFit$errors)
  predictingDataNnetarForecast <- forecast(trainingDataNnetar,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  #bestPosition <- findMatchingPattern(trainingData, testingData)
  #predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(bestPosition:(bestPosition + noOfPeriodsToForecast-1)),])
  
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok1.1BATSv1"))
  return(output)
}

Ashok2.1.ETSv1 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  etsTrainFit <- ets(trainingData$Modal)
  etsValidationFit <- ets(validationData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=etsTrainFit$residuals)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=etsValidationFit$residuals)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,validationData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok2.1.ETSv1"))
  return(output)
}

Ashok2.2.ETSv2 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  etsTrainFit <- ets(trainingData$Modal,lambda = "auto",opt.crit = "amse", ic = "bic", bounds = "both",biasadj = TRUE, model = "ANN",nmse = 10)
  etsValidationFit <- ets(validationData$Modal,lambda = "auto",opt.crit = "amse", ic = "bic", bounds = "both",biasadj = TRUE, model = "ANN",nmse = 10)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=scale(etsTrainFit$residuals),lambda = "auto",biasadj = TRUE)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=scale(etsValidationFit$residuals),lambda = "auto",biasadj = TRUE)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=scale(trainingDataNnetar$xreg),lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok2.2.ETSv2"))
  return(output)
}

Ashok1.4ETSv4 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  etsTrainFit <- ets(trainingData$Modal,lambda = "auto",opt.crit = "mae", ic = "bic", bounds = "usual",biasadj = TRUE, model = "ZZZ", nmse = 3)
  etsValidationFit <- ets(validationData$Modal,lambda = "auto",opt.crit = "mae", ic = "bic", bounds = "usual",biasadj = TRUE, model = "ZZZ", nmse = 3)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=scale(etsTrainFit$residuals),lambda = "auto",biasadj = TRUE)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=scale(etsValidationFit$residuals),lambda = "auto",biasadj = TRUE)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=scale(trainingDataNnetar$xreg),lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok1.4ETSv4"))
  return(output)
}

Ashok3.1ARFIMAV1 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  arfimaTrainFit <- arfima(trainingData$Modal, estim = "mle", lambda = "auto", drange = 0, biasadj = TRUE)
  arfimaValidationFit <- arfima(validationData$Modal, estim = "mle", lambda = "auto", drange = 0, biasadj = TRUE)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=scale(arfimaTrainFit$residuals),lambda = "auto",biasadj = TRUE)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=scale(arfimaValidationFit$residuals),lambda = "auto",biasadj = TRUE)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=scale(trainingDataNnetar$xreg),lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok3.1ARFIMAV1"))
  return(output)
}

Ashok3.1.BaggedModelv1 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedModelTrainFit <- baggedModel(trainingData$Modal)
  baggedModelValidationFit <- baggedModel(validationData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedModelTrainFit$residuals,lambda = "auto", scale.inputs = TRUE)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=baggedModelValidationFit$residuals,lambda = "auto", scale.inputs = TRUE)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = FALSE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg, lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,validationData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok3.1.BaggedModelv1"))
  return(output)
}

Ashok4.1.BaggedETSv1 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedETSTrainFit <- baggedETS(trainingData$Modal)
  baggedETSValidationFit <- baggedETS(validationData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedETSTrainFit$residuals,lambda = "auto", scale.inputs = TRUE)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=baggedETSValidationFit$residuals,lambda = "auto", scale.inputs = TRUE)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = FALSE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg, lambda = "auto")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok4.1.BaggedETSv1"))
  return(output)
}

Ashok5.1.StructTSv1 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  stsTrainFit <- StructTS(trainingData$Modal)
  stsValidationFit <- StructTS(validationData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=stsTrainFit$residuals)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=stsValidationFit$residuals)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
 # accuracy(trainingDataNnetar$fitted,trainingData$Modal)
#  accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok5.1.StructTSv1"))
  return(output)
}
##########################################################################################################################################################
Ashok.BaggedETS.BaggedModelv1 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedETSTrainFit <- baggedETS(trainingData$Modal)
  baggedModelValidationFit <- baggedModel(validationData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedETSTrainFit$residuals)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=baggedModelValidationFit$residuals)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok.BaggedETS.BaggedModelv1"))
  return(output)
}

Ashok.BaggedETS.BaggedETSv2 <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  baggedModelTrainFit <- baggedModel(trainingData$Modal)
  baggedETSValidationFit <- baggedETS(validationData$Modal)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=baggedModelTrainFit$residuals)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=baggedETSValidationFit$residuals)
  print(nrow(trainingData))
  print(nrow(testingData))
  print(nrow(validationData))
  print(length(predictingDataDates))
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  print(predictingDataFrameNnetarForecast$Point.Forecast)
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok.BaggedETS.BaggedETSv2"))
  return(output)
}

Ashok.NNFor <- function(trainingData, testingData, validationData, predictingDataDates)
{
  noOfPeriodsToForecast <- length(predictingDataDates)
  modalts <- ts(trainingData$Modal)
  NNForTrainFit <- mlp(modalts)
  modalts <- ts(validationData$Modal)
  NNForValidationFit <- mlp(modalts)
  
  trainingDataNnetar <- nnetar(trainingData$Modal,decay=0,xreg=NNForTrainFit$residuals)
  nnetarValidationFit <- nnetar(validationData$Modal,decay=0,xreg=NNForValidationFit$residuals)
  
  predictingDataNnetarForecast <- forecast(nnetarValidationFit,h=noOfPeriodsToForecast, robust = TRUE, biasadj = TRUE, bootstrap = TRUE,xreg=trainingDataNnetar$xreg)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataNnetarForecast)
  predictingDataFrameNnetarForecast <- data.frame(predictingDataFrameNnetarForecast[(1:noOfPeriodsToForecast),])
  colnames(predictingDataFrameNnetarForecast) <- c("Point.Forecast")
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictingDataFrameNnetarForecast$Point.Forecast)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates", "Point.Forecast")
  #accuracy(trainingDataNnetar$fitted,trainingData$Modal)
  #accuracy(predictingDataFrameNnetarForecast$Point.Forecast,testingData$Modal)
  output <- list(trainingDataNnetar,predictingDataFrameNnetarForecast, paste("Ashok.NNFor"))
  return(output)
}

AnvitaRandomForest <- function(trainingData, testingData, validationData, predictingDataDates){
  
  blockSize=300
  ioPairTrain <- createRollingWindow(trainingData, blockSize)
  colnames(ioPairTrain)<-c(paste0("V",1:blockSize), "output")
  f <- reformulate(setdiff(colnames(ioPairTrain), "output"), response="output")
  linearMod <- randomForest(f,ioPairTrain, hidden=30, algorithm = '', threshold=0.01)
  
  #Create a last rollingWindow for predicting the future
  temp <- trainingData[((nrow(trainingData)-blockSize+1):nrow(trainingData)),]$Modal
  temp <- data.frame(t(temp))
  colnames(temp) <- c(paste0("V",1:blockSize))
  predictedValues <- vector()
  
  for (i in 1:length(predictingDataDates)){
    newVal <- predict(linearMod,temp)
    predictedValues[i]<- newVal
    temp[,paste0("V",(1:(blockSize-1)))] <- temp[,paste0("V",(2:blockSize))]
    temp[,paste0("V",blockSize)]<- newVal
  }
  t<- t(as.data.frame(predictedValues))
  accuracy(testingData$Modal, predictedValues[1:length(testingData$Modal)])
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictedValues)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates","Point.Forecast")
  output <- list(linearMod, predictingDataFrameNnetarForecast, paste("AnvitaRegression"))
}

AnvitaSVM <- function(trainingData, testingData, validationData, predictingDataDates){
  
  blockSize=30
  ioPairTrain <- createRollingWindow(trainingData, blockSize)
  linearMod <- svm(output~.,data = ioPairTrain, kernel="radial")
  
  # perform a grid search
  tuneResult <- tune(svm, output~.,  data = ioPairTrain,
                     ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
  )
  
  print(tuneResult)
  # Draw the tuning graph
  plot(tuneResult)
  
  tunedModel <- tuneResult$best.model
  
  
  #Create a last rollingWindow for predicting the future
  temp <- trainingData[((nrow(trainingData)-blockSize+1):nrow(trainingData)),]$Modal
  temp <- data.frame(t(temp))
  colnames(temp) <- c(1:blockSize)
  predictedValues <- vector()
  
  for (i in 1:length(predictingDataDates)){
    newVal <- predict(tunedModel,temp)
    predictedValues[i]<- newVal
    temp[,(1:(blockSize-1))] <- temp[,(2:blockSize)]
    temp[,blockSize]<- newVal
  }
  t<- t(as.data.frame(predictedValues))
  accuracy(testingData$Modal, predictedValues[1:length(testingData$Modal)])
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictedValues)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates","Point.Forecast")
  output <- list(linearMod, predictingDataFrameNnetarForecast, paste("AnvitaRegression"))
}

AnvitaDecisionTree <- function(trainingData, testingData, validationData, predictingDataDates){
  
  blockSize=300
  ioPairTrain <- createRollingWindow(trainingData, blockSize)
  linearMod <- rpart(output~.,data = ioPairTrain)
  
  #Create a last rollingWindow for predicting the future
  temp <- trainingData[((nrow(trainingData)-blockSize+1):nrow(trainingData)),]$Modal
  temp <- data.frame(t(temp))
  colnames(temp) <- c(1:blockSize)
  predictedValues <- vector()
  
  for (i in 1:length(predictingDataDates)){
    newVal <- predict(linearMod,temp)
    predictedValues[i]<- newVal
    temp[,(1:(blockSize-1))] <- temp[,(2:blockSize)]
    temp[,blockSize]<- newVal
  }
  t<- t(as.data.frame(predictedValues))
  accuracy(testingData$Modal, predictedValues[1:length(testingData$Modal)])
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictedValues)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates","Point.Forecast")
  output <- list(linearMod, predictingDataFrameNnetarForecast, paste("AnvitaRegression"))
}

AnvitaLogisticRegression <- function(trainingData, testingData, validationData, predictingDataDates){
  
  blockSize=300
  ioPairTrain <- createRollingWindow(trainingData, blockSize)
  linearMod <- glm(output~.,data = ioPairTrain)
  
  #Create a last rollingWindow for predicting the future
  temp <- trainingData[((nrow(trainingData)-blockSize+1):nrow(trainingData)),]$Modal
  temp <- data.frame(t(temp))
  colnames(temp) <- c(1:blockSize)
  predictedValues <- vector()
  
  for (i in 1:length(predictingDataDates)){
    newVal <- predict(linearMod,temp)
    predictedValues[i]<- newVal
    temp[,(1:(blockSize-1))] <- temp[,(2:blockSize)]
    temp[,blockSize]<- newVal
  }
  t<- t(as.data.frame(predictedValues))
  accuracy(testingData$Modal, predictedValues[1:length(testingData$Modal)])
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictedValues)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates","Point.Forecast")
  output <- list(linearMod, predictingDataFrameNnetarForecast, paste("AnvitaRegression"))
}

AnvitaRegression <- function(trainingData, testingData, validationData, predictingDataDates){
  
  blockSize=300
  ioPairTrain <- createRollingWindow(trainingData, blockSize)
  linearMod <- lm(output~.,data = ioPairTrain)
  
  #Create a last rollingWindow for predicting the future
  temp <- trainingData[((nrow(trainingData)-blockSize+1):nrow(trainingData)),]$Modal
  temp <- data.frame(t(temp))
  colnames(temp) <- c(1:blockSize)
  predictedValues <- vector()
  
  for (i in 1:length(predictingDataDates)){
    newVal <- predict(linearMod,temp)
    predictedValues[i]<- newVal
    temp[,(1:(blockSize-1))] <- temp[,(2:blockSize)]
    temp[,blockSize]<- newVal
  }
  t<- t(as.data.frame(predictedValues))
  accuracy(testingData$Modal, predictedValues[1:length(testingData$Modal)])
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictedValues)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates","Point.Forecast")
  output <- list(linearMod, predictingDataFrameNnetarForecast, paste("AnvitaRegression"))
}

AnvitaRNN <- function(trainingData, testingData, validationData, predictingDataDates){
  
  blockSize=100
  ioPairTrain <- createRollingWindow(trainingData, blockSize)
  
  model <- keras_model_sequential() 
  
  
  # Add layers to the model
  model %>% 
    layer_dense(units = 50, activation = 'sigmoid', input_shape = c(100)) %>% 
    layer_dense(units = 20, activation = 'tanh') %>% 
    layer_dense(units = 1, activation = 'relu')
  
  model %>% compile(
    loss = 'mae',
    metrics = 'accuracy',
    optimizer = optimizer_adadelta()
  )
  
  ioPairTrainM <- as.matrix(ioPairTrain)
  history<- model %>% fit(
    ioPairTrainM[,(1:100)], 
    ioPairTrainM[,101], 
    epochs = 200, 
    batch_size = 30, 
    validation_split = 0.2
  )
  # plot(history)
  # 
  # # Plot the accuracy of the training data 
  # plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")
  # 
  # # Plot the accuracy of the validation data
  # lines(history$metrics$val_acc, col="green")
  # 
  # # Add Legend
  # legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))
  # 
  # # Evaluate on test data and labels
  # score <- model %>% evaluate(iris.test, iris.testLabels, batch_size = 128)
  # 
  # # Print the score
  # print(score)
  # 
  
  #Create a last rollingWindow for predicting the future
  temp <- trainingData[((nrow(trainingData)-blockSize+1):nrow(trainingData)),]$Modal
  temp <- data.frame(t(temp))
  colnames(temp) <- c(1:blockSize)
  predictedValues <- vector()
  temp<- as.matrix(temp)
  for (i in 1:length(predictingDataDates)){
    
    score <- model %>% evaluate(temp, testingData$Modal[i], batch_size = 100)
    newVal <- model %>% predict(temp, batch_size = 100)
    
    # Print the score
    print(score)
    
    predictedValues[i]<- newVal
    temp[,(1:(blockSize-1))] <- temp[,(2:blockSize)]
    temp[,blockSize]<- newVal
  }
  t<- t(as.data.frame(predictedValues))
  accuracy(testingData$Modal, predictedValues[1:length(testingData$Modal)])
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictedValues)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates","Point.Forecast")
  output <- list(model, predictingDataFrameNnetarForecast, paste("AnvitaRNN"))
}

AnvitaLSTM <- function(trainingData, testingData, validationData, predictingDataDates){
  
  blockSize=100
  ioPairTrain <- createRollingWindow(trainingData, blockSize)
  ioPairTrainM <- as.matrix(ioPairTrain)
  dim(ioPairTrainM[,(1:100)])
  x_train <- ioPairTrainM[,(1:100)]
  dim(x_train) <- c(nrow(x_train), 100, 1)
  X_shape2 = dim(x_train)[2]
  X_shape3 = dim(x_train)[3]
  
  y_train <- ioPairTrainM[,101]
  y_train <- array(data = y_train, dim = c(length(y_train), 1))
  
  # must be a common factor of both the train and test samples
  units = 1  
  
  model <- keras_model_sequential() 
  
  #Find a batch_size divisible by nrows(trainingData)
  batch_size=1
  no_of_rows = nrow(x_train)
  for (i in 4:no_of_rows){
    if ((floor(no_of_rows/i)*i)==no_of_rows){
      batch_size = i
      break;}
  }
  
  model %>%
    layer_lstm(units = 50,input_shape = c(X_shape2, X_shape3), return_sequences = TRUE, stateful = TRUE, batch_size = batch_size)%>%
    layer_lstm(units            = 5, 
               return_sequences = FALSE, 
               stateful         = TRUE) %>% 
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mae',
    metrics = 'accuracy',
    optimizer = optimizer_adam()
  )
  
  ioPairTrainM <- as.matrix(ioPairTrain)
  history<- model %>% fit(
    x_train, 
    y_train, 
    epochs = 200, 
    batch_size = batch_size
  )
  new_weights <- get_weights(model)
  new_model <- keras_model_sequential() 
  new_model %>%
    layer_lstm(units = 50,input_shape = c(X_shape2, X_shape3), return_sequences = TRUE, stateful = TRUE, batch_size = 1)%>%
    layer_lstm(units            = 5, 
               return_sequences = FALSE, 
               stateful         = TRUE) %>% 
    layer_dense(units = 1)
  new_model %>% compile(
    loss = 'mae',
    metrics = 'accuracy',
    optimizer = optimizer_adam()
  )
  set_weights(new_model, new_weights)
  
  
  #Create a last rollingWindow for predicting the future
  temp <- trainingData[((nrow(trainingData)-blockSize+1):nrow(trainingData)),]$Modal
  temp <- data.frame(t(temp))
  colnames(temp) <- c(1:blockSize)
  predictedValues <- vector()
  temp<- as.matrix(temp)
  dim(temp) <- c(nrow(temp), 100, 1)
  for (i in 1:length(predictingDataDates)){
    
    score <- new_model %>% evaluate(temp, testingData$Modal[i], batch_size = 1)
    newVal <- new_model %>% predict(temp, batch_size = 1)
    
    # Print the score
    print(score)
    
    predictedValues[i]<- newVal
    temp[,(1:(blockSize-1)),] <- temp[,(2:blockSize),]
    temp[,blockSize,]<- newVal
  }
  t<- t(as.data.frame(predictedValues))
  accuracy(testingData$Modal, predictedValues[1:length(testingData$Modal)])
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictedValues)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates","Point.Forecast")
  output <- list(new_model, predictingDataFrameNnetarForecast, paste("AnvitaLSTM"))
}

AnvitaNN <- function(trainingData, testingData, validationData, predictingDataDates){
  
  blockSize=300
  ioPairTrain <- createRollingWindow(trainingData, blockSize)
  colnames(ioPairTrain)<-c(paste0("V",1:blockSize), "output")
  f <- reformulate(setdiff(colnames(ioPairTrain), "output"), response="output")
  linearMod <- neuralnet(f,ioPairTrain, hidden=30, algorithm = '', threshold=0.01)
  
  #Create a last rollingWindow for predicting the future
  temp <- trainingData[((nrow(trainingData)-blockSize+1):nrow(trainingData)),]$Modal
  temp <- data.frame(t(temp))
  colnames(temp) <- c(paste0("V",1:blockSize))
  predictedValues <- vector()
  
  for (i in 1:length(predictingDataDates)){
    newVal <- compute(linearMod,temp)
    predictedValues[i]<- newVal$net.result[1]
    temp[,paste0("V",(1:(blockSize-1)))] <- temp[,paste0("V",(2:blockSize))]
    temp[,paste0("V",blockSize)]<- newVal$net.result[1]
  }
  t<- t(as.data.frame(predictedValues))
  accuracy(testingData$Modal, predictedValues[1:length(testingData$Modal)])
  predictingDataFrameNnetarForecast <- data.frame(predictingDataDates,predictedValues)
  colnames(predictingDataFrameNnetarForecast) <- c("predictingDataDates","Point.Forecast")
  output <- list(linearMod, predictingDataFrameNnetarForecast, paste("AnvitaRegression"))
}

createRollingWindow <- function(data, blockSize){
  
  rows <- (nrow(data) - (blockSize+1))
  finalOutput <- list()
  d<-data.frame()
  
  for (i in 1:rows){
    finalOutput[[i]] <- list(input = as.list(data[(i:(i+blockSize-1)),]), output = as.list(data[(i+blockSize),]))
    t<-cbind(t(as.data.frame(finalOutput[[i]]$input$Modal)),as.data.frame(finalOutput[[i]]$output$Modal))
    colnames(t) <- c(1:blockSize,"output")
    d<- rbind(d,t)
  }
  
  return(d)
}