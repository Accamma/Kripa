# Karnataka Agricultural Pricing Commission 
#Designed & developed by ANVITA in collaboration witk Prof. Lalith, Reshma, & Veda Murthy
# Data Models By Prof. Lalith, KVAFSU
#Training Data : 1JAN13-12APR17
#Testing Data: 26MAR17 Onwards
#Data Models - ANN & ARIMA

# Functionality:
#     1. Dynamic Interactive Plots
#     2. Model Accuracy
#     3. Highest Traded Vareity
#     4. Highest Traded Market
#     5. Arrivals Vs. Modal
#     6. Mouse Over Feature
#     7. Different Windows
#.....8. Seasonality Index
#.....9. PQD Input
#.....10. Neural Network Price Forecast
#.....11. MSP , Cost of Production & Cost A1+FL
#.....12. Historic Market Profile
#.....13. Commodity Profile
#.....14. No. of Historic Entries
#.....15. Window Partitioning
#.....16. Technical Analysis

gc()
makeActiveBinding("refresh", function() { system(paste0(R.home(),"/bin/i386/R")); q("no") }, .GlobalEnv)
paste0(R.home(),"/bin/i386/R --no-#save") #--#save will #save workspace
rm(list = ls())

source('load.R')
source('forecastModels.R')


dbHeader <- dashboardHeader(title="KRIPA",
tags$li(class = "dropdown",
        tags$li(class = "dropdown", actionLink(inputId='logout', label="Logout",onclick ="location.href='https://krishi-vishleshana-phalaka.auth0.com/v2/logout?returnTo=http%3A%2F%2Fkapricom.org';"))))

#dbHeader$children[[3]]$children[[3]] = actionButton(inputId='logout', label="Logout", icon = icon("th"), align = "right", 
                                                   # style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
 #                                                  onclick ="location.href='https://krishi-vishleshana-phalaka.auth0.com/v2/logout?returnTo=http%3A%2F%2Fkapricom.org';")

loggit("INFO", "app has started", app = "start")

ui <- dashboardPage(title="KRIPA",
  dbHeader,
  dashboardSidebar(
       p("Data available from", align = "center"),
       h5(textOutput("dynamicHistoricDate1"), align = "center"),
        uiOutput(("inputCommodityGroup")),
        uiOutput("inputCommodity"),
       
       conditionalPanel(
         condition = "input.tabs1 == 'Data' || input.tabs1 == 'Pattern'
         ||  input.tabs1 == 'Pattern' || input.tabs1 == 'Commerce'
         ||  input.tabs1 == 'Correlation' || input.tabs1 == 'ARIMA'
         ||  input.tabs1 == 'Forecast' ||  input.tabs1 == 'GARCH' ||  input.tabs1 == 'Analytics'",
         uiOutput("inputMarket")),

        conditionalPanel(
          condition = "input.tabs1 == 'Data' || input.tabs1 == 'Pattern'
          ||  input.tabs1 == 'Pattern' || input.tabs1 == 'Commerce'
          ||  input.tabs1 == 'Correlation' || input.tabs1 == 'ARIMA'
          ||  input.tabs1 == 'Forecast' ||  input.tabs1 == 'GARCH'||  input.tabs1 == 'Analytics'",
           uiOutput("inputVariety")),

        conditionalPanel(
          condition = "input.tabs1 == 'Correlation' || input.tabs1 == 'ARIMA'
          ||  input.tabs1 == 'Forecast' ||  input.tabs1 == 'GARCH' ||  input.tabs1 == 'Analytics'",
          uiOutput("inputGrade")),
        
       conditionalPanel(
         condition = "input.tabs1 == 'Analytics'",
         uiOutput("inputYear")),
       
       conditionalPanel(
         condition = "input.tabs1 == 'Profile' || input.tabs1 == 'Data' 
         ||  input.tabs1 == 'Pattern' || input.tabs1 == 'Commerce'",
         dateRangeInput('MSPDateRange', label = 'Reference Date Range', start = referenceDateRangeStartDate, end = referenceDateRangeEndDate, format="dd-mm-yyyy", min=minDate, max=maxDate)),
        
        conditionalPanel(
          condition = "input.tabs1 == 'Correlation' || input.tabs1 == 'ARIMA' ||  input.tabs1 == 'GARCH'
          ||  input.tabs1 == 'Forecast'",
          dateRangeInput('HistDateRange', label = 'Forecast Training Date Range', start = trainingDurationStartDate, end = trainingDurationEndDate, format = "dd-mm-yyyy", min=minDate, max=maxDate ),
          dateRangeInput('dateRange', label = 'Forecast Date Range', start = forecastDateRangeStartDate, end = forecastDateRangeEndDate, format="dd-mm-yyyy")),
        
        conditionalPanel(
          condition = "input.tabs1 == 'Correlation' || input.tabs1 == 'ARIMA'",
          textInput("sarimaForp","Number of Regular Auto-Regressive Terms (p)","5"),
          textInput("sarimaFord","Number of Non-Seasonal Differencing (d)", "1"),
          textInput("sarimaForq","Number of Moving Average Terms (q)", "1"),
          textInput("sarimaForP","Number of Seasonal Auto-Regressive Terms (P)","2"),
          textInput("sarimaForD","Number of Seasonal Differencing (D)", "1"),
          textInput("sarimaForQ","Number of Seasonal Moving Average Terms (Q)", "4"),
          textInput("arimaPeriod","Number of days in a Week (Period)", "6"))
      ),
  dashboardBody(
    useShinyjs(),
    useShinyalert(),
    tags$script(HTML('$(document).ready(function() {
    $("header").find("nav").append(\'<div class="myClass"> Krishi Dharane Vishleshana Phalaka </div>\');})')),
    tags$head(
      # Include our custom CSS
      includeCSS("www/style.css")
    ),
    HTML('<head>
         <script type="text/javascript" src="https://cdn.ywxi.net/js/1.js" async></script>
         <script type="text/javascript" src="https://cdn.trustedsite.com/js/1.js" async></script>
         </head>'),
    tags$script('
                $(function() {
                var time_now = new Date()
                $("input#client_time").val(time_now.getTime())
                $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())
                });    
                '),
    tags$style(HTML('#insertBtn{background-color:#99ccff}')),
    tags$style(type = "text/css",".shiny-output-error{visibility:visible;margin:auto;color:red;}"),
    #                     ".shiny-output-error:before {visibility: visible;margin:auto;color:red; }"),
    #div(style="float:right;height:55px",uiOutput("anvitaText")),
              #actionButton(inputId='logout', label="Logout", icon = icon("th"), align = "right",onclick ="location.href='https://krishi-vishleshana-phalaka.auth0.com/v2/logout?returnTo=http%3A%2F%2Fkapricom.org';"),
    #h3("KRIPA is undergoing maintenance. We apologize for any inconvenience. It will be available again soon.", style="color:#F00" ,align = "center"),
    fluidRow(
      column(width=3, img(src='karnataka.png', align = "right", height = 80,style="padding:3px;" )),
      column(width=6,
             h4("KARNATAKA AGRICULTURAL PRICE COMMISSION", align = "center"),
             h5("State Department of Agriculture, Government of Karnataka", align = "center"),
             h5("Farm Produce Prices Analysis Dashboard (KRIPA) for Timely Market Intervention", align = "center")),
      column(width=2, img(src='KSDA.png', height = 70, align="center")),
      column(width=1,uiOutput("anvitaText"))
    ),
    fluidRow(
      column(width=3, img(src='KVAFSU.png', height = 70, align = "left")),
      column(width = 6,br(),br(),textOutput("dynamicHistoricDate"), align="center"),
      column(width=3, img(src='ANVITAO.png', height = 50, align = "right"))
    ),
    br(),
            
    bootstrapPage(
      # Add custom CSS & Javascript;
      tagList(
        tags$head(
          tags$link(rel="stylesheet", type="text/css",href="style.css"),
          tags$script(type="text/javascript", src = "busy.js")
        ),
        tags$head(tags$style(".modal-body {padding: 10px}
                     .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
                             .modal-header {text-align: center; background-color:#8bbfef; border-top-left-radius: 6px; border-top-right-radius: 6px}
                             .modal { text-align: left; padding-right:10px; padding-top: 24px;}
                             .close { font-size: 16px}"))
      ),
      div(class = "busy",  
          p("Loading.."), 
          img(src="Blocks.gif")
      ),
              tabsetPanel(id ="tabs1",
                          tabPanel("Profile",htmlOutput("helpCommodityProfileHTML"),textOutput("SarimaForTrainerViewHeading"),textOutput("HistoricDataSubsetHeading"),textOutput("SarimaForVarietyArrivalHeading") ,br(),plotlyOutput("SarimaForPlotV"),br(),br(),br(),textOutput("SarimaForTrainerViewHeading1"),textOutput("dynamicRange60"),textOutput("SarimaForMarketArrivalHeading"), br(), dataTableOutput("SarimaForTableM"),textOutput("SarimaForTrainerViewHeading2"),textOutput("dynamicRange602"),textOutput("SarimaForGradeArrivalHeading"), br(), dataTableOutput("SarimaForTableG"),br(),uiOutput("anvitaEmail2")),
                          tabPanel("Data",htmlOutput("helpDataTableHTML"),textOutput("dynamicText"),textOutput("HistoricDateRange"),textOutput("dataTableHeading"),br(),dataTableOutput("drawTable"),textOutput("dynamicText50"),textOutput("dynamicRange50"),textOutput("freqencyTableHeading"),br(),dataTableOutput("NoOfEntriesTable"),br(), uiOutput("anvitaEmail1")),
                          tabPanel("Pattern",htmlOutput("helpHistoricCommodityMarketHTML"),textOutput("dynamicText1"),textOutput("HistoricDataSubsetHeading1"),textOutput("MSPMappingHeading"),br(),plotlyOutput("MSPVPlot"),br(), textOutput("dynamicText10"),textOutput("HistoricDataSubsetHeading2"),plotOutput("neighborMarketArrivalSubsetPlot"),textOutput("dynamicText20"),textOutput("HistoricDataSubsetHeading3"),br(),plotOutput("neighboringMarketPricesComparisonPlot"),textOutput("dynamicText21"),textOutput("HistoricDataSubsetHeading4"),textOutput("BiggestMarketAvgModalTitle"),br(),dataTableOutput("PoorMarketTable"),textOutput("dynamicText40"),textOutput("dynamicRange40"),br(),plotlyOutput("neighboringMarketsMeanPlot"),br(),uiOutput("anvitaEmail6")),
                          tabPanel("Forecast",htmlOutput("helpPriceForecastANNHTML"),textOutput("dynamicText4"),textOutput("dynamicRange302"),br(),textOutput("ANNWarning"),br(),plotlyOutput("ANNFuturePlot"),br(),textOutput("dynamicText3"),textOutput("forecastHeading"),br(),plotlyOutput("FAEPlot"),br(),textOutput("drawAccuracyHeading"),dataTableOutput("drawAccuracy"),br(),textOutput("dynamicText44"),textOutput("dynamicRange303"),textOutput("ANNFuturePredictionsHeading"),br(),dataTableOutput("NNPredictionsTable"),textOutput("AForecastHeading"),textOutput("dynamicText30"),textOutput("dynamicRange30"),plotlyOutput("seasonalForecastPlot"),br(),textOutput("dynamicText5"),textOutput("dynamicRange304"),textOutput("dataTableCheckHeading"),br() ,dataTableOutput("drawTableCheck"),br(), uiOutput("anvitaEmail")),
                          tabPanel("Analytics",htmlOutput("helpOtherAnalyticsHTML"), textOutput("dynamicText71"),textOutput("analyticsHeading1"),br(),uiOutput("proportionUI"),br(), textOutput("dynamicText74"),textOutput("analyticsHeading4"), br(),dataTableOutput("summaryTable"),br(),textOutput("dynamicText72"),textOutput("analyticsHeading2"),br(),uiOutput("otherAnalTable"), br(), textOutput("dynamicText73"),textOutput("analyticsHeading3"), br(),uiOutput("dynAnalTable"),br(),textOutput("dynamicTextWeeklyReport"),textOutput("WeeklyReportHeading"),br(),uiOutput("weeklyReportUI"),br(),uiOutput("anvitaEmail8")),
                          tabPanel("Correlation",htmlOutput("helpACFPACFPlotsHTML"),textOutput("dynamicText6"),uiOutput("acfpacfConditional"),br(),uiOutput("anvitaEmail3")),
                          tabPanel("ARIMA",htmlOutput("helpPriceForecastArimaHTML"), textOutput("dynamicText8"),textOutput("dynamicRange302Arima"),textOutput("pdqText"),br(),textOutput("ArimaWarning"),br(),plotlyOutput("ARIMAFuturePlot"),br(),textOutput("dynamicText7"),textOutput("ARIMAforecastHeading"),br(),plotlyOutput("AFPlot"),br(),textOutput("drawARIMAAccuracyHeading"),dataTableOutput("drawARIMAAccuracy"),br(),textOutput("dynamicText44Arima"),textOutput("dynamicRange303Arima"),textOutput("ArimaFuturePredictionsHeading"),br(),dataTableOutput("ArimaPredictionsTable"),br(),textOutput("AForecastHeadingArima"),textOutput("dynamicText30Arima"),textOutput("dynamicRange30Arima"),plotlyOutput("seasonalForecastArimaPlot"),br(),textOutput("dynamicText9"),textOutput("dynamicRange304Arima"), textOutput("dataTableCheckHeading1"),dataTableOutput("drawTableARIMA"), br(),uiOutput("anvitaEmail4")),
                          tabPanel("GARCH",htmlOutput("helpGarchHTML"),textOutput("dynamicGarchText"),textOutput("dynamicGarchTrainingRange"),plotOutput("garch1"),plotOutput("garch2"),plotOutput("garch3"),br(),textOutput("dynamicGarchForecastRange"),textOutput("dynamicGarchHeading4"),plotOutput("garch4"),br(),textOutput("dynamicGarchHeading5"),plotOutput("garch5"),br(),uiOutput("anvitaEmail7")),
                          tabPanel("Commerce",htmlOutput("helpTechnicalAnalysisHTML"),textOutput("dynamicText2"),textOutput("ReferenceRangeForTA"),textOutput("TechnicalAnalysis"),plotOutput("TapsBC", height = "100%"),br(),uiOutput("anvitaEmail5"))


              ),
              textOutput("acfpacfplots"),
              tags$head(tags$style("#logout{float:right}")),
              tags$head(tags$style("#acfpacfplots{color: #d5dfef; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange60{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange602{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange50{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText50{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText40{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange40{color: green; font-size: 20px; font-style: bold; text-align: center}")),   
              tags$head(tags$style("#dynamicText31{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange30{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange30Arima{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange302{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange302Arima{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange302Hybrid{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange303{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange303Arima{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange303Hybrid{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange304{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange304Arima{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicRange304Hybrid{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText{color: green; font-size: 20px; font-style: bold; text-align: center}")),        
              tags$head(tags$style("#dynamicText30{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText30Arima{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText21{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText20{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText10{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText9{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText91{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText8{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText81{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText7{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText71{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText6{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText5{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText4{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText44{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText44Arima{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText44Hybrid{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText3{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText2{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dynamicText1{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#HistoricDateRange{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#HistoricDataSubsetHeading{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#HistoricDataSubsetHeading1{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#ReferenceRangeForTA{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#HistoricDataSubsetHeading2{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#HistoricDataSubsetHeading3{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#HistoricDataSubsetHeading4{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dataTableHeading{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#freqencyTableHeading{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#drawAccuracyHeading{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#drawARIMAAccuracyHeading{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#drawHYBRIDAccuracyHeading{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dataTableCheckHeading{color: brown; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dataTableCheckHeading1{color: brown; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#dataTableCheckHeading2{color: brown; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#ANNFuturePlotHeading{color: orange; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#ANNFuturePredictionsHeading{color: orange; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#ArimaFuturePredictionsHeading{color: orange; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#HybridFuturePredictionsHeading{color: orange; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#ARIMAFuturePlotHeading{color: orange; font-size: 16px; font-style:bold; text-align: center}")),
              tags$head(tags$style("#HYBRIDFuturePlotHeading{color: orange; font-size: 16px; font-style:bold; text-align: center}")),
              tags$head(tags$style("#SarimaForTrainerViewHeading{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#SarimaForTrainerViewHeading1{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#SarimaForTrainerViewHeading2{color: green; font-size: 20px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#SarimaForVarietyArrivalHeading{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#SarimaForMarketArrivalHeading{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#SarimaForGradeArrivalHeading{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#priceAnalysisText{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#forecastHeading{color: blue; font-size: 18px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#AForecastHeading{color: blue; font-size: 18px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#AForecastHeadingArima{color: blue; font-size: 18px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#ARIMAforecastHeading{color:blue;font-size:18px;font-style:bold;text-align:center")),
              tags$head(tags$style("#HYBRIDforecastHeading{color:blue;font-size:18px;font-style:bold;text-align:center")),
              tags$head(tags$style("#analyticsHeading{color:blue;font-size:18px;font-style:bold;text-align:center")),
    
              tags$head(tags$style("#TechnicalAnalysis{color:blue; font-size: 18px; font-style:bold; text-align: center}")),
              tags$head(tags$style("#MSPMappingHeading{color:blue;font-size: 18px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#BiggestMarketAvgModalTitle{color:blue;font-size: 18px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaText{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaText6{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail6{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaText7{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail7{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaText1{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail1{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaText2{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail2{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaText3{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail3{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaText4{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail4{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaText41{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail41{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaText5{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail5{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#anvitaEmail8{color: black; font-size: 12px; font-style: bold; text-align: center}")),
              tags$head(tags$style("#pdqText{color: blue; font-size: 18px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#dynamicText71{color: green; font-size: 20px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#analyticsHeading1{color:blue;font-size:18px;font-style:bold;text-align:center")),
      
      tags$head(tags$style("#dynamicText72{color: green; font-size: 20px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#analyticsHeading2{color:blue;font-size:18px;font-style:bold;text-align:center")),
      
      tags$head(tags$style("#dynamicText73{color: green; font-size: 20px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#analyticsHeading3{color:blue;font-size:18px;font-style:bold;text-align:center")),
     
      tags$head(tags$style("#dynamicText74{color: green; font-size: 20px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#analyticsHeading4{color:blue;font-size:18px;font-style:bold;text-align:center")),
      
      tags$head(tags$style("#dynamicGarchText{color: green; font-size: 20px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#dynamicGarchTrainingRange{color: green; font-size: 20px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#dynamicGarchForecastRange{color: green; font-size: 20px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#dynamicGarchHeading4{color: blue; font-size: 18px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#dynamicGarchHeading5{color: blue; font-size: 18px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#ArimaWarning{color: red; font-size: 16px; font-style: bold; text-align: left}")),
      tags$head(tags$style("#ANNWarning{color: red; font-size: 16px; font-style: bold; text-align: left}")),
      tags$head(tags$style("#dynamicTextWeeklyReport{color: green; font-size: 20px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#WeeklyReportHeading{color: blue; font-size: 16px; font-style: bold; text-align: center}")),
      tags$head(tags$style("#ImpCommodities{color: blue; font-size: 16px; font-style: bold; text-align: left}"))
      
    ))
)


server <- function(input, output,session) {
  
  options(shiny.sanitize.errors = TRUE)
  
  session$onSessionEnded(function(){
    ##print("session ended")
    gc()
    makeActiveBinding("refresh", function() { system(paste0(R.home(),"/bin/i386/R")); q("no") }, .GlobalEnv)
    paste0(R.home(),"/bin/i386/R --no-#save") #--#save will #save workspace
    rm(list = ls(all.names = TRUE))
    
  })
  
  
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "lightgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  
  a <- list(
    title = "Dates",
    titlefont = f1
  )
  b <- list(
    title = "Modal Price",
    titlefont = f1
  )
  
  
    report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
    
  
    observeEvent(input$generate, {
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      validate(
        need(input$inputGrade != "", "Please select a Grade")) 
      
      bigDataSubFile <- bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Market %in% input$inputMarket) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Grade %in% input$inputGrade ),]
      
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Gathering data and building report.", 
                   detail = "This may take a while. This window will disappear  
                   when the report is ready.", value = 1)
      params=list(data=bigDataSubFile)
      #save(list = ls(all.names = TRUE), file = "rmdFile.RData")
      render("weeklyReportWord.Rmd", 
             output_format = "word_document", 
             params = params, 
             envir = new.env(parent = globalenv()))
      
      report$filepath <- "weeklyReportWord.docx" #Assigning in the temp file where the .pdf is located to the reactive file created above
      
    })
    
    # Hide download button until report is generated
    output$reportbuilt <- reactive({
      return(!is.null(report$filepath))
    })
    outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)
    
    
    #Download report  
    output$downloadW <- downloadHandler(
      
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        paste0("WeeklyReport_",input$inputCommodity,"_",Sys.Date(), ".docx") %>%
          gsub(" ", "_", .)
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
        
        file.copy(report$filepath, file)
        
      }
    )
  
  output$dynamicGarchText <- renderText({
    paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
  })
  output$dynamicGarchForecastRange <- renderText({
    paste("Forecast Dates:",format(input$dateRange[1], "%d-%m-%Y"), "  to  ",format(input$dateRange[2],"%d-%m-%Y"))
  })
  output$dynamicGarchHeading4 <- renderText({
    paste("Variance of Series")
  })
  output$dynamicGarchHeading5 <- renderText({
    paste("Value at Risk (VaR)")
  })
  output$dynamicGarchTrainingRange <- renderText({
    paste("Training Dates:",format(input$HistDateRange[1], "%d-%m-%Y"), "  to  ",format(input$HistDateRange[2],"%d-%m-%Y"))
  })
  
  
  output$helpCommodityProfileHTML <- renderUI({actionButton("helpCommodityProfile", "", icon("question"), align = "right", 
                                                     style="color: #fff; background-color:#a3daed; position:absolute;right:1em;")
  })       
  observeEvent(input$helpCommodityProfile, {
    showModal(modalDialog(
      title = "Profile",div(tags$b("Choose the filter criteria on the Left pane")),
      tags$p("Following Graphs and tables are generated."),
      tags$ol(tags$li("Traded Varieties in Descending Order", 
                      tags$ul(tags$li("This Graph shows the Arrivals of different Varieties of the selected Commodity during the Reference period in Descending order."),
                              tags$li("Choose the Highest Variety for further analysis."))),
              tags$li("Total Market Arrivals in Descending Order", 
                      tags$ul(tags$li("This table shows the Arrivals of different Markets of the selected Commodity during the Reference period in Descending order."),
                              tags$li("Choose the Highest Market for further analysis."))),
              tags$li("Grade-wise Arrivals in Descending Order", 
                      tags$ul(tags$li("This table shows the Arrivals of different Grades of the selected Commodity during the Reference period in Descending order."),
                              tags$li("Choose the Highest Grade for further analysis.")))
    )))
  })
  
  output$helpDataTableHTML <- renderUI({actionButton("helpDataTable", "", icon("question"), align = "right", 
                                                        style="color: #fff; background-color:#a3daed; position:absolute;right:1em;")
  })       
  #div(tags$b("Choose the filter criteria on the Left pane.", style = "color: red;"))
  observeEvent(input$helpDataTable, {
    showModal(modalDialog(
      title = "Data",div(tags$b("Choose the filter criteria on the Left pane.")),
      tags$p("Following data tables are generated."),
      tags$ol(tags$li("Filtered Data Table", 
                      tags$ul(tags$li("This table shows the data from the database for the reference period."),
                              tags$li("The Data Tables can be downloaded in CSV format."))),
              tags$li("Frequency of Arrivals in Markets", 
                      tags$ul(tags$li("This table shows the number of entries in the database for each market during the reference period"),
                              tags$li("The Table can be downloaded in CSV format.")))
              )
    ))
  })
  
  output$helpHistoricCommodityMarketHTML <- renderUI({actionButton("helpHistoricCommodityMarket", "", icon("question"), align = "right", 
                                                            style="color: #fff; background-color:#a3daed; position:absolute;right:1em;")
  })       
  observeEvent(input$helpHistoricCommodityMarket, {
    showModal(modalDialog(
      title = "Pattern",div(tags$b("Choose the filter criteria on the Left pane")),
      tags$p("Following Graphs and tables are generated."),
      tags$ol(tags$li("Commodity Arrival Superimposed With Modal Prices, MSP, Cost (A1+FL), Cost (C2) & Total Cost (C3)", 
                      tags$ul(tags$li("This Graph shows the arrivals in purple, Modal Prices in orange, MSP in red, 
                             Total Cost in Maroon, Cost (C2) in dark blue and Cost(A1+FL) in Green."), 
                              tags$li("MSP and Cost information will show only if they exist in the database."), 
                              tags$li("The Reference Date Range can be changed and the difference can be observed."),
                              tags$li("You can know the value at any point by hovering over it."),
                              tags$li("Clicking on the legend can include or remove an item."))),
              tags$li("Arrivals in Top Five Biggest Markets",
                      tags$ul(tags$li("This bar chart gives the 5 biggest markets for the chosen Commodity in the 
                                      given Reference Date Range"))),
              tags$li("Modal Prices in Top Five Biggest Markets",
                      tags$ul(tags$li("This bar chart gives the Modal Prices in the 5 biggest markets for the 
                      chosen Commodity in the given Reference Date Range"))),
              tags$li("Market with highest average Modal Price",
                      tags$ul(tags$li("This table gives the markets in descending order of their average 
                                      modal prices during the Reference Date Range"))),
              tags$li("Total Arrival Vs Average Modal Price for Markets",
                      tags$ul(tags$li("This graph plots the markets according to their Total Arrival and 
                                      Average Modal Price during the Reference Date Range."),
                              tags$li("This gives an idea of which markets have low arrivals but high modal price 
                                      and vice versa."),
                              tags$li("You can know the value at any point by hovering over it."),
                              tags$li("Clicking on the legend can include or remove an item.")))
                      )
    ))
  })
  output$helpTechnicalAnalysisHTML <- renderUI({actionButton("helpTechnicalAnalysis", "", icon("question"), align = "right", 
                                                            style="color: #fff; background-color:#a3daed; position:absolute;right:1em;")
  })       
  observeEvent(input$helpTechnicalAnalysis, {
    showModal(modalDialog(
      title = "Commerce",div(tags$b("Choose the filter criteria on the Left pane")),
      tags$p("Following Graphs are generated."),
      tags$ol(tags$li(div(tags$b("Bollinger Bands")),
                      tags$ul(tags$li("Bollinger Band are standard deviation based volatility bands placed above & below a moving average. 
                                      Bands widen when volatility increases and narrow when volatility decreases."),
                              tags$li("Bollinger Bands can be used to measure the highness or lowness of the 
                                      price relative to previous trades."),
                              tags$li("Bollinger Band consists of ",
                                      tags$ul(tags$li("a middle band being an N-period simple moving average (MA)"),
                                              tags$li("an upper band at K times an N-period standard deviation above
                                                      the middle band (MA + Kσ)"),
                                              tags$li("a lower band at K times an N-period standard deviation below the middle
                                                      band (MA − Kσ)"))),
                              tags$li("By definition, prices are high at the upper band and low at the lower band."),
                              tags$li("When the bands lie close together a period of low volatility in price is indicated.
                                      When the two bands if the Bollinger bands narrow it is an indication that the market 
                                      is going to change direction. "),
                              tags$li("When the bands are far apart a period of high volatility in price is indicated."),
                              tags$li("Bollinger Bands are helpful to traders seeking to detect the turning points in a range-bound market, 
                                      buying when the price drops and hits the lower band and selling when price rises to touch the upper band.
                                      However, as the markets enter trending, the indicator starts giving false signals,
                                      especially if the price moves away from the range it was trading. 
                                      Bollinger Bands are considered apt for low frequency trend following.")
                              )), 
              tags$li(div(tags$b("Volume 1000s")),
                      tags$ul(tags$li("This Graph gives the volume of arrivals"))),
              tags$li(div(tags$b("Commodity Channel Index (CCI)")),
                      tags$ul(tags$li("CCI is an oscillator used for identifyng cyclical trends"),
                      tags$li("The CCI typically oscillates above and below a zero line."),
                      tags$li("Normal oscillations will occur within the range of +100 and -100."),
                      tags$li("Readings above +100 imply an overbought condition, while 
                              readings below -100 imply an oversold condition."),
                      tags$li("CCI measures the difference between a security's price change and its average price change. 
                              High positive readings indicate that prices are well above their average, which is a show of strength. 
                              Low negative readings indicate that prices are well below their average, which is a show of weakness. 
                              The Commodity Channel Index (CCI) can be used as either a coincident or leading indicator. 
                              As a coincident indicator, surges above +100 reflect strong price action that can signal the 
                              start of an uptrend. Plunges below -100 reflect weak price action that can signal the start 
                              of a downtrend. 
                              As a leading indicator, chartists can look for overbought or oversold conditions that may 
                              foreshadow a mean reversion. Similarly, bullish and bearish divergences can be used to detect 
                              early momentum shifts and anticipate trend reversals."),
                      tags$li("CCI is a versatile momentum oscillator that can be used to identify overbought/oversold levels or 
                              trend reversals. The indicator becomes overbought or oversold when it reaches a relative extreme. 
                              That extreme depends on the characteristics of the underlying security and the historical range for CCI. 
                              Volatile securities are likely to require greater extremes than docile securities.
                              Trend changes can be identified when CCI crosses a specific threshold between zero and 100. 
                              of how CCI is used, chartists should use CCI in conjunction with other indicators or price analysis. 
                              Another momentum oscillator would be redundant, but On Balance Volume (OBV) or the Accumulation 
                              Distribution Line can add value to CCI signals.")
                      )),
              tags$li(div(tags$b("Chande Momentum Oscillator (CMO)")),
                      tags$ul(tags$li("Momentum is the rate of the rise or fall in price."),
                              tags$li("The CMO oscillates between 100 and -100 and a security is deemed to be overbought when 
                                      the Chande momentum oscillator is above +50 and oversold when it is below -50. Many technical
                                      traders add a 10-period moving average to this oscillator to act as a signal line. 
                                      The oscillator generates a bullish signal when it crosses above the moving average, 
                                      and a bearish signal when it moves below the moving average. The oscillator can be 
                                      used as a confirmation signal when it crosses above or below 0. For example, if a 
                                      trader notices that the 50-day moving average has crossed above the 200-day moving 
                                      average (golden cross), he or she could confirm the buy if the Chande momentum oscillator 
                                      has crossed above 0; this would suggest prices were showing upward momentum."),
                              tags$li("Trend strength can also be measured using the Chande momentum oscillator. 
                                      The higher the oscillator's value, the stronger the trend; the lower the value, the weaker the trend."),
                              tags$li("Traders can use the Chande momentum oscillator to spot positive and negative price divergence between 
                                      the indicator and the underlying security. A negative divergence occurs if the underlying security 
                                      is trending upward and the Chande momentum oscillator is moving downwards. A positive divergence occurs
                                      if price is declining, but the oscillator is rising. ")
                              )),
              tags$li(div(tags$b("Relative strength Indicator (RSI)")),
                      tags$ul(tags$li("RSI is a momentum oscillator that measures the speed and change of price movements. 
                                      RSI oscillates between zero and 100."),
                              tags$li("RSI is intended to chart the current and historical strength or weakness of a 
                                      market based on the closing prices of recent trading period."),
                              tags$li("The RSI computes momentum as the ratio of higher closes to lower closes:"),
                              tags$li("Divergence between RSI and price action is a very strong indication that a market
                                      turning point is imminent."),
                              tags$li("The Relative Strength Index (RSI) is a popular technical-momentum indicator. 
                                      It attempts to determine the overbought and oversold level in a market on a scale of 0 to 100, 
                                      thus indicating if the market has topped or bottomed. According to this indicator, the markets are 
                                      considered overbought above 70 and oversold below 30. The use of a 14-day RSI was recommended by 
                                      American technical analyst Welles Wilder. Over time, nine-day RSI and 25-day RSIs have gained popularity.
                                      RSI can be used to look for divergence and failure swings in addition to overbought and oversold signals. 
                                      Divergence occurs in situations where the asset is making a new high 
                                      while RSI fails to move beyond its previous high, signaling an impending reversal.
                                      If the RSI falls below its previous low, a confirmation to the impending reversal 
                                      is given by the failure swing. "),
                              tags$li("To get more accurate results, be aware of a trending market or ranging market since RSI divergence 
                                      is not good enough indicator in case of a trending market. RSI is very useful, especially when used 
                                      complementary to other indicators. ")
                      )),
              tags$li(div(tags$b("Willams %R")),
                      tags$ul(tags$li("%R is a technical analysis oscillator showing the current closing price in relation to the high
                              and low of the past N days"),
                              tags$li("Williams %R moves between 0 and -100, which makes -50 the midpoint.    
                                      A Williams %R cross above -50 signals that prices are trading in the  
                                      upper half of their high-low range for the given look-back period 
                                      (Bullish). Conversely, a cross below -50 means prices are trading in the 
                                      bottom half of the given look-back period (Bearish)."),
                              tags$li("Readings above -20 for the 14-day Williams %R would indicate that the 
                                      underlying commodity  was trading near the top of its 14-day high-low range. 
                                      Readings below -80 occur when a security is trading at the low end of its 
                                      high-low range. Default settings use -20 as the overbought threshold and -80 
                                      as the oversold threshold. These levels can be adjusted depending on the 
                                      security's characteristics. Keep in mind that even though a security is 
                                      overbought or oversold it can remain in this state for an extended period 
                                      of time. When using any indicator it is best to use an additional indicator 
                                      to confirm any signals."),
                              tags$li("When Williams % R moves above -20 and then, in the next move up, fails to move above -20, 
                                      this can indicate weakening momentum to the upside. Conversely, when Williams % R moves below -80 and then, 
                                      in the next move down, fails to move below -80, this can indicate weakening momentum to the downside.")
                                      )
                      ),
              tags$li(div(tags$b("Moving average cross over")),
                      tags$ul(tags$li("There are two moving averages one with a longer period called the slow moving average and another 
                                      a fast moving average of shorter duration superimposed on the chart. When the fast moving average crosses 
                                      the slow moving average from below it is an indication that the market prices are going to rise in the future.
                                      When the fast moving average crosses the slow moving average from the top it is an indication the market prices are going to fall.")))
              )))
  })
  output$helpACFPACFPlotsHTML <- renderUI({actionButton("helpACFPACFPlots", "", icon("question"), align = "right", 
                                                            style="color: #fff; background-color:#a3daed; position:absolute;right:1em;")
  })       
  observeEvent(input$helpACFPACFPlots, {
    showModal(modalDialog(
      title = "Correlation",
      "Select a Commodity Group and a Commodity to see the highest traded Variety,Grade and Biggest Market. 
      The data is filtered based on the Reference Date Range."
    ))
  })
  output$helpPriceForecastArimaHTML <- renderUI({actionButton("helpPriceForecastArima", "", icon("question"), align = "right", 
                                                            style="color: #fff; background-color:#a3daed; position:absolute;right:1em;")
  })       
  
  observeEvent(input$helpPriceForecastArima, {
    showModal(modalDialog(
      title = "ARIMA",div(tags$b("Choose the filter criteria on the Left pane")),
      tags$p("Following Graphs and tables are generated."),
      tags$ol(tags$li(div(tags$b("Price Forecast Plot")),
                      tags$ul(tags$li("This plot shows the forecast for the selected forecast period."),
                              tags$li("By default, the forecast period is set to start from three months 
                                      prior to current date and ends at three months after current date. 
                                      This can be changed."),
                              tags$li("The actual price is also displayed for dates prior to current date."),
                              tags$li("If the database has MSP and cost information for the crop, that also gets displayed."),
                              tags$li("The order(p,d,q), seasonality(P,D,Q) and period are automatically calculated and mentioned with the plot."),
                              tags$li(tags$b("It is strongly advised to consider the forecast along with GARCH and Technical analysis for a more reliable prediction."))
                              )), 
              tags$li(div(tags$b("Price Training Plot")),
                      tags$ul(tags$li("This Plot shows how well the Arima model fits the training data"),
                              tags$li("Currently the training data is considered that of last three years. This can be changed."))), 
              tags$li(div(tags$b("Prediction Accuracy Table")),
                      tags$ul(tags$li("This table shows the accuracy of Forecast w.r.t various metrics."))), 
              tags$li(div(tags$b("Price Forecast Table")),
                      tags$ul(tags$li("This table gives the daily forecast values in a downloadable format."))), 
              tags$li(div(tags$b("Arrival Seasonal Index")),
                      tags$ul(tags$li("This Graph shows the seasonality of the data superimposed with the price forecast"))),
              tags$li(div(tags$b("Training Data Table")),
                      tags$ul(tags$li("This table gives the training data in a downloadable format.")))
      )))
  })
  
  output$helpGarchHTML <- renderUI({actionButton("helpGarch", "", icon("question"), align = "right", 
                                                            style="color: #fff; background-color:#a3daed; position:absolute;right:1em;")
  })       
  observeEvent(input$helpGarch, {
    showModal(modalDialog(
      title = "GARCH",div(tags$b("Choose the filter criteria on the Left pane")),
      tags$p("Following Graphs are generated."),
      tags$ol(tags$li(div(tags$b("GARCH(1,1) Conditional Volatality"))),
              tags$li(div(tags$b("Series with 1% VaR Limits"))),
              tags$li(div(tags$b("ACF of squared observations")),
                      tags$ul(tags$li("Significant acf's indicate GARCH effects"))),
              tags$li(div(tags$b("GARCH(1,1) Forecast of h-day Return Vol")),
                      tags$ul(tags$li("Indicates whether Volatility is likely to increase in the future."))),
              tags$li(div(tags$b("VaR Forecast")),
                      tags$ul(tags$li("If there are significant acf's then volatility will tend to increase at 
                                      around the time indicated by red")))
      )
    ))
  })
  output$helpPriceForecastANNHTML <- renderUI({actionButton("helpPriceForecastANN", "", icon("question"), align = "right", 
                                                            style="color: #fff; background-color:#a3daed; position:absolute;right:1em;")
  })       
  observeEvent(input$helpPriceForecastANN, {
    showModal(modalDialog(
      title = "Forecast",div(tags$b("Choose the filter criteria on the Left pane")),
      tags$p("Following Graphs and tables are generated."),
      tags$ol(tags$li(div(tags$b("Price Forecast Plot")),
                      tags$ul(tags$li("This plot shows the forecast for the selected forecast period."),
                              tags$li("By default, the forecast period is set to start from three months 
                                      prior to current date and ends at three months after current date. 
                                      This can be changed."),
                              tags$li("The actual price is also displayed for dates prior to current date."),
                              tags$li("If the database has MSP and cost information for the crop, that also gets displayed.")
                              )),
      tags$li(div(tags$b("Price Training Plot")),
              tags$ul(tags$li("This Plot shows how well the ANN model fits the training data."),
                      tags$li("Currently the training data is considered that of last three years. This can be changed."))), 
      tags$li(div(tags$b("Prediction Accuracy Table")),
              tags$ul(tags$li("This table shows the accuracy of Forecast w.r.t various metrics."))), 
      tags$li(div(tags$b("Price Forecast Table")),
              tags$ul(tags$li("This table gives the daily forecast values in a downloadable format."))), 
      tags$li(div(tags$b("Arrival Seasonal Index")),
              tags$ul(tags$li("This Graph shows the seasonality of the data superimposed with the price forecast"))), 
      tags$li(div(tags$b("Training Data Table")),
              tags$ul(tags$li("This table gives the training data in a downloadable format.")))
      )))
  })
  output$helpOtherAnalyticsHTML <- renderUI({actionButton("helpOtherAnalytics", "", icon("question"), align = "right", 
                                                            style="color: #fff; background-color:#a3daed; position:absolute;right:1em;")
  })       
  observeEvent(input$helpOtherAnalytics, {
    showModal(modalDialog(
      title = "Analytics",div(tags$b("Choose the filter criteria on the Left pane")),
      tags$p("Following Graphs and tables are generated."),
      tags$ol(tags$li(div(tags$b("Year-wise Total Arrivals and average price")),
                      tags$ul(tags$li("These two Bar graphs give the Total Arrivals and Average Price for every year selected."),
                              tags$li("The Years to be included can be selected using the slider on top on the tab."))), 
              tags$li(div(tags$b("Proportion of Arrivals and Price w.r.t. previous year")),
                      tags$ul(tags$li("These two Bar graphs give the proportion of Arrivals and Prices of a year w.r.t its previous year"),
                              tags$li("The Years to be included can be selected using the slider on top on the tab.")
                              )), 
              tags$li(div(tags$b("Table of Percentage of Difference in Arrivals and Modal price w.r.t. previous year")),
                      tags$ul(tags$li("This table gives the summary of above two graphs in a downloadable table format."),
                              tags$li("The Years to be included can be selected using the slider on top on the tab.")
                      )), 
              
              tags$li(div(tags$b("Table of Monthly Arrivals and Modal Price")),
                      tags$ul(tags$li("This table gives the month-wise total arrivals and modal price for any selected year."),
              tags$li("The year can be selected on the left pane.")
              )),
              tags$li(div(tags$b("Period-wise comparison of Arrivals,Modal Price, MSP and Cost")),
                      tags$ul(tags$li("This can be used to compare Arrivals, Price, MSP and Cost in any dynamic periods."),
                              tags$li("Select the number of periods you wish to compare and hit Go."),
                              tags$li("Rows will appear for each new period to be compared."),
                              tags$li("Change the dates under Period column and observe the other details."),
                              tags$li("The generated table can also be downloaded.")
                              )),
              tags$li(div(tags$b("Report of Arrivals and Price until last week")),
                      tags$ul(tags$li("This feature can be used to generate and download a report in Word format."),
                              tags$li("The report contains tables and graphs comparing monthly arrivals and prices of previous year to current year."),
                              tags$li("Data from current year upto the last week is compared."),
                              tags$li("Once the document is downloaded please change the orientation to landscape and enlarge the graphs to be able to see clearly.")
                      ))
      )))
  })
  
  # Return the components of the URL in a string:
  output$urlText <- renderText({
    paste(sep = "",
          "protocol: ", session$clientData$url_protocol, "\n",
          "hostname: ", session$clientData$url_hostname, "\n",
          "pathname: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n",
          "user: ",session$user, "\n"
    )
  })
 
  
  analytics <- reactiveValues(df_data = data.frame(Period = character(), Arrivals=numeric(), Modal.Price=numeric(),MSP=character(), CostA1FL=character(), CostC3=character(),stringsAsFactors=FALSE))
  output$acfpacfConditional <- renderUI({
    
    validate(
      need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
    
    validate(
      need(input$inputCommodity != "", "Please select a Commodity"))
    
    validate(
      need(input$inputMarket != "", "Please select a Market"))
    
    validate(
      need(input$inputVariety != "", "Please select a Variety"))        
    
    validate(
      need(input$inputGrade != "", "Please select a Grade")) 
    
    tagList(
      plotOutput("acfPlot"),
      plotOutput("acfDiffPlot"), plotOutput("pacfDiffPlot"),
      plotOutput("acf2DiffPlot"), plotOutput("pacf2DiffPlot")
      
      # conditionalPanel("output.acfpacfplots == 1",
      #                  plotOutput("acfPlot")
      # ),
      # conditionalPanel("output.acfpacfplots == 2",
      #                  plotOutput("acf2DiffPlot"), plotOutput("pacf2DiffPlot")
      # ),
      # conditionalPanel("output.acfpacfplots == 3",
      #                  plotOutput("acfDiffPlot"), plotOutput("pacfDiffPlot")
      # ),
      # conditionalPanel("output.acfpacfplots == 4",
      #                  plotOutput("acf2DiffPlot1"), plotOutput("pacf2DiffPlot1")
      # )
    )
  })
  
  output$weeklyReportUI <- renderUI({
    
    validate(
      need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
    
    validate(
      need(input$inputCommodity != "", "Please select a Commodity"))
    
    validate(
      need(input$inputMarket != "", "Please select a Market"))
    
    validate(
      need(input$inputVariety != "", "Please select a Variety"))        
    
    validate(
      need(input$inputGrade != "", "Please select a Grade")) 
    
     report$filepath = NULL
    
     tagList(       
       fluidRow(column(width=2),box(column(width=6, align="center", actionButton("generate", "Generate Report", icon = icon("file"), 
                                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                    column(width=6, conditionalPanel(condition = "output.reportbuilt", downloadButton("downloadW", "Download Report", 
                                                                                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4") ))), column(width=2))) 
     
  })
  output$proportionUI <- renderUI({
    
    validate(
      need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
    
    validate(
      need(input$inputCommodity != "", "Please select a Commodity"))
    
    validate(
      need(input$inputMarket != "", "Please select a Market"))
    
    validate(
      need(input$inputVariety != "", "Please select a Variety"))        
    
    validate(
      need(input$inputGrade != "", "Please select a Grade")) 
    
    bigDataSubFile <- bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Market %in% input$inputMarket) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Grade %in% input$inputGrade ),]
    yearList = sort(unique(lubridate::year(bigDataSubFile$Date)), decreasing = TRUE)
    rm(bigDataSubFile)
    tagList(
      fluidRow(column(width=2),column(width = 8, align="center", sliderTextInput(
        inputId = "number_of_years", 
        label = "Number of years to include:", 
        grid = TRUE, 
        force_edges = TRUE,
        choices = yearList[2:length(yearList)],
        selected = yearList[3]
      )), column(width=2)),
     br(),
      fluidRow(
        column(width=1),
        column(width = 4,plotlyOutput("allArrivalsPlot")),
        column(width = 2),
        column(width = 4,plotlyOutput("allModalPlot")),
        column(width=1)
      ),
     br(),
     fluidRow(
       column(width=1),
       column(width = 4,plotlyOutput("percentageArrivalPlot")),
       column(width = 2),
       column(width = 4,plotlyOutput("percentageModalPlot")),
       column(width=1)
     )
      #plotlyOutput("percentArrivals"),
      #plotlyOutput("percentModal"),
      
      
    #   radioButtons("col",label=NULL, choices = c("Compared to last year" = 'A', "Compared to last three years" = 'B'), selected = "A", inline=TRUE),
    # conditionalPanel(
    #   condition = "input.col == 'A'", plotlyOutput("proporPlot")),
    # conditionalPanel(
    #   condition = "input.col == 'B'", plotlyOutput("proporPlot1"))
    )
  })
  
  output$dynAnalTable <- renderUI({
    
    validate(
      need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
    
    validate(
      need(input$inputCommodity != "", "Please select a Commodity"))
    
    validate(
      need(input$inputMarket != "", "Please select a Market"))
    
    validate(
      need(input$inputVariety != "", "Please select a Variety"))        
    
    validate(
      need(input$inputGrade != "", "Please select a Grade")) 
    
   tagList(fluidRow(column(width = 1), column(width=6, align="right", sliderInput("number_of_periods", "How many Periods  would you like to compare?", min = 1, max = 12, value = 1)),
    column(width=2, align="left", br(), br(), actionButton('insertBtn', 'Go')),column(width=3)),
     br(),
    fluidRow(box(width=14,column(3, align="center", textOutput("periodheading")),
                 column(1, align="center", textOutput("arrivalheading")),
                 column(1, align="center", textOutput("modalheading")),
                 column(2, align="center", textOutput("mspheading")),
                 column(2, align="center", textOutput("costa1flheading")),
                 column(2, align="center", textOutput("costc2heading")),
                 column(1, align="center", textOutput("totalc3heading"))
                 )),
    tags$div(id = 'placeholder'),
    downloadButton("downloadData", "Download"))
  })
  output$otherAnalTable <- renderUI({
    
    validate(
      need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
    
    validate(
      need(input$inputCommodity != "", "Please select a Commodity"))
    
    validate(
      need(input$inputMarket != "", "Please select a Market"))
    
    validate(
      need(input$inputVariety != "", "Please select a Variety"))     
    
    validate(
      need(input$inputGrade != "", "Please select a Grade"))   
    
    validate(
      need(input$inputYear != "", "Please select a Year"))  
    
     #yrs = input$inputYear
     ##print(yrs)
     #save(list = ls(all.names = TRUE), file = "DF1.RData")
    lapply(input$inputYear, function(i) {
      id <- paste0("year", i)
      ##print(paste0("The id is ",id))
      DT::dataTableOutput(id)
    })
  })
  
  observe({
  for (i in input$inputYear) {
    id <- paste0("year", i)
    ##print(id)
    dataFileMI <- otherDataInput1()
    ##print(nrow(dataFileMI))
    
    YearMonthData <- dataFileMI[(dataFileMI$Grade %in% input$inputGrade),]
    ##print(nrow(YearMonthData))
    
    if (nrow(YearMonthData) > 0){
    ##segregate month and year as new columns
      YearMonthData$Date <- as.Date(YearMonthData$Date)
      ##segregate month and year as new columns
      YearMonthData$Month <-   months(YearMonthData$Date)
      YearMonthData$Year <- year(YearMonthData$Date)
    
    YearMonthArrivalModalData <- data.frame(YearMonthData[, c("CommodityGroup", "Commodity", "Market", "Arrival", "Modal", "Month", "Year")])
    rm(YearMonthData)
    ##print(nrow(YearMonthArrivalModalData))
    ## aggregating arrival by month, year and markets
    AggregatedYearMonthArrivalData1 <- aggregate(x = YearMonthArrivalModalData[c("Arrival")], by = YearMonthArrivalModalData[c("Month","Year")], FUN = sum)
     
    ## aggregating arrival by current year and sorting by ascending order of months for first market
    AggregatedYearMonthArrivalMarketYearData11 <- AggregatedYearMonthArrivalData1[c(AggregatedYearMonthArrivalData1$Year == i),]
    rm(AggregatedYearMonthArrivalData1)
    AggregatedYearMonthArrivalMarketYearData11$Month <- factor(AggregatedYearMonthArrivalMarketYearData11$Month, levels = month.name)
    AggregatedYearMonthArrivalMarketYearData11 <- AggregatedYearMonthArrivalMarketYearData11[order(AggregatedYearMonthArrivalMarketYearData11$Month),]
    
    ## aggregating Modal by month, year and markets
    AggregatedYearMonthModalData1 <- aggregate(x = YearMonthArrivalModalData[c("Modal")], by = YearMonthArrivalModalData[c("Month","Year")], FUN = mean)
    rm(YearMonthArrivalModalData)
    
    ## aggregating Modal by currentyear and sorting by ascending order of months for first market
    AggregatedYearMonthModalMarketYearData11 <- AggregatedYearMonthModalData1[c(AggregatedYearMonthModalData1$Year == i),]
    rm(AggregatedYearMonthModalData1)
    ##print(paste0("Aggregated for year:", i," count is ",nrow(AggregatedYearMonthModalMarketYearData11 )))
    AggregatedYearMonthModalMarketYearData11$Month <- factor(AggregatedYearMonthModalMarketYearData11$Month, levels = month.name)
    AggregatedYearMonthModalMarketYearData11 <- AggregatedYearMonthModalMarketYearData11[order(AggregatedYearMonthModalMarketYearData11$Month),]
    
    AggregatedYearMonthArrivalModalCurrentYearData <- merge(AggregatedYearMonthArrivalMarketYearData11,AggregatedYearMonthModalMarketYearData11)
    rm(AggregatedYearMonthArrivalMarketYearData11)
    rm(AggregatedYearMonthModalMarketYearData11)
    
    AggregatedYearMonthArrivalModalCurrentYearData$Month = factor(AggregatedYearMonthArrivalModalCurrentYearData$Month, levels = month.name)
    AggregatedYearMonthArrivalModalCurrentYearData = AggregatedYearMonthArrivalModalCurrentYearData[order(AggregatedYearMonthArrivalModalCurrentYearData$Month),] 
    ##print(paste0("The id before render", id, "Rows", nrow(AggregatedYearMonthArrivalModalCurrentYearData)))
    AggregatedYearMonthArrivalModalCurrentYearData$Modal = round(AggregatedYearMonthArrivalModalCurrentYearData$Modal, digits=2)
    output[[id]] <- DT::renderDataTable({
      AggregatedYearMonthArrivalModalCurrentYearData <- subset(AggregatedYearMonthArrivalModalCurrentYearData, select = -c(Year) )
      datatable(AggregatedYearMonthArrivalModalCurrentYearData, rownames=FALSE)
      }, extensions = 'Buttons')
    }
  }
  })
  
  observe({
    currentYear <- as.numeric(format(Sys.Date(), "%Y"))
    NumberOfYears <- input$number_of_years
   
    if (!is.null(NumberOfYears)){
      if(NumberOfYears > 0){
      #NumberOfYears = currentYear
    
    bigDataSubFile <- bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Market %in% input$inputMarket) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Grade %in% input$inputGrade ),]
    if (nrow(bigDataSubFile) > 0){
      bigDataSubFile$Date <- as.Date(bigDataSubFile$Date)
      bigDataSubFile$Month <- months(bigDataSubFile$Date)
      bigDataSubFile$Year <- year(bigDataSubFile$Date)
    bigDataSubFile = data.frame(bigDataSubFile)
    bigFileAggregatedArrivals <- aggregate(x = bigDataSubFile[c("Arrival")], by = bigDataSubFile[c("Year")], FUN = sum)
    bigFileAggregatedModal <- aggregate(x = bigDataSubFile[c("Modal")], by = bigDataSubFile[c("Year")], FUN = mean)
    rm(bigDataSubFile)
    #save(list = ls(all.names = TRUE), file = "DF123.RData")
    
    output$percentageArrivalPlot <- renderPlotly({
      X = c(currentYear:NumberOfYears)
      Y = c(currentYear:NumberOfYears)
     
      
      cnt=1
      for (i in X){
        Y[cnt] = bigFileAggregatedArrivals[c(bigFileAggregatedArrivals$Year == i),]$Arrival
        cnt=cnt+1
      }
      
      ArrivalDiff = NA
      Xlbl = NA
      ArrivalText = NA
      
      #save(list = ls(all.names = TRUE), file = "DF1.RData")
      for (i in 1:cnt-2){
        ArrivalDiff[i] = round(-100 + (Y[i]/Y[i+1])*100,digits=2)
        if (length(ArrivalDiff[i]) > 0){
          if(ArrivalDiff[i] < 0){
          ArrivalText[i] = paste(abs(ArrivalDiff[i]),'% less than ',X[i+1] )}else{
            ArrivalText[i] = paste(ArrivalDiff[i],'% more than ',X[i+1] )  
          }}
       
        #Xlbl[i] = paste(X[i],"/",X[i+1])
        Xlbl[i] = paste(X[i])
      }
  
      t <- list(
        family = "sans serif",
        size = 10,
        color = 'blue')
      
      p <- plot_ly(
        x = Xlbl,
        y = ArrivalDiff,
        text = ArrivalText,
        name = "Proportion of Arrivals",
        type = "bar",
        color = I("sandybrown")
      ) %>%
        layout(title = "Proportion of Arrivals",font=t,
               xaxis = list(title = "Year"),
               yaxis = list(title = "% of Arrivals"))
      
    })
    
    output$percentageModalPlot <- renderPlotly({
      X = c(currentYear:NumberOfYears)
      Y = c(currentYear:NumberOfYears)
      
      
      cnt=1
      for (i in X){
        Y[cnt] = bigFileAggregatedModal[c(bigFileAggregatedModal$Year == i),]$Modal
        cnt=cnt+1
      }
      
      ModalDiff = NA
      Xlbl = NA
      ModalText = NA
      
      #save(list = ls(all.names = TRUE), file = "DF1.RData")
      for (i in 1:cnt-2){
        ModalDiff[i] = round(-100+(Y[i]/Y[i+1])*100,digits=2)
        ##print(ModalDiff[i])
        ##print(length(ModalDiff[i]))
        ##print(length(ModalDiff[i]) > 0)
        if (length(ModalDiff[i]) > 0 ){
        if (ModalDiff[i] < 0){
        ModalText[i] = paste(abs(ModalDiff[i]),'% less than ',X[i+1] )}else{
        ModalText[i] = paste(ModalDiff[i],'% more than ',X[i+1] )  
        }}
       # Xlbl[i] = paste(X[i],"/",X[i+1])
        Xlbl[i] = paste(X[i])
      }
      
      t <- list(
        family = "sans serif",
        size = 10,
        color = 'blue')
      
      p <- plot_ly(
        x = Xlbl,
        y = ModalDiff,
        text = ModalText,
        name = "Proportion of Modal Prices",
        type = "bar",
        color = I("sandybrown")
      ) %>%
        layout(title = "Proportion of Modal Price",font=t,
               xaxis = list(title = "Year"),
               yaxis = list(title = "% of Modal Price"))
      
    })
    
    output$allArrivalsPlot <- renderPlotly({
      
      X = c(currentYear:NumberOfYears)
      Y = c(currentYear:NumberOfYears)
      
      cnt=1
      for (i in X){
        Y[cnt] = bigFileAggregatedArrivals[c(bigFileAggregatedArrivals$Year == i),]$Arrival
        cnt=cnt+1
      }
      t <- list(
        family = "sans serif",
        size = 10,
        color = 'blue')
      
      p <- plot_ly(
        x = as.character(X),
        y = Y,
        name = "Total Arrivals",
        type = "bar",
        color = I("darkseagreen")
      ) %>%
        layout(title = "Year-wise Total Arrivals",font=t,
              xaxis = list(title = "Year"),
              yaxis = list(title = "Total Arrivals"))
    })
    
    output$allModalPlot <- renderPlotly({
      X = c(currentYear:NumberOfYears)
      Y = c(currentYear:NumberOfYears)
      
      cnt=1
      for (i in X){
        Y[cnt] = bigFileAggregatedModal[c(bigFileAggregatedModal$Year == i),]$Modal
        cnt=cnt+1
      }
      t <- list(
        family = "sans serif",
        size = 10,
        color = 'blue')
      
      p <- plot_ly(
        x = as.character(X),
        y = Y,
        name = "Average Modal Price",
        type = "bar",
        color = I("darkseagreen")
      )%>%
        layout(title = "Year-wise Average Modal Price",font=t,
               xaxis = list(title = "Year"),
               yaxis = list(title = "Average Modal Price"))
    })
    
    
    output$summaryTable <- DT::renderDataTable({
      
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))     
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))   
      
      
      X = c(currentYear:NumberOfYears)
      AnnualArrivalModalValues <- merge(bigFileAggregatedArrivals, bigFileAggregatedModal, all = TRUE)
      
      AnnualArrivalModalValues = AnnualArrivalModalValues[(AnnualArrivalModalValues$Year %in% X), ]

      AnnualArrivalModalValues <- AnnualArrivalModalValues[order(AnnualArrivalModalValues$Year, decreasing=TRUE),]
      AnnualArrivalModalValues$ArrivalDiff = NA
      AnnualArrivalModalValues$ModalDiff = NA
      
      #save(list = ls(all.names = TRUE), file = "DF1.RData")
      for (i in 1:(nrow(AnnualArrivalModalValues)-1)){
        AnnualArrivalModalValues$ArrivalDiff[i] = round((-100 + ((AnnualArrivalModalValues$Arrival[i]/AnnualArrivalModalValues$Arrival[i+1])*100)),digits=2)
        AnnualArrivalModalValues$ModalDiff[i] = round((-100 + ((AnnualArrivalModalValues$Modal[i]/AnnualArrivalModalValues$Modal[i+1])*100)),digits=2)
      }
      AnnualArrivalModalValues$Modal = round(AnnualArrivalModalValues$Modal, digits=2)
      colnames(AnnualArrivalModalValues) = c('Year', 'Total Arrivals', 'Average Modal Price', '%Difference in Arrivals', '%Difference in Modal Price')
      AnnualArrivalModalValues
    }, 
    extensions = 'Buttons')
    }
    
    }
  }
  })
   
  updateOtherDataFile1 <- reactive({
    #message("Inside update Other Data File reactive")
    
    sD = as.Date(paste0(input$inputYear,"-01-01"), "%Y-%m-%d")
    eD = as.Date(paste0(input$inputYear,"-12-31"), "%Y-%m-%d")
    
    otherDataFile <- getDataWithGrades(sD, eD)
    colnames(otherDataFile) <- c("CommodityGroup", "Commodity", "Market", "Date", "Variety", "Grade", "Arrival", "Modal", "Max", "Min", "ArrivalUnits", "ModalUnits", "MSP", "CostA1FL", "CostC2","TotalC3")
    
    otherDataFile$Date <- as.Date(otherDataFile$Date,format = "%d/%m/%Y")
    otherDataFile <- otherDataFile[order(otherDataFile$Date),]
    # otherDataFile <- unique(otherDataFile)
    ##print(paste0("size inside", nrow(otherDataFile)))
    #message(nrow(otherDataFile))
    otherDataFile
    
  })
  
  
  
  otherDataInput1 <- reactive({
    #message("Inside  other datainput reactive")
    
    df <- updateOtherDataFile1()
    ##print(nrow(df))
    
    #save(list = ls(all.names = TRUE), file = "DF1.RData")
    
    if (!is.null(input$inputCommodityGroup)){
      df <- df[( df$CommodityGroup == input$inputCommodityGroup),]
    }
    ##print(nrow(df))
    if (!is.null(input$inputCommodity)){
      df <- df[(df$Commodity == input$inputCommodity),]
    }
    ##print(nrow(df))
    if(!is.null(input$inputMarket)){
      df <- df[(df$Market %in% input$inputMarket),]
    }
    ##print(nrow(df))
    if(!is.null(input$inputVariety)){
      df <- df[(df$Variety %in% input$inputVariety),]
    }
    ##print(nrow(df))
    if(!is.null(input$inputGrade)){
      df <-  df[(df$Grade %in% input$inputGrade),]
    }
    ##print(nrow(df))
    #df <- otherDataFile[(otherDataFile$CommodityGroup == input$inputCommodityGroup) & (otherDataFile$Commodity == input$inputCommodity) & (otherDataFile$Market == input$inputMarket) & which(input$inputVariety %in% otherDataFile$Variety ) & (otherDataFile$Grade == input$inputGrade),]
    #message(nrow(df))
    df$Date <- as.Date(df$Date,format = "%d/%m/%Y")
    df <- df[order(df$Date),]
    if (nrow(df) > 0){
      set(df,which(is.na(df[["MSP"]])),"MSP",0)
      set(df,which(is.na(df[["CostA1FL"]])),"CostA1FL",0)
      set(df,which(is.na(df[["CostC2"]])),"CostC2",0)
      set(df,which(is.na(df[["TotalC3"]])),"TotalC3",0)
    }
    #save(list = ls(all.names = TRUE), file = "DF2.RData")
    
    df
  })

  # create a character vector of shiny inputs 
  observeEvent(input$insertBtn,{
    
    removeUI(
      selector = "div#OtherAnals",
      multiple = TRUE,
      immediate = TRUE,
      session = getDefaultReactiveDomain()
    )
    noOfPeriods <- input$number_of_periods
   
    
    analytics$df_data <- NULL
    analytics$df_data <- data.frame(Period = character(), Arrivals=numeric(), Modal.Price=numeric(),
                                   MSP=character(), CostA1FL=character(), CostC3=character(),stringsAsFactors=FALSE)
    
    for(lp in 1:noOfPeriods){
     
      #message(paste0("The id is",lp))
      Dateid <- paste0('period', lp)
      Arrivalid <- paste0('Arrival', lp)
      Modalid  <- paste0('Modal', lp)
      MSPid <- paste0('MSP', lp)
      CostA1FLid <- paste0('CostA1FL', lp)
      CostC2id <- paste0('CostC2', lp)
      TotalC3id <- paste0('TotalC3', lp)
    
      insertUI(
        selector = '#placeholder',
        where = "afterEnd",
        ui = tagList(
        fluidRow(id="OtherAnals", 
                 box(width = 14, status = "primary", column(3,align="left",dateRangeInput(Dateid, label = NULL, start = referenceDateRangeStartDate, end = referenceDateRangeEndDate, format="dd-mm-yyyy")),
                  column(1,align="center",textOutput(Arrivalid)), column(1,align="center",textOutput(Modalid)), column(2,align="center",htmlOutput(MSPid)), column(2,align="center",htmlOutput(CostA1FLid)), column(2,align="center",htmlOutput(CostC2id)), column(1,align="center",htmlOutput(TotalC3id))))))
      
      #message(MSPid)
     
    }
  })
 
 
  observe({
    if (!is.null(input$number_of_periods)){
    
  lapply(
    X = 1:input$number_of_periods,
    FUN = function(i){

    updateOtherDataFile <- reactive({
    #message("Inside update Other Data File reactive")

    Dateid <- paste0('period', i)
    sD = as.Date(input[[Dateid]][1])
    eD = as.Date(input[[Dateid]][2])

    otherDataFile <- getDataWithGrades(sD, eD)
    colnames(otherDataFile) <- c("CommodityGroup", "Commodity", "Market", "Date", "Variety", "Grade", "Arrival", "Modal", "Max", "Min", "ArrivalUnits", "ModalUnits", "MSP", "CostA1FL", "CostC2", "TotalC3")

    otherDataFile$Date <- as.Date(otherDataFile$Date,format = "%d/%m/%Y")
    otherDataFile <- otherDataFile[order(otherDataFile$Date),]
    # otherDataFile <- unique(otherDataFile)

    #message(nrow(otherDataFile))
    otherDataFile
  })

  otherDataInput <- reactive({
    #message("Inside  other datainput reactive")
    df <- updateOtherDataFile()

    #save(list = ls(all.names = TRUE), file = "DF1.RData")

    if (!is.null(input$inputCommodityGroup)){
      df <- df[( df$CommodityGroup == input$inputCommodityGroup),]
    }
    #message(nrow(df))
    if (!is.null(input$inputCommodity)){
      df <- df[(df$Commodity == input$inputCommodity),]
    }

    if(!is.null(input$inputMarket)){
      df <- df[(df$Market %in% input$inputMarket),]
    }
    #message(nrow(df))
    if(!is.null(input$inputVariety)){
      df <- df[(df$Variety %in% input$inputVariety),]
    }
    #message(nrow(df))
    if(!is.null(input$inputGrade)){
      df <-  df[(df$Grade %in% input$inputGrade),]
    }

    #df <- otherDataFile[(otherDataFile$CommodityGroup == input$inputCommodityGroup) & (otherDataFile$Commodity == input$inputCommodity) & (otherDataFile$Market == input$inputMarket) & which(input$inputVariety %in% otherDataFile$Variety ) & (otherDataFile$Grade == input$inputGrade),]
    #message(nrow(df))
    df$Date <- as.Date(df$Date,format = "%d/%m/%Y")
    df <- df[order(df$Date),]
    if (nrow(df) > 0){
      set(df,which(is.na(df[["MSP"]])),"MSP",0)
      set(df,which(is.na(df[["CostA1FL"]])),"CostA1FL",0)
      set(df,which(is.na(df[["CostC2"]])),"CostC2",0)
      set(df,which(is.na(df[["TotalC3"]])),"TotalC3",0)
    }
    #save(list = ls(all.names = TRUE), file = "DF2.RData")
    #message(nrow(df))
    df
  })


      observe({
          Dateid <- paste0('period', i)
          Arrivalid <- paste0('Arrival', i)
          Modalid  <- paste0('Modal', i)
          MSPid <- paste0('MSP', i)
          CostA1FLid <- paste0('CostA1FL', i)
          CostC2id <- paste0('CostC2', i)
          TotalC3id <- paste0('TotalC3', i)
          #message(paste0("Date",input[[Dateid]]))
          #message(Arrivalid)
          
          
        if (!is.null(input[[Dateid]])){
          df1 <- otherDataInput()
          
          #save(list = ls(all.names = TRUE), file = "DF.RData")
          
          sD = as.Date(input[[Dateid]][1])
          eD = as.Date(input[[Dateid]][2])
          
          output[[Arrivalid]] <- renderText({ paste0(sum(df1$Arrival))})
          output[[Modalid]] <- renderText({ paste0(round(mean(df1$Modal), 0))})
          
          analytics$df_data[i,'Arrivals'] <<- paste0(sum(df1$Arrival))
          analytics$df_data[i,'Period'] <<- paste0(format.Date(sD,"%d-%m-%Y")," to ", format.Date(eD,"%d-%m-%Y"))
          analytics$df_data[i,'Modal.Price'] <<- paste0(round(mean(df1$Modal), 0))
          
          MSPString = paste0(toString(unique(df1$MSP)))
          CostA1FLString = paste0(toString(unique(df1$CostA1FL)))
          CostC2String = paste0(toString(unique(df1$CostC2)))
          TotalC3String = paste0(toString(unique(df1$TotalC3)))
          
          output[[MSPid]] <- renderUI({ HTML(MSPString)})
          output[[CostA1FLid]] <- renderUI({ HTML(CostA1FLString) })
          output[[CostC2id]] <- renderUI({HTML(CostC2String)})
          output[[TotalC3id]] <- renderUI({ HTML(TotalC3String) })
          
          tMSPString <- str_replace_all(MSPString, "<br/>", " ")
          tCostA1FLString <- str_replace_all(CostA1FLString, "<br/>", " ")
          tCostC2String <- str_replace_all(CostC2String, "<br/>", " ")
          tTotalC3String <- str_replace_all(TotalC3String, "<br/>", " ")
          
          a <- character(0)
         
          ##print(tMSPString)
          if(!identical(a, tMSPString)){
          analytics$df_data[i,'MSP']<<- tMSPString}
          else{
            analytics$df_data[i,'MSP'] <<- "" }
          if(!identical(a, tCostA1FLString)){
          analytics$df_data[i,'CostA1FL'] <<-tCostA1FLString}
          else{
            analytics$df_data[i,'CostA1FL'] <<-""
          }
          if(!identical(a, tCostC2String)){ 
            analytics$df_data[i,'CostC2'] <<-tCostC2String
          }else{
            analytics$df_data[i,'CostC2'] <<-""
          }
          if(!identical(a, tTotalC3String)){ 
            analytics$df_data[i,'CostC3'] <<-tTotalC3String
          }else{
            analytics$df_data[i,'CostC3'] <<-""
          }
          #message("Inside the inner obserevent")
        }
    })
    })
}
  })
  
 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("analytics.csv")
    },
    content = function(file) {
      df = analytics$df_data[1:input$number_of_periods,]
      #df<-df[dim(df)[1]:1,]
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Initialize list of inputs
  inputTagList <- tagList()
  output$inputCommodityGroup <- renderUI({
    
    CommodityGroupList <- levels(factor(bigDataFile$CommodityGroup))
    #selectInput("inputCommodityGroup", "Choose Commodity Group",choices = CommodityGroupList, selected = "Cereals")
    selectInput("inputCommodityGroup", "Choose Commodity Group",choices = c("",CommodityGroupList))
  })
  
  output$inputCommodity <- renderUI({

    commodityList = levels(factor(bigDataFile[bigDataFile$CommodityGroup == input$inputCommodityGroup,]$Commodity))
    selectInput("inputCommodity", "Choose Commodity",choices = c("",commodityList), selected=NULL)
  })
  output$inputVariety <- renderUI({
    
    varietyTList = levels(factor(bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity)  & (bigDataFile$Market %in% input$inputMarket),]$Variety))
    varietyTList <- c("All Varieties", varietyTList)
   #selectInput("inputVariety", "Choose Variety",varietyTList, multiple = TRUE, selected = varietyTList[2])
    selectInput("inputVariety", "Choose Variety",varietyTList, multiple = TRUE, selected = NULL)
  })
  
  observe({
    if ("All Varieties" %in% input$inputVariety) {
      # choose all the choices _except_ "Select All"
      varietyTList = levels(factor(bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity)  & (bigDataFile$Market %in% input$inputMarket),]$Variety))
      updateSelectInput(session, "inputVariety", selected = varietyTList)
    }
  })
  
  varietyList <- reactive({
    input$inputVariety
  })
  
  output$inputMarket <- renderUI({
    
    marketTList = levels(factor(bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity),]$Market)) 
    marketTList <- c( "All Markets", marketTList)
            
    selectInput("inputMarket", "Choose Market",marketTList, multiple = TRUE, selected=NULL)
    #selectInput("inputMarket", "Choose Market",marketTList, multiple = TRUE, selected=NULL)
  })
  
  observe({
    if ("All Markets" %in% input$inputMarket) {
      # choose all the choices _except_ "Select All"
      marketTList = levels(factor(bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity),]$Market)) 
      updateSelectInput(session, "inputMarket", selected = marketTList)
    }
  })
  
  
  marketList <- reactive({
    input$inputMarket
  })
  
  output$inputGrade <- renderUI({
    
    gradeTList = levels(factor(bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Market %in% input$inputMarket),]$Grade))
    gradeTList <- c( "All Grades", gradeTList)
    #selectInput("inputGrade", "Choose Grade",gradeTList, multiple = TRUE, selected=gradeTList[2])
    selectInput("inputGrade", "Choose Grade",gradeTList, multiple = TRUE, selected=NULL) 
  })
  
  output$inputYear <- renderUI({
    yearList = sort(unique(lubridate::year(bigDataFile$Date)), decreasing = TRUE)
    selectInput("inputYear", "Choose Year", yearList, selected=NULL)
  })
  
  observe({
    if ("All Grades" %in% input$inputGrade) {
      # choose all the choices _except_ "Select All"
      gradeTList = levels(factor(bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Market %in% input$inputMarket),]$Grade))
      updateSelectInput(session, "inputGrade", selected = gradeTList)
    }
  })
  
  
  gradeList <- reactive({
    input$inputGrade
  })
  
  output$periodheading <- renderText({paste("Period")})       
  output$arrivalheading <- renderText({paste("Arrivals")})
  output$modalheading <- renderText({paste("Modal Price")})
  output$mspheading <- renderText({paste("MSP")})
  output$costa1flheading <- renderText({paste("Cost A1+FL")})
  output$costc2heading <- renderText({paste("C2")})
  output$totalc3heading <- renderText({paste("C3")})
  
  output$dataTableHeading <- renderText({paste("Filtered Data Table")})
  output$freqencyTableHeading <- renderText({paste("Frequency of Arrivals in Markets")})
  output$drawAccuracyHeading <- renderText({paste("Price Forcast Accuracy")})
  output$drawARIMAAccuracyHeading <- renderText({paste("Price Forecast Accuracy")})
  output$drawHYBRIDAccuracyHeading <- renderText({paste("Price Forecast Accuracy")})
  output$ANNFuturePlotHeading <- renderText({paste("Price Forecast from 30 October 2017")})
  output$ANNFuturePredictionsHeading <- renderText({paste("Price Forecast")})
  output$ArimaFuturePredictionsHeading <- renderText({paste("Price Forecast")})
  output$HybridFuturePredictionsHeading <- renderText({paste("Price Forecast")})
  output$ARIMAFuturePlotHeading <- renderText({paste("Forecast from 30 October 2017")})
  output$dataTableCheckHeading <- renderText({paste("Data Table")})
  output$dataTableCheckHeading1 <- renderText({paste("Data Table")})
  output$dataTableCheckHeading2 <- renderText({paste("Data Table")})
  
  output$SarimaForTrainerViewHeading <- renderText({paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity)})
  output$SarimaForTrainerViewHeading1 <- renderText({paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity)})
  output$SarimaForTrainerViewHeading2 <- renderText({paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity)})
  output$SarimaForVarietyArrivalHeading <- renderText({paste("Traded Varieties in Descending Order")})
  output$SarimaForMarketArrivalHeading <- renderText({paste("Total Market Arrivals in Descending Order")})
  output$SarimaForGradeArrivalHeading <- renderText({paste("Grade-wise Arrivals in Descending Order")})
  output$priceAnalysisText <- renderText({paste("Arrivals Vs. Modal Plot")})
  output$forecastHeading <- renderText({paste("Price Training Period :",format(input$HistDateRange[1], "%d-%m-%Y")," - ",format(input$HistDateRange[2], "%d-%m-%Y"))})
  output$AForecastHeading <- renderText({paste("Arrival Seasonal Index")})
  output$AForecastHeadingArima <- renderText({paste("Arrival Seasonal Index")})
  output$ARIMAforecastHeading <- renderText({paste("Price Training Period :",format(input$HistDateRange[1], "%d-%m-%Y")," - ",format(input$HistDateRange[2], "%d-%m-%Y"))})
  output$HYBRIDforecastHeading <- renderText({paste("Price Training Period :",format(input$HistDateRange[1], "%d-%m-%Y")," - ",format(input$HistDateRange[2], "%d-%m-%Y"))})
  output$analyticsHeading <- renderText({paste("Period-wise comparison of Arrivals and Modal Price")})
  output$TechnicalAnalysis <- renderText({paste("Technical Analysis")})
  output$MSPMappingHeading <- renderText({paste("Commodity Arrival Superimposed With Modal Prices, MSP, Cost (A1+FL) , Cost (C2) & Total Cost (C3)")})
  output$BiggestMarketAvgModalTitle <- renderText({paste("Market with highest average Modal Price")})
  anvitaUrl <- a("ANVITA, Bengaluru", href="https://anvita.org.in/")
  kvafsuUrl <- a("School of Food Business, Dairy Science College, Karnataka Veterinary Animal & Fisheries Sciences University, Bidar", href="www.kvafsu.edu.in")
  rIssue <- actionLink("reportIssue", "Feedback")
  
  output$WeeklyReportHeading <- renderText({ 
    LastSaturdayDate <- format(Sys.Date() - wday(Sys.Date()), "%d-%m-%Y")
    paste("Report of Arrivals and Price until week ending",LastSaturdayDate)
    })
  
  
  output$ImpCommodities <- renderText({
    
    #Find Last week's dates
    
    endLastWeek <- Sys.Date() - wday(Sys.Date())
    startLastWeek <- endLastWeek - 5
    #Filter those for which MSP is smaller than modal
    
    ImpCommodityList <- c("Paddy", "Jowar", "Ragi", "Maize", "Tur", "Greengram", "Blackgram", "Groundnut", "Sunflower", "Soyabeen", "Gingelly", "Gurellu", "Cotton")
    newCommodityList <- ImpCommodityList
    
    #Display Names
    for (i in 1:length(ImpCommodityList)){
      #print (ImpCommodityList[i])
      currentCommodity = ImpCommodityList[i]
      bigDataFile1 = bigDataFile
     
      bigDataSubFile <- bigDataFile[(bigDataFile$Commodity == currentCommodity) & (bigDataFile$Date >= startLastWeek) & (bigDataFile$Date <= endLastWeek),]
      #save(list = ls(all.names = TRUE), file = "comm.RData")
      if(nrow(bigDataSubFile) > 0){
        if (mean(bigDataSubFile$Modal) > mean(bigDataSubFile$MSP)){
          newCommodityList[i]=""
        }
      }else{
        newCommodityList[i]=""
      }
    }
   idx = which(newCommodityList == "")
   newCommodityList = newCommodityList[-idx]
   commList <- paste(newCommodityList, collapse = ",")
   paste("Commodites whose price exceeded MSP :",commList)
  })
  
  observeEvent(input$reportIssue, {
    showModal(modalDialog(
      title = "Feedback",
      tags$p("Please send an email in the following format."),
      tags$p(tags$b("To"),": agripricecommision@gmail.com"),
      tags$p(tags$b("Cc"), ": vedandri@gmail.com, vhbs@anvita.org.in"),
      tags$p(tags$b("Title"), ": Issue in KRIPA"),
      tags$p(tags$b("Contents"), ": Include a snapshot of the error screen along with other details")
    ))
  })
  output$anvitaText <- renderText({paste(rIssue)})
  output$anvitaEmail <- renderText({paste("Software Designed & Developed as a Service by ",anvitaUrl," in Collaboration with ",kvafsuUrl," | Copyright",icon("copyright"),"ANVITA 2019")})
  output$anvitaText7 <- renderText({paste("https://anvita.org.in/")})
  output$anvitaEmail7 <- renderText({paste("Software Designed & Developed as a Service by ",anvitaUrl," in Collaboration with ",kvafsuUrl," | Copyright",icon("copyright"),"ANVITA 2019")})
  output$anvitaText1 <- renderText({paste("https://anvita.org.in/")})
  output$anvitaEmail1 <- renderText({paste("Software Designed & Developed as a Service by ",anvitaUrl," in Collaboration with ",kvafsuUrl," | Copyright",icon("copyright"),"ANVITA 2019")})
  output$anvitaText2 <- renderText({paste("https://anvita.org.in/")})
  output$anvitaEmail2 <- renderText({paste("Software Designed & Developed as a Service by ",anvitaUrl," in Collaboration with ",kvafsuUrl," | Copyright",icon("copyright"),"ANVITA 2019")})
  output$anvitaText3 <- renderText({paste("https://anvita.org.in/")})
  output$anvitaEmail3 <- renderText({paste("Software Designed & Developed as a Service by ",anvitaUrl," in Collaboration with ",kvafsuUrl," | Copyright",icon("copyright"),"ANVITA 2019")})
  output$anvitaText4 <- renderText({paste("https://anvita.org.in/")})
  output$anvitaEmail4 <- renderText({paste("Software Designed & Developed as a Service by ",anvitaUrl," in Collaboration with ",kvafsuUrl," | Copyright",icon("copyright"),"ANVITA 2019")})
  output$anvitaText5 <- renderText({paste("https://anvita.org.in/")})
  output$anvitaEmail5 <- renderText({paste("Software Designed & Developed as a Service by ",anvitaUrl," in Collaboration with ",kvafsuUrl," | Copyright",icon("copyright"),"ANVITA 2019")})
  output$anvitaText6 <- renderText({paste("https://anvita.org.in/")})
  output$anvitaEmail6 <- renderText({paste("Software Designed & Developed as a Service by ",anvitaUrl," in Collaboration with ",kvafsuUrl," | Copyright",icon("copyright"),"ANVITA 2019")})
  output$anvitaEmail8 <- renderText({paste("Software Designed & Developed as a Service by ",anvitaUrl," in Collaboration with ",kvafsuUrl," | Copyright",icon("copyright"),"ANVITA 2019")})
  
  updateForecastData <- reactive({
    fromDate = as.Date(input$dateRange[1])
    toDate <- as.Date(input$dateRange[2])
    actualDataFile <- getDataWithGrades(fromDate, toDate)
    colnames(actualDataFile) <- c("CommodityGroup", "Commodity", "Market", "Date", "Variety", "Grade", "Arrival", "Modal", "Max", "Min", "ArrivalUnits", "ModalUnits", "MSP", "CostA1FL", "CostC2", "TotalC3")
    actualDataFile$Date <- as.Date(actualDataFile$Date,format = "%d/%m/%Y")
    actualDataFile <- actualDataFile[order(actualDataFile$Date),]
    actualDataFile <- actualDataFile[(actualDataFile$CommodityGroup == input$inputCommodityGroup) & (actualDataFile$Commodity == input$inputCommodity) & (actualDataFile$Market %in% input$inputMarket) & (actualDataFile$Variety %in% input$inputVariety ) & (actualDataFile$Grade %in% input$inputGrade ),]
    actualDataFile
  })
  
  updateDataFile <- reactive({
    try({
      
    #Update DataFile if Reference Dates change
    startDate = as.Date(input$MSPDateRange[1])
    eD <- as.Date(input$MSPDateRange[2])
    
    dataFile <- getDataWithGrades(startDate, eD)
    colnames(dataFile) <- c("CommodityGroup", "Commodity", "Market", "Date", "Variety", "Grade", "Arrival", "Modal", "Max", "Min", "ArrivalUnits", "ModalUnits", "MSP", "CostA1FL","CostC2", "TotalC3")
    dataFile$Date <- as.Date(dataFile$Date,format = "%d/%m/%Y")
    dataFile <- dataFile[order(dataFile$Date),]
    dataFile
    })
    
  })

  updateTrainDataFile <- reactive({
    
    #Update Training DataFile if Historic Dates change
    startDate = as.Date(input$HistDateRange[1])
    eD <- as.Date(input$HistDateRange[2])
    
    trainDataFile <- getDataWithGrades(startDate, eD)
    
    colnames(trainDataFile) <- c("CommodityGroup", "Commodity", "Market", "Date", "Variety", "Grade", "Arrival", "Modal", "Max", "Min", "ArrivalUnits", "ModalUnits", "MSP", "CostA1FL", "CostC2","TotalC3")
    
    trainDataFile$Date <- as.Date(trainDataFile$Date,format = "%d/%m/%Y")
    
    trainDataFile <- trainDataFile[order(trainDataFile$Date),]
    
    #trainDataFile <- unique(trainDataFile)
    
    trainDataFile
  })
  

  dataInput <- reactive({
    
    dataFile <- updateDataFile()
    
    validate(
      need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
    
    validate(
      need(input$inputCommodity != "", "Please select a Commodity"))
    
    validate(
      need(input$inputMarket != "", "Please select a Market"))
    
    validate(
      need(input$inputVariety != "", "Please select a Variety"))             
    
   
    dataFileS <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity) & (dataFile$Market %in% input$inputMarket) & (dataFile$Variety %in% input$inputVariety ),]
    rm(dataFile)
    
    if (nrow(dataFileS) > 0){
      dataFileS$Modal <- tsclean(dataFileS$Modal, replace.missing = TRUE, lambda= NULL)}
       
    dataFileS
  })
  testDataInput <- reactive({
    dataFileSD1 <- bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Market %in% input$inputMarket) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Grade %in% input$inputGrade ),]
    
    fromDate <- input$dateRange[1]
    toDate <- input$dateRange[2]
    
    testingData <- dataFileSD1[dataFileSD1$Date >= fromDate & dataFileSD1$Date <= toDate,]
  })
  trainDataInput <- reactive({
  
    trainDataFile <- updateTrainDataFile()
    #save(list = ls(all.names = TRUE), file = "DF.RData")
    
#     if (!is.null(input$inputCommodityGroup)){
#       dataFileS <- dataFileS[(trainDataFile$CommodityGroup == input$inputCommodityGroup),]
#     }
#     if (!is.null(input$inputCommodity)){
#       dataFileS <- dataFileS[(trainDataFile$inputCommodity == input$inputCommodity),]
#     }
#     if(!is.null(input$inputMarket)){
#       dataFileS <- dataFileS[(trainDataFile$Market == input$inputMarket),]
#     }
#     if(!is.null(input$inputVariety)){
#       dataFileS <- dataFileS[which(input$inputVariety %in% trainDataFile$Variety),]
#     }
#     if(!is.null(input$inputGrade)){
#       dataFileS <- dataFileS[(trainDataFile$Grade == input$inputGrade), ]
#     }
 
    dataFileS <- trainDataFile[(trainDataFile$CommodityGroup == input$inputCommodityGroup) & (trainDataFile$Commodity == input$inputCommodity) & (trainDataFile$Market %in% input$inputMarket) & (trainDataFile$Variety %in% input$inputVariety ) & (trainDataFile$Grade %in% input$inputGrade),]
    rm(trainDataFile)
    if (nrow(dataFileS) > 0){
      dataFileS$Modal <- tsclean(dataFileS$Modal, replace.missing = TRUE, lambda= NULL)}
    dataFileS
  })
 
  
 
  observe({ 
    
    getFname <- reactive({
    fname=paste0("Data_", input$inputCommodity)
    if (length(input$inputMarket)>0)
    {
      if (length(input$inputMarket)==1){
        fname <- paste0(fname,"_",input$inputMarket)
      }else{
     # Market <- paste0(input$inputMarket,collapse="_")
      fname <- paste0(fname,"_MultipleMarkets")
      }
    }
    if (length(input$inputVariety)>0 ){
      if (length(input$inputVariety) == 1 ){
        fname <- paste0(fname,"_",input$inputVariety)
      }else{
      #Variety <- paste0(input$inputVariety,collapse="_")
      fname <- paste0(fname,"_MultipleVarieties")
      }
    }
    })
    
    
    options(DT.options = list(
      lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
      "dom" = 'lfrtip<"clear">B',
      buttons = list(list(extend = "csv", filename = getFname()))
    ))
   
    output$dynamicHistoricDate <- renderText({
      paste("Data Available from ",format(minDate,"%d-%m-%Y"), sep = ,"  to  ",format(maxDate,"%d-%m-%Y"))    })
    
    output$dynamicHistoricDate1 <- renderText({
      paste(format(minDate,"%d-%m-%Y"), sep = ,"   to   ",format(maxDate,"%d-%m-%Y"))    })
    
    output$dynamicText <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList()))
    })
    output$dynamicText1 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList()))
    })
    output$dynamicText2 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList()))
    })
    output$dynamicText3 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText4 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText44 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText44Arima <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText44Hybrid <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText5 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText6 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText7 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText71 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText8 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText81 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText9 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText91 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$dynamicText10 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList()))
    })
    output$dynamicText20 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList()))
    })
    output$dynamicText21 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList()))
    })
    output$dynamicText30 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList()), ", Market: ", toString(marketList()))
    })
    output$dynamicText30Arima <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList()), ", Market: ", toString(marketList()))
    })
    
    output$dynamicText31 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList()), ", Market: ", toString(marketList()))
    })
    
    output$dynamicText40 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList()))
    })
    
    output$dynamicText50 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList()))
    })
    
    output$dynamicRange50 <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1],"%d-%m-%Y"),"  to  ",format(input$MSPDateRange[2],"%d-%m-%Y"))
    })
    
    output$dynamicRange60 <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1], "%d-%m-%Y"), "  to  ",format(input$MSPDateRange[2],"%d-%m-%Y"))
    })
    
    output$dynamicRange602 <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1], "%d-%m-%Y"), "  to  ",format(input$MSPDateRange[2],"%d-%m-%Y"))
    })
    
    output$dynamicRange40 <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1], "%d-%m-%Y"), "  to  ",format(input$MSPDateRange[2],"%d-%m-%Y"))
    })
    
    output$dynamicRange30 <- renderText({
      paste("Forecast Dates:",format(input$dateRange[1], "%d-%m-%Y"), "  to  ",format(input$dateRange[2],"%d-%m-%Y"))
    })
    output$dynamicRange30Arima <- renderText({
      paste("Forecast Dates:",format(input$dateRange[1], "%d-%m-%Y"), "  to  ",format(input$dateRange[2],"%d-%m-%Y"))
    })
    
    output$dynamicRange302 <- renderText({
      paste("Forecast Dates:",format(input$dateRange[1], "%d-%m-%Y"), "  to  ",format(input$dateRange[2],"%d-%m-%Y"))
    })
    output$dynamicRange302Arima <- renderText({
      paste("Forecast Dates:",format(input$dateRange[1], "%d-%m-%Y"), "  to  ",format(input$dateRange[2],"%d-%m-%Y"))
    })
    output$dynamicRange302Hybrid <- renderText({
      paste("Forecast Dates:",format(input$dateRange[1], "%d-%m-%Y"), "  to  ",format(input$dateRange[2],"%d-%m-%Y"))
    })
    output$dynamicRange303 <- renderText({
      paste("Forecast Dates:",format(input$dateRange[1], "%d-%m-%Y"), "  to  ",format(input$dateRange[2],"%d-%m-%Y"))
    })
    output$dynamicRange303Arima <- renderText({
      paste("Forecast Dates:",format(input$dateRange[1], "%d-%m-%Y"), "  to  ",format(input$dateRange[2],"%d-%m-%Y"))
    })
    output$dynamicRange303Hybrid <- renderText({
      paste("Forecast Dates:",format(input$dateRange[1], "%d-%m-%Y"), "  to  ",format(input$dateRange[2],"%d-%m-%Y"))
    })
    output$dynamicRange304 <- renderText({paste("Price Training Period : ",format(input$HistDateRange[1], "%d-%m-%Y")," - ",format(input$HistDateRange[2], "%d-%m-%Y"))})
    output$dynamicRange304Arima <- renderText({paste("Price Training Period : ",format(input$HistDateRange[1], "%d-%m-%Y")," - ",format(input$HistDateRange[2], "%d-%m-%Y"))})
    output$dynamicRange304Hybrid <- renderText({paste("Price Training Period : ",format(input$HistDateRange[1], "%d-%m-%Y")," - ",format(input$HistDateRange[2], "%d-%m-%Y"))})
    
    output$HistoricDataSubsetHeading <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1], "%d-%m-%Y"), "  to  ",format(input$MSPDateRange[2],"%d-%m-%Y"))
    })
    output$HistoricDataSubsetHeading1 <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1], "%d-%m-%Y"), "  to  ",format(input$MSPDateRange[2],"%d-%m-%Y"))
    })
    
    endDate = as.Date(input$MSPDateRange[2])
    startDate = endDate %m-% months(5)
    
    output$ReferenceRangeForTA <- renderText({
      paste("Reference Dates:",format(startDate, "%d-%m-%Y"), "  to  ",format(endDate,"%d-%m-%Y"))
    })
    output$HistoricDataSubsetHeading2 <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1], "%d-%m-%Y"), "  to  ",format(input$MSPDateRange[2],"%d-%m-%Y"))
    })
    output$HistoricDataSubsetHeading3 <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1], "%d-%m-%Y"), "  to  ",format(input$MSPDateRange[2],"%d-%m-%Y"))
    })
    output$HistoricDataSubsetHeading4 <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1], "%d-%m-%Y"), "  to  ",format(input$MSPDateRange[2],"%d-%m-%Y"))
    })
    output$HistoricDateRange <- renderText({
      paste("Reference Dates:",format(input$MSPDateRange[1], "%d-%m-%Y"), "  to  ",format(input$MSPDateRange[2], "%d-%m-%Y"))
    })
    
    output$dynamicText71 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$analyticsHeading1 <- renderText({paste("Comparison of current year with Previous Arrivals and Modal Price")})
    
    output$dynamicText72 <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$analyticsHeading2 <- renderText({paste("Monthly Arrivals and Modal Price for year ", toString(input$inputYear))})
    
    output$dynamicText73<- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$analyticsHeading3 <- renderText({paste("Period-wise comparison of Arrivals,Modal Price, MSP and Cost")})
   
    output$dynamicText74<- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    
    output$dynamicTextWeeklyReport <- renderText({
      paste("Commodity Group: ",input$inputCommodityGroup, sep = " ",", Commodity: ",input$inputCommodity,", Variety: ", toString(varietyList())," , Market: ", toString(marketList())," , Grade: ", toString(gradeList()))
    })
    output$analyticsHeading4 <- renderText({paste("Percentage of Difference in Arrivals and Modal price w.r.t. previous year")})
    
    output$ArimaWarning <- renderText({
      if (length(input$inputGrade) > 0){
      if ((length(input$inputMarket) > 1) | (length(input$inputVariety) > 1) | (length(input$inputGrade) > 1)){
        paste("Note: Forecasts are accurate when data over single market, variety, Grade is considered. It is strongly advised to consider 
              the forecast along with GARCH and Technical analysis results for a more reliable prediction.")}else{
                paste("Note: It is strongly advised to consider 
              the forecast along with GARCH and Technical analysis results for a more reliable prediction.")
              }
      }
    })
    
    output$ANNWarning <- renderText({
      if (length(input$inputGrade) > 0){
      if ((length(input$inputMarket) == 1) | (length(input$inputVariety) == 1) | (length(input$inputGrade) == 1)){
        paste("Note: Forecasts are accurate when data over single market, variety, Grade is considered. It is strongly advised to consider 
              the forecast along with Technical analysis results for a more reliable prediction.")}else{
                paste("Note: It is strongly advised to consider 
                the forecast along with Technical analysis results for a more reliable prediction.")
              }
      }
    })
    
    output$acfpacfplots <- renderText({
      if (input$sarimaFord == 0){
        if (input$sarimaForD == 0){
          #acf
          paste("1")
        }else{
          #acf(diff(diff())),pacf(diff(diff()))
          paste("2")
        }
      }else{
        if (input$sarimaForD == 0){
          #acf(diff), pacf(diff())
          paste("3")
        }else{
          #acf(diff(diff())),pacf(diff(diff()))
          paste("4")
        }
      }
      
    })
    
    output$NoOfEntriesTable <- DT::renderDataTable({
      
      #message("inside no of entriestable")
      #start_time <- Sys.time()
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
    
      validate(
        need(input$inputVariety != "", "Please select a Variety"))    
      
      dataFile <- updateDataFile()
      dataFileNo <- dataFile[dataFile$CommodityGroup == input$inputCommodityGroup & dataFile$Commodity == input$inputCommodity & (dataFile$Variety %in% input$inputVariety ) & dataFile$Date >= input$MSPDateRange[1] & dataFile$Date <= input$MSPDateRange[2], ]
      rm(dataFile)
      entries <- plyr::count(dataFileNo, "Market")
      rm(dataFileNo)
      colnames(entries)[2] <- "Frequency"
      #entries[order(-entries$Frequency),]
      entriesTable <- data.table(entries[order(-entries$Frequency),])
      #entriesTable[order(-entries$Frequency),]
      #message(nrow(entriesTable))
          end_time <- Sys.time()
          #message(paste0("inside no of entriestable:",end_time - start_time))
      entriesTable 
    }, 
    extensions = 'Buttons')
    
    
    output$drawTable <- DT::renderDataTable({
     
      #message("Inside drawtable")
      start_time <- Sys.time()
      dataFileMI <- dataInput()[dataInput()$Date >= input$MSPDateRange[1] & dataInput()$Date <= input$MSPDateRange[2], ]
      dataFileMI <- subset(dataFileMI, select = -c(ArrivalUnits,ModalUnits,MSP,CostA1FL, CostC2, TotalC3) )

            #message(nrow(dataFileMI))
          end_time <- Sys.time()
          #message(paste0("Inside drawtable:",end_time - start_time))
      datatable(dataFileMI, rownames=FALSE) %>% formatDate(4, method = 'toLocaleDateString', params = list('fr-FR'))
    }, 
    extensions = c("Buttons"))
    
    output$drawTableCheck <- DT::renderDataTable({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      validate(
        need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))  
      
      validate(
        need(length(input$inputGrade) == 1, "Please select a single Grade"))     
      
      #message("drawTableCheck")
      start_time <- Sys.time()
      dataFileMI <- trainDataInput()
      
      dataFileMI <- subset(dataFileMI, select = -c(ArrivalUnits,ModalUnits,MSP,CostA1FL,CostC2, TotalC3) )
      
      #message(nrow(dataFileMI))
          end_time <- Sys.time()
          #message(paste0("drawTableCheck:",end_time - start_time))
      datatable(dataFileMI, rownames=FALSE) %>% formatDate(4, method = 'toLocaleDateString', params = list('fr-FR'))
      }, 
      extensions = c("Buttons"))
    
    output$drawTableARIMA <- DT::renderDataTable({
      #message("Inside drawTableArima")
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))      
      start_time <- Sys.time()
      dataFileMI <- trainDataInput()
      
      dataFileMI <- subset(dataFileMI, select = -c(ArrivalUnits,ModalUnits,MSP,CostA1FL, CostC2,TotalC3) )
      #message(nrow(dataFileMI))
          end_time <- Sys.time()
          #message(paste0("Inside drawTableArima:",end_time - start_time))
    datatable(dataFileMI, rownames=FALSE) %>% formatDate(4, method = 'toLocaleDateString', params = list('fr-FR'))
    }, 
    extensions = c("Buttons"))
    
       #dataFileSD <- dataInput()[dataInput()$Date >= ActualDurationStartDate & dataInput()$Date <= ActualDurationEndDate, ]
    
    
      dataFileSD <- trainDataInput()
      testingData <- testDataInput()
      
      params <- reactive({
        future({calcPDQpdq(dataFileSD, testingData)})
      })
      
      params() %...>%{updateTextInput(session, "sarimaForp", value = .$p)}
      params() %...>%{updateTextInput(session, "sarimaForq", value = .$q)}
      params() %...>%{updateTextInput(session, "sarimaFord", value = .$d)}
      params() %...>%{updateTextInput(session, "sarimaForP", value = .$P)}
      params() %...>%{updateTextInput(session, "sarimaForQ", value = .$Q)}
      params() %...>%{updateTextInput(session, "sarimaForD", value = .$D)}
      params() %...>%{updateTextInput(session, "arimaPeriod", value = .$arimaPeriod)}
      
      output$pdqText <- renderText({ paste("p: ",input$sarimaForp, sep = " ",", d: ",input$sarimaFord, ", q: ",input$sarimaForq, ", P: ",input$sarimaForP, ", D: ",input$sarimaForD, ", Q: ",input$sarimaForQ, ", Period: ",input$arimaPeriod)})
      
      originalDataDates <<-  dataFileSD$Date
      originalData <- dataFileSD$Modal
      arrivalOriginal <<- dataFileSD$Arrival
      trainingDataDates <<- originalDataDates
      originalDataFrame <- data.frame(originalDataDates,originalData)
      arrivalOriginalDataFrame <- data.frame(originalDataDates,arrivalOriginal)
    
    NNForecast <- reactive({
 
      validate(
        need(nrow(dataFileSD)>10, "Generating output... please wait. If this message does not disappear in two minutes, it is most likely due to insufficient data. Please select another variety/market"))
      
      #print(paste("Going for Lookup: ", dataFileSD$CommodityGroup[1],dataFileSD$Commodity[1], dataFileSD$Variety[1], dataFileSD$Market[1], dataFileSD$Grade[1]))
      lookupMethod <- forecastLookup(dataFileSD$CommodityGroup[1], dataFileSD$Commodity[1], dataFileSD$Variety[1], dataFileSD$Market[1], dataFileSD$Grade[1])
      #print(paste("***",lookupMethod,"***"))
      lookupMethodAggregate <- forecastLookup(dataFileSD$CommodityGroup[1], dataFileSD$Commodity[1], dataFileSD$Variety[1], dataFileSD$Market[1], dataFileSD$Grade[1])
      trainingData <- dataFileSD
      
      
      if (lookupMethodAggregate == 'No'){
        Iteration = 1
      }
      dataFileSD1 <- bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Market %in% input$inputMarket) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Grade %in% input$inputGrade ),]
      
      fromDate <- input$dateRange[1]
      toDate <- input$dateRange[2]
      
      predictingDataDates <- bizseq(fromDate,toDate, "actual")
      noOfPeriodsToForecast <- length(predictingDataDates)
      
      testingData <- dataFileSD1[dataFileSD1$Date >= fromDate & dataFileSD1$Date <= toDate,]
      validationData <-  dataFileSD1[dataFileSD1$Date >= validationStartDate & dataFileSD1$Date <= validationEndDate,]
      
      #save(list = ls(all.names = TRUE), file = "Before Switch.RData")
      sapply(lookupMethod, switch,
             "Jasma1.1.ETSv1" = {
               #print("Jasma1.1.ETSv1")
               outputList <- Jasma1.1.ETSv1(trainingData, testingData, predictingDataDates)
             },
             "Jasma1.2.ETSv2" = {
               #print("Jasma1.2.ETSv2")
               outputList <- Jasma1.2.ETSv2(trainingData, testingData, predictingDataDates)
             },
             "Jasma1.3.ETSv3" = {
               #print("Jasma1.3.ETSv3")
               outputList <- Jasma1.3.ETSv3(trainingData, testingData, predictingDataDates)
             },
             "Jasma1.4.ETSv4" = {
               #print("Jasma1.4.ETSv4")
               outputList <- Jasma1.4.ETSv4(trainingData, testingData, predictingDataDates)
             },
             "Jasma2.1BATSv1" = {
               #print(Jasma2.1BATSv1)
               validationData <-  dataFileSD1[dataFileSD1$Date >= trainingDurationStartDate & dataFileSD1$Date <= trainingDurationEndDate,]
               #outputList <- Ashok1.1BATSv1(trainingData, testingData, validationData, predictingDataDates)
               outputList <- Jasma2.1BATSv1(trainingData, testingData, predictingDataDates)
             },
             "Jasma3.1.ARFIMAv1" = {
               #print("Jasma3.1.ARFIMAv1")
               outputList <- Jasma3.1.ARFIMAv1(trainingData, testingData, predictingDataDates)
             },
             "Jasma5.1.BaggedModelv1" = {
               #print(Jasma5.1.BaggedModelv1)
               outputList <- Jasma5.1.BaggedModelv1(trainingData, testingData, predictingDataDates)
             },
             "Jasma6.1.BaggedETSv1" = {
               #print("Jasma6.1.BaggedETSv1")
               outputList <- Jasma6.1.BaggedETSv1(trainingData, testingData, predictingDataDates)
             },
             "Accamma2.1.ETSv1" = {
               #print("Accamma2.1.ETSv1")
               outputList <- Accamma2.1.ETSv1(trainingData, testingData, predictingDataDates)
             },
             "Accamma2.2.ETSv2" = {
               #print("Accamma2.2.ETSv2")
               outputList <- Accamma2.2.ETSv2(trainingData, testingData, predictingDataDates)
             },
             "Accamma3.1.BaggedModelv1" = {
               #print("Accamma3.1.BaggedModelv1")
               outputList <- Accamma3.1.BaggedModelv1(trainingData, testingData, predictingDataDates)
             },
             "Accamma4.1.BaggedETSv1" = {
               #print("Accamma4.1.BaggedETSv1")
               outputList <- Accamma4.1.BaggedETSv1(trainingData, testingData, predictingDataDates)
             },
             "Accamma5.1.StructTSv1" = {
               #print("Accamma5.1.StructTSv1")
               outputList <- Accamma5.1.StructTSv1(trainingData, testingData, predictingDataDates)
             },
             "vhbs1.BaggedETS.BaggedModelv1" = {
               #print("vhbs1.BaggedETS.BaggedModelv1")
               outputList <- vhbs1.BaggedETS.BaggedModelv1(trainingData, testingData, predictingDataDates)
             },
             "Ashok1.1BATSv1" = {
               #print("Ashok1.1BATSv1")
               outputList <- Ashok1.1BATSv1(trainingData, testingData, validationData, predictingDataDates)
             },
             "Ashok2.1.ETSv1" = {
               #print("Ashok2.1.ETSv1")
               outputList <- Ashok2.1.ETSv1(trainingData, testingData, validationData, predictingDataDates)
             },
             "Ashok2.2.ETSv2" = {
               #print("Ashok2.2.ETSv2")
               outputList <- Ashok2.2.ETSv2(trainingData, testingData, validationData, predictingDataDates)
             },
             "Ashok3.1.BaggedModelv1" = {
               #print("Ashok3.1.BaggedModelv1")
               outputList <- Ashok3.1.BaggedModelv1(trainingData, testingData, validationData, predictingDataDates)
             },
             "Ashok4.1.BaggedETSv1" = {
               #print("Ashok4.1.BaggedETSv1")
               outputList <- Ashok4.1.BaggedETSv1(trainingData, testingData, validationData, predictingDataDates)
             },
             "Ashok5.1.StructTSv1" = {
               #print("Ashok5.1.StructTSv1")
               outputList <- Ashok5.1.StructTSv1(trainingData, testingData, validationData, predictingDataDates)
             },
             "Ashok.BaggedETS.BaggedModelv1" = {
               #print("Ashok.BaggedETS.BaggedModelv1")
               outputList <- Ashok.BaggedETS.BaggedModelv1(trainingData, testingData, validationData, predictingDataDates)
             },
             "Ashok.BaggedETS.BaggedETSv2" = {
               #print("Ashok.BaggedETS.BaggedETSv2")
               outputList <- Ashok.BaggedETS.BaggedETSv2(trainingData, testingData, validationData, predictingDataDates)
             },
             "AnvitaRegression" = {
               #print("AnvitaRegression")
               outputList <- AnvitaRegression(trainingData, testingData, validationData, predictingDataDates)
             },
             "AnvitaNN" ={
               #print("AnvitaNN")
               outputList <- AnvitaNN(trainingData, testingData, validationData, predictingDataDates)
             },
             "AnvitaSVM" ={
               #print("AnvitaSVM")
               outputList <- AnvitaSVM(trainingData, testingData, validationData, predictingDataDates)
             },
             "AnvitaLogisticRegression" ={
               #print("AnvitaLogisticRegression")
               outputList <- AnvitaLogisticRegression(trainingData, testingData, validationData, predictingDataDates)
             },
             "AnvitaRandomForest" ={
               #print("AnvitaRandomForest")
               outputList <- AnvitaLogisticRegression(trainingData, testingData, validationData, predictingDataDates)
             },
             "AnvitaDecisionTree"={
               #print("AnvitaDecisionTree")
               outputList <- AnvitaDecisionTree(trainingData, testingData, validationData, predictingDataDates)
             },
             {
               #outputList <- Traditional(trainingData, testingData, predictingDataDates)
               #print("DEfault Case: Ashok1.1BATSv1")
               outputList <- Ashok1.1BATSv1(trainingData, testingData, validationData, predictingDataDates)
             }
      )
      outputList
    })
    
    ArimaForecast <- reactive({
     
      #message("Inside Arima reactive")
      start_time <- Sys.time()
      #message(nrow(dataFileSD))
      
      #save(list = ls(all.names = TRUE), file = "ArimaData.RData")
      
      validate(
        need(nrow(dataFileSD)>10, "Generating output... please wait. If this message does not disappear in two minutes, it is most likely due to insufficient data. Please select another variety/market"))
      
      trainingData <- ts(dataFileSD$Modal,frequency =as.numeric(input$arimaPeriod))
      trainingDataSarimaFor <-  Arima(dataFileSD$Modal,order = c(as.numeric(input$sarimaForp),as.numeric(input$sarimaFord),as.numeric(input$sarimaForq)),seasonal=list(order=c(as.numeric(input$sarimaForP),as.numeric(input$sarimaForD),as.numeric(input$sarimaForQ)), period=as.numeric(input$arimaPeriod)),method = "CSS", optim.method = "BFGS")
          end_time <- Sys.time()
          #message(paste0("Inside Arima reactive:",end_time - start_time))
      trainingDataSarimaFor
    })
    
    output$AFPlot <- renderPlotly({
      
        #message("Inside AFP Plot")
      start_time <- Sys.time()
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      # validate(
      #   need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      # validate(
      #   need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))   
      
      # validate(
      #   need(length(input$inputGrade) == 1, "Please select a single Grade"))    
      
      trainingDataSarimaFor <-  ArimaForecast()
      trainingDataFrameSarimaFor <- data.frame(trainingDataDates,fitted(trainingDataSarimaFor))
      #save(list = ls(all.names = TRUE), file = "AFP.RData")
          
      plot_ly(originalDataFrame,x = ~originalDataDates,y = ~originalData, name = 'Historic Price Data', type = 'scatter', mode = 'lines', line = list(color = 'orange')) %>%
        layout(xaxis = a, yaxis = b) %>%
        add_trace(trainingDataFrameSarimaFor,x = ~trainingDataDates,y = ~fitted(trainingDataSarimaFor),name = 'Arima Fitted On Historic Price Data',mode = 'lines', line = list(color = 'blue'))
     
    })
    
    
    
    output$FAEPlot <- renderPlotly({
    
      #message(("Inside FAEPlot"))
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      validate(
        need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))  
      
      validate(
        need(length(input$inputGrade) == 1, "Please select a single Grade"))            
      
      start_time <- Sys.time()
      #trainingDataNnetar <- forecast(NNForecast())
      
      output <- NNForecast()
      trainingDataNnetar <- as.list(output[1][[1]])
      predictingDataFrameNnetar <- as.data.frame(output[2])
      
      trainingDataFrameNnetar <- data.frame(trainingDataDates,trainingDataNnetar$fitted)
      
          end_time <- Sys.time() 
          #message(paste0("Inside FAEPlot:",end_time - start_time))
      plot_ly(originalDataFrame,x = ~originalDataDates,y = ~originalData, name = 'Historic Price Data', type = 'scatter', mode = 'lines', line = list(color = 'orange')) %>%
        layout(title = "Price Forecast Training Using All Historic Data",xaxis = a, yaxis = b) %>%
        add_trace(trainingDataFrameNnetar,x = ~trainingDataDates,y = ~trainingDataNnetar$fitted,name = 'Neural Networks Fitted On Historic Price Data',mode = 'lines', line = list(color = 'blue'))  
      })
    
    
    output$drawARIMAAccuracy <- DT::renderDataTable({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      # validate(
      #   need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      # validate(
      #   need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))   
      
      # validate(
      #   need(length(input$inputGrade) == 1, "Please select a single Grade"))
      
      fromDate <- input$dateRange[1]
      toDate <- input$dateRange[2]
      
      predictingDataDates <- bizseq(fromDate,toDate,"actual")
      noOfPeriodsToForecast <- length(predictingDataDates)
      
      trainingDataSarimaFor <-  ArimaForecast()
      predictingDataArimaForecast <- forecast(trainingDataSarimaFor,h = noOfPeriodsToForecast)
      predictingDataFrameArima <- data.frame(predictingDataArimaForecast)
      #dataFileSD1 <- bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Market %in% input$inputMarket) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Grade %in% input$inputGrade ),]
      #originalDataL <- dataFileSD1[dataFileSD1$Date >= fromDate & dataFileSD1$Date <= toDate,]$Modal
      originalDataL <- testDataInput()
      trainingDataSarimaForAccuracyRow1 <- data.table("Training Accuracy",accuracy(fitted(trainingDataSarimaFor),originalData))
      trainingDataSarimaForAccuracyRow2 <- data.table("Testing Accuracy",accuracy(predictingDataFrameArima$Point.Forecast,originalDataL$Modal))
      displayAccuracyTable <- rbind(trainingDataSarimaForAccuracyRow1,trainingDataSarimaForAccuracyRow2 )
      #aicdf = data.frame("AIC" = trainingDataSarimaFor$aic)
      #displayAccuracyTable = cbind(displayAccuracyTable,aicdf)
    
    }, 
    extensions = c("Buttons"))
    
   
    
    output$drawAccuracy <- DT::renderDataTable({
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      validate(
        need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))  
      
      validate(
        need(length(input$inputGrade) == 1, "Please select a single Grade"))  
      
      fromDate <- input$dateRange[1]
      toDate <- input$dateRange[2]
      
      predictingDataDates <- seq(fromDate,toDate,by="days")
      noOfPeriodsToForecast <- length(predictingDataDates)
      predictingValDates <- seq(validationStartDate,validationEndDate,by="days")
      
      output <- NNForecast()
      trainingDataNnetar <- as.list(output[1][[1]])
      predictingDataFrameNnetar <- as.data.frame(output[2])
      
      
      dataFileSD1 <- bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Market %in% input$inputMarket) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Grade %in% input$inputGrade ),]
      originalDataL <- dataFileSD1[dataFileSD1$Date >= fromDate & dataFileSD1$Date <= toDate,]$Modal
      originalDataVal <-  dataFileSD1[dataFileSD1$Date >= validationStartDate & dataFileSD1$Date <= validationEndDate,]$Modal
      OG <- originalData
      #save(list = ls(all.names = TRUE), file = "acc.RData")
      
      predictingDataNnetarValForecast <- forecast(trainingDataNnetar,h=length(predictingValDates), robust = TRUE, biasadj = TRUE, bootstrap = TRUE, xreg=trainingDataNnetar$xreg)
      predictingDataNnetarValForecast <- data.frame(predictingDataNnetarValForecast)
      predictingDataNnetarValForecast <- data.frame(predictingDataNnetarValForecast[(1:365),])
      colnames(predictingDataNnetarValForecast) <- c("Point.Forecast")
      predictingDataNnetarValForecast <- data.frame(predictingValDates,predictingDataNnetarValForecast$Point.Forecast)
      colnames(predictingDataNnetarValForecast) <- c("predictingDataDates", "Point.Forecast")
      
      trainingDataNnetarAccuracyRow1 <- data.table("Training Accuracy",accuracy(fitted(trainingDataNnetar),originalData))
     # trainingDataNnetarAccuracyRow2 <- data.table("Validation Accuracy",accuracy(predictingDataNnetarValForecast$Point.Forecast,originalDataVal))
      trainingDataNnetarAccuracyRow3 <- data.table("Testing Accuracy",accuracy(predictingDataFrameNnetar$Point.Forecast,originalDataL))
      rm(trainingDataNnetar)
      displayAccuracyTable <- rbind(trainingDataNnetarAccuracyRow1,trainingDataNnetarAccuracyRow3)
      #displayAccuracyTable <- rbind(displayAccuracyTable,trainingDataNnetarAccuracyRow3)
      displayAccuracyTable
     
    }, 
    extensions = c("Buttons"))
    
  
    output$ArimaPredictionsTable <- DT::renderDataTable({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      # validate(
      #   need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      # validate(
      #   need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))   
      
      # validate(
      #   need(length(input$inputGrade) == 1, "Please select a single Grade")) 
      
      
      toDate <- input$dateRange[1]
      fromDate <- input$dateRange[2]
      
      #message("Inside Arima Predictions table")
      predictingDataDates <- bizseq(toDate,fromDate,"actual")
      noOfPeriodsToForecast <- length(predictingDataDates)
      
      trainingDataSarimaFor <-  ArimaForecast()
      predictingDataArimaForecast <- forecast(trainingDataSarimaFor,h = noOfPeriodsToForecast)
      rm(trainingDataSarimaFor)
      predictingDataFrameArima <- data.frame(predictingDataArimaForecast)
      rm(predictingDataArimaForecast)
      predictingDataFrameArimaFit <- data.frame(predictingDataDates,predictingDataFrameArima$Point.Forecast)
      
      #
      Date <- predictingDataFrameArimaFit$predictingDataDates
      ForecastValue_Modal <- round(predictingDataFrameArimaFit$predictingDataFrameArima.Point.Forecast, digits = 0)
      Commodity <- input$inputCommodity
      Variety <- input$inputVariety
      Market <- input$inputMarket

      if ((length(Variety) > 1) || (length(Market) > 1)){
        writeTable <- data.frame(Commodity,Date,ForecastValue_Modal)
        datatable(writeTable, rownames=FALSE) %>% formatDate(2, method = 'toLocaleDateString', params = list('fr-FR'))}
      else {
        writeTable <- data.frame(Commodity,Variety,Market,Date,ForecastValue_Modal)
        datatable(writeTable, rownames=FALSE) %>% formatDate(4, method = 'toLocaleDateString', params = list('fr-FR'))}

      #save(list = ls(all.names = TRUE), file = "outl.RData")
      
     
    }, 
    extensions = c("Buttons"))
   
    
    output$ARIMAFuturePlot <- renderPlotly({
      #message("Inside Arima Future Plot")
      toDate <- input$dateRange[1]
      fromDate <- input$dateRange[2]
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      # validate(
      #   need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      # validate(
      #   need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))   
      
      # validate(
      #   need(length(input$inputGrade) == 1, "Please select a single Grade"))
      
      predictingDataDates <- bizseq(toDate,fromDate,"actual")
      noOfPeriodsToForecast <- length(predictingDataDates)
      dataFileTemp = dataFileSD
      trainingDataSarimaFor <-  ArimaForecast()
      predictingDataArimaForecast <- forecast(trainingDataSarimaFor,h = noOfPeriodsToForecast)
      rm(trainingDataSarimaFor)
      predictingDataFrameArima <- data.frame(predictingDataArimaForecast)
      rm(predictingDataArimaForecast)
      predictingDataFrameArima$Point.Forecast = ifelse(predictingDataFrameArima$Point.Forecast < 0, NA ,predictingDataFrameArima$Point.Forecast)
      predictingDataFrameArimaFit <- data.frame(predictingDataDates,predictingDataFrameArima$Point.Forecast)
      
    
      p <- plot_ly(predictingDataFrameArimaFit)
      
      actualDataFile <- updateForecastData()
      
      if (nrow(actualDataFile)>0){
        p <- p %>% add_trace(x = actualDataFile$Date,y = actualDataFile$Modal, name = 'Actual Price' ,type = 'scatter' ,mode = 'lines', yaxis = 'y2', line = list(color = 'purple'))
      }
      
      lastDate = max(actualDataFile$Date)
      
      #Append remaining
      if (nrow(actualDataFile) == 0 ){
        actualDataFile <- bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Market %in% input$inputMarket) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Grade %in% input$inputGrade ),]
        actualDataFile <- actualDataFile[order(actualDataFile$Date),]
        lastDate = actualDataFile$Date[nrow(actualDataFile)]
      }
      
      #Append remaining
      remainingDates <- bizseq(toDate,fromDate,"actual")
      dfRemaining <- data.frame(Date=remainingDates,MSP = actualDataFile$MSP[nrow(actualDataFile)],CostA1FL = actualDataFile$CostA1FL[nrow(actualDataFile)],CostC2 = actualDataFile$CostC2[nrow(actualDataFile)],TotalC3 = actualDataFile$TotalC3[nrow(actualDataFile)] )
      #actualDataFile <- rbind(actualDataFile, dfRemaining, fill=TRUE)
      
      actualDataFile <- dfRemaining
      
      #save(list = ls(all.names = TRUE), file = "after.RData")
      
        if ( ! (is.na(sum(actualDataFile$MSP))) & sum(actualDataFile$MSP) > 0 ){
          p <- p %>% add_trace(x = ~actualDataFile$Date, y = ~actualDataFile$MSP, visible = "legendonly", type = 'scatter', mode = 'lines', name = 'MSP', yaxis = 'y2', line = list(color = 'red'))
        }
        if (! (is.na(sum(actualDataFile$CostA1FL))) &  sum(actualDataFile$CostA1FL) > 0 ){
          p <- p %>% add_trace(x = ~actualDataFile$Date, y = ~actualDataFile$CostA1FL, visible = "legendonly", type = 'scatter', mode = 'lines', name = 'Cost (A1+FL)', yaxis = 'y2', line = list(color = 'green'))
        }
        if (! (is.na(sum(actualDataFile$CostC2))) & sum(actualDataFile$CostC2) > 0 ){
          p <- p %>% add_trace(x = ~actualDataFile$Date, y = ~actualDataFile$CostC2, visible = "legendonly", type = 'scatter', mode = 'lines', name = 'Cost (C2)', yaxis = 'y2', line = list(color = 'rgb(12, 58, 116)'))
        }
        if (! (is.na(sum(actualDataFile$TotalC3))) & sum(actualDataFile$TotalC3) > 0 ){
          p <- p %>% add_trace(x = ~actualDataFile$Date, y = ~actualDataFile$TotalC3, visible = "legendonly", type = 'scatter', mode = 'lines', name = 'Total Cost (C3)', yaxis = 'y2', line = list(color = 'brown'))
        }
      
      ##save(list = ls(all.names = TRUE), file = "before.RData")
      #originalDataFrame1 = originalDataFrame
      #predictingDataDates1 = predictingDataDates
      #filteredData <-   getDataInForecastRange(originalDataFrame,predictingDataDates)
      
      
      

      p <- p %>% add_trace(x = ~predictingDataFrameArimaFit$predictingDataDates, y = ~predictingDataFrameArimaFit$predictingDataFrameArima.Point.Forecast,name = 'ARIMA Forecast' ,type = 'scatter' ,mode = 'lines', yaxis = 'y2', line = list(color = 'blue')) %>%
        layout(title = "Forecast Plot", xaxis = a, yaxis = b) 
      p
    })
    
    getDataInForecastRange <- function(originalDataFrame,predictingDataDates){
      listAsDF=data.frame(listOfDates = as.POSIXct(character()),listOfModal=numeric())
      
      #save(list = ls(all.names = TRUE), file = "season.RData")
      for (lp in 1:length(predictingDataDates))
      {
        aDate = predictingDataDates[lp]
        index = (match(aDate, originalDataFrame$originalDataDates))
        if (!is.na(index)){
          listAsDF = rbind(listAsDF,data.frame(originalDataFrame$originalDataDates[index], originalDataFrame$originalData[index]))
        }
      }
    return (listAsDF)
  }
    output$SarimaForPlotV <- renderPlotly({
      #message("Inside SarimaFor PlotV")
      dataFile <- updateDataFile()
      #par(mar=c(15,4,4,2))
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      
      dataFileC <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity),]
      rm(dataFile)
      
      toMSPCDate <- input$MSPDateRange[1]
      fromMSPCDate <- input$MSPDateRange[2]
      #save(list = ls(all.names = TRUE), file = "season.RData")
      dataFileCC <- dataFileC[dataFileC$Date >= toMSPCDate & dataFileC$Date <= fromMSPCDate,]
      rm(dataFileC)
      arrivalunits <- unique(dataFileCC$ArrivalUnits)
      modalunits <- unique(dataFileCC$ModalUnits)
      
      if(length(dataFileCC$Arrival) > 0)
      {
        dataFileCArrivalsAggregatedByVariety <- aggregate(dataFileCC$Arrival, by = list(dataFileCC$Variety), FUN = sum,na.action =NULL)
        save(list = ls(all.names = TRUE), file = "season.RData")
        dataFileCArrivalsAggregatedByVariety <- dataFileCArrivalsAggregatedByVariety[order(-xtfrm(dataFileCArrivalsAggregatedByVariety$x)),]
        
        dataFileCArrivalsAggregatedByVariety$Group.1 <- factor(dataFileCArrivalsAggregatedByVariety$Group.1, levels = c(as.character(dataFileCArrivalsAggregatedByVariety$Group.1)))
        
        p <- plot_ly(
          x = dataFileCArrivalsAggregatedByVariety$Group.1,
          y = dataFileCArrivalsAggregatedByVariety$x,
          type = "bar",
          color = I("sandybrown")
        ) %>%
          layout(margin = list(b = 140), yaxis = list(title = paste0("Arrival Quantity In ",arrivalunits), 
                              #  tickformat = "f", 
                              range = c(0,dataFileCArrivalsAggregatedByVariety$x[1] + (dataFileCArrivalsAggregatedByVariety$x[1]/3))), 
                 xaxis = list(title="Variety"))
        
        # lbl = 
        # mp <- barplot(dataFileCArrivalsAggregatedByVariety$x,ylim = c(0,dataFileCArrivalsAggregatedByVariety$x[1] + (dataFileCArrivalsAggregatedByVariety$x[1]/3)), ylab = paste0("Arrival Quantity In ",arrivalunits),xlab = "Variety",width = 1, space = 1, names.arg = dataFileCArrivalsAggregatedByVariety$Group.1, cex.names = 0.8, col = "orange",las=2)
        # text(mp,dataFileCArrivalsAggregatedByVariety$x, labels = dataFileCArrivalsAggregatedByVariety$x, pos = 3)
        # 
        # 
        p
      }
    })
   
   
    output$MSPVPlot <- renderPlotly({
      #message("MSPVPlot")
      toMSPDate <- input$MSPDateRange[1]
      fromMSPDate <- input$MSPDateRange[2]
      
      dataFileMI <- dataInput()[dataInput()$Date >= toMSPDate & dataInput()$Date <= fromMSPDate, ]
      #save(list = ls(all.names = TRUE), file = "mspv.RData") 
      #message(nrow(dataFileMI))
      validate(need(nrow(dataFileMI) > 1, "Generating output... please wait. If this message does not disappear in two minutes, it is most likely due to insufficient data. Please select another variety/market"))
      p <- plot_ly(dataFileMI)
      
      arrivalunits <- unique(dataFileMI$ArrivalUnits)
      modalunits <- unique(dataFileMI$ModalUnits)
      
      #Extend the cost values until the end
      #dataFileMI <- updateValueUpto2019Mar(dataFileMI)
      dataFileMI[is.na(dataFileMI)] <- 0
      
        #message(time(sum(dataFileMI$MSP)))
        if (! (is.na(sum(dataFileMI$MSP))) & sum(dataFileMI$MSP) > 0 ){
         p <- p %>% add_trace(x = ~dataFileMI$Date, y = ~dataFileMI$MSP, type = 'scatter', mode = 'lines', name = 'MSP', yaxis = 'y2', line = list(color = 'red'))
        }
        #message(time(sum(dataFileMI$CostA1FL)))
        if (! (is.na(sum(dataFileMI$CostA1FL))) & sum(dataFileMI$CostA1FL) > 0 ){
          p <- p %>% add_trace(x = ~dataFileMI$Date, y = ~dataFileMI$CostA1FL, type = 'scatter', mode = 'lines', name = 'Cost (A1+FL)', yaxis = 'y2', line = list(color = 'green'))
        }
        if (! (is.na(sum(dataFileMI$CostC2))) & sum(dataFileMI$CostC2) > 0 ){
          p <- p %>% add_trace(x = ~dataFileMI$Date, y = ~dataFileMI$CostC2, type = 'scatter', mode = 'lines', name = 'Cost (C2)', yaxis = 'y2', line = list(color = 'rgb(12, 58, 116)'))
        }
        #message(sum(dataFileMI$TotalC3))
        if (! (is.na(sum(dataFileMI$TotalC3))) & sum(dataFileMI$TotalC3) > 0 ){
          p <- p %>% add_trace(x = ~dataFileMI$Date, y = ~dataFileMI$TotalC3, type = 'scatter', mode = 'lines', name = 'Total Cost (C3)', yaxis = 'y2', line = list(color = 'brown'))
        }
      
    
      p <- p %>%
        add_trace(x = ~dataFileMI$Date, y = ~dataFileMI$Modal, type = 'scatter', mode = 'lines', name = paste0('Modal Price ',modalunits), yaxis = 'y2', line = list(color = 'orange')) %>%
        add_trace(x = ~dataFileMI$Date, y = ~dataFileMI$Arrival, type = 'scatter', mode = 'lines',fill = 'tozeroy', name = paste0("Arrival In ",arrivalunits), line = list(color = 'purple')) %>%
        layout(xaxis = list(title = 'Date'), 
               yaxis = list(side = 'left', title = paste0("Arrival In ",arrivalunits), showgrid = TRUE, zeroline = TRUE),
               yaxis2 = list(side = 'right', overlaying = "y", title = paste0('Modal Price ',modalunits), showgrid = TRUE, zeroline = TRUE),
               margin = list(l = 50, r = 50, b = 50, t = 50),
               legend = list(orientation = 'h',x = 0.1 ,y = 1.0, yanchor = "bottom"))
    })
    
    
    output$SarimaForTableM <- DT::renderDataTable({
      #message("Inside SarimaForTableM")
      dataFile <- updateDataFile()
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      dataFileC <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity),]
      rm(dataFile)
      toMSPTDate <- input$MSPDateRange[1]
      fromMSPTDate <- input$MSPDateRange[2]
      
      dataFileCC <- dataFileC[dataFileC$Date >= toMSPTDate & dataFileC$Date <= fromMSPTDate,]
      rm(dataFileC)
      
      #message(nrow(dataFileCC))
      arrivalunits <- unique(dataFileCC$ArrivalUnits)
      modalunits <- unique(dataFileCC$ModalUnits)
      
      #save(list = ls(all.names = TRUE), file = "sarima.RData") 
      
      if(length(dataFileCC$Arrival)>0)
      {
        dataFileCArrivalsAggregatedByMarket <- aggregate(dataFileCC$Arrival, by = list(dataFileCC$Market), FUN = sum)
        arrString <- paste("Sum(Arrival) In ",arrivalunits)
        names(dataFileCArrivalsAggregatedByMarket)[2] <- arrString
        names(dataFileCArrivalsAggregatedByMarket)[1] <- paste("Market")
        dataFileCArrivalsAggregatedByMarket <- data.table(dataFileCArrivalsAggregatedByMarket[order(-xtfrm(dataFileCArrivalsAggregatedByMarket[2])),])
        dataFileCArrivalsAggregatedByMarket
      }
      
    }, 
    extensions = 'Buttons')
    
    output$SarimaForTableG <- DT::renderDataTable({
      #message("Inside SarimaForTableG")
      dataFile <- updateDataFile()
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      dataFileC <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity),]
      rm(dataFile)
      toMSPTDate <- input$MSPDateRange[1]
      fromMSPTDate <- input$MSPDateRange[2]
      
      dataFileCC <- dataFileC[dataFileC$Date >= toMSPTDate & dataFileC$Date <= fromMSPTDate,]
      rm(dataFileC)
      #message(nrow(dataFileCC))
      arrivalunits <- unique(dataFileCC$ArrivalUnits)
      modalunits <- unique(dataFileCC$ModalUnits)
      
      #save(list = ls(all.names = TRUE), file = "sarima.RData") 
      
      if(length(dataFileCC$Arrival)>0)
      {
        dataFileCArrivalsAggregatedByGrade <- aggregate(dataFileCC$Arrival, by = list(dataFileCC$Grade), FUN = sum)
        arrString <- paste("Sum(Arrival) In ",arrivalunits)
        names(dataFileCArrivalsAggregatedByGrade)[2] <- arrString
        names(dataFileCArrivalsAggregatedByGrade)[1] <- paste("Grade")
        dataFileCArrivalsAggregatedByGrade <- data.table(dataFileCArrivalsAggregatedByGrade[order(-xtfrm(dataFileCArrivalsAggregatedByGrade[2])),])
        dataFileCArrivalsAggregatedByGrade
      }
      
    }, 
    extensions = 'Buttons')
    
    output$PoorMarketTable <- DT::renderDataTable({
      #message("Inside PoorMarketTable")
      
      toMSP33Date <- input$MSPDateRange[1]
      fromMSP33Date <- input$MSPDateRange[2]
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      
      dataFile <- updateDataFile()
      dataFilePP <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity) & (dataFile$Variety %in% input$inputVariety ),]
      rm(dataFile)
      arrivalunits <- unique(dataFilePP$ArrivalUnits)
      modalunits <- unique(dataFilePP$ModalUnits)
      
      dataFileQQ <- dataFilePP[dataFilePP$Date >= toMSP33Date & dataFilePP$Date <= fromMSP33Date,]
      rm(dataFilePP)
      dataFileNeighborMarketsPP <- aggregate(dataFileQQ$Modal, by = list(dataFileQQ$Market), FUN = mean)
      rm(dataFileQQ)
      dataFileNeighborMarketsPP$x <- round(dataFileNeighborMarketsPP$x, digits = 0)
      modalString <- paste("Average Modal Price ",modalunits)
      names(dataFileNeighborMarketsPP)[2] <- modalString
      names(dataFileNeighborMarketsPP)[1] <- paste("Market")
      dataFileNeighborMarketsPP <- data.table(dataFileNeighborMarketsPP[order(-dataFileNeighborMarketsPP[2]),])
      dataFileNeighborMarketsPP
    }, 
    extensions = 'Buttons')
    
    output$neighborMarketArrivalSubsetPlot <- renderPlot({
      #message("Inside neighborMarketArrivalSubsetPlot ")
      toMSP11Date <- input$MSPDateRange[1]
      fromMSP11Date <- input$MSPDateRange[2]
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      dataFile <- updateDataFile()
      
      dataFileT <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity) & (dataFile$Variety  %in% input$inputVariety ),]
      rm(dataFile)
      dataFileN <- dataFileT[dataFileT$Date >= toMSP11Date & dataFileT$Date <= fromMSP11Date, ]
      rm(dataFileT)
      arrivalunits <- unique(dataFileN$ArrivalUnits)
      modalunits <- unique(dataFileN$ModalUnits)
      
      dataFileNeighborMarketsSumArrival <- aggregate(dataFileN$Arrival, by=list(dataFileN$Market), FUN=sum) # assumes that dataFileNeighborMarkets has been generated after selection of Variety and Grade
      rm(dataFileN)
      dataFileNeighborMarketsSumArrival <- dataFileNeighborMarketsSumArrival[order(-dataFileNeighborMarketsSumArrival$x),] # sorting markets in decending order
      colnames(dataFileNeighborMarketsSumArrival) <- c("Market", "SumArrival")
      
      dataFileNeighborMarkets <- dataFileNeighborMarketsSumArrival[order(-dataFileNeighborMarketsSumArrival$SumArrival),]
      TopFiveBiggestMarkets <- dataFileNeighborMarkets$Market[1:5]
     
      mq <- barplot(dataFileNeighborMarkets$SumArrival[1:5],ylim = c(0, dataFileNeighborMarkets$SumArrival[1] + (dataFileNeighborMarkets$SumArrival[1]/5)),names.arg = dataFileNeighborMarkets$Market[1:5], xlab = "Markets", ylab = paste0("Arrival In ",arrivalunits), col = "orange", main = "Arrivals in Top Five Biggest Markets")
      text(mq,dataFileNeighborMarkets$SumArrival[1:5], labels = dataFileNeighborMarkets$SumArrival[1:5], pos = 3)
           
    })
    
    output$neighboringMarketPricesComparisonPlot <- renderPlot({
      #message("Inside neighboringMarketPricesComparisonPlot")
      toMSP22Date <- input$MSPDateRange[1]
      fromMSP22Date <- input$MSPDateRange[2]
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      
      
      dataFile <- updateDataFile()
      dataFileT <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity) & (dataFile$Variety %in% input$inputVariety),]
      rm(dataFile)
      dataFileTT  <- dataFileT[dataFileT$Date >= toMSP22Date & dataFileT$Date <= fromMSP22Date, ]
      rm(dataFileT)
      ##save(list = ls(all.names = TRUE), file = "season.RData") 
      
      dataFileNeighborMarketsMinArrival <- aggregate(dataFileTT$Arrival, by=list(dataFileTT$Market), FUN=min) # assumes that dataFileNeighborMarkets has been generated after selection of Variety and Grade
      colnames(dataFileNeighborMarketsMinArrival) <- c("Market", "MinArrival")
      dataFileNeighborMarketsMeanArrival <- aggregate(dataFileTT$Arrival, by=list(dataFileTT$Market), FUN=mean) # assumes that dataFileNeighborMarkets has been generated after selection of Variety and Grade
      colnames(dataFileNeighborMarketsMeanArrival) <- c("Market", "MeanArrival")
      dataFileNeighborMarketsMeanArrival$MeanArrival <- round(dataFileNeighborMarketsMeanArrival$MeanArrival, digits = 0)
      dataFileNeighborMarketsMaxArrival <- aggregate(dataFileTT$Arrival, by=list(dataFileTT$Market), FUN=max) # assumes that dataFileNeighborMarkets has been generated after selection of Variety and Grade
      colnames(dataFileNeighborMarketsMaxArrival) <- c("Market", "MaxArrival")
      dataFileNeighborMarketsSumArrival <- aggregate(dataFileTT$Arrival, by=list(dataFileTT$Market), FUN=sum) # assumes that dataFileNeighborMarkets has been generated after selection of Variety and Grade
      dataFileNeighborMarketsSumArrival <- dataFileNeighborMarketsSumArrival[order(-dataFileNeighborMarketsSumArrival$x),] # sorting markets in decending order
      colnames(dataFileNeighborMarketsSumArrival) <- c("Market", "SumArrival")
      
      dataFileNeighborMarketsMinModal <- aggregate(dataFileTT$Modal, by=list(dataFileTT$Market), FUN=min) # assumes that dataFileNeighborMarkets has been generated after selection of Variety and Grade
      colnames(dataFileNeighborMarketsMinModal) <- c("Market", "MinModal")
      dataFileNeighborMarketsMeanModal <- aggregate(dataFileTT$Modal, by=list(dataFileTT$Market), FUN=mean) # assumes that dataFileNeighborMarkets has been generated after selection of Variety and Grade
      colnames(dataFileNeighborMarketsMeanModal) <- c("Market", "MeanModal")
      dataFileNeighborMarketsMeanModal$MeanModal <- round(dataFileNeighborMarketsMeanModal$MeanModal, digits = 0)
      dataFileNeighborMarketsMaxModal <- aggregate(dataFileTT$Modal, by=list(dataFileTT$Market), FUN=max) # assumes that dataFileNeighborMarkets has been generated after selection of Variety and Grade
      colnames(dataFileNeighborMarketsMaxModal) <- c("Market", "MaxModal")
      
      rm(dataFileTT)
      dataFileNeighborMarketsMinMeanArrival <- merge(dataFileNeighborMarketsMinArrival, 
                                                     dataFileNeighborMarketsMeanArrival) 
      dataFileNeighborMarketsMinMeanMaxArrival <- merge(dataFileNeighborMarketsMinMeanArrival,
                                                        dataFileNeighborMarketsMaxArrival) 
      rm(dataFileNeighborMarketsMinMeanArrival)
      dataFileNeighborMarketsMinMeanMaxSumArrival <- merge(dataFileNeighborMarketsMinMeanMaxArrival, 
                                                           dataFileNeighborMarketsSumArrival)
      rm(dataFileNeighborMarketsMinMeanMaxArrival)
      dataFileNeighborMarketsMinMeanMaxSumArrivalMinModal <- merge(dataFileNeighborMarketsMinMeanMaxSumArrival, 
                                                                   dataFileNeighborMarketsMinModal)
      rm(dataFileNeighborMarketsMinMeanMaxSumArrival)
      dataFileNeighborMarketsMinMeanMaxSumArrivalMinMeanModal <- merge(dataFileNeighborMarketsMinMeanMaxSumArrivalMinModal, 
                                                                       dataFileNeighborMarketsMeanModal)
      dataFileNeighborMarketsMinMeanMaxSumArrivalMinMeanMaxModal <- merge(dataFileNeighborMarketsMinMeanMaxSumArrivalMinMeanModal, dataFileNeighborMarketsMaxModal)
      
      rm(dataFileNeighborMarketsMinMeanMaxSumArrivalMinMeanModal)
      dataFileNeighborMarkets <- dataFileNeighborMarketsMinMeanMaxSumArrivalMinMeanMaxModal[order(-dataFileNeighborMarketsMinMeanMaxSumArrivalMinMeanMaxModal$SumArrival),]
      TopFiveBiggestMarkets <- dataFileNeighborMarkets$Market[1:5] # selection of a vector with highest markets
      dataFileNeighborTopFiveBiggestMarketsAll <- dataFileNeighborMarkets[dataFileNeighborMarkets$Market %in% TopFiveBiggestMarkets,]
     
      dataFileNeighborTopFiveBiggestMarkets <- dataFileNeighborTopFiveBiggestMarketsAll[,c("Market", "MinArrival", "MeanArrival", "MaxArrival")]
      dataFileNeighborTopFiveBiggestMarketsWithModal <- dataFileNeighborTopFiveBiggestMarketsAll[,c("Market", "MinModal", "MeanModal", "MaxModal")]
      
      rm(dataFileNeighborTopFiveBiggestMarketsAll)
      
      mq <- barplot(dataFileNeighborTopFiveBiggestMarketsWithModal$MeanModal[1:5],ylim = c(0, dataFileNeighborMarkets$MeanModal[1] + (dataFileNeighborMarkets$MeanModal[1]/5)),names.arg = dataFileNeighborMarkets$Market[1:5], xlab = "Markets", ylab = "Average Modal Prices", col = "orange", main = "Modal Prices in Top Five Biggest Markets")
      text(mq,dataFileNeighborMarkets$MeanModal[1:5], labels = dataFileNeighborMarkets$MeanModal[1:5], pos = 3)
    
      
    })
    
    output$NNPredictionsTable <- DT::renderDataTable({
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      validate(
        need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))  
      
      validate(
        need(length(input$inputGrade) == 1, "Please select a single Grade"))           
      
      #message("Insidde Predictions Table")
      fromDate <- input$dateRange[1]
      toDate <- input$dateRange[2]
      
      predictingDataDates <- bizseq(fromDate,toDate,"actual")
      noOfPeriodsToForecast <- length(predictingDataDates)
      
      output <- NNForecast()
      trainingDataNnetar <- as.list(output[1][[1]])
      predictingDataFrameNnetar <- as.data.frame(output[2])
     
           #
      Date <- predictingDataFrameNnetar$predictingDataDates
      ForecastValue_Modal <- round(predictingDataFrameNnetar$Point.Forecast, digits = 0)
      Commodity <- input$inputCommodity
      Variety <- input$inputVariety
      Market <- input$inputMarket
     
      if ((length(Variety) > 1) || (length(Market) > 1)){
        writeTable <- data.frame(Commodity,Date,ForecastValue_Modal)
        datatable(writeTable, rownames=FALSE)  %>% formatDate(2, method = 'toLocaleDateString', params = list('fr-FR'))}
      else{
        writeTable <- data.frame(Commodity,Variety,Market,Date,ForecastValue_Modal)
        datatable(writeTable, rownames=FALSE)  %>% formatDate(4, method = 'toLocaleDateString', params = list('fr-FR'))
        }
    }, 
    extensions = c("Buttons"))
    
    output$ANNFuturePlot <- renderPlotly({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      validate(
        need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))  
      
      validate(
        need(length(input$inputGrade) == 1, "Please select a single Grade"))   
      
      validate(
        need(nrow(dataFileSD) > 250, "Insufficient Data for Training, cannot forecast")
      )
      
     #message("Inside ANNFuture Plot")
      toDate <- input$dateRange[1]
      fromDate <- input$dateRange[2]
      
      predictingDataDates <- bizseq(toDate,fromDate,"actual")
      noOfPeriodsToForecast = length(predictingDataDates)
      
      dataFileTemp <- dataFileSD
      actualDataFile <- updateForecastData()
      dataFileTemp = rbind(dataFileSD, actualDataFile)
      
      output <- NNForecast()
      trainingDataNnetar <- as.list(output[1][[1]])
      predictingDataFrameNnetar <- as.data.frame(output[2])
      
      predictingDataFrameNnetarFit <- data.frame(predictingDataDates,predictingDataFrameNnetar$Point.Forecast)
      
      
      #
      # Date <- predictingDataFrameNnetarFit$predictingDataDates
      # ForecastValue_Modal <- round(predictingDataFrameNnetarFit$predictingDataFrameNnetar.Point.Forecast, digits = 0)
      # Commodity <- input$inputCommodity
      # Variety <- input$inputVariety
      # Market <- input$inputMarket
      # 
      # writeTable <- data.frame(Commodity,Variety,Market,Date,ForecastValue_Modal)
     
      p <- plot_ly(predictingDataFrameNnetarFit)

      
      
      if (nrow(actualDataFile)>0){
        p <- p %>% add_trace(x = actualDataFile$Date,y = actualDataFile$Modal, name = 'Actual Price' ,type = 'scatter' ,mode = 'lines', yaxis = 'y2', line = list(color = 'purple'))
      }
      
      lastDate = max(actualDataFile$Date)
      
      #Append remaining
      if (nrow(actualDataFile) == 0 ){
        actualDataFile <- bigDataFile[(bigDataFile$CommodityGroup == input$inputCommodityGroup) & (bigDataFile$Commodity == input$inputCommodity) & (bigDataFile$Market %in% input$inputMarket) & (bigDataFile$Variety %in% input$inputVariety ) & (bigDataFile$Grade %in% input$inputGrade ),]
        actualDataFile <- actualDataFile[order(actualDataFile$Date),]
        lastDate = actualDataFile$Date[nrow(actualDataFile)]
      }
      
      #Append remaining
      remainingDates <- bizseq(toDate,fromDate,"actual")
      dfRemaining <- data.frame(Date=remainingDates,MSP = actualDataFile$MSP[nrow(actualDataFile)],CostA1FL = actualDataFile$CostA1FL[nrow(actualDataFile)],CostC2 = actualDataFile$CostC2[nrow(actualDataFile)],TotalC3 = actualDataFile$TotalC3[nrow(actualDataFile)] )
      #actualDataFile <- rbind(actualDataFile, dfRemaining, fill=TRUE)
      
      actualDataFile <- dfRemaining
   
      if ( ! (is.na(sum(actualDataFile$MSP))) & sum(actualDataFile$MSP) > 0 ){
        p <- p %>% add_trace(x = ~actualDataFile$Date, y = ~actualDataFile$MSP, type = 'scatter', mode = 'lines', name = 'MSP', yaxis = 'y2', line = list(color = 'red'))
      }
      if (! (is.na(sum(actualDataFile$CostA1FL))) &  sum(actualDataFile$CostA1FL) > 0 ){
        p <- p %>% add_trace(x = ~actualDataFile$Date, y = ~actualDataFile$CostA1FL, type = 'scatter', mode = 'lines', name = 'Cost (A1+FL)', yaxis = 'y2', line = list(color = 'green'))
      }
      if (! (is.na(sum(actualDataFile$CostC2))) & sum(actualDataFile$CostC2) > 0 ){
        p <- p %>% add_trace(x = ~actualDataFile$Date, y = ~actualDataFile$CostC2, type = 'scatter', mode = 'lines', name = 'Cost (C2)', yaxis = 'y2', line = list(color = 'rgb(12, 58, 116)'))
      }
      if (! (is.na(sum(actualDataFile$TotalC3))) & sum(actualDataFile$TotalC3) > 0 ){
        p <- p %>% add_trace(x = ~actualDataFile$Date, y = ~actualDataFile$TotalC3, type = 'scatter', mode = 'lines', name = 'Total Cost (C3)', yaxis = 'y2', line = list(color = 'brown'))
      }
      
      ##save(list = ls(all.names = TRUE), file = "before.RData")
      #originalDataFrame1 = originalDataFrame
      #predictingDataDates1 = predictingDataDates
      #filteredData <-   getDataInForecastRange(originalDataFrame,predictingDataDates)
      #save(list = ls(all.names = TRUE), file = "after.RData")
      
      
      
      p <- p %>% add_trace(x = ~predictingDataDates, y = ~predictingDataFrameNnetar.Point.Forecast,name = 'Price Forecast' ,type = 'scatter' ,mode = 'lines',yaxis = 'y2', line = list(color = 'blue')) %>%
        layout(title = "Price Forecast", xaxis = a, yaxis = b, showgrid = TRUE, zeroline = TRUE)
      
    })
    
    output$SeasonalIndexPlot <- renderPlotly({
      #message("Inside SeasonalIndexPlot")
      
      to22Date <- input$dateRange[1]
      from22Date <- input$dateRange[2]
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      dataFile <-  trainDataInput()
      dataFileSI <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity) & (dataFile$Market %in% input$inputMarket) & (dataFile$Variety %in% input$inputVariety ),]
      
      validate(
        need(nrow(dataFileSI)>10, "Generating output... please wait. If this message does not disappear in two minutes, it is most likely due to insufficient data. Please select another variety/market"))
      
      dataFileSI$Date <- as.Date(dataFileSI$Date, format = "%Y-%m-%d")
      
      onion.blr.arriveSI <- aggregate(dataFileSI$Arrival~ month(Date)+ year(Date), data = dataFileSI, mean)
      onion.blr.arriveSI$`dataFileSI$Arrival` <- round(onion.blr.arriveSI$`dataFileSI$Arrival`, digits = 0)
      
      onion.blr.arriveSI <- ts(onion.blr.arriveSI$`dataFileSI$Arrival`, frequency = 12, start = c(2013,1))
      rm(dataFileSI)
      componentsSI <- decompose(log(onion.blr.arriveSI))
      
      seasonalindex1SI <- data.frame((componentsSI$figure + max(componentsSI$figure))*100)
      rm(componentsSI)
      seasonalindex1SI$month <- c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      seasonalindexcurrentyearSI <- melt(seasonalindex1SI,ID = "month")
      seasonalindexcurrentyearSI$monthday <- 15
      seasonalindexcurrentyearSI$monthnumber <- c(1:12)
      currentYearSI = getRmetricsOptions("currentYear")
      tempToDate <- as.Date(to22Date, format = "%Y-%m-%d")
      currentYear <- lubridate::year(tempToDate)
      
      seasonalindexcurrentyearSI$year <- currentYearSI
      seasonalindexcurrentyearSI$value <- round(seasonalindexcurrentyearSI$value,digits = 0)
      seasonalindexcurrentyearSI <- seasonalindexcurrentyearSI[,c(1,3,4,5,6)]
      
      #Generating the date column for the entire next year
      seasonalindexnextyearSI <- melt(seasonalindex1SI,ID = "month")
      seasonalindexnextyearSI$monthday <- 15
      seasonalindexnextyearSI$monthnumber <- c(1:12)
      seasonalindexnextyearSI$year <- currentYearSI + 1
      seasonalindexnextyearSI$value <- round(seasonalindexnextyearSI$value,digits = 0)
      seasonalindexnextyearSI <- seasonalindexnextyearSI[,c(1,3,4,5,6)]
      
      seasonalindexSI <- rbind(seasonalindexcurrentyearSI,seasonalindexnextyearSI)
      seasonalindexSI$Date <- paste(seasonalindexSI$year,seasonalindexSI$monthnumber,seasonalindexSI$monthday, sep = "-")
      seasonalindexSI$Date <- as.Date(seasonalindexSI$Date, format = "%Y-%m-%d")
      
      seasonalindexSI <- seasonalindexSI[,c(6,2)]
      seasonalindexSI$Date <- as.Date(seasonalindexSI$Date, format = "%Y-%m-%d")
      
      
      #GENERATE  A FILE WITH A COMPLETE SEQUENCE OF DATES TO FIND OUT THE MISSING DATES IN THE DATA FRAME
      currentYear <- as.numeric(format(Sys.Date(), "%Y"))
      indexdatesSI <- seq(as.Date(input$dateRange[1]-days(60), format = "%Y-%m-%d"), as.Date(paste0(currentYear,"-12-31"),format = "%Y-%m-%d"),"days")
      indexdatesSI <- data.frame(list(Date = indexdatesSI))  
      indexdatesSI$Date <- as.Date(indexdatesSI$Date, format = "%Y-%m-%d")
      
      #MERGING THE DATA FILE TO THE CREATED FILE WITH THE COMPLETE SEQUENCE OF DATES
      seasonalindex2yearsSI <- merge(indexdatesSI, seasonalindexSI, all.x = TRUE)
      rm(indexdatesSI)
      seasonalindex2yearsSI <- data.frame(seasonalindex2yearsSI)
      seasonalindex2yearsSI$Date <- as.Date(seasonalindex2yearsSI$Date, format = "%Y-%m-%d")
      
      
      # NA fill, drop leading/trailing NAs
      seasonalindex2yearsSI$value <- na.spline(seasonalindex2yearsSI$value)
      seasonalindex2yearsSI <- seasonalindex2yearsSI[(isWeekday(seasonalindex2yearsSI$Date, wday = 1:5)),]
      
      seasonalindex2yearsSI$value <- round(seasonalindex2yearsSI$value, digits = 0)
      seasonalindex2yearsSI$value <- abs(seasonalindex2yearsSI$value)
      
      vLineStartDate <- input$dateRange[1]
      vLineEndDate <- input$dateRange[2]
      
      seasonalIndexSelection <- seasonalindex2yearsSI[(seasonalindex2yearsSI$Date >= vLineStartDate & seasonalindex2yearsSI$Date <= vLineEndDate),]
      
      hlineStartValue <- min(seasonalIndexSelection$value)
      hLineEndValue <- max(seasonalIndexSelection$value)
      
      plot_ly(seasonalindex2yearsSI) %>%
        add_trace(x = ~Date, y = ~value, type = 'bar', fill = 'tozeroy', name = 'Arrival Seasonal Index', yaxis = 'y', line = list(color = 'light purple'),
                  #line = list(color = 'red'),
                  hoverinfo = "text",
                  text = ~paste('Arrival Seasonal Index:',round(seasonalindex2yearsSI$value,digits=0),';',seasonalindex2yearsSI$Date)) %>%
        layout(title = 'Forecast of Modal Prices with MSP, CostA1&FL, Cost C2 and Total Cost C3',
               xaxis = list(title = 'Date'),
               yaxis = list(side = 'left', overlaying = "y", title = 'Arrival Seasonal Index', showgrid = TRUE, zeroline = TRUE),
               margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4),
               shapes = list(list(type = "rect", fillcolor = "purple", line = list(color = "light purple"), opacity = 0.3, x0 = vLineStartDate, x1 = vLineEndDate, xref = "x", y0 = hlineStartValue, y1 = hLineEndValue, yref = "y")),
               legend = list(orientation = 'h',x = 0.1,y = 1.0,yanchor = "bottom"))
      
    })
    
    output$seasonalForecastArimaPlot <- renderPlotly({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      # validate(
      #   need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      # validate(
      #   need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))   
      
      # validate(
      #   need(length(input$inputGrade) == 1, "Please select a single Grade"))       
      
      #message("Inside Seasonal Forecast Plot")
      to22Date <- input$dateRange[1]
      from22Date <- input$dateRange[2]
      
      #to22Date <- input$dateRange[2]
      #from22Date <- input$dateRange[1]
      dataFileSee <-  trainDataInput()
      
      validate(
        need(nrow(dataFileSee)>10, "Generating output... please wait. If this message does not disappear in two minutes, it is most likely due to insufficient data. Please select another variety/market"))
      
      
      #dataFileSee <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity) & (dataFile$Market == input$inputMarket) & which(input$inputVariety == dataFile$Variety ),]
      #save(list = ls(all.names = TRUE), file = "seasonal.RData")
      arrivalunits <- unique(dataFileSee$ArrivalUnits)
      modalunits <- unique(dataFileSee$ModalUnits)
      
      onion.blr.arrive <- aggregate(dataFileSee$Arrival~ month(Date)+ year(Date), data = dataFileSee, mean)
      onion.blr.arrive$`dataFileSee$Arrival` <- round(onion.blr.arrive$`dataFileSee$Arrival`, digits = 0)
      
      onion.blr.arrive <- ts(onion.blr.arrive$`dataFileSee$Arrival`, frequency = 12, start = c(2013,1))
      
      components <- decompose(log(onion.blr.arrive))
      
      seasonalindex1 <- data.frame((components$figure + max(components$figure))*100)
      seasonalindex1$month <- c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")
      
      seasonalindexcurrentyear <- melt(seasonalindex1,ID = "month")
      seasonalindexcurrentyear$monthnumber <- c(1:12)
      seasonalindexcurrentyear$monthday <- 15
      #currentYear = getRmetricsOptions("currentYear")
      tempToDate <- as.Date(to22Date, format = "%Y-%m-%d")
      currentYear <- lubridate::year(tempToDate)
      
      seasonalindexcurrentyear$year <- currentYear
      seasonalindexcurrentyear$value <- round(seasonalindexcurrentyear$value,digits = 0)
      seasonalindexcurrentyear <- seasonalindexcurrentyear[,c(1,3,5,4,6)]
      
      #Generating the date column for the entire next year
      seasonalindexnextyear <- melt(seasonalindex1,ID = "month")
      seasonalindexnextyear$monthnumber <- c(1:12)
      seasonalindexnextyear$monthday <- 15
      seasonalindexnextyear$year <- currentYear + 1
      seasonalindexnextyear$value <- round(seasonalindexnextyear$value,digits = 0)
      seasonalindexnextyear <- seasonalindexnextyear[,c(1,3,5,4,6)]
      
      seasonalindex <- rbind(seasonalindexcurrentyear,seasonalindexnextyear)
      seasonalindex$Date <- paste(seasonalindex$year,seasonalindex$monthnumber,seasonalindex$monthday, sep = "-")
      seasonalindex$Date <- as.Date(seasonalindex$Date, format = "%Y-%m-%d")
      
      seasonalindex <- seasonalindex[,c(6,2)]
      seasonalindex$Date <- as.Date(seasonalindex$Date, format = "%Y-%m-%d")
      
      currentYear <- as.numeric(format(Sys.Date(), "%Y"))
      #GENERATE  A FILE WITH A COMPLETE SEQUENCE OF DATES TO FIND OUT THE MISSING DATES IN THE DATA FRAME
      #indexdates <- seq(as.Date(min("2017-01-01"), format = "%Y-%m-%d"), as.Date(max("2017-12-31"),format = "%Y-%m-%d"),"days")
      # indexdates <- seq(as.Date(min("2018-01-01"), format = "%Y-%m-%d"), as.Date(max("2018-12-31"),format = "%Y-%m-%d"),"days")
      indexdates <- seq(as.Date(input$dateRange[1]-days(60), format = "%Y-%m-%d"), as.Date(paste0(currentYear,"-12-31"),format = "%Y-%m-%d"),"days")
      
      
      indexdates <- data.frame(list(Date = indexdates))  
      indexdates$Date <- as.Date(indexdates$Date, format = "%Y-%m-%d")
      
      ##save(list = ls(all.names = TRUE), file = "C:/Users/Balasangameshwara Vo/Dropbox/ANVITA/KVAFSU/Crop Dashboard/Data/season1.RData")
      
      #MERGING THE DATA FILE TO THE CREATED FILE WITH THE COMPLETE SEQUENCE OF DATES
      seasonalindex2years <- merge(indexdates, seasonalindex, all.x = TRUE)
      seasonalindex2years <- data.frame(seasonalindex2years)
      seasonalindex2years$Date <- as.Date(seasonalindex2years$Date, format = "%Y-%m-%d")
      
      # NA fill, drop leading/trailing NAs
      seasonalindex2years$value <- na.spline(seasonalindex2years$value)
      seasonalindex2years <- seasonalindex2years[(isWeekday(seasonalindex2years$Date, wday = 1:5)),]
      
      seasonalindex05NOV17to04FEB18 <- seasonalindex2years[(seasonalindex2years$Date >= to22Date & seasonalindex2years$Date <= from22Date),]
      seasonalindex05NOV17to04FEB18$Date <- as.Date(seasonalindex05NOV17to04FEB18$Date, format = "%Y-%m-%d")
      
      noOfSeasonalPeriodsToForecast <- bizdays(to22Date, from22Date, "actual")
      predictingSeasonalDataDates <- bizseq(to22Date,from22Date,"actual")
      trainingSeasonalDataNnetar <- ArimaForecast()
      
      predictingSeasonalDataNnetarForecast <- forecast(trainingSeasonalDataNnetar,h = noOfSeasonalPeriodsToForecast)
      predictingSeasonalDataFrameNnetar <- data.frame(predictingSeasonalDataNnetarForecast)
      
      if(length(predictingSeasonalDataDates)==length(predictingSeasonalDataFrameNnetar$Point.Forecast))
      {
        predictingSeasonalDataFrameNnetarFit <- data.frame(predictingSeasonalDataDates,predictingSeasonalDataFrameNnetar$Point.Forecast)
      }
      else
      {
        predictingSeasonalDataNnetarForecast <- forecast(trainingSeasonalDataNnetar,h = noOfSeasonalPeriodsToForecast+1)
        predictingSeasonalDataFrameNnetar <- data.frame(predictingSeasonalDataNnetarForecast)
        predictingSeasonalDataFrameNnetarFit <- data.frame(predictingSeasonalDataDates,predictingSeasonalDataFrameNnetar$Point.Forecast)
      }
      
      seasonalindex05NOV17to04FEB18$value <- round(seasonalindex05NOV17to04FEB18$value, digits = 0)
      seasonalindex05NOV17to04FEB18$value <- abs(seasonalindex05NOV17to04FEB18$value)
      predictingSeasonalDataFrameNnetarFit$predictingSeasonalDataFrameNnetar.Point.Forecast <- round(predictingSeasonalDataFrameNnetarFit$predictingSeasonalDataFrameNnetar.Point.Forecast, digits = 0)
      
      p <- plot_ly(x = ~predictingSeasonalDataFrameNnetarFit$predictingSeasonalDataDates, y = ~predictingSeasonalDataFrameNnetarFit$predictingSeasonalDataFrameNnetar.Point.Forecast, type = 'scatter', mode = 'lines', name = 'Price Forecast', yaxis = 'y')
      
      # if (! (is.na(sum(dataFileSee$MSP))) & sum(dataFileSee$MSP) > 0 ){
      #   p <- p %>% add_trace(x = ~dataFileSee$Date, y = ~dataFileSee$MSP, type = 'scatter', mode = 'lines', name = 'MSP', yaxis = 'y2', line = list(color = 'red'))
      # }
      # if (! (is.na(sum(dataFileSee$CostA1FL))) & sum(dataFileSee$CostA1FL) > 0 ){
      #   p <- p %>% add_trace(x = ~dataFileSee$Date, y = ~dataFileSee$CostA1FL, type = 'scatter', mode = 'lines', name = 'Cost (A1+FL)', yaxis = 'y2', line = list(color = 'green'))
      # }
      # if (! (is.na(sum(dataFileSee$TotalC3))) & sum(dataFileSee$TotalC3) > 0 ){
      #   p <- p %>% add_trace(x = ~dataFileSee$Date, y = ~dataFileSee$TotalC3, type = 'scatter', mode = 'lines', name = 'Total Cost (C3)', yaxis = 'y2', line = list(color = 'brown'))
      # }
      
      p <- p %>% 
        add_trace( x = ~seasonalindex05NOV17to04FEB18$Date, y = ~seasonalindex05NOV17to04FEB18$value, type = 'scatter',mode = 'lines', name = 'Arrival Seasonal Index', yaxis = 'y2',fill = 'tozeroy', line = list(color = 'light purple')) %>%
        layout(title = 'Arrival Seasonal Index Superimposed With Modal Prices Forcast',
               xaxis = list(title = 'Date'),
               yaxis = list(side = 'left', title = paste0("Price Forecast ",modalunits), showgrid = TRUE, zeroline = TRUE ),
               yaxis2 = list(side = 'right', overlaying = "y", title = 'Seasonal Index of Arrival' ,showgrid = TRUE, zeroline = TRUE),
               margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4),
               legend = list(orientation = 'h',x = 0.1,y = 1.0,yanchor = "bottom"))
      
    })
    
    output$seasonalForecastPlot <- renderPlotly({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(length(input$inputMarket) == 1, "Please select a single Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))  
      
      validate(
        need(length(input$inputVariety) == 1, "Please select a single Variety"))    
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))  
      
      validate(
        need(length(input$inputGrade) == 1, "Please select a single Grade"))          
      
      #message("Inside Seasonal Forecast Plot")
      to22Date <- input$dateRange[1]
      from22Date <- input$dateRange[2]
      
      #to22Date <- input$dateRange[2]
      #from22Date <- input$dateRange[1]
      dataFileSee <-  trainDataInput()
      
      validate(
        need(nrow(dataFileSee)>10, "Generating output... please wait. If this message does not disappear in two minutes, it is most likely due to insufficient data. Please select another variety/market"))
      
      
      #dataFileSee <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity) & (dataFile$Market == input$inputMarket) & which(input$inputVariety == dataFile$Variety ),]
      #save(list = ls(all.names = TRUE), file = "seasonal.RData")
      arrivalunits <- unique(dataFileSee$ArrivalUnits)
      modalunits <- unique(dataFileSee$ModalUnits)
      
      onion.blr.arrive <- aggregate(dataFileSee$Arrival~ month(Date)+ year(Date), data = dataFileSee, mean)
      onion.blr.arrive$`dataFileSee$Arrival` <- round(onion.blr.arrive$`dataFileSee$Arrival`, digits = 0)
      
      onion.blr.arrive <- ts(onion.blr.arrive$`dataFileSee$Arrival`, frequency = 12, start = c(2013,1))
     
      components <- decompose(log(onion.blr.arrive))
      
      seasonalindex1 <- data.frame((components$figure + max(components$figure))*100)
      seasonalindex1$month <- c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")
      
      seasonalindexcurrentyear <- melt(seasonalindex1,ID = "month")
      seasonalindexcurrentyear$monthnumber <- c(1:12)
      seasonalindexcurrentyear$monthday <- 15
      #currentYear = getRmetricsOptions("currentYear")
      tempToDate <- as.Date(to22Date, format = "%Y-%m-%d")
      currentYear <- lubridate::year(tempToDate)
      
      seasonalindexcurrentyear$year <- currentYear
      seasonalindexcurrentyear$value <- round(seasonalindexcurrentyear$value,digits = 0)
      seasonalindexcurrentyear <- seasonalindexcurrentyear[,c(1,3,5,4,6)]
      
      #Generating the date column for the entire next year
      seasonalindexnextyear <- melt(seasonalindex1,ID = "month")
      seasonalindexnextyear$monthnumber <- c(1:12)
      seasonalindexnextyear$monthday <- 15
      seasonalindexnextyear$year <- currentYear + 1
      seasonalindexnextyear$value <- round(seasonalindexnextyear$value,digits = 0)
      seasonalindexnextyear <- seasonalindexnextyear[,c(1,3,5,4,6)]
      
      seasonalindex <- rbind(seasonalindexcurrentyear,seasonalindexnextyear)
      seasonalindex$Date <- paste(seasonalindex$year,seasonalindex$monthnumber,seasonalindex$monthday, sep = "-")
      seasonalindex$Date <- as.Date(seasonalindex$Date, format = "%Y-%m-%d")
      
      seasonalindex <- seasonalindex[,c(6,2)]
      seasonalindex$Date <- as.Date(seasonalindex$Date, format = "%Y-%m-%d")
      
      #GENERATE  A FILE WITH A COMPLETE SEQUENCE OF DATES TO FIND OUT THE MISSING DATES IN THE DATA FRAME
      #indexdates <- seq(as.Date(min("2017-01-01"), format = "%Y-%m-%d"), as.Date(max("2017-12-31"),format = "%Y-%m-%d"),"days")
      # indexdates <- seq(as.Date(min("2018-01-01"), format = "%Y-%m-%d"), as.Date(max("2018-12-31"),format = "%Y-%m-%d"),"days")
      currentYear <- as.numeric(format(Sys.Date(), "%Y"))
      indexdates <- seq(as.Date(input$dateRange[1]-days(60), format = "%Y-%m-%d"), as.Date(paste0(currentYear,"-12-31"),format = "%Y-%m-%d"),"days")
      
      
      indexdates <- data.frame(list(Date = indexdates))  
      indexdates$Date <- as.Date(indexdates$Date, format = "%Y-%m-%d")
      
      ##save(list = ls(all.names = TRUE), file = "C:/Users/Balasangameshwara Vo/Dropbox/ANVITA/KVAFSU/Crop Dashboard/Data/season1.RData")
      
      #MERGING THE DATA FILE TO THE CREATED FILE WITH THE COMPLETE SEQUENCE OF DATES
      seasonalindex2years <- merge(indexdates, seasonalindex, all.x = TRUE)
      seasonalindex2years <- data.frame(seasonalindex2years)
      seasonalindex2years$Date <- as.Date(seasonalindex2years$Date, format = "%Y-%m-%d")
      
      # NA fill, drop leading/trailing NAs
      seasonalindex2years$value <- na.spline(seasonalindex2years$value)
      seasonalindex2years <- seasonalindex2years[(isWeekday(seasonalindex2years$Date, wday = 1:5)),]
      
      seasonalindex05NOV17to04FEB18 <- seasonalindex2years[(seasonalindex2years$Date >= to22Date & seasonalindex2years$Date <= from22Date),]
      seasonalindex05NOV17to04FEB18$Date <- as.Date(seasonalindex05NOV17to04FEB18$Date, format = "%Y-%m-%d")
      
      noOfSeasonalPeriodsToForecast <- bizdays(to22Date, from22Date, "actual")
      predictingSeasonalDataDates <- bizseq(to22Date,from22Date,"actual")
      output <- NNForecast()

      trainingSeasonalDataNnetar <- as.list(output[1][[1]])
      predictingSeasonalDataFrameNnetar <- as.data.frame(output[2])
      
      #predictingSeasonalDataNnetarForecast <- forecast(trainingSeasonalDataNnetar,h = noOfSeasonalPeriodsToForecast)
      #predictingSeasonalDataFrameNnetar <- data.frame(predictingSeasonalDataNnetarForecast)
      
      if(length(predictingSeasonalDataDates)==length(predictingSeasonalDataFrameNnetar$Point.Forecast))
      {
        predictingSeasonalDataFrameNnetarFit <- data.frame(predictingSeasonalDataDates,predictingSeasonalDataFrameNnetar$Point.Forecast)
      }
      else
      {
       # predictingSeasonalDataNnetarForecast <- forecast(trainingSeasonalDataNnetar,h = noOfSeasonalPeriodsToForecast+1)
      #  predictingSeasonalDataFrameNnetar <- data.frame(predictingSeasonalDataNnetarForecast)
        predictingSeasonalDataFrameNnetarFit <- data.frame(predictingSeasonalDataDates,predictingSeasonalDataFrameNnetar$Point.Forecast)
      }
      
      seasonalindex05NOV17to04FEB18$value <- round(seasonalindex05NOV17to04FEB18$value, digits = 0)
      seasonalindex05NOV17to04FEB18$value <- abs(seasonalindex05NOV17to04FEB18$value)
      predictingSeasonalDataFrameNnetarFit$predictingSeasonalDataFrameNnetar.Point.Forecast <- round(predictingSeasonalDataFrameNnetarFit$predictingSeasonalDataFrameNnetar.Point.Forecast, digits = 0)
      
      p <- plot_ly(x = ~predictingSeasonalDataFrameNnetarFit$predictingSeasonalDataDates, y = ~predictingSeasonalDataFrameNnetarFit$predictingSeasonalDataFrameNnetar.Point.Forecast, type = 'scatter', mode = 'lines', name = 'Price Forecast', yaxis = 'y')
      
      # if (! (is.na(sum(dataFileSee$MSP))) & sum(dataFileSee$MSP) > 0 ){
      #   p <- p %>% add_trace(x = ~dataFileSee$Date, y = ~dataFileSee$MSP, type = 'scatter', mode = 'lines', name = 'MSP', yaxis = 'y2', line = list(color = 'red'))
      # }
      # if (! (is.na(sum(dataFileSee$CostA1FL))) & sum(dataFileSee$CostA1FL) > 0 ){
      #   p <- p %>% add_trace(x = ~dataFileSee$Date, y = ~dataFileSee$CostA1FL, type = 'scatter', mode = 'lines', name = 'Cost (A1+FL)', yaxis = 'y2', line = list(color = 'green'))
      # }
      # if (! (is.na(sum(dataFileSee$TotalC3))) & sum(dataFileSee$TotalC3) > 0 ){
      #   p <- p %>% add_trace(x = ~dataFileSee$Date, y = ~dataFileSee$TotalC3, type = 'scatter', mode = 'lines', name = 'Total Cost (C3)', yaxis = 'y2', line = list(color = 'brown'))
      # }
      
      p <- p %>% 
        add_trace( x = ~seasonalindex05NOV17to04FEB18$Date, y = ~seasonalindex05NOV17to04FEB18$value, type = 'scatter',mode = 'lines', name = 'Arrival Seasonal Index', yaxis = 'y2',fill = 'tozeroy', line = list(color = 'light purple')) %>%
        layout(title = 'Arrival Seasonal Index Superimposed With Modal Prices Forcast',
               xaxis = list(title = 'Date'),
               yaxis = list(side = 'left', title = paste0("Price Forecast ",modalunits), showgrid = TRUE, zeroline = TRUE ),
               yaxis2 = list(side = 'right', overlaying = "y", title = 'Seasonal Index of Arrival' ,showgrid = TRUE, zeroline = TRUE),
               margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4),
               legend = list(orientation = 'h',x = 0.1,y = 1.0,yanchor = "bottom"))
      
    })
    output$garch1 <- renderPlot({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))        
      
      trainingDataSarimaFor <-  ArimaForecast()
      arimaResiduals <- trainingDataSarimaFor$residuals
      
      arimaClean <- Return.clean(arimaResiduals, method="boudt")
      garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                                mean.model = list(armaOrder=c(0,0)))
      fromDate <- input$HistDateRange[1]
      toDate <- input$HistDateRange[2]
      
      trainingDataDates <- bizseq(fromDate,toDate,"actual")
      noOfPeriodsToForecast <- length(trainingDataDates)
      # refit GARCH(1,1) to cleaned data
      garch11.fit = ugarchfit(spec=garch11.spec, data=arimaClean,
                              solver.control=list(trace = 0))
      plot.ts(sigma(garch11.fit), main="GARCH(1,1) conditional Volatality",
              ylab="sigma(t)", xlab = "Training Days", col="blue")
    })
    
    output$garch2 <- renderPlot({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))        
      
      trainingDataSarimaFor <-  ArimaForecast()
      
      arimaResiduals <- trainingDataSarimaFor$residuals

      garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)),
                                mean.model = list(armaOrder=c(0,0)))
      tomato.garch11.fit = ugarchfit(spec=garch11.spec, data=arimaResiduals,
                                     solver.control=list(trace = 1))
      plot(tomato.garch11.fit, which=2)

    })
    
    output$garch3 <- renderPlot({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))        
      
      trainingDataSarimaFor <-  ArimaForecast()
      arimaResiduals <- trainingDataSarimaFor$residuals
      
      garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                                mean.model = list(armaOrder=c(0,0)))
      tomato.garch11.fit = ugarchfit(spec=garch11.spec, data=arimaResiduals,
                                     solver.control=list(trace = 1))   
      
      plot(tomato.garch11.fit, which=5)
      
    })
    
    output$garch4 <- renderPlot({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))        
      
      trainingDataSarimaFor <-  ArimaForecast()
      arimaResiduals <- trainingDataSarimaFor$residuals
      
      garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                                mean.model = list(armaOrder=c(0,0)))
      tomato.garch11.fit = ugarchfit(spec=garch11.spec, data=arimaResiduals,
                                     solver.control=list(trace = 1))   
      
      fromDate <- input$dateRange[1]
      toDate <- input$dateRange[2]
      
      predictingDataDates <- bizseq(fromDate,toDate,"actual")
      noOfPeriodsToForecast <- length(predictingDataDates)
      
      tomato.garch11.fcst <- ugarchforecast(tomato.garch11.fit, n.ahead=noOfPeriodsToForecast)
      
      tomato.fcst.df = as.data.frame(attributes(tomato.garch11.fcst)[[1]])
      colnames(tomato.fcst.df) <- c("n.ahead", "N" ,"n.start", "n.roll", "SigmaFor" ,"SeriesFor")
      tomato.fcst.var.hDay = cumsum(tomato.fcst.df$SigmaFor^2)
      tomato.fcst.vol.hDay = sqrt(tomato.fcst.var.hDay)
      fcst.vol.hDay.zoo = zoo(tomato.fcst.vol.hDay, predictingDataDates)
      #save(list = ls(all.names = TRUE), file = "Garch.Rdata")
      chart.TimeSeries(fcst.vol.hDay.zoo, main="GARCH(1,1) Forecast of h-day Return Vol",
                       colorset="blue", ylab="h-day vol forecast")
      
    })
    output$garch5 <- renderPlot({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))        
      
      trainingDataSarimaFor <-  ArimaForecast()
      arimaResiduals <- trainingDataSarimaFor$residuals
      
      garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                                mean.model = list(armaOrder=c(0,0)))
      fromDate <- input$dateRange[1]
      toDate <- input$dateRange[2]
      
      predictingDataDates <- bizseq(fromDate,toDate,"actual")
      noOfPeriodsToForecast <- length(predictingDataDates)
      
      ###Required - Plot Graph 4
      tomato.garch11.roll = ugarchroll(garch11.spec, arimaResiduals, n.ahead=1,
                                       forecast.length = noOfPeriodsToForecast,
                                       refit.every=20, refit.window="moving")
      
      plot(tomato.garch11.roll, which=4)
      
    })
    
    output$TapsBC <- renderPlot({
      
      #message("Inside TapsBC")
      #TADurationNo <- length(seq(from = as.Date(input$MSPDateRange[1]), to = as.Date(input$MSPDateRange[2]), by = 'month')) - 1
      
      ##messageTA <- paste("last",TADurationNo, sep = "  " ,"months")
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      dataFile<-updateDataFile()
      dataFileTap <- dataFile[(dataFile$CommodityGroup == input$inputCommodityGroup) & (dataFile$Commodity == input$inputCommodity) & (dataFile$Market %in% input$inputMarket) & (dataFile$Variety %in% input$inputVariety),]
      rm(dataFile)
      
      ##print(input$MSPDateRange[1]) 
      ##print(input$MSPDateRange[2])
      startDate = as.Date(input$MSPDateRange[1])
     
      endDate = as.Date(input$MSPDateRange[2])
      startDate = endDate %m-% months(5)
      
      dataFileTap <- dataFileTap[dataFileTap$Date >= startDate & dataFileTap$Date <= endDate,]
      
      onion.blr <- aggregate(cbind(dataFileTap$Max,dataFileTap$Min,dataFileTap$Modal,dataFileTap$Arrival), by = list(dataFileTap$Date), FUN = mean)
      rm(dataFileTap)
      onion.blr$V1 <- round(onion.blr$V1, digits = 0)
      onion.blr$V2 <- round(onion.blr$V2, digits = 0)
      onion.blr$V3 <- round(onion.blr$V3, digits = 0)
      onion.blr$V4 <- round(onion.blr$V4, digits = 0)
      onion.blr$V5 <- shift(onion.blr$V3,1)
      onion.blr$V5 <- round(onion.blr$V5, digits = 0)
      onion.blr <- onion.blr[(2:length(onion.blr$V1)),c(1,6,2,3,4,5)]
      
      #RENAMING THE COLUMNS INTO HLC FORMAT
      colnames(onion.blr) <- c("Date", "Open", "High","Low", "Close", "Volume")
      onion.blr$Date <- as.Date(onion.blr$Date, format = "%Y-%m-%d")
      #save(list = ls(all.names = TRUE), file = "TA.Rdata")
      #GENERATE  A FILE WITH A COMPLETE SEQUENCE OF DATES TO FIND OUT THE MISSING DATES IN THE DATA FRAME
      
      alldates <- seq(as.Date(min(onion.blr$Date), format = "%Y-%m-%d"), as.Date(max(onion.blr$Date),format = "%Y-%m-%d"),"days")
      alldates <- alldates[!weekdays(alldates) %in% c( "Sunday") ]
      alldates <- data.frame(list(Date = alldates)) 
      
      alldates$Date <- as.Date(alldates$Date, format = "%Y-%m-%d")
      
      #MERGING THE DATA FILE TO THE CREATED FILE WITH THE COMPLETE SEQUENCE OF DATES
      onion.blr.full <- merge( alldates, onion.blr, all.x = TRUE)
      onion.blr.full <- data.frame(onion.blr.full)
      
      onion.blr.full[(onion.blr.full$Close == 0),]
      onion.blr.full[onion.blr.full == 0] <- NA
      any(is.na(onion.blr$Close))
      which(is.na(onion.blr$Close))
      
      
      onion.blr.full$Open <- na.approx(onion.blr.full$Open)
      onion.blr.full$Low <- na.approx(onion.blr.full$Low)
      onion.blr.full$High <- na.approx(onion.blr.full$High)
      onion.blr.full$Close <- na.approx(onion.blr.full$Close)
      onion.blr.full$Volume <- na.approx(onion.blr.full$Volume)
      
      onion.blr.full$Open <- round(onion.blr.full$Open,digits = 0)
      onion.blr.full$High <- round(onion.blr.full$High,digits = 0)
      onion.blr.full$Low <- round(onion.blr.full$Low,digits = 0)
      onion.blr.full$Close <- round(onion.blr.full$Close,digits = 0)
      onion.blr.full$Volume <- round(onion.blr.full$Volume,digits = 0)
      
      onion.blr.xts <- xts(onion.blr.full[,-1], order.by = as.POSIXct(onion.blr.full[,1], format = "%Y-%m-%d"))
      onion.blr.xts <- data.frame(onion.blr.xts)
      #save(list = ls(all.names = TRUE), file = "TA.Rdata")
      chartSeries(onion.blr.xts,name = "Technical Analysis", theme = "white", TA = c(addVo(),addBBands(), addCCI(), addCMO(), addRSI(), addWPR())) 
      
      
      
      #TechnicalAnalysisModal <- xts(dataFileTapDataFrame$Modal,order.by = dataFileTapDataFrame$Date)
      #myPars <- chart_pars()
      #myPars$mar <- c(3,2,0,.2)
      #myPars$cex <- 5.0
      
      #chartSeries(TechnicalAnalysisModal,pars = myPars,subset = 'last 3 months',theme = chartTheme('white'),TA = c(addBBands(),addCCI(),addCMO(),addRSI(),addWPR()))
      #},height = 700)
    }, height = 1500)
    
    Modal <- trainDataInput()$Modal
    
    output$acfPlot <- renderPlot({
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
      
      validate(
        need(input$inputMarket != "", "Please select a Market"))
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      validate(
        need(input$inputGrade != "", "Please select a Grade"))        
      
      acf(Modal)
      
      })
 
    output$acfDiffPlot <- renderPlot({acf(diff(Modal))})
    
    output$pacfPlot <- renderPlot({pacf(Modal)})
    
    output$pacfDiffPlot <- renderPlot({pacf(diff(Modal))})
    
    output$acf2DiffPlot <- renderPlot({ acf(diff(diff(Modal))) })
    output$acf2DiffPlot1 <- renderPlot({ acf(diff(diff(Modal))) })
    
    output$pacf2DiffPlot <- renderPlot({ pacf(diff(diff(Modal))) })
    output$pacf2DiffPlot1 <- renderPlot({ pacf(diff(diff(Modal))) })
 
    output$neighboringMarketsMeanPlot <- renderPlotly({
      
      validate(
        need(input$inputCommodityGroup != "", "Please select a Commodity Group"))
      
      validate(
        need(input$inputCommodity != "", "Please select a Commodity"))
   
      
      validate(
        need(input$inputVariety != "", "Please select a Variety"))        
      
      
      #message("Inside neighboringMarketsMeanPlot")
      dataFile <- updateDataFile()
      
      dataFileNAS <- dataFile[dataFile$CommodityGroup == input$inputCommodityGroup & dataFile$Commodity == input$inputCommodity & (dataFile$Variety %in% input$inputVariety ),]
      rm(dataFile)
      dataFileAg <- dataFileNAS[dataFileNAS$Date >= input$MSPDateRange[1] & dataFileNAS$Date <= input$MSPDateRange[2],]
      rm(dataFileNAS)
      
      
      #save(list = ls(all.names = TRUE), file = "mspv.RData")
     
      dataFileAg.Sum.Arrrival <- aggregate(dataFileAg$Arrival, by = list(dataFileAg$Market), FUN = sum)
      colnames(dataFileAg.Sum.Arrrival) <- c("Market","Sum.Arrival")
      dataFileAg.Mean.Modal <- aggregate(dataFileAg$Modal, by = list(dataFileAg$Market), FUN = mean)
      colnames(dataFileAg.Mean.Modal) <- c("Market","Avg.Modal.Prices")
      dataFileAg <- merge(dataFileAg.Sum.Arrrival,dataFileAg.Mean.Modal)
      dataFileAg$Sum.Arrival <- round(dataFileAg$Sum.Arrival, digits = 0)
      dataFileAg$Avg.Modal.Prices <- round(dataFileAg$Avg.Modal.Prices, digits = 0)
      
      p <- plot_ly(dataFileAg)
     
      p <- p %>% 
        add_trace(x = ~Sum.Arrival, y = ~Avg.Modal.Prices,type = 'scatter', mode = 'markers', color = ~Market, marker = list(size = 15, opacity = 1.5),
                  hoverinfo = "text",
                  text = ~paste('Total Arrival:',Sum.Arrival,", Avg Modal:", Avg.Modal.Prices,"Market:",Market)) %>%
        
        layout(title = 'Total Arrival Vs Average Modal Prices For Markets',
               xaxis = list(title = 'Total Arrival'),
               yaxis = list(side = 'left', title = 'Avg Modal Prices', showgrid = TRUE, zeroline = TRUE),
               #yaxis2 = list(side = 'right', overlaying = "y", title = 'Daily Modal Price in Rs.', showgrid = TRUE, zeroline = TRUE),
               margin=list(l = 50, r = 50, b = 100, t = 100, pad = 4),
               legend = list(orientation = 'v',x=1,y=1.0,yanchor="bottom"))
      
    })
    
  })
}

shinyApp(ui = ui, server = server)