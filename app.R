#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Merck Challenge: Bioprocess Demo
# Author: Qinaho Wu, Alex Mey, Linmei Shang, Hua Ke
# Date: 4/12/2023

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(FNN)
library(scales)
library(gridExtra)
library(grid)
library(reactable)
library(e1071)#SVM regression

#jscode for controlling close window
jsCloseWindow <- "shinyjs.closeWindow = function() { window.close(); }"

# data generation function for upstream
# Here is to generate the demo data, which is stored at the backend of the R host.
# input: mode (manufacturing mode: default 1), 
#        bioVol (bioreactor vol: default 2000L)
# output: generated demo dataset
# Note: mode replace by number: 1 = "batch basal only", 2 = "normal fed-batch"
#                               3 = "w/N-1 perfusion", 4 = "Basal only continuous"...
#                               (refer to "Cost Calculations.xlsx")
modeUSData <- function(mode = 4, bioVol = 2000, basal){
  outDF <- c()
  if(!(mode %in% c(1:10))){
    e <- "the mode selection is not batch nor continuous."
    base::stop(e)
  }
  bioVolRef <- c(2000,	2000,	2000,	500,	500,	500,	500,	500,	2000,	500)
  mAbTarget <- c(1303,	9747,	12274,	9660,	27480,	4560,	7116,	12203,	9747,	27480)
  endTime <- c(8,	14,	14,	35,	35,	18,	18,	18,	14,	35)
  basalRef <- c(2040,	1378,	1618,	16250,	16250,	7970,	7450,	6930,	1378,	16250)
  labour <- c(12,	12,	12,	6,	6,	12,	12,	12,	12,	6)
  superLabour <- floor(labour*0.2)
  wage <- 1000
  carbon <- c(550.1710968,	600.1710968,	650.1710968,	117.5427742,	137.5427742,	127.5427742,	147.5427742,	157.5427742,	560.1710968,	167.5427742)
  mAbCarbon <- 14.15
  
  if(bioVol != bioVolRef[mode]){
    mAbTarget[mode] <- mAbTarget[mode]/bioVolRef[mode]*bioVol
  }
  if(basal > basalRef[mode]){
    basalRef[mode] <- basalRef[mode]/bioVolRef[mode]*bioVol
  }else{
    basalRef[mode] <- basal
  }
  
  if(mode %in% c(1,2)){
    df <- read.csv("batch.csv")
    x <- c(0, rep(endTime[1], nrow(df)))
    y <- c(0, df$mAb[1:(nrow(df)/2)]/2, df$mAb[(nrow(df)/2+1):nrow(df)])
    x1 <- rep(bioVol, n=nrow(df)+1)
    x2 <- rep(ceiling(mAbTarget[1]/bioVol), n=nrow(df)+1)
    basal <- c(mean(df$basal), df$basal)
    labourNo <- rep((labour[1]+superLabour[1]), n=nrow(df)+1)
    carbon <- y*mAbCarbon
    outDF <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2, basal = basal, labour = labourNo, carbon = carbon)
  }
  
  if(mode %in% c(3,4)){
    df <- read.csv("cont_basal_only.csv")
    x <- c(0, rep(endTime[4], nrow(df)))
    y <- c(0, df$mAb[1:(nrow(df)/2)]/2, df$mAb[(nrow(df)/2+1):nrow(df)])
    x1 <- rep(bioVol, n=nrow(df)+1)
    x2 <- rep(ceiling(mAbTarget[4]/bioVol), n=nrow(df)+1)
    basal <- c(mean(df$basal), df$basal)
    labourNo <- rep((labour[4]+superLabour[4]), n=nrow(df)+1)
    carbon <- y*mAbCarbon
    outDF <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2, basal = basal, labour = labourNo, carbon = carbon)
  }
  
  if(mode %in% c(5,6)){
    df <- read.csv("batch_feed.csv")
    feed <- c(0, df$feed[1:(nrow(df)/2)], df$feed[(nrow(df)/2+1):nrow(df)])
    x <- c(0, rep(endTime[1], nrow(df)))
    y <- c(0, df$mAb[1:(nrow(df)/2)]/2, df$mAb[(nrow(df)/2+1):nrow(df)])
    x1 <- rep(bioVol, n=nrow(df)+1)
    x2 <- rep(ceiling(mAbTarget[1]/bioVol), n=nrow(df)+1)
    basal <- c(mean(df$basal), df$basal)
    labourNo <- rep((labour[1]+superLabour[1]), n=nrow(df)+1)
    carbon <- y*mAbCarbon
    outDF <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2, basal = basal, feed = feed, labour = labourNo, carbon = carbon)
  }
  
  if(mode %in% c(7,8)){
    df <- read.csv("cont_basal_feed_new.csv")
    x <- c(0, rep(endTime[5], nrow(df)))
    y <- c(0, df$mAb[1:(nrow(df)/2)]/2, df$mAb[(nrow(df)/2+1):nrow(df)])
    x1 <- rep(bioVol, n=nrow(df)+1)
    x2 <- rep(ceiling(mAbTarget[5]/bioVol), n=nrow(df)+1)
    basal <- c(mean(df$basal), df$basal)
    labourNo <- rep((labour[5]+superLabour[5]), n=nrow(df)+1)
    feed <- c(0, df$feed[1:(nrow(df)/2)]/0.01, df$feed[(nrow(df)/2+1):nrow(df)])
    carbon <- y*mAbCarbon
    outDF <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2, basal = basal, feed = feed, labour = labourNo, carbon = carbon)
  }
  return(outDF)
}

# data generation function for downstream
# Here is to generate the demo data, which is stored at the backend of the R host.
# input: mode (manufacturing mode: default 1),
#        predMAB (default 2000g)
# output: generated demo dataset
# Note: mode replace by number: 1 = "batch basal only", 2 = "normal fed-batch"
#                               3 = "w/N-1 perfusion", 4 = "Basal only continuous"...
#                               (refer to "Cost Calculations.xlsx")
generateDSData <- function(mode = 4, predMAB = 2000){
  outDF <- c()
  if(!(mode %in% c(1:10))){
    e <- "the mode selection is not batch nor continuous."
    base::stop(e)
  }
  costRef <- c(261840,	261840,	261840,	242595,	242595,	261840,	261840,	261840,	261840,	242595)
  mAbTarget <- c(1303,	9747,	12274,	9660,	27480,	4560,	7116,	12203,	9747,	27480)
  endTime <- c(8,	14,	14,	28,	24,	18,	18,	18,	14,	24)
  TimeTotal <- 42
  labour <- c(12,	12,	12,	6,	6,	12,	12,	12,	12,	6)
  superLabour <- floor(labour*0.2)
  wage <- 1000
  n=50
  carbon <- c(550.1710968,	600.1710968,	650.1710968,	117.5427742,	137.5427742,	127.5427742,	147.5427742,	157.5427742,	560.1710968,	167.5427742)
  mAbCarbon <- 14.15
  
  if(mode %in% c(1,3,5,7)){
    cost_total <- rnorm(n,costRef[mode],1000)+(labour[mode]+superLabour[mode])*wage*TimeTotal
    mab_target <- rnorm(n,predMAB, 100)
    finalFill_target <- mab_target*0.95
    days <- rep(TimeTotal, n)
    outDF <- data.frame(day = days, ff = finalFill_target, cost = cost_total, target = mab_target)
  }else{
    cost_total <- rnorm(n,costRef[mode],1000)+(labour[mode]+superLabour[mode])*wage*endTime[mode]
    mab_target <- rnorm(n,predMAB, 100)
    finalFill_target <- mab_target*0.95
    days <- rep(TimeTotal, n)
    outDF <- data.frame(day = days, ff = finalFill_target, cost = cost_total, target = mab_target)
  }
  outDF[1:(n/2),] <- outDF[1:(n/2),]/2
  return(outDF)
}

# data generation function for carbon footprint
# Here is to generate the demo data, which is stored at the backend of the R host.
# input: mode (manufacturing mode: default 1),
#        predMAB (default 2000g)
# output: generated demo dataset
# Note: mode replace by number: 1 = "batch basal only", 2 = "normal fed-batch"
#                               3 = "w/N-1 perfusion", 4 = "Basal only continuous"...
#                               (refer to "Cost Calculations.xlsx")
generateCF <- function(mode = 4, predMAB = 2000, bioVol = 2000, basal){
  outDF <- c()
  if(!(mode %in% c(1:10))){
    e <- "the mode selection is not batch nor continuous."
    base::stop(e)
  }
  bioVolRef <- c(2000,	2000,	2000,	500,	500,	500,	500,	500,	2000,	500)
  costRef <- c(261840,	261840,	261840,	242595,	242595,	261840,	261840,	261840,	261840,	242595)
  mAbTarget <- c(1303,	9747,	12274,	9660,	27480,	4560,	7116,	12203,	9747,	27480)
  endTime <- c(8,	14,	14,	28,	24,	18,	18,	18,	14,	24)
  basalRef <- c(2040,	1378,	1618,	16250,	16250,	7970,	7450,	6930,	1378,	16250)
  carbon <- c(550.1710968,	600.1710968,	650.1710968,	117.5427742,	137.5427742,	127.5427742,	147.5427742,	157.5427742,	560.1710968,	167.5427742)
  mAbCarbon <- 14.15
  
  TimeTotal <- 42
  labour <- c(12,	12,	12,	6,	6,	12,	12,	12,	12,	6)
  superLabour <- floor(labour*0.2)
  wage <- 1000
  n=50
  
  if(bioVol != bioVolRef[mode]){
    mAbTarget[mode] <- mAbTarget[mode]/bioVolRef[mode]*bioVol
  }
  if(basal > basalRef[mode]){
    basalRef[mode] <- basalRef[mode]/bioVolRef[mode]*bioVol
  }else{
    basalRef[mode] <- basal
  }
  carbonTarget <- carbon + mAbTarget*mAbCarbon/1000
  
  # if(mode < 4 || mode > 5){
    x <- c(0.0001, ceiling(endTime[mode]/3*2), endTime[mode])
    y <- c(carbon[mode]/endTime[mode], carbonTarget[mode]/3*2, carbonTarget[mode])
    x1 <- rep(bioVol, n=3)
    x2 <- rep(ceiling(carbonTarget[mode]/bioVol), n=3)
    basalCol <- rep(basalRef[mode], 3)
    labourNo <- rep((labour[mode]+superLabour[mode]), n=3)
    outDF <- data.frame(day = x, carbon = y, basal = basalCol)
  # }
  
  # if(mode == 4 || mode == 5){
  #   x <- c(0.0001, ceiling(endTime[mode]/2), endTime[mode])
  #   y <- c(carbon[mode]/endTime[mode], carbonTarget[mode]/2, carbonTarget[mode])
  #   x1 <- rep(bioVol, n=3)
  #   x2 <- rep(ceiling(carbonTarget[mode]/bioVol), n=3)
  #   basalCol <- c(basal/3, basal/3*2, basal)
  #   labourNo <- rep((labour[mode]+superLabour[mode]), n=3)
  #   outDF <- data.frame(day = x, carbon = y, basal = basalCol)
  # }
  
  return(outDF)
}

# data generation function for upstream
# Here is to generate the demo data, which is stored at the backend of the R host.
# input: mode (manufacturing mode: default 1), 
#        bioVol (bioreactor vol: default 2000L)
# output: generated demo dataset
# Note: mode replace by number: 1 = "batch basal only", 2 = "normal fed-batch"
#                               3 = "w/N-1 perfusion", 4 = "Basal only continuous"...
#                               (refer to "Cost Calculations.xlsx")
nnModeUSData <- function(mode = 4, bioVol = 2000, endTime = 8, predProp = 1, basalInput = 2000){
  outDF <- c()
  if(!(mode %in% c(1:10))){
    e <- "the mode selection is not batch nor continuous."
    base::stop(e)
  }
  bioVolRef <- c(2000,	2000,	2000,	500,	500,	500,	500,	500,	2000,	500)
  mAbTarget <- c(1303,	9747,	12274,	9660,	27480,	4560,	7116,	12203,	9747,	27480)
  
  cost_basal <- 10
  cost_feed <- 100
  days <- endTime
  n=50 # number of historic campaigns
  
  if(bioVol != bioVolRef[mode]){
    mAbTarget[mode] <- mAbTarget[mode]/bioVolRef[mode]*bioVol
  }
  df <- c()
  if(mode %in% c(1,2)){
    df <- read.csv("batch.csv")
    if(mode == 1){
      df <- df*rnorm(ncol(df), 0.85, 0.1)
    }
  }
  if(mode %in% c(3,4)){
    df <- read.csv("cont_basal_only.csv")
    if(mode == 3){
      df <- df*rnorm(ncol(df), 0.95, 0.1)
    }
  }
  if(mode %in% c(5,6)){
    df <- read.csv("batch_feed.csv")
    if(mode == 6){
      df <- df*rnorm(ncol(df), 1.01, 0.1)
    }
  }
  if(mode %in% c(7,8)){
    df <- read.csv("cont_basal_feed_new.csv")
    if(mode == 8){
      df <- df*rnorm(ncol(df), 1.05, 0.1)
    }
  }

  if(mode %in% c(1,2,5,6)){
    # Mode batch input
    set.seed(endTime)

    target_supernatant <- rnorm(n,0.7,0.05)
    pcv_adjusted=target_supernatant*rnorm(n,0.99,0.001)
    basal=df$basal
    feed_batch = rnorm(n, 88, 10)
    if("feed" %in% colnames(df)){
      feed_batch <- df$feed
    }
    mAb_output=df$mAb
    cost=cost_basal*basal
    
    cost_g=cost/mAb_output
    output_day=mAb_output/days
    X_out <- data.frame(target = df$mAb,
                        pcv = pcv_adjusted,
                        basal = df$basal,
                        feed = feed_batch,
                        cost = cost_g,
                        COGs = output_day)
  }else{
    set.seed(endTime)
    target_supernatant_basal_cont=rnorm(n,0.69,0.05)
    basal_cont=df$basal
    feed_cont = rnorm(n, 88, 10)
    if("feed" %in% colnames(df)){
      feed_batch <- df$feed
    }
    # Mode continuous basal only output
    mAb_output_basal_cont=df$mAb
    cost_cont=cost_basal*basal_cont
    
    cost_g_cont=cost_cont/mAb_output_basal_cont
    output_day_cont=mAb_output_basal_cont/days
    X_out <- data.frame(target = df$mAb,
                        basal = df$basal,
                        feed = feed_cont,
                        cost = cost_g_cont,
                        COGs = output_day_cont)
  }
  outDF <- X_out*predProp
  return(outDF)
}

# replace the mode with number
# input: input list from shiny platform
# output: corresponding mode number
# Note: mode replace by number: 1 = "batch basal only", 2 = "normal fed-batch"
#                               3 = "w/N-1 perfusion", 4 = "Basal only continuous"...
#                               (refer to "Cost Calculations.xlsx")
modeNumber <- function(input){
  outNo <- 1
  USPBase <- input$selectUSP
  feed <- input$selectFed
  DSP <- input$selectDSP
  perfusion <- input$selectPerfusion
  concentrate <- input$selectConcentrate
  if(USPBase == "Batch"){
    if(concentrate){
      outNo <- 6
      if(feed == "Feed Condition 1"){
        outNo <- 7
      }
      if(feed == "Feed Condition 1"){
        outNo <- 8
      }
    }else{
      if(grepl("Feed", feed)){
        outNo <- 2
      }
      if(perfusion){
        outNo <- 3
      }
    }
  }
  if(USPBase == "Continuous"){
    if(feed == "None"){
      outNo <- 4
    }else{
      outNo <- 5
    }
  }
  if(USPBase == "Mixed"){
    if(perfusion){
      outNo <- 10
    }else{
      outNo <- 9
    }
  }
  
  if(DSP != "Batch"){
    outNo <- outNo + 1
  }
  return(outNo)
}

modeNoCombine <- function(USPBase = "Batch", DSP = "Batch", input){
  outNo <- 1
  feed <- input$selectFed
  if(feed == "None"){
    if(USPBase == "Batch"){
      if(DSP == "Batch"){
        outNo <- 1
      }else{
        outNo <- 2
      }
    }else{
      if(DSP == "Batch"){
        outNo <- 3
      }else{
        outNo <- 4
      }
    }
  }else{
    if(USPBase == "Batch"){
      if(DSP == "Batch"){
        outNo <- 5
      }else{
        outNo <- 6
      }
    }else{
      if(DSP == "Batch"){
        outNo <- 7
      }else{
        outNo <- 8
      }
    }
  }

  return(outNo)
}

# replace the upstream mode with number
multiMode <- function(input){
  outNo <- c()
  USPBase <- input$selectUSP
  DSP <- input$selectDSP
  
  comb <-  expand.grid(USPBase,DSP)
  outNo <- rep(0, nrow(comb))
  for(i in 1:nrow(comb)){
    outNo[i] <- modeNoCombine(comb[i,1], comb[i,2], input)
  }
  outNo <- unique(outNo)
  return(outNo)
}

replaceMode <- function(modeList){
  outList <- rep("", length(modeList))
  for(i in 1:length(modeList)){
    if(modeList[i] %in% c(1, 5)){
      outList[i] <- "Batch-Batch"
    }
    if(modeList[i] %in% c(2, 6)){
      outList[i] <- "Batch-Continuous"
    }
    if(modeList[i] %in% c(3, 7)){
      outList[i] <- "Continuous-Batch"
    }
    if(modeList[i] %in% c(4, 8)){
      outList[i] <- "Continuous-Continuous"
    }
  }
  return(outList)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(
    titleWidth = 400,
    title = "Bioprocess Demo"),
  dashboardSidebar(
    width = 400,
    mainPanel(
      # sidebarPanel(
      tags$div(
        HTML(paste(
          "<b>Process Settings: </b>", 
          sep = ""))
      ),
      sliderInput("bioVolInput",
                  "Volume of bioreactor (L):",
                  min = 1,
                  max = 2000,
                  value = 2000),
      sliderInput("basalInput",
                  "Basal (g/batch):",
                  min = 1400,
                  max = 2000,
                  value = 1700, step = 50),
      sliderInput("feedInput",
                  "Feed (g/batch):",
                  min = 0,
                  max = 167,
                  value = 0, step = 6.2),
      sliderInput("dayInput",
                  "Total upstream duration (days):",
                  min = 1,
                  max = 36,
                  value = 8),
      
      checkboxGroupButtons( # or radioGroupButtons
        inputId = "selectUSP",
        label = "Upstream Process",
        choices = c("Batch", "Continuous"
                    # , "Mixed"
        ),
        selected = "Batch"
      ),
      fluidRow(
        column(6,
               selectInput( # or radioGroupButtons
                 inputId = "selectFed",
                 label = "Feed Condition",
                 choices = c("None", "Feed Condition 1", "Feed Condition 2"),
                 selected = "None"
               )
        ),
        column(3,
               selectInput( # or radioGroupButtons
                 inputId = "selectConcentrate",
                 label = "Concentrate",
                 choices = c("True", "False"),
                 selected = "False"
               )
        ),
        column(3,
               selectInput( # or radioGroupButtons
                 inputId = "selectPerfusion",
                 label = "Perfusion",
                 choices = c("True", "False"),
                 selected = "False"
               )
        )
      ),
      
      checkboxGroupButtons( # or radioGroupButtons
        inputId = "selectDSP",
        label = "Downstream Process",
        choices = c("Batch", "Continuous"),
        selected = "Batch"
      ),
      numericInput(
        inputId = "neighbourNumber",
        label = "Nearest Neighbour Number",value = 3,
        min = 1, max = 50, step = 1
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabsetPanel(
      tabPanel("KPIs",
               
               br(),
               box(title = "KPIs",collapsible = TRUE,
                   collapsed = FALSE,solidHeader = TRUE,
                 width = 12,
                 fluidRow(
                   column(6,
                          plotOutput("NNPlot")
                          ),
                   column(6,
                          plotOutput("throughputPlot")
                          )
                 ),
                 
               ),

               box(title = "Basal Profile",collapsible = TRUE,
                   collapsed = TRUE,solidHeader = TRUE,
                 width = 6,
                 plotOutput("basalPlot")
               ),
               box(title = "Sustainablility Profile",collapsible = TRUE,
                   collapsed = TRUE,solidHeader = TRUE,
                   width = 6,
                   plotOutput("cfPlot")
               ),
               box(title = "History Profile", collapsible = TRUE,
                   collapsed = FALSE,solidHeader = FALSE,
                   width = 12,
                   reactableOutput("historyTable")
               ),
      ),
      tabPanel("Process Analytics",
               br(),
               box(
                 width = 12,
                 fluidRow(column(12, 
                                 tags$div(
                                   HTML(paste(
                                     "<b>Upstream Process: </b>", 
                                     sep = "")),
                                   
                                 ),
                                 plotOutput("distPlot"),
                                 tags$div(
                                   HTML(paste(
                                     "<b>Note: </b>", 
                                     "<ul>
                                             <li><font color='#E5415D'>Red line</font>: predicted mAb outcome; </li>
                                             <li><font color='#1DD1A1'>Green curve</font>: growing curve of mAb;</li></ul>", 
                                     sep = ""))
                                 )),
                          column(12, 
                                 tags$div(
                                   HTML(paste(
                                     "<b>Downstream Process: </b>", 
                                     sep = ""))
                                 ),
                                 plotOutput("svmDSPlot"),
                                 tags$div(
                                   HTML(paste(
                                     "<b>Note: </b>", 
                                     "<ul>
                                             <li><font color='#1DD1A1'>Green curve</font>: productive curve of final fill;</li>
                                             <li><font color='#E5415D'>Red line</font>: average final fill per day.</li></ul>", 
                                     sep = ""))
                                 ))
                 ),
                 fluidRow(column(12, 
                                 tags$div(
                                   HTML(paste(
                                     "<b>Carbon Footprint: </b>", 
                                     sep = "")),
                                   
                                 ),
                                 plotOutput("carbonPlot"),
                                 tags$div(
                                   HTML(paste(
                                     "<b>Note: </b>", 
                                     "<ul>
                                             <li><font color='#E5415D'>Red line</font>: predicted CO2 outcome; </li>
                                             <li><font color='#1DD1A1'>Green curve</font>: growing curve of CO2;</li></ul>", 
                                     sep = ""))
                                 ))))
      ),selected = "Process Analytics"
      
    ),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCloseWindow, functions = c("closeWindow"))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  preliminaryResult <- reactiveValues()
  
  observeEvent(input$feedInput, {
    if(input$feedInput>0){
      updateSelectInput(session, inputId = "selectFed", selected = "Feed Condition 1")
    }else{
      updateSelectInput(session, inputId = "selectFed", selected = "None")
    }
  })

  #only for testing
  observe({
    selectVol <- input$bioVolInput
    selectBasal <- input$basalInput
    # modeNo <- modeNumber(input = input)
    modeNo <- multiMode(input = input)
    outData <- c()
    for(i in 1:length(modeNo)){
      outData[[i]] <- modeUSData(mode = modeNo[i], bioVol = selectVol, basal = selectBasal)
    }
    #save the generated data
    preliminaryResult$histData <- outData
  })

  #generate the prediction
  predDF <- reactive({
    modeList <- multiMode(input = input)
    preliminaryResult$propPred <- c()
    outDF <- c()
    for(i in 1:length(preliminaryResult$histData)){
      modeNo <- modeList[i]
      df <- preliminaryResult$histData[[i]]
      model <- c()
      if("feed" %in% colnames(df)){
        model <- glm( (mAb/max(mAb)) ~ day+basal+feed, data = df, family = quasibinomial)
        days <- seq(1, input$dayInput, 0.01)
        basal <- rep(input$basalInput, length(days))
        if(modeNo %in% c(3,7)){
          basal <- basal*unique(df$batchNo)
        }
        feed <- rep(input$feedInput, length(days))
        slope <- mean(df$labour)*1000
        intercept <- mean(df$basal)*10
        newdata = data.frame(day=days, basal = basal, feed = feed)
        pred <- predict(model, newdata, type="response")
        mAb <- predict(model, newdata, type="response")*unique(df$batchNo)*max(df$mAb)
        if(modeNo[i] %in% c(3,4,7,8)){
          mAb <- mAb*unique(df$batchNo)
        }
        cost <- (intercept+slope*days)/100
        outDF[[i]] <- data.frame(days = days, mAb = mAb, cost = cost, pred = pred)
      }else{        
        model <- glm( (mAb/max(mAb)) ~ day+basal, data = df, family = quasibinomial)
        days <- seq(1, input$dayInput, 0.01)
        basal <- rep(input$basalInput, length(days))
        if(modeNo %in% c(3,7)){
          basal <- basal*unique(df$batchNo)
        }
        slope <- mean(df$labour)*1000
        intercept <- mean(df$basal)*10
        newdata = data.frame(day=days, basal = basal)
        pred <- predict(model, newdata, type="response")
        mAb <- predict(model, newdata, type="response")*unique(df$batchNo)*max(df$mAb)
        if(modeNo %in% c(3,7,8)){
          mAb <- mAb*unique(df$batchNo)*9
        }
        cost <- (intercept+slope*days)/100
        outDF[[i]] <- data.frame(days = days, mAb = mAb, cost = cost, pred = pred)
      }
      # preliminaryResult$propPred[[i]] <- outDF[[i]]$pred
    }
    return(outDF)
  })
  predCF <- reactive({
    modeNo <- multiMode(input = input)
    selectVol <- input$bioVolInput
    selectBasal <- input$basalInput
    
    outDF <- c()
    for(i in 1:length(preliminaryResult$histData)){
      df <- generateCF(mode = modeNo[i], bioVol = selectVol, basal = selectBasal)
      # if(modeNo==1){
      model <- glm( (carbon/(carbon+0.1)) ~ day+basal, data = df, family = binomial)
      # }else{
      #   model <- glm( (mAb/(bioVol*batchNo)) ~ day+basal, data = carbonDF(), family = binomial)
      # }
      days <- seq(1, input$dayInput, 0.01)
      basal <- rep(input$basalInput, length(days))
      
      newdata = data.frame(day=days, basal = basal)
      preliminaryResult$carbonPred <- predict(model, newdata, type="response")
      carbon <- predict(model, newdata, type="response")*df$carbon[3]
      outDF[[i]] <- data.frame(days = days, carbon = carbon)
    }
    return(outDF)
  })
  output$carbonPlot <- renderPlot({
    dataFrame <- predCF()
    modeNo <- multiMode(input = input)
    pTemp <- c()
    for(i in 1:length(modeNo)){
      
      df <- dataFrame[[i]]
      specificDayValue <- df[df$days==input$dayInput,]
      preliminaryResult$carbonPred <- specificDayValue
      preliminaryResult$carbonNo <- df$days==input$dayInput

      df <- cbind(df, specificDayValue$carbon)
      colnames(df)[3] <- "predCarbon"
      pTemp[[i]] <- ggplotGrob(
        ggplot(df, aes(x = days)) +
          geom_line(aes(y = carbon), colour = "#1DD1A1") +
          theme(text = element_text(size=25)) +
          geom_hline(yintercept = specificDayValue$carbon, size=1, colour = "#E5415D")+
          theme_bw() +
          scale_y_continuous(
            name = "CO2 (kg)",
            breaks = sort(unique(c(specificDayValue$carbon, pretty(df$carbon))))
            ,guide = guide_axis(check.overlap = TRUE)
          )
      )
    }
    minNum <- 2
    if(length(dataFrame)>=minNum){
      minNum <- length(dataFrame)
    }
    outputPlot <- do.call(grid.arrange, c(pTemp, ncol=minNum))

    return(outputPlot)
  })
  output$distPlot <- renderPlot({
    dataFrame <- predDF()
    preliminaryResult$rowNo <- c()
    preliminaryResult$mAbPred <- c()
    pTemp <- c()
    for(i in 1:length(input$selectUSP)){
      df <- dataFrame[[i]]

      specificDayValue <- df[df$days==input$dayInput,]
      preliminaryResult$mAbPred <- specificDayValue
      preliminaryResult$rowNo <- df$days==input$dayInput

      pTemp[[i]] <- ggplotGrob(

        ggplot(df, aes(x = days, y = mAb)) +
          geom_line(colour = "#1DD1A1") +
          theme(text = element_text(size=25)) +
          geom_hline(yintercept = specificDayValue$mAb, size=1, colour = "#E5415D")+
          # geom_line( aes(y=cost), size=1, color="#F9A602") +
          theme_bw() +
          scale_y_continuous(
            
            # Features of the first axis
            name = "mAb (g)",
            
            # Add a second axis and specify its features
            # sec.axis = sec_axis(~.*1, name="Cost (hundred dollars)"),
            # ) +
            # scale_y_continuous(
            breaks = sort(unique(c(specificDayValue$mAb, pretty(df$mAb))))
            ,guide = guide_axis(check.overlap = TRUE)
          )
      )
    }
    if(length(dataFrame) == 1){
      outputPlot <- do.call(grid.arrange, c(pTemp, ncol=2))
    }else{
      outputPlot <- do.call(grid.arrange, c(pTemp, ncol=2))
    }
    return(outputPlot)
  })

  #nn data generate
  output$NNPlot <- renderPlot({
    dataFrame <- predDF()

    modeNo <- multiMode(input = input)
    histDF <- c()
    cost_pred <- c()
    saveHist <- c()
    num <- 1
    for(i in 1:length(dataFrame)){
      df <- dataFrame[[i]]
      preliminaryResult$rowNo <- df$days==input$dayInput
      histDF[[i]] <- nnModeUSData(mode = modeNo[i],
                              bioVol = input$bioVolInput,
                              endTime = input$dayInput,
                              # predProp = dataFrame[[i]]$pred[preliminaryResult$rowNo],
                              predProp = 1,
                              basalInput = input$basalInput)
    
      historyNN <- histDF[[i]]
      trainX <- c()
      testX <- c()
      refVol <- 1700
      if(modeNo[i] %in% c(3,4,7,8)){
        trainX <- as_tibble(historyNN[,c(2)])
        colnames(trainX) <- c("basal")
        if(input$feedInput > 0){
          trainX <- historyNN[,c(2:3)] 
        }

        testX <- trainX[1,]
        # testX$target <-dataFrame[[i]]$pred[preliminaryResult$rowNo]*9
        testX$basal <- input$basalInput
        if(input$feedInput > 0){
          testX$feed <- input$feedInput
        }
        refVol <- 16000
      }else{
        trainX <- historyNN[,c(2:3)]
        
        if(input$feedInput > 0){
          trainX <- historyNN[,c(2:4)]
        }
        testX <- trainX[1,]
        # testX$target <-dataFrame[[i]]$pred[preliminaryResult$rowNo]
        # testX$pcv <- 0.6
        if(input$feedInput > 0){
          testX$feed <- input$feedInput
        }
        testX$basal <- input$basalInput
      }
      trainY <- historyNN$target

      pred = knn.reg(train = trainX, test = testX, y = trainY, k = input$neighbourNumber)
      X_all <- rbind(trainX, testX)
      nnOut <- get.knn(X_all, k = input$neighbourNumber+1, algorithm = "cover_tree")
      dist <- nnOut$nn.dist[nrow(nnOut$nn.dist),1]
      nb <- nnOut$nn.index[nrow(nnOut$nn.index),c(1:(input$neighbourNumber))]
      nnPlot <- historyNN[nb,]
      historyNN$group <- rep(0, n = nrow(historyNN))
      historyNN[nb,]$group <- 1
      nnDF <- historyNN[nb,]
      num <- 3
      if(nrow(nnDF)<3){
        num <- nrow(nnDF)
      }
      saveHist[[i]] <- nb[1:input$neighbourNumber]
      # saveHist[[i]] <- cbind(nnDF[1:input$neighbourNumber,], data.frame("Mode" = rep(modeNo[i], input$neighbourNumber)))
      cost_basal=10
      cost_feed_batch=150
      cost_feed_cont=100
      cost_pred[[i]] <- (input$basalInput*cost_basal)/pred$pred
      if(input$feedInput > 0){
        if(modeNo[i] %in% c(1,2,5,6)){
          cost_pred[[i]] <- (input$basalInput*cost_basal+input$feedInput*cost_feed_batch)/pred$pred
          if(modeNo[i]%in% c(5,6)){
            cost_pred[[i]] <- (input$basalInput*cost_basal+input$feedInput*50)/pred$pred
          }
        }else{
          cost_pred[[i]] <- (input$basalInput*cost_basal+input$feedInput*cost_feed_cont)/pred$pred
        }
      }
      
      cost_ref <- nnDF$cost[1:num]
      cost_pred[[i]] <- c(cost_pred[[i]], cost_ref)
    }
    cost_total <- c()
    cost_total <- do.call(c, cost_pred)
    preliminaryResult$neighbourHistory <- saveHist
    yerror <- sd(nnPlot$COGs)
    xerror <- sd(nnPlot$cost)
    
    errorDF <- data.frame(KPI = rep(c("Pred.", paste("Ref.", 1:num)), n = length(dataFrame)),
                          value = cost_total,
                          group = (rep(replaceMode(modeNo), each = length(cost_total)/length(modeNo))))
    
    ggplot(errorDF, aes(x = group, y = value, fill = KPI))+
      theme(text = element_text(size=25)) +
      theme_bw() +
      scale_fill_manual(values = c("Pred." = "#4060C0"))+
      geom_bar(stat="identity", width=0.7, position="dodge")+ 
      ylab("Cost per mAb (dollar/g)") + xlab("Mode")+
      coord_cartesian(ylim = c(5, NA))
   
  })
  output$throughputPlot <- renderPlot({

    dataFrame <- predDF()
    
    modeNo <- multiMode(input = input)
    histDF <- c()
    cost_pred <- c()
    num <- 1
    for(i in 1:length(dataFrame)){
      df <- dataFrame[[i]]
      preliminaryResult$rowNo <- df$days==input$dayInput
      histDF[[i]] <- nnModeUSData(mode = modeNo[i],
                                  bioVol = input$bioVolInput,
                                  endTime = input$dayInput,
                                  # predProp = dataFrame[[i]]$pred[preliminaryResult$rowNo],
                                  predProp = 1,
                                  basalInput = input$basalInput)
      
      historyNN <- histDF[[i]]
      trainX <- c()
      testX <- c()
      refVol <- 1700
      
      if(modeNo[i] %in% c(3,4,7,8)){
        trainX <- historyNN[,c(1:2)]
        if(input$feedInput > 0){
          trainX <- historyNN[,c(1:3)]
        }
        testX <- trainX[1,]
        # testX$target <-dataFrame[[i]]$pred[preliminaryResult$rowNo]*9
        testX$basal <- input$basalInput
        if(input$feedInput > 0){
          testX$feed <- input$feedInput
        }
        refVol <- 16000
      }else{
        trainX <- historyNN[,c(1:3)]
        if(input$feedInput > 0){
          trainX <- historyNN[,c(1:4)]
        }
        testX <- trainX[1,]
        # testX$target <-dataFrame[[i]]$pred[preliminaryResult$rowNo]
        # testX$pcv <- mean(trainX$pcv)
        if(input$feedInput > 0){
          testX$feed <- input$feedInput
        }
        testX$basal <- input$basalInput
      }
      trainY <- historyNN$target
      pred = knn.reg(train = trainX, test = testX, y = trainY, k = input$neighbourNumber)
      X_all <- rbind(trainX, testX)
      nnOut <- get.knn(X_all, k = input$neighbourNumber+1, algorithm = "cover_tree")
      dist <- nnOut$nn.dist[nrow(nnOut$nn.dist),1]
      nb <- nnOut$nn.index[nrow(nnOut$nn.index),c(1:(input$neighbourNumber))]
      nnPlot <- historyNN[nb,]
      historyNN$group <- rep(0, n = nrow(historyNN))
      historyNN[nb,]$group <- 1
      nnDF <- historyNN[nb,]
      num <- 3
      if(nrow(nnDF)<3){
        num <- nrow(nnDF)
      }
      cost_basal=10
      cost_feed_batch=150
      cost_feed_cont=100
      cost_pred[[i]] <- pred$pred/input$dayInput
      cost_ref <- nnDF$target[1:num]/input$dayInput
      cost_pred[[i]] <- c(cost_pred[[i]], cost_ref)
    }
    cost_total <- c()
    cost_total <- do.call(c, cost_pred)
    
    errorDF <- data.frame(KPI = rep(c("Pred.", paste("Ref.", 1:num)), n = length(dataFrame)),
                          value = cost_total,
                          group = (rep(replaceMode(modeNo), each = length(cost_total)/length(modeNo))))
    
    ggplot(errorDF, aes(x = group, y = value, fill = KPI))+
      theme(text = element_text(size=25)) +
      theme_bw() +
      scale_fill_manual(values = c("Pred." = "#4060C0"))+
      geom_bar(stat="identity", width=0.7, position="dodge")+ 
      ylab("mAb per day (g/day)") + xlab("Mode")+
      coord_cartesian(ylim = c(50, NA))
  })
  output$basalPlot <- renderPlot({
    
    dataFrame <- predDF()
    
    modeNo <- multiMode(input = input)
    histDF <- c()
    cost_pred <- c()
    num <- 1
    for(i in 1:length(dataFrame)){
      df <- dataFrame[[i]]
      preliminaryResult$rowNo <- df$days==input$dayInput
      histDF[[i]] <- nnModeUSData(mode = modeNo[i],
                                  bioVol = input$bioVolInput,
                                  endTime = input$dayInput,
                                  # predProp = dataFrame[[i]]$pred[preliminaryResult$rowNo],
                                  predProp = 1,
                                  basalInput = input$basalInput)
      
      historyNN <- histDF[[i]]
      trainX <- c()
      testX <- c()
      refVol <- 1700
      
      if(modeNo[i] %in% c(3,4,7,8)){
        trainX <- historyNN[,c(1:2)]
        if(input$feedInput > 0){
          trainX <- historyNN[,c(1:3)]
        }
        testX <- trainX[1,]
        # testX$target <-dataFrame[[i]]$pred[preliminaryResult$rowNo]*9
        testX$basal <- input$basalInput
        if(input$feedInput > 0){
          testX$feed <- input$feedInput
        }
        refVol <- 16000
      }else{
        trainX <- historyNN[,c(1:3)]
        if(input$feedInput > 0){
          trainX <- historyNN[,c(1:4)]
        }
        testX <- trainX[1,]
        # testX$target <-dataFrame[[i]]$pred[preliminaryResult$rowNo]
        # testX$pcv <- mean(trainX$pcv)
        if(input$feedInput > 0){
          testX$feed <- input$feedInput
        }
        testX$basal <- input$basalInput
      }
      trainY <- historyNN$target
      pred = knn.reg(train = trainX, test = testX, y = trainY, k = input$neighbourNumber)
      X_all <- rbind(trainX, testX)
      nnOut <- get.knn(X_all, k = input$neighbourNumber+1, algorithm = "cover_tree")
      dist <- nnOut$nn.dist[nrow(nnOut$nn.dist),1]
      nb <- nnOut$nn.index[nrow(nnOut$nn.index),c(1:(input$neighbourNumber))]
      nnPlot <- historyNN[nb,]
      historyNN$group <- rep(0, n = nrow(historyNN))
      historyNN[nb,]$group <- 1
      nnDF <- historyNN[nb,]
      num <- 3
      if(nrow(nnDF)<3){
        num <- nrow(nnDF)
      }
      cost_basal=10
      cost_feed_batch=150
      cost_feed_cont=100
      cost_pred[[i]] <- input$basalInput
      cost_ref <- nnDF$basal[1:num]
      cost_pred[[i]] <- c(cost_pred[[i]], cost_ref)
    }
    cost_total <- c()
    cost_total <- do.call(c, cost_pred)
    
    errorDF <- data.frame(KPI = rep(c("Input", paste("Ref.", 1:num)), n = length(dataFrame)),
                          value = cost_total,
                          group = (rep(replaceMode(modeNo), each = length(cost_total)/length(modeNo))))
    
    ggplot(errorDF, aes(x = group, y = value, fill = KPI))+
      theme(text = element_text(size=25)) +
      theme_bw() +
      scale_fill_manual(values = c("Input" = "#E5415D"))+
      geom_bar(stat="identity", width=0.7, position="dodge")+ 
      ylab("Basal (g)") + xlab("Mode")+
      coord_cartesian(ylim = c(1200, NA))
  })
  output$svmDSPlot <- renderPlot({
    modeNo <- multiMode(input = input)[1]
    mAbPred <- preliminaryResult$mAbPred
    modeList <- multiMode(input = input)
    preliminaryResult$propPred <- c()
    outDF <- c()
    plotDF <- c()
    predFF <- c()
    for(i in 1:length(preliminaryResult$histData)){
      modeNo <- modeList[i]
      df <- preliminaryResult$histData[[i]]
      model <- c()
      if("feed" %in% colnames(df)){
        model <- glm( (mAb/max(mAb)) ~ day+basal+feed, data = df, family = quasibinomial)

        days <- seq(1, input$dayInput, 0.01)
        basal <- rep(input$basalInput, length(days))
        if(modeNo %in% c(3,7)){
          basal <- basal*unique(df$batchNo)
        }
        feed <- rep(input$feedInput, length(days))
        slope <- mean(df$labour)*1000
        intercept <- mean(df$basal)*10
        newdata = data.frame(day=days, basal = basal, feed = feed)
        pred <- predict(model, newdata, type="response")
        pred[pred<0] <- log(abs(pred[pred<0]))
        mAb <- predict(model, newdata, type="response")*unique(df$batchNo)*max(df$mAb)
        if(modeNo[i] %in% c(3,4,7,8)){
          mAb <- mAb*unique(df$batchNo)
        }
        cost <- (intercept+slope*days)/100
        outDF[[i]] <- data.frame(days = days, mAb = mAb, cost = cost, pred = pred)
      }else{        
        model <- glm( (mAb/max(mAb)) ~ day+basal, data = df, family = quasibinomial)
        days <- seq(1, input$dayInput, 0.01)
        basal <- rep(input$basalInput, length(days))
        if(modeNo %in% c(3,7)){
          basal <- basal*unique(df$batchNo)
        }
        slope <- mean(df$labour)*1000
        intercept <- mean(df$basal)*10
        newdata = data.frame(day=days, basal = basal)
        pred <- predict(model, newdata, type="response")
        pred[pred<0] <- log(abs(pred[pred<0]))
        mAb <- predict(model, newdata, type="response")*unique(df$batchNo)*max(df$mAb)
        if(modeNo %in% c(3,7,8)){
          mAb <- mAb*unique(df$batchNo)
        }
        cost <- (intercept+slope*days)/100
        outDF[[i]] <- data.frame(days = days, mAb = mAb, cost = cost, pred = pred)
      }
      # preliminaryResult$propPred[[i]] <- outDF[[i]]$pred
      mAbPred <- outDF[[i]]
      genDF <- generateDSData(modeNo, mAbPred$mAb)
      modelsvm = svm(ff~day+cost+target,genDF)
      costRef <- c(261840,	261840,	261840,	242595,	242595,	261840,	261840,	261840,	261840,	242595)
      days <- seq(1, input$dayInput, 0.01)
      labour <- c(12,	12,	12,	6,	6,	12,	12,	12,	12,	6)
      superLabour <- floor(labour*0.2)
      wage <- 1000
      cost <- (labour[modeNo]+superLabour[modeNo])*1000*days + costRef[modeNo]
      target <- mAbPred$mAb*(days)
      DF <- data.frame(day = days, cost = cost, target = target)
      predTotal <- predict(modelsvm, DF)
      predTotal <- predTotal + abs(min(predTotal))
      plotDF[[i]] <- data.frame(days = days, ff = predTotal)
      predFF[[i]] <- mean(predTotal)
    }
    pTemp <- c()
    for(i in 1:length(input$selectDSP)){

      pTemp[[i]] <- ggplotGrob(
        ggplot(plotDF[[i]], aes(x = days, y = ff)) + 
          geom_line(colour = "#1DD1A1") +
          theme(text = element_text(size=25)) +
          theme_bw() +
          geom_hline(yintercept = predFF[[i]], size=1, colour = "#E5415D")+
          xlab("days") + ylab("Final fill (g)")+
          scale_y_continuous(
            breaks = sort(unique(c(predFF[[i]], pretty(plotDF[[i]]$ff))))
            ,guide = guide_axis(check.overlap = TRUE)
          )
      )
    }
    if(length(plotDF) == 1){
      outputPlot <- do.call(grid.arrange, c(pTemp, ncol=2))
    }else{
      outputPlot <- do.call(grid.arrange, c(pTemp, ncol=2))
    }
    return(outputPlot)
  })
  output$cfPlot <- renderPlot({
    
    dataFrame <- predDF()
    
    modeNo <- multiMode(input = input)
    histDF <- c()
    cost_pred <- c()
    num <- 1
    for(i in 1:length(dataFrame)){
      df <- dataFrame[[i]]
      preliminaryResult$rowNo <- df$days==input$dayInput
      histDF[[i]] <- nnModeUSData(mode = modeNo[i],
                                  bioVol = input$bioVolInput,
                                  endTime = input$dayInput,
                                  # predProp = dataFrame[[i]]$pred[preliminaryResult$rowNo],
                                  predProp = 1,
                                  basalInput = input$basalInput)
      
      historyNN <- histDF[[i]]
      trainX <- c()
      testX <- c()
      refVol <- 1700
      
      if(modeNo[i] %in% c(3,4,7,8)){
        trainX <- historyNN[,c(1:2)]
        if(input$feedInput > 0){
          trainX <- historyNN[,c(1:3)]
        }
        testX <- trainX[1,]
        # testX$target <-dataFrame[[i]]$pred[preliminaryResult$rowNo]*9
        testX$basal <- input$basalInput
        if(input$feedInput > 0){
          testX$feed <- input$feedInput
        }
        refVol <- 16000
      }else{
        trainX <- historyNN[,c(1:3)]
        if(input$feedInput > 0){
          trainX <- historyNN[,c(1:4)]
        }
        testX <- trainX[1,]
        # testX$target <-dataFrame[[i]]$pred[preliminaryResult$rowNo]
        # testX$pcv <- mean(trainX$pcv)
        if(input$feedInput > 0){
          testX$feed <- input$feedInput
        }
        testX$basal <- input$basalInput
      }
      trainY <- historyNN$target
      pred = knn.reg(train = trainX, test = testX, y = trainY, k = input$neighbourNumber)
      X_all <- rbind(trainX, testX)
      nnOut <- get.knn(X_all, k = input$neighbourNumber+1, algorithm = "cover_tree")
      dist <- nnOut$nn.dist[nrow(nnOut$nn.dist),1]
      nb <- nnOut$nn.index[nrow(nnOut$nn.index),c(1:(input$neighbourNumber))]
      nnPlot <- historyNN[nb,]
      historyNN$group <- rep(0, n = nrow(historyNN))
      historyNN[nb,]$group <- 1
      nnDF <- historyNN[nb,]
      num <- 3
      if(nrow(nnDF)<3){
        num <- nrow(nnDF)
      }
      carbon <- c(550.1710968,	600.1710968,	650.1710968,560.1710968,137.5427742,	147.5427742,	157.5427742,		167.5427742)
      mAbCarbon <- 14.15
      cost_pred[[i]] <- pred$pred*mAbCarbon + carbon[modeNo[i]]*input$dayInput
      cost_ref <- nnDF$target[1:num]*mAbCarbon + carbon[modeNo[i]]*input$dayInput
      if(modeNo[i] %in% c(2,3,6,7)){
        cost_pred[[i]] <- pred$pred*mAbCarbon*1.5 + carbon[modeNo[i]]*input$dayInput
        cost_ref <- nnDF$target[1:num]*mAbCarbon*1.5 + carbon[modeNo[i]]*input$dayInput
      }
      if(modeNo[i] %in% c(4,8)){
        cost_pred[[i]] <- pred$pred*mAbCarbon*2 + carbon[modeNo[i]]*input$dayInput
        cost_ref <- nnDF$target[1:num]*mAbCarbon*2 + carbon[modeNo[i]]*input$dayInput
      }
      cost_pred[[i]] <- c(cost_pred[[i]], cost_ref)
    }
    cost_total <- c()
    cost_total <- do.call(c, cost_pred)
    
    errorDF <- data.frame(KPI = rep(c("Pred.", paste("Ref.", 1:num)), n = length(dataFrame)),
                          value = cost_total,
                          group = (rep(replaceMode(modeNo), each = length(cost_total)/length(modeNo))))
    
    ggplot(errorDF, aes(x = group, y = value, fill = KPI))+
      theme(text = element_text(size=25)) +
      theme_bw() +
      scale_fill_manual(values = c("Pred." = "#1DD1A1"))+
      geom_bar(stat="identity", width=0.7, position="dodge")+ 
      ylab("Total carbon dioxide (g)") + xlab("Mode")+
      coord_cartesian(ylim = c(10000, NA))
  })
  output$historyTable <- renderReactable({
    dataFrame <- predDF()
    modeNo <- multiMode(input = input)
    histDF <- c()
    cost_pred <- c()
    saveHist <- c()
    num <- 1
    for(i in 1:length(dataFrame)){
      df <- dataFrame[[i]]
      preliminaryResult$rowNo <- df$days==input$dayInput
      histDF[[i]] <- nnModeUSData(mode = modeNo[i],
                                  bioVol = input$bioVolInput,
                                  endTime = input$dayInput,
                                  predProp = 1,
                                  basalInput = input$basalInput)
      if("pcv" %in% colnames(histDF[[i]])){
        historyNN <- subset(histDF[[i]], select=-c(pcv))
      }else{
        historyNN <- histDF[[i]]
      }
      nnDF <- historyNN[preliminaryResult$neighbourHistory[[i]],]
      saveHist[[i]] <- cbind(data.frame("Mode" = rep(modeNo[i], input$neighbourNumber)), nnDF[1:input$neighbourNumber,])
      
    }
    outTable <- do.call(rbind,saveHist)
    outTable$Mode <- replaceMode(outTable$Mode)

    reactable(outTable,
              groupBy = "Mode",
              columns = list(
                Mode = colDef(width = 210),
                target = colDef(aggregate = "mean",format = colFormat(separators = TRUE, digits = 0)),
                basal = colDef(aggregate = "mean",format = colFormat(separators = TRUE, digits = 0)),
                feed = colDef(aggregate = "mean",format = colFormat(separators = TRUE, digits = 4)),
                cost = colDef(aggregate = "mean",format = colFormat(separators = TRUE, digits = 4)),
                COGs = colDef(aggregate = "mean",format = colFormat(separators = TRUE, digits = 4))
              ))
  })
  
  #Kill the process when the app has been closed or interrupted
  #this is the settings for the standalone software
  if (!interactive()) {
    session$onSessionEnded(function() {
      #get rid of all the backend data when the app terminated
      rm(list = ls())
      stopApp()
      q("no")
    })
  }
}

# Run the application 
shinyApp(ui = ui, server = server)
