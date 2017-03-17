library(ggplot2)
library(scales)
library(diptest)
library(shiny)
library(TTR)


shinyServer(function(input, output) {
  workspace = "/data"
  bcr = "BCR.csv"
  
  observeEvent(input$do, {
     print(as.numeric(input$do))
  })
  
  ebird = reactive({
    withProgress(message = 'Loading dataset', value = 0, {
      # Input ArcGIS Model csv file
      infile = input$species
      inbird = paste(infile,".txt",sep="")
      print(inbird)
      ############################################################################
      # Read in ebird data
      tempin = read.delim(paste(workspace,inbird,sep="/"), sep="\t", header=TRUE, quote = "", stringsAsFactors = FALSE, na.strings=c(""))
      incProgress(0.7, detail = "Finished pulling in eBird.  Importing BCR")
      bcrData = read.csv(paste(workspace, bcr, sep="/"), header=TRUE)
      temp = merge(tempin, bcrData, by.x = "BCR.CODE", by.y = "BCR")
      incProgress(0.8, detail = "Finished pulling in BCRs.  Formatting data")
      # Drop extra columns and data
      temp = subset(temp, temp$COUNTRY_CODE == "US")
      temp = subset(temp, temp$APPROVED == "1")
      temp$PROJECT.CODE = NULL
      temp$PROTOCOL.TYPE = NULL
      temp$SAMPLING.EVENT.IDENTIFIER = NULL
      temp$FIRST.NAME = NULL
      temp$LAST.NAME = NULL
      temp$OBSERVER.ID = NULL
      temp$BREEDING.BIRD.ATLAS.CODE = NULL
      temp$GLOBAL.UNIQUE.IDENTIFIER = NULL
      temp$SUBSPECIES.COMMON.NAME = NULL
      temp$SUBSPECIES.SCIENTIFIC.NAME = NULL
      temp$AGE.SEX = NULL
      temp$COUNTRY = NULL
      temp$IBA.CODE = NULL
      temp$SUBNATIONAL1_CODE = NULL
      temp$SUBNATIONAL2_CODE = NULL
      temp$ATLAS.BLOCK = NULL
      temp$LOCALITY = NULL
      temp$LOCALITY.ID = NULL
      temp$LOCALITY.TYPE = NULL
      temp$REVIEWED = NULL
      temp$REASON = NULL
      temp$TRIP.COMMENTS = NULL
      temp$EFFORT.AREA.HA = NULL
      temp$EFFORT.DISTANCE.KM = NULL
      temp$SPECIES.COMMENTS = NULL
      temp$X = NULL
      temp$APPROVED = NULL
      temp$TIME.OBSERVATIONS.STARTED = NULL
      temp$DURATION.MINUTES = NULL
      temp$NUMBER.OBSERVERS = NULL
      temp$ALL.SPECIES.REPORTED = NULL
      temp$GROUP.IDENTIFIER = NULL
      temp$TAXONOMIC.ORDER = NULL
      temp$CATEGORY = NULL
      
      # Set date field and divide up by year, month, week for plotting
      temp$Date = as.Date(temp$OBSERVATION.DATE, "%Y-%m-%d")
      temp$Year = strtoi(format(temp$Date, "%Y"))
      temp$Month = strtoi(format(temp$Date, "%m"))
      temp$Day = strtoi(format(temp$Date, "%d"))
      temp$Month = ifelse(format(temp$Date, "%m") == "09", 9, temp$Month)
      # Remove years < 2005 and months 6,7,8
      temp = subset(temp, temp$Year > 2005 & temp$Year <= 2016)
      temp = subset(temp, temp$Month >= 9 | temp$Month <= 4)
      incProgress(0.9, detail = "Finished pulling in BCRs.  Adding sprinkles")
      temp$Winter = ifelse(temp$Month <= 4, paste(temp$Year - 1,temp$Year, sep = "/"),paste(temp$Year, temp$Year + 1, sep = "/"))
      temp$Week = as.numeric(format(temp$Date, "%U"))
      #ebird$Month = ifelse(ebird$Month <= 5, substring(ebird$Month,2),ebird$Month)
      temp = subset(temp, temp$Winter != "2005/2006" & temp$Winter != "2016/2017")
      
      # Reorder months
      temp$Month = factor(temp$Month, levels=c(9, 10, 11, 12, 1, 2, 3, 4))
      temp$Week = factor(temp$Week, levels=c(35:53,1:17))
      #Set X as na
      temp$OBSERVATION.COUNT = ifelse(temp$OBSERVATION.COUNT == "X", 1, temp$OBSERVATION.COUNT)
      temp$OBSERVATION.COUNT = as.numeric(temp$OBSERVATION.COUNT)
      temp$BCRNUMNAME = paste(temp$BCR.CODE, temp$BCRNAME, sep="_")
    })
    temp
  })
  
  output$selectedSpecies = renderUI({
    df = ebird()
    items = unique(df$BCRNUMNAME)
    selectInput("bcr", "BCR:", items)
    })
  
  output$whichSpecies = renderText({
    input$species
  })
  
  computeSummary = reactive({
    df = ebird()
    df = subset(df, df$BCRNUMNAME == input$bcr)
    aggMean = aggregate(df$OBSERVATION.COUNT, list(Week=df$Week, BCR=df$BCR.CODE), mean)
    #plot(aggMean$Week, aggMean$OBSERVATION.COUNT)
    ggplot(aggMean, aes(x=aggMean$Week, y=x)) +
      stat_summary(aes(y = x,group=1), fun.y=mean, colour="red", geom="line",group=1) + 
      labs(y="Mean Observation count", x="Week number from the first week in September until the last week in April") +
      ggtitle(paste("Figure 2. Observation count mean by BCR plotted over wintering period for ", input$species, sep=""))
  })
  computeSmooth = reactive({
    df = ebird()
    df = subset(df, df$BCRNUMNAME == input$bcr)
    aggMean = aggregate(df$OBSERVATION.COUNT, list(Week=df$Week, BCR=df$BCR.CODE), mean)
    aggMean$smooth = SMA(aggMean[, "x"],3)
    #plot(aggMean$Week, aggMean$OBSERVATION.COUNT)
    ggplot(aggMean, aes(x=aggMean$Week, y=aggMean$smooth)) +
      stat_summary(aes(y = aggMean$smooth,group=1), fun.y=mean, colour="red", geom="line",group=1) + 
      labs(y="Mean Observation count", x="Week number from the first week in September until the last week in April") +
      scale_x_discrete(labels=c("Sept", 36:43, "November", 45:53, "Jan", 2:17)) +
      ggtitle(paste("Figure 2. Smoothed Observation count mean by BCR plotted over wintering period for ", input$species, sep=""))
  })
  
  computePVal = reactive({
    df = ebird()
    df = subset(df, df$BCRNUMNAME == input$bcr)
    testsetup = aggregate(df$OBSERVATION.COUNT, list(Week=df$Week, BCR=df$BCR.CODE, BCRNUMNAME = df$BCRNUMNAME), mean)
    testsmooth = SMA(testsetup[, "x"], 3)
    test = dip.test(testsmooth)
    bcr_name = unique(testsetup$BCRNUMNAME)
    paste("P-value:", test$p.value[[1]]," / BCR:", bcr_name, sep=" ")
  })
  
  output$statsTable = renderPlot({
    if(input$do == 0) return(NULL)
    computeSummary()
  })
  output$smoothTable = renderPlot({
    if(input$do == 0) return(NULL)
    computeSmooth()
  })
  
  output$pVal = renderText({
    if(input$do == 0) return(NULL)
    computePVal()
  })
  
})