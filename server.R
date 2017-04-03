library(ggplot2)
library(scales)
library(diptest)
library(shiny)
library(TTR)


shinyServer(function(input, output) {
  workspace = "/data"
  
  observeEvent(input$do, {
    print(as.numeric(input$do))
    df = ebird()
    items = unique(df$BCRNUMNAME)
    updateSelectInput("bcr", "BCR:", items)
  })
  
  ebird = reactive({
    withProgress(message = 'Loading:', detail='eBird data incoming', value = 0, {
      # Input ArcGIS Model csv file
      infile = input$species
      inbird = paste(infile,".csv",sep="")
      print(inbird)
      ############################################################################
      # Read in ebird data
      temp = read.csv(paste(workspace,inbird,sep="/"), sep=",", header=TRUE, quote = "", stringsAsFactors = FALSE, na.strings=c(""))
      incProgress(0.6, detail = "Finished pulling in eBird.  Making fancy")

      # Reorder months
      temp$Month = factor(temp$Month, levels=c(9, 10, 11, 12, 1, 2, 3, 4))
      temp$Week = factor(temp$Week, levels=c(31:53,1:17))
      #Set X as na
      temp$OBSERVATION.COUNT = ifelse(temp$OBSERVATION.COUNT == "X", 1, temp$OBSERVATION.COUNT)
      temp$OBSERVATION.COUNT = as.numeric(temp$OBSERVATION.COUNT)
      temp$BCRNUMNAME = paste(temp$BCR.CODE, temp$BCRNAME, sep="_")
      test = c()
      getFactor = function(x) {
        for (m in 9:12){
          if (m %in% c(9,11)) {
            maxVal = 30
          } else {
            maxVal = 31
          }
          for (d in 1:maxVal){
            test = append(test, paste(m,d,sep="/"))
          }
        }
        for (m in 1:4){
          if (m == 2) {
            maxVal = 28
          } else if (m == 4) {
            maxVal = 30
          } else {
            maxVal = 31
          }
          for (d in 1:maxVal){
            test = append(test, paste(m,d,sep="/"))
          }
        }
        return(test)
      }
      temp$MonthDay = factor(temp$MonthDay,levels=(getFactor()))
    })
    temp
  })
  

  output$whichSpecies = renderText({
    input$species
  })
  
  computeSummary = reactive({
    df = ebird()
    df = subset(df, df$BCRNUMNAME == input$bcr)
    aggMean = aggregate(df$OBSERVATION.COUNT, list(Week=df$MonthDay, BCR=df$BCR.CODE), mean)
    plot(x=aggMean$Week, y=aggMean$x, 
      main=paste("Figure 1. Observation count mean by BCR plotted over wintering period for ", input$species, sep=""),
      ylab="Average count",
      xlab="Date",
      cex.lab=1.5
    )
    
    lines(x=aggMean$Week, y=aggMean$x, col="red")
  })
  computeSmooth = reactive({
    df = ebird()
    df = subset(df, df$BCRNUMNAME == input$bcr)
    aggMean = aggregate(df$OBSERVATION.COUNT, list(Week=df$MonthDay, BCR=df$BCR.CODE), mean)
    ss = smooth.spline(x=aggMean$Week, y=aggMean$x, spar=0.7, keep.data = TRUE)
    ss$x = aggMean$Week
    plot(x=ss$x, y=ss$y, type="l", 
         main=paste("Figure 2. Smoothed Observation count mean by BCR plotted over wintering period for ", input$species, sep=""),
         ylab="Average count",
         xlab="Date",
         cex.lab=1.5
         )
    
    lines(x=ss$x,y=ss$y, col="red")
  })
  
  computePVal = reactive({
    df = ebird()
    df = subset(df, df$BCRNUMNAME == input$bcr)
    testsetup = aggregate(df$OBSERVATION.COUNT, list(Week=df$MonthDay, BCR=df$BCR.CODE, BCRNUMNAME = df$BCRNUMNAME), mean)
    #testsmooth = SMA(testsetup[, "x"], 3)
    ss = smooth.spline(x=testsetup$Week, y=testsetup$x, spar=0.7, keep.data = TRUE)
    test = dip.test(ss$y)
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