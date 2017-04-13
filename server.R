library(ggplot2)
library(scales)
library(diptest)
library(shiny)
library(TTR)
library(splines)

###################################################################################
# Silvermans test
#
# http://www-bcf.usc.edu/~gourab/code-bmt/tables/table-2/
# silverman.test <-function(x,k,M=999,adjust=FALSE,digits=6)
###################################################################################
silverman.test <-function(x,k,M=999,adjust=FALSE,digits=6){
  # x: data
  # k: number of modes to be tested
  # M: number of bootstrap replications
  
  #check if seed is available (as done in boot package)
  #if so save it
  seedAvailable = exists(x=".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(seedAvailable)
    saved_seed = .Random.seed 
  else{
    rnorm(1)
    saved_seed = .Random.seed
  }
  
  #temp function for bootstrapping
  y.obs <- function(x,h,sig=sd(x)){
    mean(x) + (x-mean(x)+h*rnorm(length(x),0,1))/((1+h^2/sig^2)^(1/2))
    #(x+h*rnorm(length(x),0,1))/((1+h^2/sig^2)^(1/2))
  }
  
  #temp function for density calculation
  nor.kernel <- function(x,h){
    density(x,bw=h,kernel ="gaussian")$y
  }
  
  #start of the test
  h0 <- h.crit(x, k)
  n <- 0
  
  for (i in 1:M) {
    x.boot <- sort(y.obs(sample(x, replace=TRUE),h0))
    mod.temp <- nr.modes(nor.kernel(x.boot,h0))
    if (mod.temp > k){
      n <- n+1
    }
  }
  
  p <- n/M
  ptemp=p
  
  if(adjust==TRUE){
    if(k==1){
      #asymptotic levels of silvermantest by Hall/York
      x=c(0,0.005,0.010,0.020,0.030,0.040,0.050,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.25,0.30,0.35,0.40,0.50)
      y=c(0,0,0,0.002,0.004,0.006,0.010,0.012,0.016,0.021,0.025,0.032,0.038,0.043,0.050,0.057,0.062,0.07,0.079,0.088,0.094,0.102,0.149,0.202,0.252,0.308,0.423)
      sp = interpSpline(x,y)
      #adjusting the p-value
      if(p<0.005)
        p=0
      else{
        p = predict(sp,p)$y
        p = round(p,digits)
      }
      
    }
    else{
      print("The option to adjust the p-value is valid only for k=1")
    } 
  }
  
  #return(list(saved_seed=saved_seed,p_value=p))
  #test_obj = new("Silvermantest", data=x, p_value = p,saved_seed=saved_seed,k=k)
  return(p)
}

h.crit <-
  function(x,k,prec=6){
    
    #temp function
    nor.kernel <- function(x,h){
      density(x,bw=h,kernel ="gaussian")$y
    }
    
    
    digits=prec
    prec=10^(-prec)
    x <- sort(x)
    minh <- min(diff(x))		#minimal possible h
    maxh <- diff(range(x))/2	#maximal possible h
    a <- maxh
    b <- minh
    zaehler=0
    
    while (abs(b-a)>prec){
      m <- nr.modes(nor.kernel(x,a))
      
      b <- a
      if (m > k){
        minh <- a
        a <- (a + maxh)/2
      } 
      else {
        maxh <- a
        a <- (a - minh)/2
      }
    }
    
    a=round(a,digits)
    
    
    if(nr.modes( nor.kernel(x,a) ) <= k){
      #subtract until more than k modes
      while(nr.modes( nor.kernel(x,a) ) <= k){
        a = a - prec
      }
      a=a+prec
    }
    
    if(nr.modes( nor.kernel(x,a) ) > k){
      #add until nr. of moodes correct
      while(nr.modes( nor.kernel(x,a) ) > k){
        a = a + prec
      }
    }
    
    a
  }


nr.modes <-
  function(y){
    
    d1 <- diff(y)
    signs <- diff(d1/abs(d1))
    length(signs[signs==-2])
    
  }
###################################################################################
# End Silvermans test http://www-bcf.usc.edu/~gourab/code-bmt/tables/table-2/

shinyServer(function(input, output) {
  workspace = "/data"
  
  observeEvent(input$do, {
     print(as.numeric(input$do))
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
    test = dip.test(ss$yin)
    bcr_name = unique(testsetup$BCRNUMNAME)
    paste("P-value:", test$p.value[[1]],"  /  Silverman: ", silverman.test(ss$y, 2, M=999, adjust=FALSE),"  /  BCR:", bcr_name, sep=" ")
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