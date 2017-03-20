shinyUI(fluidPage(
  
  titlePanel("Waterfowl eBird analysis"),
  
  sidebarLayout(
    sidebarPanel(
      "Please choose a species and wait for the BCR list and species code to update.  Sometimes it takes awhile to load the datasets.  When ready, the fields will not be greyed out.  You only need to hit run Analysis once.  The rest will be dynamic", br(),br(),
      selectInput("species", "Species:", sort(c("Green-winged teal" = "agwt","Black Duck" = "abdu", "American Widgeon" = "amwi", "Blue-winged teal" = "bwte", "Canvasback" = "canv", "Cinnamon teal" = "citi", "Gadwall"= "gadw", "Great Scaup" = "grsc", "Lesser" = "lesc", "Mallard" = "mall", "Pintail" = "nopi", "Northern Shoveler" = "nsho", "Redhead (Dale complained)"= "redh", "Ringed neck" = "rndu", "Ruddy Duck" = "rudu", "Wood duck" = "wodu"))),
      uiOutput("selectedSpecies"),
      actionButton("do", "Run Analysis")
    ),
      
    mainPanel(
      strong(
      div(style="display:inline-block","Species Code: "),  
      div(style="display:inline-block; text-transform:uppercase", textOutput("whichSpecies"))), br(),
      plotOutput(("statsTable")), br(),
      plotOutput(("smoothTable")),br(),
      "P-Values less than 0.05 indicate significant bimodality and values greater than 0.05 but less than 0.10 suggest bimodality with marginal significance.",br(),
      strong(textOutput(("pVal"))),br(),
      "The test statistic comes from dip.test: https://cran.r-project.org/web/packages/diptest/diptest.pdf"
      
    )
  )
))