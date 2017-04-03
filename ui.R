shinyUI(fluidPage(
  
  titlePanel("Waterfowl eBird analysis"),
  
  sidebarLayout(
    sidebarPanel(
      "Please choose a species and wait for the BCR list and species code to update.  Sometimes it takes awhile to load the datasets.  When ready, the fields will not be greyed out.  You only need to hit run Analysis once.  The rest will be dynamic", br(),br(),
      selectInput("species", "Species:", sort(c("Green-winged teal" = "AGWT","Black Duck" = "ABDU", "American Widgeon" = "AMWI", "Blue-winged teal" = "BWTE", "Canvasback" = "CANV", "Cinnamon teal" = "CITE", "Gadwall"= "GADW", "Great/Lesser Scaup" = "SCAU", "Mallard" = "MALL", "Pintail" = "NOPI", "Northern Shoveler" = "NOSH", "Redhead"= "REDH", "Ringed neck" = "RNDU", "Ruddy Duck" = "RUDU", "Wood duck" = "WODU"))),
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