shinyUI(fluidPage(
  
  titlePanel("Waterfowl eBird analysis"),
  
  sidebarLayout(
    sidebarPanel(
      "Please choose a species and wait for the BCR list and species code to update.  Sometimes it takes awhile to load the datasets.  When ready, the fields will not be greyed out.  You only need to hit run Analysis once.  The rest will be dynamic", br(),br(),
      selectInput("species", "Species:", sort(c("Black Duck" = "ABDU", "Green-winged teal" = "AGWT", "American Widgeon" = "AMWI", "Barrow's Goldeneye" = "BAGO", "Black-bellied Whistling-Duck" = "BBWD", "Blue-winged and Cinnamon Teal" = "BCTE", "Black Scoter" = "BLSC", "Bufflehead" = "BUFF", "Blue-winged Teal" = "BWTE", "Canvasback" = "CANV", "Cinnamon Teal" = "CITE", "Common Eider" = "COEI", "Common Goldeneye" = "COGO", "Common Merganser" = "COME", "Common and King Eider" = "EIDR", "Gadwall" = "GADW", "Greater Scaup" = "GRSC", "Harlequin Duck" = "HADU", "Hooded Merganser" = "HOME", "King Eider" = "KIEI", "Lesser Scaup" = "LESC", "Long-tailed Duck"= "LTDU", "Mallard" = "MALL", "Merganser"= "MERG", "Pintail" = "NOPI", "Northern Shoveler" = "NSHO", "Red-breasted Merganser"= "RBME", "Redhead"= "REDH", "Ringed neck" = "RNDU", "Ruddy Duck" = "RUDU", "Great and Lesser Scaup" = "SCAU", "Black, White-winged, and Surf Scoter" = "SCOT", "Surf Scoter" = "SUSC", "Wood duck" = "WODU", "White-winged Scoter" = "WWSC"))),
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
