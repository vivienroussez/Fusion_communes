#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(tidyverse)
require(sf)
require(cartography)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("TOTO"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("seuil_pop",
                     "Seuil de population :",
                     min = 200,
                     max = 10000,
                     value = 10000)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("MapPOP")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  load("Base.RData")
  # Création de la couche contour
  contour <- mapLight %>% mutate(v="FR") %>% group_by(v) %>% summarise()
  
   output$mapPOP <- renderPlot({
      m <- select(mapLight,codgeo,P13_POP) %>%
        mutate(inf=as.factor((P13_POP<=input$seuil_pop)+0))
      typoLayer(x=m,var="inf",border=NA,col = c("blue","white"),legend.pos = "n")
      plot(contour,border="black",col=NA,add=T)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

