#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinydashboard)
library(htmltools)
library(rgdal)


content <- paste(sep = "<br/>",
                 "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                 "606 5th Ave. S",
                 "Seattle, WA 98138"
)


data("cars")

tab <- cars


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
    dashboardHeader(title="Fusion Communes "),
    ## Sidebar content *********************************************************************************************************
    dashboardSidebar(
      sidebarMenu(
          menuItem("Contexte", tabName = "dashboard0", icon = icon("dashboard")),
          menuItem("Analyse Descriptive", tabName = "dashboard1", icon = icon("dashboard")),
          menuItem("Modélisation", tabName = "dashboard2", icon = icon("dashboard")),
          menuItem("Validation croisée", tabName = "dashboard3", icon = icon("dashboard")),
          menuItem("Tableau comparatif des modèles", tabName = "dashboard4", icon = icon("dashboard"))
      
    )),

    # Body Content ***********************************************************************************************************
    dashboardBody(
      # Boxes need to be put in a row (or column)
        
      
        # 109082 Couples fusion / 1774 fusion  loi de fusion 2015, 2 années de données 2016/2017
          tabItems(
            tabItem(tabName = "dashboard0",
                  # pour la petite histoire
                  
                  fluidRow(
                    tabBox(
                      title = "",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "tabset1", height = "400px",width = 12,
                      
                      tabPanel("Un peu d'histoire !  ",textOutput("L1"),textOutput("L2"))
                            
                             
                  )),
                  
                    
                   
                    
                    # Les KPIs 
                    fluidRow(
                      valueBox(109082, "Couples de Communes", icon = icon("List"),width=4,color="light-blue"),
                   
                      valueBox(1774, "Fusions", icon = icon("List"), width=4,color="yellow"),
                  
                     valueBox(2016, "Historique depuis", icon = icon("List"), width=4,color="light-blue")
                   )
                   ),
            
            
            # First tab content
            tabItem(tabName = "dashboard1",
                    fluidRow(
                      plotOutput("plot1", height = 400, width="100%")),
                    fluidRow(
                      box(
                        title = "Controls",width = 700,height = 300,
                        selectInput("choix", "Variable:",c("ellipse","circle","pie"))
                        
                      )
                    )
            ),
          # Second tab content*********************************************
          tabItem(tabName = "dashboard2",
                  box(
                    title = "Histogram", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("ZONE2", height = 250)
                  ),
                  
                  box(
                    title = "Inputs", status = "warning", solidHeader = TRUE,
                    "Box content here", br(), "More box content",
                    sliderInput("slider2", "Slider input:", 1, 100, 50),
                    textInput("text", "Text input:")
                  )
          ),
          # third tab content*********************************************
          tabItem(tabName = "dashboard3",
                  fluidRow(
                    box(title = "cars ", "speed vs distant",plotOutput("ZONE3BOX1",height = 250)),
                    box(status = "warning", "Box content")
                  ),
                  
                  fluidRow(
                    box(
                      title = "Title 1", width = 4, solidHeader = TRUE, status = "primary",
                      "Box content"
                    ),
                    box(
                      title = "Title 2", width = 4, solidHeader = TRUE,
                      "Box content"
                    ),
                    box(
                      title = "Title 1", width = 4, solidHeader = TRUE, status = "warning",
                      "Box content"
                    )
                  ),
                  
                  fluidRow(
                    box(
                      width = 4, background = "black",
                      "A box with a solid black background"
                    ),
                    box(
                      title = "Title 5", width = 4, background = "light-blue",
                      "A box with a solid light-blue background"
                    ),
                    box(
                      title = "Title 6",width = 4, background = "maroon",
                      "A box with a solid maroon background"
                    )
                  )
          ),
          # fourth tab content *********************************************
          tabItem(tabName = "dashboard4",
              leafletOutput("ZONE4")
                
          )
        )
      )
    )
    

# Define server logic required to draw a histogram
server <- function(input, output) {
  # On prepare les donnees
  set.seed(122)
  histdata <- rnorm(500)
  # 0 tab content*********************************************
  output$L1<-renderText({
    "La fusion de communes est l'unification en une seule commune de plusieurs communes 
    jusqu'alors distinctes. La procédure de fusion fait l'objet des articles L. 2113-1 à L. 2113-5,
    L. 2113-9, L. 2113-11 et L. 2113-12 du Code général des collectivités territoriales (CGCT)."
  })
  output$L2<-renderText({
    "La fusion de communes ne peut intervenir qu'entre communes limitrophes et entraîne la disparition de la personnalité 
     morale de l'ensemble des communes concernées pour donner naissance à une personne juridique nouvelle et différente. "
  })
  
  
  # First tab content*********************************************
  output$plot1 <- renderPlot({
    
    corrplot(dat1_cor, method = input$choix)
     })
  # Second tab content*********************************************  
  output$ZONE2 <- renderPlot({
    data <- histdata[1:input$slider2]
    plot(data)
    })
  # Third tab content*********************************************
  output$ZONE3BOX1 <- renderPlot({
    data <- histdata[1:40]
    plot.design(data=tab,speed~dist,ylab = "speed",xlab = "dist", title="graphe plot") })
  # Forth tab content*********************************************
  output$ZONE4 <- renderLeaflet({
    m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=174.768, lat=-36.852, label="test") %>%
      addPopups(174.768, -36.852, content,options = popupOptions(closeButton = TRUE))
})}

# Run the application 
shinyApp(ui = ui, server = server)

