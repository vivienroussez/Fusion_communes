<<<<<<< HEAD
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
install.packages("mapview")
library(shiny)
library(leaflet)
library(shinydashboard)
library(htmltools)
library(rgdal)
library(corrplot)
library(mapview)
library(sf)
Carte<-mapview(mapCom, col.regions = sf.colors(36000))

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
          tabItem(tabName = "dashboard4",mapviewOutput("ZONE4", width = "100%", height = 400
)
             # leafletOutput("ZONE4")
                
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
    "La fusion de() communes ne peut intervenir qu'entre communes limitrophes et entraîne la disparition de la personnalité 
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
  output$ZONE4 <- renderMapview({
     Carte 
    
    #mapview(mapCom, col.regions = sf.colors(109000))
    
    #m <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    #addMarkers(lng=174.768, lat=-36.852, label="test") %>%
     # addPopups(174.768, -36.852, content,options = popupOptions(closeButton = TRUE))
})}

# Run the application 
shinyApp(ui = ui, server = server)

=======
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("mapview")
# install.packages("leaflet")
# install.packages("sf")
library(shiny)
library(leaflet)
library(shinydashboard)
library(htmltools)
library(rgdal)
library(corrplot)
library(mapview)
library(sf)

## les données shiny
#ROC
par(mfrow=c(3,3))

for (ii in 1:length(prev.roc))
{ 
  plot(prev.roc[[ii]],main=c(names(prev.conf)[ii],paste("AUC = ",round(prev.roc[[ii]]$auc,3),sep="") ))
  
}
# CARTE
Carte<-mapview(com, col.regions = sf.colors(36000))




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
                    # pour la petite histoire
                    
                    fluidRow(
                      tabBox(
                        height = "500px",
                        width = "500px",
                        selected = "Tab3",
                        tabPanel("Graphe du seuil", "Note that when side=right, the tab order is reversed."),
                        tabPanel("Matrice de Confusion", "Tab content 2"),
                        tabPanel("Courbe ROC", plotOutput("ZONE6"))
                      )
                    )#fin fluidRow
                    
                    
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
          tabItem(tabName = "dashboard4",mapviewOutput("ZONE4", width = "100%", height = 400
)
             # leafletOutput("ZONE4")
                
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
    "La fusion de() communes ne peut intervenir qu'entre communes limitrophes et entraîne la disparition de la personnalité 
     morale de l'ensemble des communes concernées pour donner naissance à une personne juridique nouvelle et différente. "
  })
  
  
  # First tab content*********************************************
  output$plot1 <- renderPlot({
    
    corrplot(dat1_cor, method = input$choix)
     })
  # Second tab content*********************************************  
  output$ZONE6 <- renderPlot({
    par(mfrow=c(3,3))
    
    for (ii in 1:length(prev.roc))
    { 
      plot(prev.roc[[ii]],main=c(names(prev.conf)[ii],paste("AUC = ",round(prev.roc[[ii]]$auc,3),sep="") ))
      
    } 
    
  })
  # output$ZONE2 <- renderPlot({
  #   data <- histdata[1:input$slider2]
  #   plot(data)
  #   })
  # Third tab content*********************************************
  output$ZONE3BOX1 <- renderPlot({
    data <- histdata[1:40]
    plot.design(data=tab,speed~dist,ylab = "speed",xlab = "dist", title="graphe plot") })
  # Forth tab content*********************************************
  output$ZONE4 <- renderMapview({
     Carte 
    
    #mapview(mapCom, col.regions = sf.colors(109000))
    
    #m <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    #addMarkers(lng=174.768, lat=-36.852, label="test") %>%
     # addPopups(174.768, -36.852, content,options = popupOptions(closeButton = TRUE))
})}

# Run the application 
shinyApp(ui = ui, server = server)

>>>>>>> 4a0fb2ed7a5a3780fbdf29e6735e5ef255f6ff73
