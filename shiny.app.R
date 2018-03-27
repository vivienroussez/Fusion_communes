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
library(DT)
library(tidyverse)
library(pROC)

## les données shiny
k=1000



#addMarkers(lng=174.768, lat=-36.852, label="test")




# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
  
    dashboardHeader(title="Fusion Communes "),
    ## Sidebar content *********************************************************************************************************
    dashboardSidebar(
      sidebarMenu(
          menuItem("Contexte", tabName = "dashboard0", icon = icon("dashboard")),
          menuItem("Analyse Descriptive", tabName = "dashboard1", icon = icon("dashboard")),
          menuItem("Modélisation", tabName = "dashboard2", icon = icon("dashboard")),
          menuItem("Population Par commune", tabName = "dashboard3", icon = icon("dashboard")),
          menuItem("Les communes Fusionnées", tabName = "dashboard4", icon = icon("dashboard")),
          sliderInput('k','Population maximale par Commune:',min=100,max=10000,value=1000)
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
                   
                      valueBox(1774, "1,6% Fusions", icon = icon("List"), width=4,color="black"),
                  
                     valueBox(2015, "Historique depuis", icon = icon("List"), width=4,color="light-blue")
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
                        selected = "Matrice de Confusion",
                        #tabPanel("Graphe du seuil", "Note that when side=right, the tab order is reversed."),
                        tabPanel("Matrice de Confusion",
                                 box(
                                   title = "",width = 700,height = 200,
                                   selectInput("choixMC", "Type de Modèle:",names(prev.conf))
                                   
                                 ),tableOutput("ZONE51")
                                 ),
                        tabPanel("Courbe ROC",   box(
                          title = "",width = 700,height = 200,
                          selectInput("choixROC", "Type de Modèle:",names(prev.roc),multiple = TRUE,selected = "MCO")
                          
                        ),plotOutput("ZONE6"))
                      )
                    )#fin fluidRow
                    
                    
                    ),
                        
                      
          
          # third tab content*********************************************
          tabItem(tabName = "dashboard3",leafletOutput("ZONE33", width = "100%", height = 600
                  
                  
                    # box(
                    # title = "",
                    # width = "100%",
                    # background = "black",
                    # selectInput("k", "Population",
                    #                choices = c(100,1000,10000),
                    #                selected = 1000)

                    #sliderInput("Pop", "Population :", min=1,max=1000,value=100)
                  )),
          # fourth tab content *********************************************
          tabItem(tabName = "dashboard4",mapviewOutput("ZONE4", width = "100%", height = 700))
             # leafletOutput("ZONE4")
                
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
  
   # tab 3 ROC 
  
  colr <- rainbow(10)
  output$ZONE6 <- renderPlot({
  
    for (jj in 1:length(input$choixROC))
    {
    x<-prev.roc[[input$choixROC[jj]]]
    ## S3 method for class 'roc'
    plot.roc(x, add=ifelse(jj==1,FALSE,TRUE), reuse.auc=TRUE,
             axes=TRUE,
             #Generic arguments for par:
             xlim=if(x$percent){c(100, 0)} else{c(1, 0)},
             ylim=if(x$percent){c(0, 100)} else{c(0, 1)},
             asp=1,
             mar=c(4, 4, 2, 2)+.1,
             mgp=c(2.5, 1, 0),
             col=colr[jj],
             # col, lty and lwd for the ROC line only
             #col=par("col"),
             lty=par("lty"),
             lwd=2,#largeur trait 
             type="l",# type trait
             # Identity line
             identity=TRUE,
             identity.col="blue",
             identity.lty=1,
             identity.lwd=1,
             # Print the AUC on the plot
             print.auc=TRUE,
             print.auc.pattern=NULL,
             print.auc.x=ifelse(x$percent, 50, .5),
             print.auc.y=ifelse(x$percent, 50, .5),
             print.auc.adj=c(1,jj+2),
             print.auc.col=colr[jj],
             print.auc.cex=par("cex"))
    
    }
    # par(mfrow=c(3,3))
    # 
    # for (ii in 1:length(prev.roc))
    # { 
    #   plot(prev.roc[[ii]],main=c(names(prev.conf)[ii],paste("AUC = ",round(prev.roc[[ii]]$auc,3),sep="") ))
    #   
    # } 
    
  })
  #tab2 MATRICE CONFUSION 
  prev<-NULL
  for(j in names(prev.conf))
  { #mise ne forme de la matrice 
    prev[[j]]<-spread(as.data.frame(prev.conf[[which(names(prev.conf) == j)]]$table),Reference,Freq)
    names(prev[[j]])[1]<-"pred/ref"
    
  } 
  #affichage matrice si selection 
  output$ZONE51 <- renderTable(width = "80%",
    prev[[input$choixMC]]
  )
   
  # Third tab content*********************************************
  output$ZONE3BOX1 <- renderPlot({
    data <- histdata[1:40]
    plot.design(data=tab,speed~dist,ylab = "speed",xlab = "dist", title="graphe plot") })
  # Forth tab content*********************************************
  # CARTE
  b <- filter(base,fusion==1) %>% select(first,second) %>% unlist() %>% unique() %>% mapCom[.,"codgeo"]
  Carte<-mapview(b, col.regions = sf.colors(360))
  
  output$ZONE4 <-renderMapview({
     Carte 
    
    #mapview(mapCom[1:100,], col.regions = sf.colors(109000),options = popupOptions(closeButton = TRUE))
    
    # m <- leaflet(b) %>%
    # addTiles("test") %>%  # Add default OpenStreetMap map tiles
    # Markers(b, label="test") %>%
    # addPopups(174.768, -36.852, content,options = popupOptions(closeButton = TRUE))
  })
  
  ##### dashboard 4 communes par popolation 
  
  ### CODE préparation carte ###########
  
  com <- st_read("Sources/com_15.shp")
  # On va prendre le fonds généralisé pour gagner un peu de place, à la place du fonds IGN
  proj <- st_crs(mapCom)
  st_crs(com) <- proj
  com <- st_transform(com,3857) # projection en mercator
  
 
  ################
  
  #Cartefull<-mapview(mapCom[1:4000,], col.regions = sf.colors(360))
  #CartePOP <- select(mapCom,"codgeo","P13_POP")
  #m <- st_transform(mapCom, "+proj=longlat +datum=WGS84") %>% select(codgeo,P13_POP)%>%filter(.,P13_POP>k))
  
 # m1<-reactive(filter(CartePOP,CartePOP$P13_POP>input$Pop))
  m<- eventReactive(input$k,{ st_transform(mapCom, "+proj=longlat +datum=WGS84") %>% select(codgeo,P13_POP)%>%filter(.,P13_POP<input$k)})
  
  
   output$ZONE33<-renderLeaflet(
                                map<-leaflet()%>% addProviderTiles(providers$OpenStreetMap.France)%>%setView(7.3,48,zoom=5)
                               
    )
    
    observeEvent(input$k,{
      print(m())
      v <- m()
      leafletProxy('ZONE33')%>%addPolygons(data=v,color = rainbow(20))
    })
    
   # m<-filter(CartePOP,CartePOP$P13_POP> 10000)%>%
    #leaflet()%>%addTiles()%>%addPolygons(data=m)
  
  #   
  }

# Run the application 
shinyApp(ui = ui, server = server)

