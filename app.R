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
library(leaflet.minicharts)
library(geosphere)
library(utils)
library(shinythemes)

#setwd("C:/Users/arjun/Downloads/Ships")
D = read.csv("ships.csv")
D$DATETIME = strptime(D$DATETIME, "%Y-%m-%dT%H:%M:%SZ", tz="UTC")

# Function for Observations with MAX distance

MAX_Distance_Observations <- function(ship_type, ship_name){
    
    d = D[D$SHIPTYPE==ship_type,]
    d = d[order(d$DATETIME),]
    
    if(is.null(ship_name)){
        d = d[d$SHIPNAME==head(d$SHIPNAME,1),]
    }
    else if(!ship_name %in% d$SHIPNAME){
        d = d[d$SHIPNAME==head(d$SHIPNAME,1),]
    }
    else{
        d = d[d$SHIPNAME==ship_name,]
    }
    
    
    d$DISTANCE <- NA
    for (i in 2:nrow(d)){
        d$DISTANCE[i] = round(distm(d[i-1,c(2,1)],d[i,c(2,1)]),2)
    }
    
    x = nrow(d) - which.max(rev(d$DISTANCE)) + 1
    
    return(as.data.frame(d[c(x-1,x),]))
}


#Dropdown module

SelectInputUI <- function(id){
    
    ns <- NS(id)
    
    tagList(
        fluidRow(
            selectInput(ns("vessel_type"), label = "Vessel Type", choices = sort(unique(D$SHIPTYPE)), selected = 9),
            uiOutput(ns("vessel_names"))
        )
    )
}


SelectInputServer <- function(id){
    
    moduleServer(id, function(input, output, session){
        
        ship_names = reactive({unique(D$SHIPNAME[D$SHIPTYPE==input$vessel_type])})
        output$vessel_names <- renderUI({
            
            ns <- NS(id)
            
            selectInput(ns("vessel_name"), "Vessel Name", choices = ship_names(), selected = ship_names()[1])
            
        })
        

        return(
            list(
            v_type  = reactive({input$vessel_type}),
            v_name = reactive({input$vessel_name})
        ))
    })
    
}



shipApp <- function(){
    
    ui <- fluidPage(
        theme = shinytheme("superhero"),
        titlePanel("Vessel Location"),
        
        sidebarLayout(
            
            sidebarPanel(width = 2,
                         
                SelectInputUI("vessel")
            ),
            
            mainPanel(width = 10,
                      
                leafletOutput("location", height = 900)
            )
        )

    )
    
    server <- function(input, output, session){
        
        vals <- SelectInputServer("vessel")
        max_obs <- reactive({MAX_Distance_Observations(vals$v_type(), vals$v_name())})
        
        output$location <- renderLeaflet({
            
            m <- leaflet() %>%
                addTiles() %>%
                addMarkers(lng=max_obs()$LON, lat=max_obs()$LAT)%>%
                addControl(tags$h2(paste0("Distance Sailed in meters: "), max_obs()$DISTANCE[2]), position = "topleft")%>%
                addFlows(max_obs()$LON[1], max_obs()$LAT[1], max_obs()$LON[2], max_obs()$LAT[2],
                         maxThickness = 6, opacity = 0.5)
            
        })
    }
    
    shinyApp(ui, server)
}


shipApp()
