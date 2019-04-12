#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Traffic Collisions in Los Angeles from 2010 - Current"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = 2010,
                        max = 2019,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("collisionMap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    traffic <- read_rds("traffic.rds")
    
    output$collisionMap <- renderLeaflet({
        # generate bins based on input$bins from ui.R
        year <- input$year
        x    <- traffic %>% filter(year(date_occ) == year)

        leaflet(data = x) %>% 
            addTiles() %>%
            addMarkers(~long, 
                       ~lat, 
                       popup = ~as.character(date_occ), 
                       label = ~as.character(date_occ),
                       clusterOptions = markerClusterOptions())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
