# library(shiny)
# library(shinydashboard)
# library(shinythemes)
# library(tidyverse)
# library(visNetwork)
# library(fontawesome)
# library(plotly)
# library(rsconnect)
# library(igraph)
# library(tmap)
# library(sf)
# library(raster)
packages = c('tidyverse', 'lubridate', 'MASS','ggplot2', 'plotly', 'geosphere', 
             'sf','rgeos','raster', 'tmap','visNetwork','igraph','shiny','shinydashboard',
             'fontawesome','rsconnect','shinyWidgets','ggiraph')

for(p in packages){
    if(!require(p, character.only=T)){
        install.packages(p)
    }
    library(p, character.only=T)
}

server <- function(input, output,session){
    
    visNetworkServer("vis")
    cardMappingServer("parcoord")
    mapServer("map")
    location_transServer("heatmap")
}

ui <- dashboardPage(
    skin="blue",
    dashboardHeader(title = "Vast Mini Challenge 2: Group 5"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview" ,tabName = "overview", icon = icon("dashboard")),
            menuItem("Abila Kronos Map" ,tabName = "map", icon = icon("map-marked-alt")),
            menuItem("Locations Transactions" ,tabName = "LocationTrans", icon = icon("money-bill-wave")),
            menuItem("Card Mapping" ,tabName = "card_mapping", icon = icon("credit-card")),
            menuItem("Network Analysis" ,tabName = "network", icon = icon("network-wired"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(
                tabName="overview",
                fluidPage(
                    overviewUI()
                )
            ),
            tabItem(
                tabName="map",
                fluidPage(
                    mapUI("map")
                )
            ),
            tabItem(
                tabName="LocationTrans",
                fluidPage(
                    location_transUI("heatmap")
                )
            ),
            tabItem(
                tabName="card_mapping",
                fluidPage(
                    cardMappingUI("parcoord")
                )
            ),
            tabItem(
                tabName = "network",
                fluidPage(
                    visNetworkUI("vis")
                )
            )
        )
    )
)

shinyApp(ui = ui, server = server)
