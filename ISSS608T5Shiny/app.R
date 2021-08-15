library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(MASS)
library(geosphere)
library(rgeos)
library(tmap)
library(visNetwork)
library(fontawesome)
library(plotly)
library(rsconnect)
library(igraph)
library(mapview)
library(sf)
library(raster)
library(shinyWidgets)
library(ggiraph)
library(rgdal)
library(clock)
library(patchwork)

server <- function(input, output,session){
    visNetwork2Server("vis2")
    visNetworkServer("vis")
    cardMappingServer("parcoord")
    mapServer("map")
    map2Server("dotplot")
    location_transServer("heatmap")
}

ui <- dashboardPage(
    skin="blue",
    dashboardHeader(title = "STAGEM"),
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
                    tabsetPanel(
                        tabPanel(
                            "Map",
                            mapUI("map")
                        ),
                        tabPanel(
                            "Scatter Plot",
                            map2UI("dotplot")
                        )
                    )
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
                    tabsetPanel(
                        tabPanel(
                            "Transaction Network",
                            visNetworkUI("vis")
                        ),
                        tabPanel(
                            "GPS Network",
                            visNetwork2UI("vis2")
                        )
                    )
                )
            )
        )
    )
)

shinyApp(ui = ui, server = server)
