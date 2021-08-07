library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(visNetwork)
library(fontawesome)
library(plotly)
library(rsconnect)
library(igraph)

ui <- dashboardPage(
    skin="blue",
    dashboardHeader(title = "Vast Mini Challenge 2: Group 5"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview" ,tabName = "overview", icon = icon("dashboard")),
            menuItem("Abila Kronos Map" ,tabName = "overview", icon = icon("map-marked-alt")),
            menuItem("Locations Transactions" ,tabName = "LocationTrans", icon = icon("money-bill-wave")),
            menuItem("Card Mapping" ,tabName = "card_mapping", icon = icon("credit-card")),
            menuItem("Network Analysis" ,tabName = "network", icon = icon("network-wired"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(
                tabName="LocationTrans",
                fluidPage(
                    location_transUI("a")
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


# Define server logic required to draw a histogram
server <- function(input, output,session){
    
    visNetworkServer("vis")
    cardMappingServer("parcoord")
    # location_transServer("a")
}


# Run the application 
shinyApp(ui = ui, server = server)
