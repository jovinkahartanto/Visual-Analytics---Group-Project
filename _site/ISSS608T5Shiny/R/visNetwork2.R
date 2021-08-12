# Network UI
visNetwork2UI <- function(id) {
  stop_fin <- read_csv("datasets/stop_fin.csv")
  final_tagging<- read_csv("datasets/final_tagging.csv")
  cc <- read_csv("datasets/cc.csv")
  tagList(
    titlePanel(
      h1("Network Analysis by Location and Employee via car GPS locations",
         style='background-color:lightgrey',
         align="center")
      ),
    sidebarLayout(
      sidebarPanel(width=3,
                   pickerInput(NS(id,"employee"),
                               "Select by Employee",
                               choices=unique(final_tagging$name),
                               multiple=TRUE,options = list(
                                 `actions-box` = TRUE),
                               selected=unique(final_tagging$name)[1:34]),
                   pickerInput(NS(id,"location"),
                               "Select by Location",
                               choices=unique(stop_fin$Possible_Location)[order(nchar(unique(stop_fin$Possible_Location)), unique(stop_fin$Possible_Location))],
                               multiple=TRUE,options = list(
                                 `actions-box` = TRUE),
                               selected=unique(stop_fin$Possible_Location)[1:67]),
                   pickerInput(NS(id,"department"),
                               "Select by Department",
                               choices=unique(final_tagging$CurrentEmploymentType),
                               multiple=TRUE,options = list(
                                 `actions-box` = TRUE),
                               selected=unique(final_tagging$CurrentEmploymentType)[1:5]),
                   dateRangeInput(NS(id,"date"),"Select by Date", 
                                  start="2014-01-06", end="2014-01-19",
                                  min="2014-01-06", max="2014-01-19"),
                   pickerInput(NS(id,"time_period"),
                               "Select by Time Period",
                               choices=unique(cc$timeperiod),
                               multiple=TRUE,options = list(
                                 `actions-box` = TRUE),
                               selected=unique(cc$timeperiod)[1:7]),

                   actionButton(NS(id,"submit"),"Submit")
        
      ),
      mainPanel(
        visNetworkOutput(NS(id, "vis2"))
      )
    )
  )
}

visNetwork2Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    gps_nodes <- read_csv("datasets/gps_nodes.csv")
    gps_edges <- read_csv("datasets/gps_edges.csv")
    
    gps_nodes$locations <- if_else(gps_nodes$group=="Locations",1,0)
    
    location <- eventReactive(input$submit,{
      input$location
    }, ignoreNULL = FALSE)
    employee <- eventReactive(input$submit,{
      input$employee
    }, ignoreNULL = FALSE)
    min_date <- eventReactive(input$submit,{
      input$date[1]
    }, ignoreNULL = FALSE)
    max_date <- eventReactive(input$submit,{
      input$date[2]
    }, ignoreNULL = FALSE)
    time_period <- eventReactive(input$submit,{
      input$time_period
    }, ignoreNULL = FALSE)
    department <- eventReactive(input$submit,{
      input$department
    }, ignoreNULL = FALSE)
    
  output$vis2 <- renderVisNetwork({
    gps_nodes_vis <- gps_nodes %>%
      filter(label %in% location() | label %in% employee()) %>% 
      filter(locations==1 | group %in% department())
    
    gps_edges_vis <- gps_edges %>% 
      filter(from %in% gps_nodes_vis$id & to %in% gps_nodes_vis$id &
               date >= min_date() & date<=max_date() &
               time_bin %in% time_period())
    
    visNetwork(gps_nodes_vis, gps_edges_vis) %>% 
      visIgraphLayout(layout = "layout_on_grid") %>% 
      visInteraction(multiselect = TRUE) %>% 
      visLegend() %>% 
      visOptions(selectedBy="group",highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
      visGroups(groupname="Engineering") 
  })
  
})
}
