# Network UI
visNetworkUI <- function(id) {
  cc <- read_csv("datasets/cc.csv")
  final_tagging<- read_csv("datasets/final_tagging.csv")
  
  tagList(
    titlePanel(
      h1("Network Analysis by Location and Employee via Credit Card Transactions",
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
                               choices=unique(cc$location),
                               multiple=TRUE,options = list(
                                 `actions-box` = TRUE),
                               selected=unique(cc$location)[1:34]),
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
        visNetworkOutput(NS(id, "vis"), height =600)
      )
    )
  )
}

visNetworkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    cc_nodes <- read_csv("datasets/cc_nodes.csv")
    cc_edges <- read_csv("datasets/cc_edges.csv")
    
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
    
   output$vis <- renderVisNetwork({
      cc_nodes_vis <- cc_nodes %>%
        filter(label %in% location() | label %in% employee()) %>% 
        filter(locations==1 | group %in% department())
      
      cc_edges_vis <- cc_edges %>% 
        filter(from %in% cc_nodes_vis$id & to %in% cc_nodes_vis$id &
                 date >= min_date() & date<=max_date() &
                  time_bin %in% time_period())

      visNetwork(cc_nodes_vis, cc_edges_vis) %>% 
      visIgraphLayout(layout = "layout_on_grid") %>% 
      visInteraction(multiselect = TRUE) %>% 
      visLegend() %>% 
      visOptions(selectedBy="group",highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
      visGroups(groupname="Engineering") 
   })
  })
}
