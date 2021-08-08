# Network UI
visNetworkUI <- function(id) {
  tagList(
    titlePanel(
      h1("Network Analysis by Location and Employee",
         style='background-color:lightgrey',
         align="center")
      ),
    visNetworkOutput(NS(id, "vis"))
  )
}

visNetworkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    cc_nodes <- read_csv("datasets/cc_nodes.csv")
    cc_edges <- read_csv("datasets/cc_edges.csv")
    
   output$vis <- renderVisNetwork({
      visNetwork(cc_nodes, cc_edges, main="Network analysis by location and employee") %>% 
      visIgraphLayout(layout = "layout_on_grid") %>% 
      visInteraction(multiselect = TRUE) %>% 
      visLegend() %>% 
      visOptions(selectedBy="group",highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
      visGroups(groupname="Engineering") 
  
    })
  })
}
