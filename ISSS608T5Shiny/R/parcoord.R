cardMappingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    parcoord<-read_csv("datasets/parcoord.csv") 
    card_tag <- read_csv("datasets/card_tag.csv")
    final_tag<- read_csv("datasets/final_tagging.csv") 
    tag <- left_join(card_tag, final_tag, by=c("last4ccnum")) %>% 
      dplyr::select(-id,-name)
    
    parcoord<-parcoord %>% 
      mutate(location.numeric=as.numeric(factor(location, levels=unique(c(location)))),
             cc.date.numeric=as.numeric(factor(as.character(cc.date), levels=unique(c(as.character(parcoord$cc.date))))),
             loyalty.date.numeric=as.numeric(factor(as.character(loyalty.date), levels=unique(c(as.character(parcoord$loyalty.date))))),
             diff=cc.price-loyalty.price
      )
    output$table_DT <- DT::renderDataTable({
      data=tag %>% filter(last4ccnum %in% input$last4ccnum)
    })
    output$parcoord <- renderPlotly({
      parcoord %>% dplyr::filter(last4ccnum %in% input$last4ccnum) %>% 
        plot_ly(type = 'parcoords',
                line = list(color = ~last4ccnum),
                dimensions = list(
                  list(tickvals=unique(parcoord$cc.date.numeric),
                       ticktext = unique(parcoord$cc.date),
                       label = 'CC Transaction Date', values = ~cc.date.numeric),
                  list(tickvals=unique(parcoord$loyalty.date.numeric),
                       ticktext = unique(parcoord$loyalty.date),
                       label = 'Loyalty Transaction Date', values = ~loyalty.date.numeric),
                  list(tickvals = unique(parcoord$location.numeric),
                       ticktext = unique(parcoord$location),
                       label = 'Location', values = ~location.numeric),
                  list(range = c(~min(diff),~max(diff)),
                       label = 'Price difference', values = ~diff)
                )
        ) %>% layout(margin=list(l=80,r=50))
    })
    
    
  })
}
cardMappingUI <- function(id) {
  parcoord<-read_csv("datasets/parcoord.csv") 
  tagList(
    titlePanel(
      h1("Credit Card and Loyalty Card Mapping",
         style='background-color:lightgrey',
         align="center")
    ),
    sidebarLayout(
      sidebarPanel(width=3,
                   pickerInput(NS(id,"last4ccnum"),
                               "Select CC number",
                               choices=unique(parcoord$last4ccnum),
                               multiple=TRUE,options = list(
                                 `actions-box` = TRUE),
                               selected=7108)
                   ),
      
    mainPanel(
        DT::dataTableOutput(NS(id,"table_DT")),
        plotlyOutput(NS(id, "parcoord"))
      )
  )
  )
}