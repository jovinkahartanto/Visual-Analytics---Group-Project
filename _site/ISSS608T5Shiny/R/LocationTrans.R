location_transUI <- function(id) {
  tagList(
    titlePanel("LOCATION POPULARITY & TRANSACTION AMOUNT OVER TIME​"),
    sidebarLayout(
      sidebarPanel(
        selectInput("location","Select by Location",choices = c("A","B")),
        selectInput("employee","Select by Employee",choices = c("C","D")),
        dateRangeInput("date","Select by Date", start="2014-01-06",end="2014-01-19"),
        selectInput("time_period","Select by Time Period:", choices=c("E","F")),
        selectInput("deptartment","Selec by Department", choices=c("G","H"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Heatmap","heatmap"),
          tabPanel("Boxplot","boxplot"),
          tabPanel("Barplot", "barplot")
        )
      )
    )
    #visNetworkOutput(NS(id, "vis"))
  )
}

location_transServer <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
