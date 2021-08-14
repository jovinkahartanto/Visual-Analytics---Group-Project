mapUI <- function(id) {
  cc <- read_csv("datasets/cc.csv")
  final_tagging<- read_csv("datasets/final_tagging.csv")
  gps_stop <- read_csv("datasets/gps_stop.csv")
  
  tagList(
    titlePanel(
      h1("KRONOS – ABILA Map: GPS Movement Dashboard", 
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
                   dateRangeInput(NS(id,"date"),"Select by Date", 
                                  start="2014-01-06", end="2014-01-19",
                                  min="2014-01-06", max="2014-01-19"),
                   pickerInput(NS(id,"time_period"),
                               "Select by Time Period",
                               choices=unique(cc$timeperiod),
                               multiple=TRUE,options = list(
                                 `actions-box` = TRUE),
                               selected=unique(cc$timeperiod)[1:7]),
                   pickerInput(NS(id,"location"),
                               "Select by Location",
                               choices=unique(gps_stop$Possible_Location)[order(nchar(unique(gps_stop$Possible_Location)), unique(gps_stop$Possible_Location))],
                               multiple=TRUE,options = list(
                                 `actions-box` = TRUE),
                               selected=unique(gps_stop$Possible_Location)[1:10]),
                   actionButton(NS(id,"submit"),"Submit")
      ),
      mainPanel(
        tabsetPanel(
        tabPanel("Map",tmapOutput(NS(id,"map"),height=750, width=1000)),
        tabPanel("Scatter Plot", 
                 plotlyOutput(NS(id, "dotplot")))
        )
      )
    )
  )
}

mapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    stop_fin <- read_csv("datasets/stop_fin.csv")
    gps_stop <- read_csv("datasets/gps_stop.csv") 
    location_tag <- read_csv("datasets/location_tag.csv")
    final_tagging<- read_csv("datasets/final_tagging.csv") %>% dplyr::select(id,name)%>%unique()
    
    bgmap <- raster("datasets/MC2-tourist.tif")
    gps<-read_csv("datasets/gps_with_name.csv")
    Abila_st <- st_read(dsn = "datasets/Geospatial",
                        layer = "Abila")
    
    
    P <- npts(Abila_st, by_feature = TRUE)
    Abila_st_2 <- cbind(Abila_st,P) %>% filter(P > 1)
    
    ## STOP POINT
    
    gps_stop$hour <- as.numeric(format(gps_stop$timestamp,"%H"))
    gps_stop$timeperiod <- case_when(
      gps_stop$hour >= 21 ~ "Night",
      gps_stop$hour >= 18 ~ "Evening",
      gps_stop$hour >= 15 ~ "Late afternoon",
      gps_stop$hour >= 12 ~ "Early afternoon",
      gps_stop$hour >= 9 ~ "Late morning",
      gps_stop$hour >= 6 ~ "Early morning",
      TRUE ~ "Late night")
    
    gps_stop$date2 <- as_date(gps_stop$timestamp)

    ## Transform the structure of GPS data for Map
    location_tag_sf <- st_as_sf(location_tag, coords=c("long","lat"), crs=4326)
    
    gps$date <- gps$timestamp <- date_time_parse(gps$Timestamp, 
                                                 zone = "", 
                                                 format = "%m/%d/%Y")
    gps$timestamp <- date_time_parse(gps$Timestamp, 
                                     zone = "", 
                                     format = "%m/%d/%Y  %H:%M:%S")
    
    gps$hour <- as.numeric(format(gps$timestamp,"%H"))
    
    gps$timeperiod <- case_when(
      gps$hour >= 21 ~ "Night",
      gps$hour >= 18 ~ "Evening",
      gps$hour >= 15 ~ "Late afternoon",
      gps$hour >= 12 ~ "Early afternoon",
      gps$hour >= 9 ~ "Late morning",
      gps$hour >= 6 ~ "Early morning",
      TRUE ~ "Late night")
    
    gps$id <- as_factor(gps$id)

    
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
    
    ## Dot Plot
  
    
    output$dotplot <- renderPlotly({
      
      gps_stop_dp <- gps_stop %>% filter(
        Possible_Location %in% location()&
          timeperiod %in% time_period()&
          name %in% employee()&
          date2 >= min_date() & date2 <=max_date())
    
      dotplot<-gps_stop_dp %>% 
        ggplot(aes(x=timeperiod, y=date2, fill=name)) +
        ggtitle("Location Visitor") +
        geom_jitter() +
        facet_wrap(~Possible_Location,ncol=4) +
        xlab("Timeperiod")+ylab("Date")+
        labs(fill="name") +
        theme(plot.title=element_text(size=20,face="bold"),
              axis.title=element_text(size=14,face="bold",hjust=10),
              axis.ticks = element_blank(),
              strip.text = element_text(size = 8, face="bold"),
              axis.text.x=element_text(angle=-45),
              panel.spacing.x=unit(0.5, "lines"),
              panel.spacing.y=unit(0.5, "lines"))
      
      ggplotly(dotplot) %>% layout(autosize = F, height = 650, width=1000)
    })
  
      
    ## Plot interactive map
      
    output$map <- renderTmap({
      gps_filter <- gps %>% 
        filter(name %in% employee()&
               timeperiod %in% time_period()&
               date >= min_date() & date<=max_date())
      
      gps_selected <- gps_filter%>% dplyr::select(id,timestamp,long, lat)
      gps_sf <- st_as_sf(gps_selected, coords=c("long","lat"), crs=4326) %>% 
        group_by(id) %>% 
        summarise(m=mean(timestamp), do_union=FALSE) %>% 
        st_cast("LINESTRING")
      
      gps_stop_dp <- gps_stop %>% filter(
        date2 >= min_date() & date2 <=max_date()&
          timeperiod %in% time_period()&
          name %in% employee())
      
      gps_stop_sf_2 <- st_as_sf(gps_stop_dp, coords=c("long","lat"),crs=4326)
      
      
      tmap_mode("view")
      
      map1<-tm_shape(bgmap) +
        tm_rgb(bgmap, r=1, g=2, b=3, alpha=NA, saturation=1, 
               interpolate=TRUE, max.value=255) +
        tm_shape(Abila_st_2)+
        tm_lines(col = "red", scale = 1)+
        tm_shape(gps_sf)+
        tm_lines()+
        tm_shape(gps_stop_sf_2)+
        tm_dots(col="blue", shape=30)
      
      map1
      })
  })
}

