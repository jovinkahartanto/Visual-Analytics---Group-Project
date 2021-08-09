mapUI <- function(id) {
  cc <- read_csv("datasets/cc.csv")
  final_tagging<- read_csv("datasets/final_tagging.csv")
  
  tagList(
    titlePanel(
      h1("KRONOS – ABILA Map: GPS Movement Dashboard", 
         style='background-color:lightgrey',
         align="center")
      ),
    sidebarLayout(
      sidebarPanel(width=3,
                   selectInput("employee","Select by Employee",choices = unique(final_tagging$name)),
                   dateRangeInput("date","Select by Date", start="2014-01-06",end="2014-01-19"),
                   selectInput("time_period","Select by Time Period:", choices=unique(cc$timeperiod)),
                   selectInput("location","Select by Location",choices = unique(cc$location))
      ),
      mainPanel(
        tmapOutput(NS(id,"map"))
      )
    )
    #visNetworkOutput(NS(id, "vis"))
  )
}

mapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    stop_fin<- read_csv("datasets/stop_fin.csv")
    bgmap <- raster("datasets/MC2-tourist.tif")
    gps<-read_csv("datasets/gps.csv")
    
    stop_fin1<-st_as_sf(stop_fin, coords=c("long","lat"),crs=4326)
    ## Transform the structure of GPS data for Map
    gps <- gps %>% mutate(timestamp=mdy_hms(Timestamp),id=as_factor(id))
    gps_sf <- st_as_sf(gps, coords=c("long","lat"), crs=4326) %>% 
      group_by(id) %>% 
      summarize(m = mean(timestamp), do_union=FALSE) %>% st_cast("LINESTRING")
    
    ## Plot interactive map
    tmap_mode("view")
    map1<-tm_shape(bgmap) +
      tm_rgb(bgmap, r=1, g=2, b=3, alpha=NA, saturation=1, 
             interpolate=TRUE, max.value=255) +
      tm_shape(gps_sf)+
      tm_lines() +
      tm_shape(stop_fin1)+
      tm_dots(col="blue", shape=30)
    
    output$map <- renderTmap({
      map1
      })
  })
}
