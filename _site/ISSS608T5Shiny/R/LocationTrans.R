location_transUI <- function(id) {
  cc <- read_csv("datasets/cc.csv")
  final_tagging<- read_csv("datasets/final_tagging.csv")
  
  tagList(
    titlePanel(
      h1("Location Popularity & Transaction Amount Over Time",
         style='background-color:lightgrey',
         align="center")
    ),
    sidebarLayout(
      sidebarPanel(width=3,
                   pickerInput(NS(id,"location"),
                                "Select by Location",
                                choices=unique(cc$location),
                                multiple=TRUE,options = list(
                                  `actions-box` = TRUE),
                                selected=unique(cc$location)[1:34]),
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
                   pickerInput(NS(id,"department"),
                               "Select by Department",
                               choices=unique(final_tagging$CurrentEmploymentType),
                               multiple=TRUE,options = list(
                                 `actions-box` = TRUE),
                               selected=unique(final_tagging$CurrentEmploymentType)[1:5]),
                   actionButton(NS(id,"submit"),"Submit")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Heatmap",
                   plotlyOutput(NS(id, "heatmap"))),
          tabPanel("Boxplot",
                   plotlyOutput(NS(id, "boxplot"))),
          tabPanel("Barplot", 
                   plotlyOutput(NS(id, "barplot")))
        )
      )
    )
  )
}

location_transServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    cc <- read_csv("datasets/cc.csv")
    final_tagging<- read_csv("datasets/final_tagging.csv")
    cc <- left_join(cc,final_tagging,by=c("last4ccnum"))
    
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
    
    output$heatmap <- renderPlotly({
      cc_cal <-  cc %>% 
        filter(location %in% location() &
               name %in% employee()&
               date >= min_date() & date<=max_date() &
               timeperiod %in% time_period() &
               CurrentEmploymentType %in% department()
               ) %>% 
        count(date, location) %>% 
        complete(date,location) %>% 
        rename(visit_frequency = n)
      
      heatmap <- cc_cal %>% ggplot(aes(x = date, y = location))+
                 geom_tile(aes(fill = visit_frequency), color = "white")+
                 scale_fill_gradient(low = "pale green", high = "black", na.value = "light grey") +
                 scale_x_date(date_labels = "%a \n %d/%m",
                     date_breaks = "2 day") +
        
        labs(title = "Location Visit Frequency By Date",
             fill = "Visit \nCount") +
        theme_bw() +
        theme(plot.title = element_text(size=15, face="bold",hjust=0.5),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())  
      
      ggplotly(heatmap) %>% layout(autosize = F, height = 600, width=900)
    })
    
    median<- cc %>% group_by(location) %>% summarise(median=median(price))
    cc<-cc %>% left_join(median, by=("location"))
    
    output$boxplot <- renderPlotly({
      cc_bpp<- cc %>% filter(location %in% location() &
                       name %in% employee()&
                       date >= min_date() & date<=max_date() &
                       timeperiod %in% time_period() &
                       CurrentEmploymentType %in% department()
      )
      
      boxplot <- ggplot(cc_bpp, aes(x=location, y=price, text=paste("last4ccnum:",last4ccnum,"<br>median:",median))) +
        geom_boxplot(outlier.colour = "red", outlier.fill="red") + 
        geom_point(alpha=0) + scale_y_log10() + coord_flip() +
        scale_x_discrete(limits = rev) +
        ggtitle("Credit Card Transaction Boxplot") +
        theme(axis.title=element_blank(),
              plot.title=element_text(size=15, face="bold", hjust=0.5)) 
      
      
      bpp <- ggplotly(boxplot)
      bpp <- bpp %>% layout(autosize = F, height = 600, width=900)
      bpp$x$data[[1]]$hoverinfo <- "none"
      bpp$x$data[[1]]$marker$line$color = "red"
      bpp$x$data[[1]]$marker$outliercolor = "red"
      bpp$x$data[[1]]$marker$color = "red"
      bpp
    })
    
    output$barplot <- renderPlotly({
      cc_bp <- cc %>% filter(location %in% location() &
                               name %in% employee()&
                               date >= min_date() & date<=max_date() &
                               timeperiod %in% time_period() &
                               CurrentEmploymentType %in% department()
      )
      
      barplot<-cc_bp %>% 
        group_by(location, date, timeperiod) %>%
        summarise(count=n()) %>%  
        ggplot(aes(x=date, y=count, fill=timeperiod,
                   text = paste0(count, " transactions at ",location, " on ", date, timeperiod))) +
        geom_col() + 
        facet_wrap(~location,ncol=6) +
        ggtitle("Number of transactions per day by location") +
        xlab("Date") + ylab("Number of transactions") +
        labs(fill="Time period") +
        theme(plot.title=element_text(size=20,face="bold"),
              axis.title=element_text(size=14,face="bold"),
              axis.ticks = element_blank(),
              strip.text = element_text(size = 8, face="bold"),
              axis.text.x=element_text(angle=45, hjust=1),
              panel.spacing.x=unit(0.1, "lines"),
              panel.spacing.y=unit(0.1, "lines")) 
      
      ggplotly(barplot) %>% layout(autosize = F, height = 700, width=1000)
    })
  })
}

# cc <- read_csv("datasets/cc_data.csv")
# loyalty<-read_csv("datasets/loyalty_data.csv")
# cc$timestamp <- mdy_hm(cc$timestamp)
# cc$date <- date(cc$timestamp)
# cc$last4ccnum <- as_factor(cc$last4ccnum)
# cc$hour <- hour(cc$timestamp)
# cc$hour <- as.numeric(format(cc$timestamp,"%H"))
# cc$weekday <- wday(cc$timestamp, label = TRUE)
# cc$dayofmonth <- day(cc$timestamp)
# cc$timeperiod <- case_when(
#   cc$hour >= 21 ~ "Night",
#   cc$hour >= 18 ~ "Evening",
#   cc$hour >= 15 ~ "Late afternoon",
#   cc$hour >= 12 ~ "Early afternoon",
#   cc$hour >= 9 ~ "Late morning",
#   cc$hour >= 6 ~ "Early morning",
#   TRUE ~ "Late night"
# )
# cc[grep("Katerina", cc$location),2] <- "Katerina's Cafe"
# cc <- tibble::rowid_to_column(cc, "NO")
# cc$concat <- paste(cc$date, cc$weekday, cc$dayofmonth, cc$location, cc$price)
# loyalty$timestamp <- mdy(loyalty$timestamp)
# loyalty$weekday <- wday(loyalty$timestamp, label = TRUE)
# loyalty$dayofmonth <- day(loyalty$timestamp)
# loyalty[grep("Katerina", loyalty$location),2] <- "Katerina's Cafe"
# loyalty <- tibble::rowid_to_column(loyalty, "NO")
# loyalty$concat <- paste(loyalty$timestamp, loyalty$weekday, loyalty$dayofmonth, loyalty$location, loyalty$price)
# cc_cal <- cc %>% count(date, location) %>% rename(Visit_frequency=n)
# 
# write_csv(cc_cal, "datasets/cc_cal.csv")
# write_csv(cc, "datasets/cc.csv")
# cc_cal_ggplot <- ggplot(complete(cc_cal, date, location), aes(x = date, y = location)) +
#   geom_tile(aes(fill = n), color = "white") +
#   scale_fill_gradient(low = "pale green", high = "black", na.value = "light grey") +
#   scale_x_date(date_labels = "%a \n %d/%m",
#                date_breaks = "2 day") +
#   scale_y_discrete(limits = rev) +
#   labs(title = "Location Visit Frequency By Date",
#        fill = "Visit \nCount") +
# 
#   theme_bw() +
#   theme(plot.title = element_text(size=15, face="bold",hjust=0.5),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#   )
# 
# cc_cal_ggplot


# boxplot <- ggplot(cc, aes(x=location, y=price, tooltip=last4ccnum)) +
#   geom_boxplot() + 
#   geom_point(alpha=0) + scale_y_log10() + coord_flip() +
#   scale_x_discrete(limits = rev) +
#   ggtitle("Credit Card Transaction Boxplot") +
#   theme(axis.title=element_blank(),
#         plot.title=element_text(size=15, face="bold", hjust=0.5)) 
# 
# 
# bpp <- ggplotly(boxplot)
# bpp <- bpp %>% layout(autosize = F, height = 500)
# bpp$x$data[[1]]$hoverinfo <- "none"
# # bpp
# 
# final_trans_1 <- final_trans %>% ungroup() %>%
#   mutate(day = as.factor(wday(date)),
#          wkday = ifelse(day == "6" | day =="7", "weekend", "weekday"),
#          time_bin = case_when(
#            hour(datetime)>=0 & hour(datetime)<6 ~ "Midnight",
#            hour(datetime)>=6 & hour(datetime)<12 ~ "Morning",
#            hour(datetime)>=12 & hour(datetime) <18 ~ "Afternoon",
#            hour(datetime)>=18 ~ "Night"),
#          time_bin = factor(time_bin, 
#                            levels = c("Midnight", "Morning", "Afternoon", "Night"))
#   )
# 
# ## Data transformation to plot Bar graph for transaction frequency
# freq<- cc %>% 
#   group_by(location, date, timeperiod) %>% summarise(count=n())
# cc %>% 
#   group_by(location, date, timeperiod) %>% summarise(count=n()) %>%  ggplot(aes(x=date, y=count, fill=timeperiod, 
#                                   tooltip= paste(count, " transactions at ",location, " on ", date, timeperiod))) +
#   gm_col() + 
#   annotate(geom="rect", xmin=ymd(20140111)-.5, xmax=ymd(20140113)-.5, 
#            ymin=-Inf, ymax=Inf, fill='dark grey' , alpha=0.5) +
#   annotate(geom="rect", xmin=ymd(20140118)-.5, xmax=ymd(20140120)-.5, 
#            ymin=-Inf, ymax=Inf, fill='dark grey' , alpha=0.5) +
#   facet_wrap(~location) +
#   xlab("Date") + ylab("Number of transactions") +
#   labs(fill="Time period") +
#   theme(plot.title=element_text(size=20,face="bold"),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text = element_text(size = 6),
#         axis.text=element_text(size=6),
#         axis.text.x=element_text(angle=45, hjust=1),
#         legend.position="bottom") 