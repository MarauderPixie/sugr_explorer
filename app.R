library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinyWidgets)

geom_boxplot  <- purrr::partial(geom_boxplot, alpha = .4,
                                outlier.alpha = 0,
                                color = as.character(hrbrthemes::ft_cols[8]),
                                fill  = as.character(hrbrthemes::ft_cols[8]))

geom_point <- purrr::partial(geom_point, shape = 23, 
                             color = "white") # hrbrthemes::ft_cols[8])

# theme_set(hrbrthemes::theme_ipsum_rc())
theme_set(hrbrthemes::theme_ft_rc(plot_margin = margin(10, 10, 10, 10)))

# dt <- readRDS("data_done/basal.rds")
# dt <- readRDS("data_done/bolus.rds")
# dt <- readRDS("data_tidepool_done/cgm.rds") %>% 
#    select(-starts_with("tmp_"),
#           -starts_with("wiz_"),
#           -bg_input, -normal) %>% 
#    mutate(
#       level_range = case_when(
#          value > 250 ~ 6,
#          between(value, 181, 250) ~ 5,
#          between(value, 131, 180) ~ 4,
#          between(value, 81, 130) ~ 3,
#          between(value, 61, 80) ~ 2,
#          value < 60 ~ 1
#       ) %>% factor(labels = c("too low! (<60)", "low (61-80)",
#                               "ideal (81-130)", "above ideal (131-180)",
#                               "high (181-250)", "too high! (>250)"), ordered = TRUE)
#    )

dt <- readRDS("data_carelink_done/cgm.rds") %>% 
    rename(value = "sensor_glucose")

day_labels <- levels(dt$weekday)

# Define UI for application ----
ui <- fluidPage(
   theme = shinythemes::shinytheme("slate"),
   # good shiny themes: cosmo, slate (dark), spacelab
   
   # Application title
   titlePanel("Glucose Level Explorer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         dateRangeInput("date_range",
                        "Select time range to display",
                        min = min(dt$date),
                        max = max(dt$date),
                        start = max(dt$date) - days(14),
                        end = max(dt$date)),
         
         # radioGroupButtons("weekends", "Include Weekends?",
         #                   choices = c("All Days", 
         #                               "No Weekends",
         #                               "Weekends only"),
         #                   selected  = "All Days",
         #                   direction = "vertical",
         #                   justified = TRUE),
         
         prettyCheckboxGroup("quantis", label = "Show middle range of data", 
                             choices = list("50%" = 50, "80%" = 80, "100%" = 100),
                             selected = 50, animation = "smooth"),
         
         prettyCheckbox("mean", "Show mean", animation = "smooth")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
            type = "pills",
            # plotOutput("plot_lines"),
            tabPanel("Trend", plotOutput("plot_trend")),
            tabPanel("Average Day", 
                     plotOutput("plot_avgday"),
                     
                     checkboxGroupButtons(
                        "daycheck", "", justified = TRUE,
                        choices  = day_labels,
                        selected = day_labels
                     ))
         )
      )
   )
)

# Define server logic required ----
server <- function(input, output) {
   
   #### old plot stuff ----
   # output$plot_lines <- renderPlot({
   #    deadline <- today() - days(input$drange)
   #    checks <- as.numeric(input$quantis)
   #    perc <- (1 - max(checks)/100) / 2
   #    
   #    dtf <- dt %>% 
   #       group_by(date, hour(datetime)) %>% 
   #       filter(datetime > deadline,
   #              between(value, 
   #                      quantile(value, perc, na.rm = T),
   #                      quantile(value, 1-perc, na.rm = T))) %>% 
   #       ungroup()
   #    
   #    ggplot(dtf, aes(datetime, value)) +
   #       geom_line(size = .2) +
   #       # geom_point(size = .1, alpha = .5) +
   #       geom_smooth() +
   #       scale_y_continuous(limits = c(0, 350)) +
   #       labs(x = "Timerange", y = "Glucose Level (mg/dl)")
   # })
   
   #### ----
   output$plot_trend <- renderPlot({
      checks    <- as.numeric(input$quantis)
      
      date_from <- input$date_range[1]
      date_to   <- input$date_range[2]
      
      dtf <- dt %>% 
         filter(between(date, date_from, date_to)) %>% 
         group_by(date) %>% 
         summarise(
            min = min(value),
            q10 = quantile(value, .10),
            q25 = quantile(value, .25),
            mid = median(value),
            mean = mean(value),
            q75 = quantile(value, .75),
            q90 = quantile(value, .90),
            max = max(value)
         ) %>% 
         ungroup()
      
      print(paste("Class of dft$date:", class(dtf$date)))
      print(paste("Class of as.Date:", 
                  class(
                     as.Date("2012-02-02")
                  )
      ))
      
      mean_size <- ifelse(length(dtf$date) <= 21, 5, 3)
      
      p <- ggplot(dtf, aes(date, group = date))
      p <- p + scale_y_continuous(limits = c(0, 350),
                                  breaks = c(50, 80, 130, 200, 250, 300, 350),
                                  minor_breaks = NULL)
      p <- p + scale_x_date(date_breaks = "3 days",
                            date_minor_breaks = "1 day",
                            date_labels = "%d.%b",
                            sec.axis = dup_axis(labels = scales::date_format("%a"),
                                                breaks = scales::date_breaks("2 day"))
      )
      # p <- p + scale_x_date(date_minor_breaks = "1 day")
      p <- p + labs(x = "Timerange", y = "Glucose Level (mg/dl)")
      if (50 %in% checks){
         p <- p + geom_boxplot(aes(lower = q25, upper = q75,
                                   middle = mid, ymin = q25, ymax = q75),
                               stat = "identity")
      }
      if (80 %in% checks){
         p <- p + geom_boxplot(aes(lower = q10, upper = q90,
                                   middle = mid, ymin = q10, ymax = q90),
                               stat = "identity")
      }
      if (100 %in% checks){
         p <- p + geom_boxplot(aes(lower = min, upper = max,
                                   middle = mid, ymin = min, ymax = max),
                               stat = "identity")
      }
      if (isTRUE(input$mean)){
         p <- p + geom_point(aes(y = mean), size = mean_size) 
      }
      p
   })
   
   output$plot_avgday <- renderPlot({
      checks    <- as.numeric(input$quantis)
      
      date_from <- input$date_range[1]
      date_to   <- input$date_range[2]
      daycheck  <- input$daycheck
      
      dtf <- dt %>% 
         filter(between(date, date_from, date_to),
                weekday %in% daycheck) %>% 
         group_by(hour(time)) %>% 
         summarise(
            min = min(value),
            q10 = quantile(value, .10),
            q25 = quantile(value, .25),
            mid = median(value),
            mean = mean(value),
            q75 = quantile(value, .75),
            q90 = quantile(value, .90),
            max = max(value)
         ) %>% 
         ungroup() %>% 
         dplyr::rename("Hour" = `hour(time)`)
      
      p <- ggplot(dtf, aes(Hour, group = Hour))
      p <- p + scale_y_continuous(limits = c(0, 350),
                                  breaks = c(50, 80, 130, 200, 250, 300, 350),
                                  minor_breaks = NULL)
      # p <- p + geom_hline(yintercept = c(90, 150), 
      #                     lty = "dashed", color = "dark green")
      p <- p + labs(x = "Hour", y = "Glucose Level (mg/dl)")
      if (50 %in% checks){
         p <- p + geom_boxplot(aes(lower = q25, upper = q75,
                                   middle = mid, ymin = q25, ymax = q75),
                               stat = "identity")
      }
      if (80 %in% checks){
         p <- p + geom_boxplot(aes(lower = q10, upper = q90,
                                   middle = mid, ymin = q10, ymax = q90),
                               stat = "identity")
      }
      if (100 %in% checks){
         p <- p + geom_boxplot(aes(lower = min, upper = max,
                                   middle = mid, ymin = min, ymax = max),
                               stat = "identity")
      }
      if (isTRUE(input$mean)){
         p <- p + geom_point(aes(y = mean), size = 3) 
      }
      p
   })
}

# Run the application ----
shinyApp(ui = ui, server = server)

