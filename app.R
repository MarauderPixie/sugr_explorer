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
#          bg_sensor > 250 ~ 6,
#          between(bg_sensor, 181, 250) ~ 5,
#          between(bg_sensor, 131, 180) ~ 4,
#          between(bg_sensor, 81, 130) ~ 3,
#          between(bg_sensor, 61, 80) ~ 2,
#          bg_sensor < 60 ~ 1
#       ) %>% factor(labels = c("too low! (<60)", "low (61-80)",
#                               "ideal (81-130)", "above ideal (131-180)",
#                               "high (181-250)", "too high! (>250)"), ordered = TRUE)
#    )

cgm <- readRDS("data_carelink_done/cgm.rds") %>% 
    select(-date, -time, -index) %>% 
    mutate(
        datetime = round_date(datetime, unit = "5 mins")
    ) %>% 
    filter(!is.na(bg_sensor)) %>% 
    distinct()
pmp <- readRDS("data_carelink_done/pump.rds") %>% 
    select(-date, -time, -index) %>% 
    mutate(
        datetime = round_date(datetime, unit = "5 mins")
    ) %>% 
    distinct()

dt <- left_join(cgm, pmp, by = c("file", "datetime", "wday")) %>% 
    mutate(
        date = date(datetime),
        time = hms::as_hms(datetime),
        .after = datetime
    )

day_labels <- levels(dt$wday)

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
            
            # dateRangeInput("date_range_comp",
            #                "Select time range to compare to",
            #                min = min(dt$date),
            #                max = max(dt$date),
            #                start = max(dt$date) - days(7),
            #                end = max(dt$date)),
            
            # radioGroupButtons("weekends", "Include Weekends?",
            #                   choices = c("All Days", 
            #                               "No Weekends",
            #                               "Weekends only"),
            #                   selected  = "All Days",
            #                   direction = "vertical",
            #                   justified = TRUE),
            
            # conditionalPanel(
            #     condition = 'panelid.tabname == "tab1"',
                prettyCheckboxGroup("quantis", label = "Show middle range of data", 
                                    choices = list("50%" = 50, "80%" = 80, "100%" = 100),
                                    selected = 50, animation = "smooth"),
                
                prettyCheckbox("mean", "Show mean", animation = "smooth")
            # )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                id = "panelid",
                type = "pills",
                # plotOutput("plot_lines"),
                tabPanel("Summary", 
                         tabName = "tab1",
                         plotOutput("plot_trend")),
                tabPanel("Average Day - Quantiles", 
                         tabName = "tab2",
                         plotOutput("plot_avgday"),
                         
                         checkboxGroupButtons(
                             "quant_daycheck", "", justified = TRUE,
                             choices  = day_labels,
                             selected = day_labels
                         )),
                tabPanel("Average Day - Trend", 
                         tabName = "tab3",
                         plotOutput("plot_gctrend"),
                         
                         checkboxGroupButtons(
                             "daycheck2", "", justified = TRUE,
                             choices  = day_labels,
                             selected = day_labels
                         )),
                tabPanel("Average Day - Comparison",
                         tabName = "tab4",
                         plotOutput("plot_gccomp"))
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
    #              between(bg_sensor, 
    #                      quantile(bg_sensor, perc, na.rm = T),
    #                      quantile(bg_sensor, 1-perc, na.rm = T))) %>% 
    #       ungroup()
    #    
    #    ggplot(dtf, aes(datetime, bg_sensor)) +
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
            filter(between(date, date_from, date_to), !is.na(bg_sensor)) %>% 
            group_by(date) %>% 
            summarise(
                min = min(bg_sensor),
                q10 = quantile(bg_sensor, .10),
                q25 = quantile(bg_sensor, .25),
                mid = median(bg_sensor),
                mean = mean(bg_sensor),
                q75 = quantile(bg_sensor, .75),
                q90 = quantile(bg_sensor, .90),
                max = max(bg_sensor)
            ) %>% 
            ungroup()
        
        # print(paste("Class of dft$date:", class(dtf$date)))
        # print(paste("Class of as.Date:", 
        #             class(
        #                as.Date("2012-02-02")
        #             )
        # ))
        
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
        daycheck  <- input$quant_daycheck
        
        dtf <- dt %>% 
            filter(between(date, date_from, date_to),
                   wday %in% daycheck,
                   !is.na(bg_sensor)) %>% 
            group_by(hour(time)) %>% 
            summarise(
                min = min(bg_sensor),
                q10 = quantile(bg_sensor, .10),
                q25 = quantile(bg_sensor, .25),
                mid = median(bg_sensor),
                mean = mean(bg_sensor),
                q75 = quantile(bg_sensor, .75),
                q90 = quantile(bg_sensor, .90),
                max = max(bg_sensor)
            ) %>% 
            ungroup() %>% 
            dplyr::rename("Hour" = `hour(time)`)
        
        p <- ggplot(dtf, aes(Hour, group = Hour))
        p <- p + scale_y_continuous(limits = c(0, 350),
                                    breaks = c(50, 80, 130, 200, 250, 300, 350),
                                    minor_breaks = NULL)
        p <- p + scale_x_time(breaks = seq(0, 24, 3),
                              labels = paste0(seq(0, 24, 3), ":00"))
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
    
    output$plot_gctrend <- renderPlot({
        date_from <- input$date_range[1]
        date_to   <- input$date_range[2]
        daycheck  <- input$daycheck2
        
        dt_subset <- dt %>% 
            filter(between(date, date_from, date_to),
                   wday %in% daycheck,
                   !is.na(bg_sensor))
        
        p <- ggplot(dt_subset, aes(x = time, y = bg_sensor, group = date))
        p <- p + scale_y_continuous(breaks = seq(60, 380, 80),
                                    minor_breaks = seq(60, 380, 40))
        p <- p + scale_x_time(breaks = seq(0, 24, 3)*3600,
                              labels = paste0(seq(0, 24, 3), ":00"))
        p <- p + geom_hline(yintercept = c(80, 220),
                            linetype = "longdash", color = "red")
        # p <- p + geom_hline(yintercept = c(90, 150), 
        #                     lty = "dashed", color = "dark green")
        p <- p + labs(
            color = "Weekday", fill = "Weekday",
            x = "Hour", y = "Glucose level (mg/dl)"
        )
        p <- p + theme(legend.position = "top")
        p <- p + geom_line(aes(color = wday), linewidth = .3, alpha = .2)
        p <- p + geom_smooth(aes(group = wday, color = wday, fill = wday), 
                             se = TRUE, method = "gam")
        ## needs options for:
        ## - show all days
        # if (50 %in% checks){
        #     p <- p + geom_boxplot(aes(lower = q25, upper = q75,
        #                               middle = mid, ymin = q25, ymax = q75),
        #                           stat = "identity")
        # }
        # if (80 %in% checks){
        #     p <- p + geom_boxplot(aes(lower = q10, upper = q90,
        #                               middle = mid, ymin = q10, ymax = q90),
        #                           stat = "identity")
        # }
        # if (100 %in% checks){
        #     p <- p + geom_boxplot(aes(lower = min, upper = max,
        #                               middle = mid, ymin = min, ymax = max),
        #                           stat = "identity")
        # }
        # if (isTRUE(input$mean)){
        #     p <- p + geom_point(aes(y = mean), size = 3) 
        # }
        p
    })
    
    output$plot_gccomp <- renderPlot({
        date_from <- input$date_range[1]
        date_to   <- input$date_range[2]
        
        date_comp_from <- input$date_range_comp[1]
        date_comp_to   <- input$date_range_comp[2]
        
        dt_subset <- dt %>% 
            filter(between(date, date_from, date_to),
                   between(date, date_comp_from, date_comp_to),
                   !is.na(bg_sensor)) %>% 
            mutate(
                Comparison = ifelse(
                    between(date, date_from, date_to),
                    paste("Period 1:", date_from, "to", date_to),
                    paste("Period 2:", date_comp_from, "to", date_comp_to),
                )
            )
        
        p <- ggplot(dt_subset, aes(x = time, y = bg_sensor, group = date))
        p <- p + scale_y_continuous(breaks = seq(60, 380, 80),
                                    minor_breaks = seq(60, 380, 40))
        p <- p + scale_x_time(breaks = seq(0, 24, 3)*3600,
                              labels = paste0(seq(0, 24, 3), ":00"))
        p <- p + geom_hline(yintercept = c(80, 220),
                            linetype = "longdash", color = "red")
        p <- p + labs(
            color = "Weekday", fill = "Weekday",
            x = "Hour", y = "Glucose level (mg/dl)"
        )
        p <- p + theme(legend.position = "top")
        
        p <- p + geom_line(aes(color = Comparison), linewidth = .3, alpha = .2)
        p <- p + geom_smooth(aes(group = Comparison, color = Comparison, fill = Comparison), 
                             se = TRUE, method = "gam")
        p
    })
}

# Run the application ----
shinyApp(ui = ui, server = server)

