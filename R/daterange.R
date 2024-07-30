daterange_ui <- function(id) {
    dateRangeInput(NS(id, "date_range"),
                   "Select time range to display",
                   min = min(dt$date),
                   max = max(dt$date),
                   start = max(dt$date) - days(14),
                   end = max(dt$date)) 
}

daterange_server <- function(id) {
    moduleServer(id, function(input, output, session) {
    })
}