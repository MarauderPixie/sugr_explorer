library(lubridate)

data_full <- read_delim("data_carelink_raw/carelink-export-23-01-17.csv", 
                  delim = ";", escape_double = FALSE, 
                  col_types = cols(Date = col_date(format = "%Y/%m/%d"), 
                                   Time = col_time(format = "%H:%M:%S")), 
                  locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE, skip = 6) %>% 
    transmute(
        index = Index,
        date  = Date,
        weekday = wday(date, label = TRUE, week_start = 1),
        time  = Time, # round_date(Time, unit = "minute"),
        # alarm = Alarm,
        bg_reading = `BG Reading (mg/dL)`,
        basal_rate = `Basal Rate (U/h)`,
        # calibration_bg = `Sensor Calibration BG (mg/dL)`,
        sensor_glucose = `Sensor Glucose (mg/dL)`,
        bolus_vol_delivered = `Bolus Volume Delivered (U)`,
        prime_type = `Prime Type`,
        prime_vol_delivered = `Prime Volume Delivered (U)`,
        wiz_ratio = `BWZ Carb Ratio (g/U)`,
        wiz_carbs = `BWZ Carb Input (grams)`, # carbs / ratio ~= delivered
        wiz_bg_input = `BWZ BG Input (mg/dL)`,
        wiz_correction = `BWZ Correction Estimate (U)`,
        wiz_food = `BWZ Food Estimate (U)`,
        wiz_unabsorbed = `BWZ Unabsorbed Insulin Total (U)`,
        bolus_final = `Final Bolus Estimate`
    ) %>% 
    filter(!is.na(index))

cgm <- data_full %>% 
    select(index, date, weekday, time, sensor_glucose) %>% 
    filter(!is.na(sensor_glucose))

# basal <- data_full %>% 
#     select(time, basal_rate) %>% 
#     filter(!is.na(basal_rate))

bolus <- data_full %>% 
    select(index, date, weekday, time, bolus_vol_delivered, 
           wiz_ratio, wiz_carbs, wiz_correction) %>% 
    filter(if_any(c(bolus_vol_delivered, wiz_ratio), ~ !is.na(.)))

saveRDS(bolus, "data_carelink_done/bolus.rds")
saveRDS(cgm, "data_carelink_done/cgm.rds")



### tsibblerize all
wip <- cgm %>% 
    mutate(
        datetime = round_date(ymd_hms(paste(date, time)), 
                              unit = "minute")
    ) %>% 
    distinct(datetime, .keep_all = TRUE) %>% 
    as_tsibble(index = datetime, 
               # key = weekday, 
               regular = FALSE)

wip %>% 
    ACF(sensor_glucose, lag_max = 100) %>% 
    autoplot()

wip %>% 
    model(STL(sensor_glucose ~ season(datetime)))
