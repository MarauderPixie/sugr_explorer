library(lubridate)

#############################################################
## preparation
prev_cgm  <- readRDS("data_carelink_done/cgm.rds")
prev_pump <- readRDS("data_carelink_done/pump.rds")

max_cgm  <- max(prev_cgm$datetime)
max_pump <- max(prev_pump$datetime)

files <- fs::dir_ls("all_csv")
file  <- files[length(files)]

#############################################################
## read-in and cleanup
temp <- read_delim(file, delim = ";", escape_double = FALSE, 
                   col_types = cols(Date = col_date(format = "%Y/%m/%d"), 
                                    Time = col_time(format = "%H:%M:%S")), 
                   locale = locale(decimal_mark = ","), 
                   trim_ws = TRUE, skip = 6) %>% 
    janitor::clean_names() %>% 
    filter(!is.na(index)) %>% 
    transmute(
        file               = file, 
        index              = index,
        datetime           = ymd_hms(paste(date, time)),
        date               = date,
        wday               = wday(date, label = TRUE, week_start = 1),
        time               = time,
        basal_rate         = basal_rate_u_h,
        bg_direct          = bg_reading_mg_d_l,
        bg_sensor          = sensor_glucose_mg_d_l,
        wiz_ratio          = bwz_carb_ratio_g_u,
        wiz_carbs          = bwz_carb_input_grams,
        wiz_bg             = bwz_bg_input_mg_d_l,
        wiz_est_correction = bwz_correction_estimate_u,
        wiz_est_food       = bwz_food_estimate_u,
        wiz_est_unabsorbed = bwz_unabsorbed_insulin_total_u,
        bolus_final        = final_bolus_estimate, 
        bolus_delivered    = bolus_volume_delivered_u
    )

#############################################################
## splitting sensor and pump data
cutoff_idx <- temp$index[min(which(!is.na(temp$bg_sensor)))]

tmp_cgm <- temp %>% 
    filter(index > cutoff_idx,
           datetime > max_cgm) %>% 
    select(file, index, datetime, date, wday, time, bg_sensor)

tmp_pump <- temp %>% 
    select(-bg_sensor) %>% 
    filter(index <= cutoff_idx,
           datetime > max_pump)

merged_cgm  <- bind_rows(prev_cgm, tmp_cgm) %>% 
    mutate(index = seq_along(file))
merged_pump <- bind_rows(prev_pump, tmp_pump) %>% 
    mutate(index = seq_along(file))

#############################################################
## store final data
saveRDS(merged_cgm,  "data_carelink_done/cgm.rds")
saveRDS(merged_pump, "data_carelink_done/pump.rds")

write_csv(merged_cgm,  "data_carelink_done/cgm.csv")
write_csv(merged_pump, "data_carelink_done/pump.csv")

rm(ls())


# basal <- data_full %>% 
#     select(time, basal_rate) %>% 
#     filter(!is.na(basal_rate))
# bolus <- data_full %>% 
#     select(index, date, weekday, time, bolus_vol_delivered, 
#            wiz_ratio, wiz_carbs, wiz_correction) %>% 
#     filter(if_any(c(bolus_vol_delivered, wiz_ratio), ~ !is.na(.)))
