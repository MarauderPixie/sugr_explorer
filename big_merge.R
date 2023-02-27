library(tidyverse)
library(lubridate)

files <- fs::dir_ls("all_csv")
alles <- NULL
max_cgm  <- NULL
max_pump <- NULL
merged_cgm  <- NULL
merged_pump <- NULL

for (file in files) {
    temp <- read_delim(file, 
                            delim = ";", escape_double = FALSE, 
                            col_types = cols(Date = col_date(format = "%Y/%m/%d"), 
                                             Time = col_time(format = "%H:%M:%S")), 
                            locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE, skip = 6) %>% 
        janitor::clean_names() %>% 
        filter(!is.na(index)) %>% 
        transmute(
            file = file, 
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
    
    if (is.null(max_cgm)) {
        max_cgm  <- min(temp$datetime)
        max_pump <- min(temp$datetime)
    }
    
    cutoff_idx <- temp$index[min(which(!is.na(temp$bg_sensor)))]
    
    tmp_cgm <- temp %>% 
        filter(index > cutoff_idx,
               datetime > max_cgm) %>% 
        select(file, index, datetime, date, wday, time, bg_sensor)
    
    tmp_pump <- temp %>% 
        select(-bg_sensor) %>% 
        filter(index <= cutoff_idx,
               datetime > max_pump)
    
    
    # alles <- bind_rows(alles, temp)
    merged_cgm  <- bind_rows(merged_cgm, tmp_cgm)
    merged_pump <- bind_rows(merged_pump, tmp_pump)
    
    
    max_cgm  <- max(merged_cgm$datetime)
    max_pump <- max(merged_pump$datetime)
}

## minor cleanup
## index is on a by-file basis, therefore creating a new one
merged_cgm  <- merged_cgm %>% 
    mutate(index = seq_along(file))
merged_pump <- merged_pump %>% 
    mutate(index = seq_along(file))


## save to disc
## index is 
write_csv(merged_cgm, "data_carelink_done/cgm.csv")
write_csv(merged_pump, "data_carelink_done/pump.csv")

saveRDS(merged_cgm, "data_carelink_done/cgm.rds")
saveRDS(merged_pump, "data_carelink_done/pump.rds")


## any overlaps or gaps?
merged_cgm %>% 
    group_by(file) %>% 
    summarise(
        min = min(date),
        max = max(date),
        .groups = "drop"
    ) %>% 
    gather(minmax, date, min, max) %>% 
    ggplot(aes(file, date, group = file)) +
        coord_flip() +
        # geom_errorbar(size = 5, aes(ymin = date, ymax = date),
        #               lty = "dashed", linewidth = .1) +
        geom_hline(aes(yintercept = date, color = minmax), 
                   lty = "dashed", linewidth =.3) +
        scale_color_manual(values = c("red", "green")) +
        geom_line(linewidth = 2) +
        geom_point(size = 4, color = "#3c4c72", fill = "#f0f0f0", shape = 21)
