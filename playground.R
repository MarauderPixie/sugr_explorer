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


# cgm_carbs <- cgm %>%
#     left_join(bolus, by = c(
#         "file", "datetime", "wday"
# )) %>%
# mutate(
#     bg_at_bolus = ifelse(is.na(bolus_delivered), NA, bg_sensor)
# )


dt %>% 
    filter(date(datetime) == "2024-06-13") %>%
    mutate(
        bg_at_bolus = ifelse(is.na(bolus_delivered), NA, bg_sensor)
    ) %>% 
    ggplot(aes(x = datetime, y = bg_sensor)) +
    geom_line(linetype = "dashed") +
    geom_text(aes(y = bg_at_bolus+10, label = paste("💉", bolus_delivered, "IU"), size = 5)) +
    geom_text(
        # data = carbs %>% filter(date(datetime) == "2024-06-13",
        #                               wiz_carbs != 0,
        #                               !is.na(wiz_carbs)),
        aes(x = datetime, y = wiz_carbs, label = "🍴", size = 5)) +
    # geom_point(data = filter(bolus, date(datetime) == "2024-06-13"),
    #            aes(x = datetime, y = 250), shape = 23) +
    scale_x_datetime(date_labels = "%H:%M",
                     date_breaks = "3 hours")


ggplot(dt, aes(x = hms::as_hms(datetime), y = bg_sensor, group = date(datetime))) +
    # geom_ribbon(aes(ymin = 90, ymax = 140), fill = "#0db77e", alpha = .01) +
    geom_hline(yintercept = c(80, 220), linetype = "longdash", color = "red") +
    geom_line(aes(color = wday), linewidth = .3, alpha = .2) +
    geom_smooth(aes(group = wday, color = wday, fill = wday), se = TRUE, method = "gam") +
    scale_y_continuous(breaks = seq(60, 380, 80),
                       minor_breaks = seq(60, 380, 40)) +
    scale_x_time(breaks = seq(0, 24, 3)*3600,
                 labels = paste0(seq(0, 24, 3), ":00")) +
    labs(
        color = "Weekday", fill = "Weekday",
        x = "Hour", y = "Glucose level (mg/dl)"
    ) +
    theme(legend.position = "top")


# comparison
ggplot(dt, aes(x = hms::as_hms(datetime), y = bg_sensor, group = date(datetime))) +
    # geom_ribbon(aes(ymin = 90, ymax = 140), fill = "#0db77e", alpha = .01) +
    geom_hline(yintercept = c(80, 220), linetype = "longdash", color = "red") +
    geom_line(aes(color = wday), linewidth = .3, alpha = .2) +
    geom_smooth(aes(group = wday, color = wday, fill = wday), se = TRUE, method = "gam") +
    scale_y_continuous(breaks = seq(60, 380, 80),
                       minor_breaks = seq(60, 380, 40)) +
    scale_x_time(breaks = seq(0, 24, 3)*3600,
                 labels = paste0(seq(0, 24, 3), ":00")) +
    labs(
        color = "Weekday", fill = "Weekday",
        x = "Hour", y = "Glucose level (mg/dl)"
    ) +
    theme(legend.position = "top")


# modeling idee:
# carbs + bolus als prädiktor für bz, bzw die ratio aus beidem
ts_gluco <- as_tsibble(gluco, index = datetime)
ts_bolus <- as_tsibble(bolus, index = datetime)
