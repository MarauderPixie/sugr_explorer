library(jsonlite)
library(lubridate)
# pluck <- purrr::partial(pluck, .default = NA)

cgm_json <- read_json("data_raw/2020_02.json")

# [-c(1, 2)] bc 1st & 2nd entry is device information & settings
fin <- map_df(seq_along(cgm_json[-c(1, 2)]), function(i){
  tibble(
    i        = i,
    # look into timezoneOffset!!
    datetime = pluck(cgm_json, i, "time", .default = NA), # %>% lubridate::as_datetime(),
    event    = pluck(cgm_json, i, "type", .default = NA),
    # units    = pluck(cgm_json, i, "units"),
    normal   = pluck(cgm_json, i, "normal", .default = NA),
    value    = pluck(cgm_json, i, "value", .default = NA),
    bg_input = pluck(cgm_json, i, "bgInput", .default = NA),
    wiz_carb = pluck(cgm_json, i, "recommended", 1, .default = NA),
    wiz_corr = pluck(cgm_json, i, "recommended", 2, .default = NA),
    wiz_net  = pluck(cgm_json, i, "recommended", 3, .default = NA),
    tmp_type = pluck(cgm_json, i, "deliveryType", .default = NA),
    tmp_dur  = pluck(cgm_json, i, "duration", .default = NA),
    tmp_rate = pluck(cgm_json, i, "rate", .default = NA),
    tmp_perc = pluck(cgm_json, i, "percent", .default = NA),
    tmp_status = pluck(cgm_json, i, "status", .default = NA)
  )
}) %>%
  mutate(
    datetime = as_datetime(datetime),
    date = as_date(datetime),
    time = paste0(hour(datetime), ":", minute(datetime)) %>% hm(),
    weekday = wday(datetime, label = TRUE, week_start = 1, abbr = F),
    value = value / .0555
  )

cgm   <- filter(fin, event == "cbg")
bolus <- filter(fin, event == "bolus")
basal <- filter(fin, event == "basal")

saveRDS(cgm, "data_done/cgm.rds")
saveRDS(bolus, "data_done/bolus.rds")
saveRDS(basal, "data_done/basal.rds")

rm(cgm_json, fin, cgm, bolus, basal)

## DIAGNOSTICS!
diag %>%
  filter(event_type == "bolus") %>%
  sample_n(5)
