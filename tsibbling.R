library(tsibble)
library(feasts)
library(fable)

# first, get a somewhat cleaner / more useful cgm dataset
ts_df <- cgm %>% 
  mutate(
    # get hour variable to...
    hour = hour(datetime),
    # ...get a single index-variable of day & hour
    ymdh = ymd_h(paste(date, hour))
  ) %>% 
  select(ymdh, value) %>% 
  group_by(ymdh) %>% 
  summarise(
    bg_m = mean(value),
    bg_sd = sd(value)
  ) %>% 
  ungroup()


## coerce to subdaily-indexed tsibble object
tsb <- as_tsibble(ts_df) %>% 
  fill_gaps()


## model away! ... /o/
tsb %>% 
  model(
    # ets = ETS(box_cox(bg_m, 0.3)),
    arima = ARIMA(log(bg_m))
    # snaive = SNAIVE(bg_m)
  ) %>%
  forecast(h = "48 hours") %>% 
  autoplot(filter(tsb, day(ymdh) > 16, month(ymdh) == 2), level = NULL)
