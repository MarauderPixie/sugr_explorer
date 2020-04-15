library(jsonlite)
library(lubridate)
# better be explicit than:
# pluck <- purrr::partial(pluck, .default = NA)

cgm_json <- read_json("data_raw/2020_03.json")

bolus   <- cgm_json[["data"]][["current"]][["data"]][["bolus"]]
cont_gm <- cgm_json[["data"]][["current"]][["data"]][["cbg"]]
self_gm <- cgm_json[["data"]][["current"]][["data"]][["smbg"]]
wizard  <- cgm_json[["data"]][["current"]][["data"]][["wizard"]]

tidy_bolus <- map_df(seq_along(bolus), function(i){
    tibble(
        datetime = pluck(bolus, i, "_deviceTime", .default = NA),
        type     = pluck(bolus, i, "type", .default = NA),
        subtype  = pluck(bolus, i, "subType", .default = NA),
        normal   = pluck(bolus, i, "normal", .default = NA),
    )
}) %>% 
    mutate(
        datetime = as_datetime(datetime)
    )

tidy_cgm <- map_df(seq_along(cont_gm), function(i){
    tibble(
        datetime = pluck(cont_gm, i, "_deviceTime", .default = NA),
        type     = pluck(cont_gm, i, "type", .default = NA),
        glucose  = pluck(cont_gm, i, "value", .default = NA),
        units    = pluck(cont_gm, i, "units", .default = NA)
    )
}) %>% 
    mutate(
        datetime = as_datetime(datetime)
    )

tidy_sgm <- map_df(seq_along(self_gm), function(i){
    tibble(
        datetime = pluck(self_gm, i, "_deviceTime", .default = NA),
        type     = pluck(self_gm, i, "type", .default = NA),
        glucose  = pluck(self_gm, i, "value", .default = NA),
        units    = pluck(self_gm, i, "units", .default = NA)
    )
}) %>% 
    mutate(
        datetime = as_datetime(datetime)
    )

tidy_wiz <- map_df(seq_along(wizard), function(i){
    tibble(
        datetime = pluck(wizard, i, "_deviceTime", .default = NA),
        type     = pluck(wizard, i, "type", .default = NA),
        carbs    = pluck(wizard, i, "carbInput", .default = NA),
        carbs_ratio = pluck(wizard, i, "insulinCarbRatio", .default = NA),
        carbs_lingering = pluck(wizard, i, "insulinOnBoard", .default = NA),
        insulin_actual  = pluck(wizard, i, "bolus", "normal", .default = NA),
    )
}) %>% 
    mutate(
        datetime = as_datetime(datetime),
        insulin_recomm = carbs * carbs_ratio
    )

rm(bolus, cgm_json, cont_gm, self_gm, wizard)
