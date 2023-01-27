library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)

theme_set(hrbrthemes::theme_ipsum_rc())
# theme_set(hrbrthemes::theme_modern_rc())

update_geom_defaults("point", list(colour = "#3c4c72"))
update_geom_defaults("line", list(colour = "#3c4c72"))
update_geom_defaults("area", list(colour = "#f0f0f0", 
                                  fill   = "#3c4c72"))
update_geom_defaults("rect", list(colour = "#f0f0f0", 
                                  fill   = "#3c4c72"))
update_geom_defaults("density", list(colour = "#f0f0f0", 
                                     fill   = "#3c4c72"))
update_geom_defaults("bar", list(colour = "white", 
                                 fill   = "#3c4c72"))
update_geom_defaults("col", list(colour = "#f0f0f0", 
                                 fill   = "#3c4c72"))
update_geom_defaults("text", list(colour = "#3c4c72"))

