library(tidyverse)

theme_set(hrbrthemes::theme_ipsum_rc())

geom_col   <- purrr::partial(geom_col, color = "white")
geom_bar   <- purrr::partial(geom_bar, color = "white")
# geom_point <- purrr::partial(geom_point, alpha = .5)
geom_histogram  <- purrr::partial(geom_histogram, color = "white", bins = 21)
scale_colorfill <- purrr::partial(scale_color_brewer,
                                  palette = "Dark2",
                                  aesthetics = c("color", "fill"))
s