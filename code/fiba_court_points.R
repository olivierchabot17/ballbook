# Offical FIBA court dimensions
# https://www.fiba.basketball/documents/official-basketball-rules/2020.pdf
# https://www.fiba.basketball/documents/BasketballEquipment.pdf
# All lengths will be in meters

# sf package vignette
# https://cran.r-project.org/web/packages/sf/vignettes/sf1.html

# # Setting the working directory
# directory <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(directory)

#source("court_themes.R")

# Load the libraries
library(sf)
library(tidyverse)

line_thick = 0.05
width = 15
height = 28 / 2
key_height = 5.8
key_width = 4.9
key_radius = 1.8
backboard_width = 1.8
backboard_thick = 0.1
backboard_offset = 1.2
hoop_radius = 0.45 / 2
hoop_center_y = 1.575
rim_thick = 0.02
neck_length = hoop_center_y - (backboard_offset + hoop_radius + rim_thick)
three_point_radius = 6.75
three_point_side_offset = 0.9
three_point_side_height = sqrt(three_point_radius^2 - (three_point_side_offset - width/2)^2) + hoop_center_y
restricted_area_radius = 1.25


# Always will go around the polygons clockwise starting at bottom left
# Draw a rectangle that defines the half-court 
half_court_int <- rbind(
  c(0, 0),
  c(0, height),
  c(width, height),
  c(width, 0),
  c(0,0)
  )

half_court_ext <- rbind(
  c(0-line_thick, 0-line_thick),
  c(0-line_thick, height + line_thick),
  c(width + line_thick, height + line_thick),
  c(width + line_thick, 0-line_thick),
  c(0-line_thick, 0-line_thick)
  )

half_court <- st_polygon(list(half_court_ext, half_court_int))

# Draw a rectangle for the key
key_int <- rbind(
  c(width/2 - key_width/2 + line_thick, 0),
  c(width/2 - key_width/2 + line_thick, key_height - line_thick),
  c(width/2 + key_width/2 - line_thick, key_height - line_thick),
  c(width/2 + key_width/2 - line_thick, 0),
  c(width/2 - key_width/2 + line_thick, 0)
)

key_ext <- rbind(
  c(width/2 - key_width/2, 0),
  c(width/2 - key_width/2, key_height),
  c(width/2 + key_width/2, key_height),
  c(width/2 + key_width/2, 0),
  c(width/2 - key_width/2, 0)
)

key <- st_polygon(list(key_ext, key_int))

# Draw a rectangle for the backboard
backboard_points <- rbind(
  c(width/2 - backboard_width/2, backboard_offset - backboard_thick),
  c(width/2 - backboard_width/2, backboard_offset),
  c(width/2 + backboard_width/2, backboard_offset),
  c(width/2 + backboard_width/2, backboard_offset - backboard_thick),
  c(width/2 - backboard_width/2, backboard_offset - backboard_thick)
)

backboard <- st_polygon(list(backboard_points))

# Neck
neck_points <- rbind(
  c(width/2 - line_thick/2, backboard_offset),
  c(width/2 - line_thick/2, backboard_offset + neck_length),
  c(width/2 + line_thick/2, backboard_offset + neck_length),
  c(width/2 + line_thick/2, backboard_offset),
  c(width/2 - line_thick/2, backboard_offset)
)

neck <- st_polygon(list(neck_points))

# Draw hoop
hoop_center <- st_point(c(width/2, hoop_center_y))

hoop_int <- hoop_center %>%
  st_buffer(dist = hoop_radius)

hoop_ext <- hoop_center %>%
  st_buffer(dist = hoop_radius + rim_thick)

hoop <- st_polygon(list(st_coordinates(hoop_ext)[ , 1:2], st_coordinates(hoop_int)[ , 1:2]))

# Draw the half-circle at the top of the key
key_center <- st_point(c(width/2, key_height))

key_circle_int <- st_crop(
  st_sfc(st_buffer(key_center, dist = key_radius - line_thick)),
  xmin = 0, ymin = key_height, xmax = width, ymax = height
  )

key_circle_ext <- st_crop(
  st_sfc(st_buffer(key_center, dist = key_radius)),
  xmin = 0, ymin = key_height, xmax = width, ymax = height
)

key_circle <- st_polygon(list(
  st_coordinates(key_circle_ext)[ , 1:2],
  st_coordinates(key_circle_int)[ , 1:2]
  ))

# Draw the half-circle at the bottom of half-court
half_center <- st_point(c(width/2, height))

half_circle_int <- st_crop(
  st_sfc(st_buffer(half_center, dist = key_radius - line_thick)),
  xmin = 0, ymin = 0, xmax = width, ymax = height
)

half_circle_ext <- st_crop(
  st_sfc(st_buffer(half_center, dist = key_radius)),
  xmin = 0, ymin = 0, xmax = width, ymax = height
)

half_circle <- st_polygon(list(
  st_coordinates(half_circle_ext)[ , 1:2],
  st_coordinates(half_circle_int)[ , 1:2]
))

# Draw the three-point arc
three_center <- st_point(c(width/2, hoop_center_y))

three_int <- st_crop(
  st_sfc(st_buffer(three_center, dist = three_point_radius - line_thick)),
  xmin = three_point_side_offset + line_thick, ymin = three_point_side_height,
  xmax = width - (three_point_side_offset + line_thick), ymax = height
)

n <- dim(st_coordinates(three_int))[1]
three_int <- rbind(
  c(three_point_side_offset + line_thick, 0),
  c(three_point_side_offset + line_thick, three_point_side_height),
  st_coordinates(three_int)[1:(n-2), 1:2],
  c(width - (three_point_side_offset + line_thick), three_point_side_height),
  c(width - (three_point_side_offset + line_thick), 0),
  c(three_point_side_offset + line_thick, 0)
)

three_ext <- st_crop(
  st_sfc(st_buffer(three_center, dist = three_point_radius)),
  xmin = three_point_side_offset, ymin = three_point_side_height,
  xmax = width - three_point_side_offset, ymax = height
)

three_ext <- rbind(
  c(three_point_side_offset, 0),
  c(three_point_side_offset, three_point_side_height),
  st_coordinates(three_ext)[1:(n-2), 1:2],
  c(width - three_point_side_offset, three_point_side_height),
  c(width - three_point_side_offset, 0),
  c(three_point_side_offset, 0)
)

three_point_line <- st_polygon(list(three_int, three_ext))

# Restricted area
ra_center <- st_point(c(width/2, hoop_center_y))

ra_ext <- st_crop(
  st_sfc(st_buffer(ra_center, dist = restricted_area_radius + line_thick)),
  xmin = 0, ymin = hoop_center_y,
  xmax = width, ymax = height
)

n <- dim(st_coordinates(ra_ext))[1]

ra_ext <- tibble(
  x = st_coordinates(ra_ext)[1:(n-2), 1],
  y = st_coordinates(ra_ext)[1:(n-2), 2]
)


ra_ext <- rbind(
  c(width/2 - restricted_area_radius - line_thick, backboard_offset),
  c(width/2 - restricted_area_radius - line_thick, hoop_center_y),
  ra_ext,
  c(width/2 + restricted_area_radius + line_thick, hoop_center_y),
  c(width/2 + restricted_area_radius + line_thick, backboard_offset)
)

ra_int <- st_crop(
  st_sfc(st_buffer(ra_center, dist = restricted_area_radius)),
  xmin = 0, ymin = hoop_center_y,
  xmax = width, ymax = height
)

ra_int_flip <- tibble(
  x = st_coordinates(ra_int)[1:(n-2), 1],
  y = st_coordinates(ra_int)[1:(n-2), 2]
) %>%
  arrange(desc(x))

ra_int <- rbind(
  c(width/2 + restricted_area_radius, backboard_offset),
  c(width/2 + restricted_area_radius, hoop_center_y),
  ra_int_flip,
  c(width/2 - restricted_area_radius, hoop_center_y),
  c(width/2 - restricted_area_radius, backboard_offset),
  c(width/2 - restricted_area_radius - line_thick, backboard_offset)
)

restricted_area <- st_polygon(list(as.matrix(rbind(ra_ext, ra_int))))

###########################
court_sf <- st_sf(
  description = c("half_court", "key", "hoop", "backboard",
                  "neck", "key_circle", "three_point_line",
                  "half_circle", "restricted_area"), 
  geom = c(st_geometry(half_court), st_geometry(key), st_geometry(hoop),
           st_geometry(backboard), st_geometry(neck), st_geometry(key_circle), 
           st_geometry(three_point_line), st_geometry(half_circle),
           st_geometry(restricted_area))
)

plot_court = function(court_theme = court_themes$light) {
  ggplot() +
    geom_sf(data = court_sf, fill = court_theme$lines, col = court_theme$lines) +
    theme_void() +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

########## plot_court_df
court_coords <- as_tibble(st_coordinates(court_sf))

plot_court_df = function(court_theme = court_themes$light) {
  ggplot() +
    geom_polygon(data = court_coords, aes(x = X, y = Y, group = L2),
                 fill = court_theme$lines, col = court_theme$lines) +
    theme_void() +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

#plot_court_df()
##################################

# # Remove objects from working directory
# rm(
#   half_court_int, half_court_ext, half_court,
#   key, key_circle, key_circle_ext, key_circle_int, key_int, key_center,
#   hoop, hoop_ext, hoop_int,
#   backboard, backboard_points,
#   neck, neck_points,
#   three_center, three_ext, three_int, three_point_line,
#   half_circle, half_circle_int, half_circle_ext, half_center,
#   restricted_area, ra_center, ra_ext, ra_int, ra_int_flip,
#   n
# )
# 
# rm(
#   line_thick, height, key_height, key_width, key_radius, backboard_width,
#   backboard_thick, backboard_offset, hoop_radius, rim_thick,
#   neck_length, three_point_radius, three_point_side_offset, 
#   three_point_side_height, restricted_area_radius
# )

