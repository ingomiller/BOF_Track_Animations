
# devtools::install_github("16EAGLE/moveVis", force = TRUE, build_vignettes = TRUE)

library(move)
library(moveVis)
library(tidyverse)
library(terra)
library(magick)
library(grid)  # Required for rasterGrob
library(cowplot)


sf <- terra::vect("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/Flatback-Turtles/0_AKDE_06-2022/turtles.all_filtered.proj.shp")
sf <- terra::vect("C:/Users/jc563815/OneDrive - James Cook University/Flatback-Turtles/0_AKDE_06-2022/turtles.all_filtered.proj.shp")

sf <- terra::project(sf, "EPSG:4326")
plot(sf)
summary(sf)
head(sf)
sf

coordinates <- terra::crds(sf)


df <- sf |>
  as.data.frame() |>
  dplyr::mutate(lon = coordinates[,1],
                lat = coordinates[,2],
                DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")) |>
  dplyr::select(id, DateTime, tag_loc, lon, lat)


str(df)

head(df)



## unify timesatmps 

# Define unified Start_date 

uni_start_date <- lubridate::ymd_hms("1970-01-01 12:00:00")


df2 <- df |>
  dplyr::group_by(id) |>
  dplyr::mutate(time_diff = as.numeric(difftime(DateTime, min(DateTime), units = "secs")),
                uni_timestamp = uni_start_date + seconds(time_diff)) |>
  ungroup() |>
  arrange() |>
  dplyr::select(-time_diff)
                  
str(df2)


unique(is.na(df2$uni_timestamp))



# quick map
flat_sf <- df2 |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs= 4326, remove = F) 

str(flat_sf)

# # Define a custom color palette excluding blues and greens
# custom_palette <- c("red", "blue")

flat_sf |> mapview::mapview(zcol = "tag_loc", legend = TRUE)



# use df2move to convert the data.frame into a moveStack
tracks <- moveVis::df2move(df2,
                           proj = "+init=epsg:4326", 
                           x = "lon", y = "lat", time = "uni_timestamp", track_id = "id")

str(tracks)
head(tracks)

# align move_data to a uniform time scale
m <- moveVis::align_move(tracks, res = 'mean', unit = "days")
m <- moveVis::align_move(tracks, res = 2, unit = "days")
head(m)

summary(m@timestamps)
summary(m@data$x)
summary(m@data$y)




str(df2)

m.df <- as.data.frame(m) |>
  dplyr::mutate(id = sub("^X", "", trackId),
                id = as.integer(id))
str(m.df)



# Initialize the `tag_loc` column with NA values
m.df$tag_loc <- NA

# Map the `tag_loc` information from input_df to move_data
for (i in seq_len(nrow(df2))) {
  id <- df2$id[i]
  tag_loc_value <- df2$tag_loc[i]
  m.df$tag_loc[m.df$id == id] <- tag_loc_value
}

unique(m.df$tag_loc)

# Assign colors based on the `tag_loc` values
color_palette <- c(
  "Eimeo" = "#FF0000", # Red
  "Halliday Bay" = "#0000FF", # Blue
  "Peak Island" = "#00FF00", # Green
  "Curtis Island" = "#FFFF00", # Yellow
  "Blacks Beach" = "#FFA500", # Orange
  "Ball Bay" = "#800080", # Purple
  "Wunjunga" = "#00FFFF", # Cyan
  "Mon Repos" = "#FF00FF"  # Magenta
)


m.df$colour <- color_palette[m.df$tag_loc]


# Check for any NA values in the `color` column
if (any(is.na(m.df$colour))) {
  stop("There are NA values in the `color` column. Please check the `tag_loc` mapping.")
}


str(m.df)
unique(m.df$colour)
unique(m.df$tag_loc)

# Update the MoveStack object with the new column
m@data$tag_loc <- m.df$tag_loc
m$colour <- m.df$colour

# m$colour <- "red"


head(m)
str(m)

m@data
m@coords

# create spatial frames


# get_maptypes()

frames <- moveVis::frames_spatial(m,
                                  map_service = "esri", 
                                  map_type = "world_imagery", 
                                  alpha = 1, 
                                  path_legend = FALSE, 
                                  path_legend_title = "Flatback\nID", 
                                  path_size = 3, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.8) %>% 
  add_labels(title = "Flatback turtle tracks",
             caption = "Note: Dates unified to common year",
             x = "Longitude", 
             y = "Latitude",) %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white", x=153.5, y=-25.5) %>% 
  #add_scalebar(colour = "white") %>% 
  #add_timestamps(type = "label") %>% 
  add_progress()





frames[[100]] # preview one of the frames, e.g. the 100th frame

# add logo:
logo_path <- "Biopixel.OceansFoundation.Logo.Final.png"  # Replace with the actual path to your logo image
logo <- image_read(logo_path)


logo_grob <- rasterGrob(image = as.raster(logo), interpolate = TRUE)



frames2 <- frames |>
  add_gg(gg = expr(annotation_custom(logo_grob, xmin = 141.5, xmax = 146, ymin = -27, ymax = -23))) |>
  add_scalebar(colour = "white", distance = 250, units = "km", x=147, y=-25.5)


frames2[[50]]

# animate frames

animate_frames(frames2, out_file = "Flatback_Turtles_Animation_Region.col_unidate.mov", end_pause = 0, overwrite=TRUE, res=120, fps = 35)







library(moveVis)
library(move)

data("move_data", package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

head(move_data)

# align move_data to a uniform time scale
m <- align_move(move_data, res = 4, unit = "mins")
head(m)

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, 
                         #path_colours = c("red", "green", "blue"),
                         map_service = "esri", map_type = "world_imagery", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
animate_frames(frames, out_file = "moveVis.gif")



