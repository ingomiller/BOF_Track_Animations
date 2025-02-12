
# devtools::install_github("16EAGLE/moveVis", force = TRUE, build_vignettes = TRUE)

library(tidyverse)
library(aniMotum)
library(moveVis)
library(move)
library(move2)
library(sf)
library(terra)
library(readxl)


## Data Import 


# meta data 

METAFILE <- read_excel("C:/Users/jc563815/OneDrive - James Cook University/02_PhD/03_DATA/Tagging_DATA_SHEETS/DATASHEETS_Review/Tagging_Catch_Tissue_MASTERFILE.xlsx", sheet = "Elasmo_CATCH_Master")

str(METAFILE)



## Norfolk Island 
data_folder <- "~/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/10_Coding_Work/Track_Animations/Raw_data_Tigers/Norfolk"
data_folder <- "C:/Users/jc563815/OneDrive - James Cook University/02_PhD/10_Coding_Work/Track_Animations/Raw_data_Tigers/Norfolk"


file_pattern <- ".*Locations.csv"

loc_files <- list.files(path = data_folder, pattern = file_pattern, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

#chnage the weird Argos date format:
NFI <- loc_files |>
  lapply(read_csv) |>
  bind_rows()
str(NFI)

NFI_raw <- NFI

# fix date 
NFI <- NFI |>
  dplyr::mutate(Date = as.POSIXct(Date, format = "%H:%M:%S %d-%b-%Y", tz="UTC"),
                Loc = "Norfolk",
                id  = as.character(Ptt),
                #Longitude_360 = if_else(Longitude < 0, Longitude + 360, Longitude)
                )


str(NFI)


meta <- readxl::read_excel("Raw_data_Tigers/Norfolk Island metadata_combined.xlsx")
str(meta)


meta <- meta |>
  dplyr::rename(id = `Sat tag`,
                Tag_date = Date,
                Name = Names) |>
  dplyr::distinct(id, .keep_all = TRUE) |>
  dplyr::mutate(tag_loc = "Norfolk Is.",
                id = as.character(id)) |>
  dplyr::select(id, Tag_date, Name, Sex, TL, tag_loc)



# delete rows before tagging date:
NFI$Date2 <- as.Date(NFI$Date, format = "%Y-%m-%d")  # Adjust the date format if necessary
meta$Date <- as.Date(meta$Tag_date, format = "%Y-%m-%d")  # Adjust the date format if necessary


str(meta)
str(NFI)



# Merge Tag_date from meta into WI based on id
NFI <- dplyr::left_join(NFI, meta |>
                          dplyr::select(id, Tag_date), by = "id")

# Filter WI where Date2 is on or after Tag_date
NFI <- NFI |>
  dplyr::filter(Date2 >= Tag_date) |>
  dplyr::select(-Tag_date)







# Filter the main data frame based on the specific date for each id
NFI <- NFI |>
  group_by(id) |>
  filter(Date >= meta$Tag_date[match(id, meta$id)]) |>
  dplyr::select(-Date2) |>
  ungroup()

str(NFI)




# transform data to animotum compatable format:
NFI <- aniMotum::format_data(NFI,
                             id = "id",
                             date = "Date",
                             lc = "Quality",
                             coord = c("Longitude", "Latitude"),
                             epar = c("Error Semi-major axis", "Error Semi-minor axis", "Error Ellipse orientation"),
                             sderr = c("x.sd", "y.sd"),
                             tz = "UTC")



NFI <- NFI |>
  dplyr::filter(!id %in% c("209117", "209123")) |>
  dplyr::filter(lc != "Z")


unique(NFI$id)

str(NFI)

# inspect data 


meta_data <- NFI |> 
  dplyr::group_by(id, Loc) |> 
  dplyr::summarise(minDate = as.Date(min(date)),
                   maxDate = as.Date(max(date)),
                   N_Locations = n(),
                   DAL = maxDate - minDate) |>
  dplyr::arrange(minDate)
meta_data





fit_NFI <- aniMotum::fit_ssm(NFI, 
                         vmax= 5, #max travelling speed of 100 km/day 
                         model = "crw", 
                         time.step = 24, 
                         #map = list(psi = factor(NA)),
                         #control = ssm_control(verbose = 1, se = FALSE),
                         pf=FALSE) #just pre-filter the data, does not fit the SSM


summary(fit_NFI)

fit.r <- aniMotum::route_path(fit, what = "fitted", map_scale = 10, dist = 1000, append = TRUE) 

my.aes <- aes_lst(obs=FALSE, line=TRUE, mp=FALSE, conf = FALSE)

aniMotum::map(fit.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=FALSE, by.id=TRUE)
aniMotum::map(fit.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=TRUE, by.id=FALSE)


NFI_ssm <- aniMotum::grab(fit.r, what = "rerouted")

str(NFI_ssm)

unique(NFI_ssm$id)
min(NFI_ssm$lon)
max(NFI_ssm$lon)


# dealing with dateline issue:

# Convert the data frame to an sf object with the original CRS (EPSG:4326)
NFI_ssm_sf <- sf::st_as_sf(NFI_ssm, coords = c("lon", "lat"), crs = 4326)

# # Define the new CRS with 0-360 longitude range
# new_crs <- "+proj=longlat +lon_wrap=180 +datum=WGS84"
# 
# # Transform the coordinates to the new CRS
# NFI_ssm_sf <- sf::st_transform(NFI_ssm_sf, crs = new_crs)


NFI_ssm_sf |> mapview::mapview()


str(NFI_ssm_sf)
st_crs(NFI_ssm_sf)

# # Extract transformed coordinates to check the result
# coords_360 <- sf::st_coordinates(NFI_ssm_sf)
# print(coords_360)
# 
# # Add the transformed coordinates back to the data frame
# NFI_ssm_360 <- NFI_ssm_sf %>%
#   st_drop_geometry() %>%
#   dplyr::mutate(lon = coords_360[, 1],
#                 lat = coords_360[, 2])
# 
# 
# NFI_ssm <- NFI_ssm_360


####



## unify timesatmps 

# Define unified Start_date 

uni_start_date <- lubridate::ymd_hms("1970-01-01 12:00:00")

NFI_ssm <- NFI_ssm |>
  dplyr::mutate(tag_loc = "Norfolk Is.")

str(NFI_ssm)
NFI.df <- NFI_ssm |>
  dplyr::group_by(id) |>
  dplyr::mutate(time_diff = as.numeric(difftime(date, min(date), units = "secs")),
                uni_timestamp = uni_start_date + seconds(time_diff)) |>
  ungroup() |>
  dplyr::select(-time_diff)


NFI.df
min(NFI.df$lon)
max(NFI.df$lon)

unique(is.na(NFI.df$uni_timestamp))

# use df2move to convert the data.frame into a moveStack
NFI_tracks <- moveVis::df2move(NFI.df,
                           #proj = "+init=epsg:4326", 
                           #proj = "+proj=longlat +lon_wrap=180 +datum=WGS84",
                           proj = 4326,
                           x = "lon", y = "lat", time = "uni_timestamp", track_id = "id")

str(NFI_tracks)
extent(NFI_tracks)

NFI_tracks@bbox
min(NFI_tracks@data$x)

# # Define the custom CRS (using a projection that handles longitudes from 0 to 360 degrees)
# custom_crs <- "+proj=longlat +datum=WGS84 +pm=180"
# 
# # Transform to the custom CRS
# NFI_tracks_360 <- st_transform(NFI_tracks, crs = custom_crs)
# 
# 



# align move_data to a uniform time scale
m <- moveVis::align_move(NFI_tracks, res = 'mean', unit = "days")

str(m)


projection(m)
# ## [1] "+proj=longlat +datum=WGS84 +no_defs"
# m <- spTransform(m, CRSobj="+proj=longlat +lon_wrap=180 +datum=WGS84")
# projection(m)



ext <- extent(m)
ext
# ext@xmin <- ext@xmin - (ext@xmin*0.003)
# ext@xmax <- ext@xmax + (ext@xmax*0.003)
# ext@xmin <- 144
# ext@xmax <- 190
# ext

# m@proj4string
# m@bbox <- NFI_tracks@bbox
# m@bbox
# extent(m)
# 
# extent(m) <- extent(NFI_tracks)
# crs(m)
summary(m)
bbox(m)

#m@data <- NFI_tracks@data

min(m@data$x)

m$colour <- "red"



# create spatial frames with a OpenStreetMap watercolour map


get_maptypes()



frames <- moveVis::frames_spatial(m,
                                  map_service = "esri", 
                                  map_type = "world_imagery", 
                                  alpha = 1, 
                                  path_legend = FALSE, 
                                  #path_legend_title = "Tiger shark\nID", 
                                  path_size = 3, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.8,
                                  cross_dateline = TRUE) 
# %>% 
  # add_labels(#title = "Tiger shark tracks",
  #            #subtitle = "Norfolk Island",
  #            #caption = "Note: Dates unified to common year",
  #            x = "Longitude", 
  #            y = "Latitude",) %>% # add some customizations, such as axis labels
  # add_northarrow(colour = "white", x=153.5, y=-25.5) %>% 
  # #add_scalebar(colour = "white") %>% 
  # #add_timestamps(type = "label") %>% 
  # add_progress()




summary(m@timestamps)
summary(m@data$x)
min(m@data$x)
summary(m@data$y)

m@bbox


# Extract the current bounding box
current_bbox <- m@bbox

# Define your custom bounding box coordinates
custom_bbox <- matrix(c(144, -36.87367, 185, -14.88334), ncol = 2, byrow = TRUE)
dimnames(custom_bbox) <- dimnames(current_bbox)

# Assign the custom bounding box to your MoveStack object
m@bbox <- custom_bbox
m@bbox

m@proj4string
crs(m)

st_crs(m)


frames <- moveVis::frames_spatial(m,
                                  # map_service = "carto", 
                                  # map_type = "light", 
                                  alpha = 1, 
                                  path_legend = FALSE, 
                                  path_size = 3, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.8,
                                  cross_dateline = TRUE) |>
  add_gg(gg = expr(theme(aspect.ratio = 0.5))) # stretching the y axis a bit



frames[[300]] 

# animate frames


animate_frames(frames, out_file = "Flatback_Turtles_Animation_date.mov", end_pause = 0, overwrite=T, res=100, fps = 25)




  



#--------------------------------------------------------
#--------------------------------------------------------

## Whitsunday Islands

data_folder <- "~/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/10_Coding_Work/Track_Animations/Raw_data_Tigers/Whitsundays"
data_folder <- "C:/Users/jc563815/OneDrive - James Cook University/02_PhD/10_Coding_Work/Track_Animations/Raw_data_Tigers/Whitsundays"


file_pattern <- ".*Locations.csv"

loc_files <- list.files(path = data_folder, pattern = file_pattern, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

#chnage the weird Argos date format:
WI <- loc_files |>
  lapply(read_csv) |>
  bind_rows()
str(WI)

WI_raw <- WI

# fix date 
WI <- WI |>
  dplyr::mutate(Date = as.POSIXct(Date, format = "%H:%M:%S %d-%b-%Y", tz="UTC"),
                Loc = "Whitsundays",
                id  = as.character(Ptt)) |>
  dplyr::filter(Quality != "Z")


str(WI)
unique(WI$id)


str(METAFILE)




meta <- METAFILE |>
  dplyr::filter(Common_Name == "Tiger_shark" & Sat_Tag == "TRUE" & Region == "Whitsundays") |>
  dplyr::distinct(Sat_PTT, .keep_all = TRUE) |>
  dplyr::rename(id = Sat_PTT,
                Tag_date = Date,
                Name = Name) |>
  dplyr::mutate(tag_loc = "Whitsundays",
                id = as.character(id)) |>
  dplyr::select(id, Tag_date, Name, Sex, TL_cm, tag_loc)

str(meta)
str(WI)


unique(WI$id)
unique(meta$id)


# delete rows before tagging date:
WI$Date2 <- as.Date(WI$Date, format = "%Y-%m-%d")  # Adjust the date format if necessary
meta$Date <- as.Date(meta$Tag_date, format = "%Y-%m-%d")  # Adjust the date format if necessary





# Merge Tag_date from meta into WI based on id
WI <- dplyr::left_join(WI, meta |>
                         dplyr::select(id, Tag_date), by = "id")

# Filter WI where Date2 is on or after Tag_date
WI <- WI |>
  dplyr::filter(Date2 >= Tag_date) |>
  dplyr::select(-Tag_date)


str(WI)

# transform data to animotum compatable format:
WI <- aniMotum::format_data(WI,
                             id = "id",
                             date = "Date",
                             lc = "Quality",
                             coord = c("Longitude", "Latitude"),
                             epar = c("Error Semi-major axis", "Error Semi-minor axis", "Error Ellipse orientation"),
                             sderr = c("x.sd", "y.sd"),
                             tz = "UTC")





str(WI)

# inspect data 


meta_data <- WI |> 
  dplyr::group_by(id, Loc) |> 
  dplyr::summarise(minDate = as.Date(min(date)),
                   maxDate = as.Date(max(date)),
                   N_Locations = n(),
                   DAL = maxDate - minDate) |>
  dplyr::arrange(minDate)
meta_data


unique(WI$lc)


fit_WI <- aniMotum::fit_ssm(WI, 
                         vmax= 5, #max travelling speed of 100 km/day 
                         model = "crw", 
                         time.step = 24, 
                         map = list(psi = factor(NA)),
                         control = ssm_control(verbose = 1, se = FALSE),
                         pf=FALSE) #just pre-filter the data, does not fit the SSM


summary(fit_WI)

fit_WI.r <- aniMotum::route_path(fit_WI, what = "fitted", map_scale = 10, dist = 1000, append = TRUE) 

my.aes <- aes_lst(obs=FALSE, line=TRUE, mp=FALSE, conf = FALSE)

aniMotum::map(fit_WI.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=FALSE, by.id=TRUE)
aniMotum::map(fit_WI.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=TRUE, by.id=FALSE)


WI_ssm <- aniMotum::grab(fit_WI.r, what = "rerouted")

WI_ssm <- WI_ssm |>
  dplyr::mutate(tag_loc = "Whitsundays")

str(WI_ssm)

unique(WI_ssm$id)
min(WI_ssm$lon)
max(WI_ssm$lon)


# dealing with dateline issue:

# Convert the data frame to an sf object with the original CRS (EPSG:4326)
WI_ssm_sf <- sf::st_as_sf(WI_ssm, coords = c("lon", "lat"), crs = 4326)

# # Define the new CRS with 0-360 longitude range
# new_crs <- "+proj=longlat +lon_wrap=180 +datum=WGS84"
# 
# # Transform the coordinates to the new CRS
# NFI_ssm_sf <- sf::st_transform(NFI_ssm_sf, crs = new_crs)

str(WI_ssm_sf)

WI_ssm_sf |> 
  #dplyr::filter(id == "178942") |>
  mapview::mapview(zcol = "id", legend = TRUE)


str(WI_ssm_sf)
sf::st_crs(WI_ssm_sf)



####



## unify timesatmps 

# Define unified Start_date 

uni_start_date <- lubridate::ymd_hms("1970-01-01 12:00:00")



str(WI_ssm)
WI.df <- WI_ssm |>
  dplyr::group_by(id) |>
  dplyr::mutate(time_diff = as.numeric(difftime(date, min(date), units = "secs")),
                uni_timestamp = uni_start_date + seconds(time_diff)) |>
  ungroup() |>
  dplyr::select(-time_diff)


WI.df
min(WI.df$lon)
max(WI.df$lon)

unique(is.na(WI.df$uni_timestamp))

# use df2move to convert the data.frame into a moveStack
WI_tracks <- moveVis::df2move(WI.df,
                               #proj = "+init=epsg:4326", 
                               #proj = "+proj=longlat +lon_wrap=180 +datum=WGS84",
                               proj = 4326,
                               x = "lon", y = "lat", time = "uni_timestamp", track_id = "id")

str(WI_tracks)
extent(WI_tracks)

WI_tracks@bbox
min(WI_tracks@data$x)
unique(WI_tracks@trackId)


# align move_data to a uniform time scale
m <- moveVis::align_move(WI_tracks, res = 1, unit = "days")

head(m)
str(m)
as.data.frame(m)


projection(m)
# ## [1] "+proj=longlat +datum=WGS84 +no_defs"
# m <- spTransform(m, CRSobj="+proj=longlat +lon_wrap=180 +datum=WGS84")
# projection(m)



ext <- extent(m)
ext
# ext@xmin <- ext@xmin - (ext@xmin*0.003)
# ext@xmax <- ext@xmax + (ext@xmax*0.003)
# ext@xmin <- 144
# ext@xmax <- 190
# ext

# m@proj4string
# m@bbox <- NFI_tracks@bbox
# m@bbox
# extent(m)
# 
# extent(m) <- extent(NFI_tracks)
# crs(m)
summary(m)
bbox(m)

m@data <- WI_tracks@data

min(m@data$x)

m$colour <- "red"



# create spatial frames with a OpenStreetMap watercolour map


get_maptypes()


m@data
m@coords

frames <- moveVis::frames_spatial(m,
                                  map_service = "esri", 
                                  map_type = "world_imagery", 
                                  alpha = 1, 
                                  path_legend = TRUE, 
                                  path_legend_title = "Tiger shark\nID", 
                                  path_size = 3, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.8,
                                  cross_dateline = TRUE) 
# %>% 
# add_labels(#title = "Tiger shark tracks",
#            #subtitle = "Norfolk Island",
#            #caption = "Note: Dates unified to common year",
#            x = "Longitude", 
#            y = "Latitude",) %>% # add some customizations, such as axis labels
# add_northarrow(colour = "white", x=153.5, y=-25.5) %>% 
# #add_scalebar(colour = "white") %>% 
# #add_timestamps(type = "label") %>% 
# add_progress()


frames[[100]]

summary(m@timestamps)
summary(m@data$x)
min(m@data$x)
summary(m@data$y)

m@bbox


# Extract the current bounding box
current_bbox <- m@bbox

# Define your custom bounding box coordinates
custom_bbox <- matrix(c(144, -36.87367, 185, -14.88334), ncol = 2, byrow = TRUE)
dimnames(custom_bbox) <- dimnames(current_bbox)

# Assign the custom bounding box to your MoveStack object
m@bbox <- custom_bbox
m@bbox

m@proj4string
crs(m)

st_crs(m)


frames <- moveVis::frames_spatial(m,
                                  # map_service = "carto", 
                                  # map_type = "light", 
                                  alpha = 1, 
                                  path_legend = FALSE, 
                                  path_size = 3, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.8,
                                  cross_dateline = TRUE) |>
  add_gg(gg = expr(theme(aspect.ratio = 0.5))) # stretching the y axis a bit



frames[[300]] 

# animate frames


animate_frames(frames, out_file = "Flatback_Turtles_Animation_date.mov", end_pause = 0, overwrite=T, res=100, fps = 25)

























  
library(ggplot2)
library(ggspatial)
library(gganimate)
library(transformr)



# Convert MoveStack to data frame
movement_data <- as.data.frame(m)

# Create a static plot with ESRI World Imagery basemap
p <- ggplot() +
  annotation_map_tile(type = "esri", service = "world_imagery") +
  geom_path(data = movement_data, aes(x = x, y = y, group = trackId, color = trackId), size = 1) +
  theme_minimal() +
  labs(title = 'Movement Tracks')

# Create an animated plot
p_anim <- p + 
  transition_time(movement_data$time) +
  labs(title = 'Movement Tracks: {frame_time}')

# Render the animation
animate(p_anim, renderer = gifski_renderer("movement_tracks.gif"))




