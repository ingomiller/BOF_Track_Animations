


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
METAFILE <- read_excel("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/03_DATA/Tagging_DATA_SHEETS/DATASHEETS_Review//Tagging_Catch_Tissue_MASTERFILE.xlsx", sheet = "Elasmo_CATCH_Master")

str(METAFILE)




## Tigers
data_folder <- "~/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/10_Coding_Work/Track_Animations/Raw_data_Tigers/NWI"
data_folder <- "C:/Users/jc563815/OneDrive - James Cook University/02_PhD/10_Coding_Work/Track_Animations/Raw_data_Tigers/NWI"


file_pattern <- ".*Locations.csv"

loc_files <- list.files(path = data_folder, pattern = file_pattern, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

#chnage the weird Argos date format:
tigers <- loc_files |>
  #lapply(read_csv) |>
  purrr::map(~ read_csv(.x) %>% mutate(across(all_of("DeployID"), as.character))) |>
  bind_rows() 
str(tigers)



tigers_raw <- tigers

# fix date 
tigers <- tigers |>
  dplyr::mutate(Date = as.POSIXct(Date, format = "%H:%M:%S %d-%b-%Y", tz="UTC"),
                Loc = "NWI",
                id  = as.character(Ptt),
                Species = "Tiger_shark"
                #Longitude_360 = if_else(Longitude < 0, Longitude + 360, Longitude)
  )



str(tigers)



meta_tigers <- METAFILE |>
  dplyr::filter(Common_Name == "Tiger_shark" & Sat_Tag == "TRUE" & Region == "North_West_Island") |>
  dplyr::distinct(Sat_PTT, .keep_all = TRUE) |>
  dplyr::rename(id = Sat_PTT,
                Tag_date = Date,
                Name = Sat_Name,
                Species = Common_Name) |>
  dplyr::mutate(tag_loc = "NWI",
                id = as.character(id)) |>
  dplyr::mutate(Name = case_when(id == '184222' ~ "Colette", TRUE ~ Name )) |>
  dplyr::select(id, Tag_date, Name, Sex, TL_cm, tag_loc, Species)

str(meta_tigers)
str(tigers)


unique(tigers$id)
unique(meta_tigers$id)
unique(meta_tigers$Name)


# delete rows before tagging date:
tigers$Date2 <- as.Date(tigers$Date, format = "%Y-%m-%d")  # Adjust the date format if necessary
meta$Date <- as.Date(meta_tigers$Tag_date, format = "%Y-%m-%d")  # Adjust the date format if necessary



# Merge Tag_date from meta into WI based on id
tigers <- dplyr::left_join(tigers, meta_tigers |>
                          dplyr::select(id, Tag_date), by = "id")

# Filter WI where Date2 is on or after Tag_date
tigers <- tigers |>
  dplyr::filter(Date2 >= Tag_date) |>
  dplyr::select(-Tag_date)




# transform data to animotum compatable format:
tigers <- aniMotum::format_data(tigers,
                             id = "id",
                             date = "Date",
                             lc = "Quality",
                             coord = c("Longitude", "Latitude"),
                             epar = c("Error Semi-major axis", "Error Semi-minor axis", "Error Ellipse orientation"),
                             sderr = c("x.sd", "y.sd"),
                             tz = "UTC")



tigers <- tigers |>
  dplyr::filter(lc != "Z")


unique(tigers$id)

str(tigers)

# inspect data 


meta_data <- tigers |> 
  dplyr::group_by(id, Loc) |> 
  dplyr::summarise(minDate = as.Date(min(date)),
                   maxDate = as.Date(max(date)),
                   N_Locations = n(),
                   DAL = maxDate - minDate) |>
  dplyr::arrange(minDate)
meta_data





fit_tigers <- aniMotum::fit_ssm(tigers, 
                             vmax= 5, #max travelling speed of 100 km/day 
                             model = "crw", 
                             time.step = 24, 
                             #map = list(psi = factor(NA)),
                             #control = ssm_control(verbose = 1, se = FALSE),
                             pf=FALSE) #just pre-filter the data, does not fit the SSM


summary(fit_tigers)

fit.r <- aniMotum::route_path(fit_tigers, what = "fitted", map_scale = 10, dist = 1000, append = TRUE) 

my.aes <- aes_lst(obs=FALSE, line=TRUE, mp=FALSE, conf = FALSE)

aniMotum::map(fit.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=FALSE, by.id=TRUE)
aniMotum::map(fit.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=TRUE, by.id=FALSE)


tigers_ssm <- aniMotum::grab(fit.r, what = "rerouted")

str(tigers_ssm)

unique(tigers_ssm$id)
min(tigers_ssm$lon)
max(tigers_ssm$lon)


tigers_ssm_sf <- sf::st_as_sf(tigers_ssm, coords = c("lon", "lat"), crs = 4326)
tigers_ssm_sf |> mapview::mapview(zcol = "id", legend = TRUE)


####


## Other species
data_folder <- "~/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/10_Coding_Work/Track_Animations/NWI_sharks"
data_folder <- "C:/Users/jc563815/OneDrive - James Cook University/02_PhD/10_Coding_Work/Track_Animations/NWI_sharks"


file_pattern <- ".*Locations.csv"

loc_files <- list.files(path = data_folder, pattern = file_pattern, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

#chnage the weird Argos date format:
sharks <- loc_files |>
  #lapply(read_csv) |>
  purrr::map(~ read_csv(.x) %>% mutate(across(all_of("DeployID"), as.character))) |>
  bind_rows() 
str(sharks)



sharks_raw <- sharks

# fix date 
sharks <- sharks |>
  dplyr::mutate(Date = as.POSIXct(Date, format = "%H:%M:%S %d-%b-%Y", tz="UTC"),
                Loc = "NWI",
                id  = as.character(Ptt),
                #Longitude_360 = if_else(Longitude < 0, Longitude + 360, Longitude)
  )



str(sharks)

unique(METAFILE$Common_Name)

meta_sharks <- METAFILE |>
  dplyr::filter(Common_Name %in% c("Great_hammerhead", "Lemon_shark") & Sat_Tag == "TRUE" & Region == "North_West_Island") |>
  dplyr::distinct(Sat_PTT, .keep_all = TRUE) |>
  dplyr::rename(id = Sat_PTT,
                Tag_date = Date,
                Name = Sat_Name,
                Species = Common_Name) |>
  dplyr::mutate(tag_loc = "NWI",
                id = as.character(id)) |>
  dplyr::select(id, Tag_date, Name, Sex, TL_cm, tag_loc, Species)

str(meta_sharks)
str(sharks)


unique(sharks$id)
unique(meta_sharks$id)
unique(meta_sharks$Name)


# delete rows before tagging date:
sharks$Date2 <- as.Date(sharks$Date, format = "%Y-%m-%d")  # Adjust the date format if necessary
meta$Date <- as.Date(meta$Tag_date, format = "%Y-%m-%d")  # Adjust the date format if necessary



# Merge Tag_date from meta into WI based on id
sharks <- dplyr::left_join(sharks, meta |>
                             dplyr::select(id, Tag_date), by = "id")

# Filter WI where Date2 is on or after Tag_date
sharks <- sharks |>
  dplyr::filter(Date2 >= Tag_date) |>
  dplyr::select(-Tag_date)




# transform data to animotum compatable format:
sharks <- aniMotum::format_data(sharks,
                                id = "id",
                                date = "Date",
                                lc = "Quality",
                                coord = c("Longitude", "Latitude"),
                                epar = c("Error Semi-major axis", "Error Semi-minor axis", "Error Ellipse orientation"),
                                sderr = c("x.sd", "y.sd"),
                                tz = "UTC")



sharks <- sharks |>
  dplyr::filter(lc != "Z")


unique(sharks$id)

str(sharks)

# inspect data 


meta_data <- sharks |> 
  dplyr::group_by(id, Loc) |> 
  dplyr::summarise(minDate = as.Date(min(date)),
                   maxDate = as.Date(max(date)),
                   N_Locations = n(),
                   DAL = maxDate - minDate) |>
  dplyr::arrange(minDate)
meta_data





fit_sharks <- aniMotum::fit_ssm(sharks, 
                                vmax= 5, #max travelling speed of 100 km/day 
                                model = "crw", 
                                time.step = 24, 
                                #map = list(psi = factor(NA)),
                                #control = ssm_control(verbose = 1, se = FALSE),
                                pf=FALSE) #just pre-filter the data, does not fit the SSM


summary(fit_sharks)

fit.r <- aniMotum::route_path(fit_sharks, what = "fitted", map_scale = 10, dist = 1000, append = TRUE) 

my.aes <- aes_lst(obs=FALSE, line=TRUE, mp=FALSE, conf = FALSE)

aniMotum::map(fit.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=FALSE, by.id=TRUE)
aniMotum::map(fit.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=TRUE, by.id=FALSE)


sharks_ssm <- aniMotum::grab(fit.r, what = "rerouted")

str(sharks_ssm)

unique(sharks_ssm$id)
min(sharks_ssm$lon)
max(sharks_ssm$lon)


## Merge files 

meta_NWI <- bind_rows(meta_sharks, meta_tigers)
head(meta_NWI)

locs <- bind_rows(sharks_ssm, tigers_ssm)
locs

str(meta_NWI)
str(locs)

locs <- locs |>
  left_join(meta_NWI %>% dplyr::select(id, Species), by = "id")

locs

## unify timesatmps 

# Define unified Start_date 

uni_start_date <- lubridate::ymd_hms("1970-01-01 12:00:00")



str(locs)
locs.df <- locs |>
  dplyr::group_by(id) |>
  dplyr::mutate(time_diff = as.numeric(difftime(date, min(date), units = "secs")),
                uni_timestamp = uni_start_date + seconds(time_diff)) |>
  ungroup() |>
  dplyr::select(-time_diff)


locs.df

unique(is.na(locs.df$uni_timestamp))

# use df2move to convert the data.frame into a moveStack
tracks <- moveVis::df2move(locs.df,
                               #proj = "+init=epsg:4326", 
                               #proj = "+proj=longlat +lon_wrap=180 +datum=WGS84",
                               proj = 4326,
                               x = "lon", y = "lat", time = "uni_timestamp", track_id = "id")

str(tracks)
extent(tracks)

tracks@bbox
min(tracks@data$x)



# align move_data to a uniform time scale
m <- moveVis::align_move(tracks, res = 'mean', unit = "days")

str(m)
projection(m)
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



m.df <- as.data.frame(m) |>
  dplyr::mutate(id = sub("^X", "", trackId),
                id = as.integer(id))
str(m.df)


# Initialize the `tag_loc` column with NA values
m.df$Species <- NA

# Map the `tag_loc` information from input_df to move_data
for (i in seq_len(nrow(locs.df))) {
  id <- locs.df$id[i]
  Species <- locs.df$Species[i]
  m.df$Species[m.df$id == id] <- Species
}

unique(m.df$Species)

# Assign colors based on the `tag_loc` values
color_palette <- c(
  "Tiger_shark" = "#FF0000", # Red
  "Great_hammerhead" = "#0000FF", # Blue
  "Lemon_shark" = "#FFFF00" # Yellow
)


m.df$colour <- color_palette[m.df$Species]


# Check for any NA values in the `color` column
if (any(is.na(m.df$colour))) {
  stop("There are NA values in the `color` column. Please check the `tag_loc` mapping.")
}


str(m.df)
unique(m.df$colour)
unique(m.df$Species)

# Update the MoveStack object with the new column
m@data$Species <- m.df$Species
m$colour <- m.df$colour



# create spatial frames with a OpenStreetMap watercolour map


get_maptypes()



frames <- moveVis::frames_spatial(m,
                                  map_service = "esri", 
                                  map_type = "world_imagery", 
                                  alpha = 1, 
                                  path_legend = FALSE, 
                                  #path_legend_title = "Flatback\nID", 
                                  path_size = 3, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.8) %>% 
  add_labels(title = "North West Island Shark Tracks",
             subtitle = "Note: Dates unified to common year",
             caption = "Red: Tiger sharks, Blue:Great hammerheads, Yellow: Lemon sharks",
             x = "Longitude", 
             y = "Latitude",) %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white", x=144.5, y=-35) %>% 
  #add_scalebar(colour = "white") %>% 
  #add_timestamps(type = "label") %>% 
  add_progress()





frames[[100]] # preview one of the frames, e.g. the 100th frame

# add logo:
logo_path <- "Biopixel.OceansFoundation.Logo.Final.png"  # Replace with the actual path to your logo image
logo <- magick::image_read(logo_path)


logo_grob <- grid::rasterGrob(image = as.raster(logo), interpolate = TRUE)



frames2 <- frames |>
  add_gg(gg = expr(annotation_custom(logo_grob, xmin = 143.5, xmax = 155, ymin = -42, ymax = -35))) |>
  add_scalebar(colour = "white", distance = 750, units = "km", x=157, y=-39)


frames2[[125]]

# animate frames

animate_frames(frames2, out_file = "North_West_Island_SHARKS_Animation_unidate.mov", end_pause = 0, overwrite=TRUE, res=100, fps = 25)




## Barnicle Betty

locs.df <- locs |>
  dplyr::filter(id %in% c("222233")) |>
  dplyr::group_by(id) |>
  dplyr::mutate(time_diff = as.numeric(difftime(date, min(date), units = "secs")),
                uni_timestamp = uni_start_date + seconds(time_diff)) |>
  ungroup() |>
  dplyr::select(-time_diff)


locs.df <- locs |>
  dplyr::filter(id %in% c("222233"))


locs.df

unique((locs.df$id))

unique(is.na(locs.df$uni_timestamp))

# use df2move to convert the data.frame into a moveStack
tracks <- moveVis::df2move(locs.df,
                           #proj = "+init=epsg:4326", 
                           #proj = "+proj=longlat +lon_wrap=180 +datum=WGS84",
                           proj = 4326,
                           x = "lon", y = "lat", time = "date", track_id = "id")

str(tracks)
extent(tracks)

tracks@bbox
min(tracks@data$x)



# align move_data to a uniform time scale
m <- moveVis::align_move(tracks, res = 1, unit = "days")


m$colour <- "red"

# create spatial frames with a OpenStreetMap watercolour map

frames <- moveVis::frames_spatial(m,
                                  map_service = "esri", 
                                  map_type = "world_imagery", 
                                  alpha = 1, 
                                  path_legend = FALSE, 
                                  #path_legend_title = "Flatback\nID", 
                                  path_size = 3, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.8) %>% 
  add_labels(title = "Tiger Shark Barnicle Betty",
             #subtitle = "Note: Dates unified to common year",
             #caption = "Red: Tiger sharks, Blue:Great hammerheads, Yellow: Lemon sharks",
             x = "Longitude", 
             y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white", x=144.5, y=-35) %>%  
  # add_scalebar(colour = "white") %>% 
  add_timestamps(type = "label") %>% 
  add_progress()





frames[[100]] # preview one of the frames, e.g. the 100th frame

# add logo:
logo_path <- "Biopixel.OceansFoundation.Logo.Final.png"  # Replace with the actual path to your logo image
logo <- magick::image_read(logo_path)


logo_grob <- grid::rasterGrob(image = as.raster(logo), interpolate = TRUE)



frames2 <- frames |>
  add_gg(gg = expr(annotation_custom(logo_grob, xmin = 150, xmax = 160, ymin = -42, ymax = -35))) |>
  add_scalebar(colour = "white", distance = 750, units = "km", x=162, y=-39)


frames2[[125]]

# animate frames

animate_frames(frames2, out_file = "North_West_Island_Betty_Animation_unidate.mov", end_pause = 0, overwrite=TRUE, res=100, fps = 25)







#==== Without Betty

locs <- bind_rows(sharks_ssm, tigers_ssm)

locs <- locs |>
  filter(id != "222233") |>
  left_join(meta_NWI %>% dplyr::select(id, Species), by = "id")

locs

## unify timesatmps 

# Define unified Start_date 

uni_start_date <- lubridate::ymd_hms("1970-01-01 12:00:00")



str(locs)
locs.df <- locs |>
  dplyr::group_by(id) |>
  dplyr::mutate(time_diff = as.numeric(difftime(date, min(date), units = "secs")),
                uni_timestamp = uni_start_date + seconds(time_diff)) |>
  ungroup() |>
  dplyr::select(-time_diff)


locs.df

unique(is.na(locs.df$uni_timestamp))

# use df2move to convert the data.frame into a moveStack
tracks <- moveVis::df2move(locs.df,
                           #proj = "+init=epsg:4326", 
                           #proj = "+proj=longlat +lon_wrap=180 +datum=WGS84",
                           proj = 4326,
                           x = "lon", y = "lat", time = "uni_timestamp", track_id = "id")

str(tracks)
extent(tracks)

tracks@bbox
min(tracks@data$x)



# align move_data to a uniform time scale
m <- moveVis::align_move(tracks, res = 2, unit = "days")

str(m)

#m@data <- NFI_tracks@data

min(m@data$x)



m.df <- as.data.frame(m) |>
  dplyr::mutate(id = sub("^X", "", trackId),
                id = as.integer(id))
str(m.df)


# Initialize the `tag_loc` column with NA values
m.df$Species <- NA

# Map the `tag_loc` information from input_df to move_data
for (i in seq_len(nrow(locs.df))) {
  id <- locs.df$id[i]
  Species <- locs.df$Species[i]
  m.df$Species[m.df$id == id] <- Species
}

unique(m.df$Species)

# Assign colors based on the `tag_loc` values
color_palette <- c(
  "Tiger_shark" = "#FF0000", # Red
  "Great_hammerhead" = "#0000FF", # Blue
  "Lemon_shark" = "#FFFF00" # Yellow
)


m.df$colour <- color_palette[m.df$Species]


# Check for any NA values in the `color` column
if (any(is.na(m.df$colour))) {
  stop("There are NA values in the `color` column. Please check the `tag_loc` mapping.")
}


str(m.df)
unique(m.df$colour)
unique(m.df$Species)

# Update the MoveStack object with the new column
m@data$Species <- m.df$Species
m$colour <- m.df$colour



# create spatial frames with a OpenStreetMap watercolour map

frames <- moveVis::frames_spatial(m,
                                  map_service = "esri", 
                                  map_type = "world_imagery", 
                                  alpha = 1, 
                                  path_legend = FALSE, 
                                  #path_legend_title = "Flatback\nID", 
                                  path_size = 3, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.8) %>% 
  add_labels(title = "North West Island Shark Tracks",
             subtitle = "Note: Dates unified to common year",
             caption = "Red: Tiger sharks, Blue:Great hammerheads, Yellow: Lemon sharks",
             x = "Longitude", 
             y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white", x=144.5, y=-23) %>% 
  #add_scalebar(colour = "white") %>% 
  #add_timestamps(type = "label") %>% 
  add_progress()





frames[[100]] # preview one of the frames, e.g. the 100th frame

# add logo:
logo_path <- "Biopixel.OceansFoundation.Logo.Final.png"  # Replace with the actual path to your logo image
logo <- magick::image_read(logo_path)


logo_grob <- grid::rasterGrob(image = as.raster(logo), interpolate = TRUE)



frames2 <- frames |>
  add_gg(gg = expr(annotation_custom(logo_grob, xmin = 143.5, xmax = 147.5, ymin = -23.5, ymax = -25.5))) |>
  add_scalebar(colour = "white", distance = 250, units = "km", x=148, y=-25)


frames2[[125]]

# animate frames

animate_frames(frames2, out_file = "North_West_Island_SHARKS_wo_Betty_Animation_unidate.mov", end_pause = 0, overwrite=TRUE, res=100, fps = 15)









