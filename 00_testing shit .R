
### Whale Sharks



library(tidyverse)
library(aniMotum)
library(moveVis)
library(move)
library(move2)
library(sf)
library(terra)
library(readxl)
library(transformr)




# Data Import -------------------------------------------------------------



# meta data 

# Dynamically determine column types
columns <- names(read_excel(
  "/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/03_DATA/Tagging_DATA_SHEETS/DATASHEETS_Review/Tagging_Catch_Tissue_MASTERFILE.xlsx",
  sheet = "Elasmo_CATCH_Master",
  n_max = 1
))

# Create a col_types vector where only 'Sat_PTT' is text
col_types <- ifelse(columns == "Sat_PTT", "text", "guess")



METAFILE <- read_excel("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/03_DATA/Tagging_DATA_SHEETS/DATASHEETS_Review//Tagging_Catch_Tissue_MASTERFILE.xlsx", sheet = "Elasmo_CATCH_Master", 
                       col_types = col_types)



str(METAFILE)




## loc data
data_folder <- "~/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/06_Chapters/DataChapters/Chapter2_WhaleSharks_Mantas/Data_Analysis/R_workfolder/TrackAnalysis/Whale_sharks_Argos_downloads/SPOT_SPLASH"
data_folder <- "C:/Users/jc563815/OneDrive - James Cook University/02_PhD/06_Chapters/DataChapters/Chapter2_WhaleSharks_Mantas/Data_Analysis/R_workfolder/TrackAnalysis/Whale_sharks_Argos_downloads/SPOT_SPLASH"


file_pattern <- ".*Locations.csv"

loc_files <- list.files(path = data_folder, pattern = file_pattern, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

#chnage the weird Argos date format:
sharks <- loc_files |>
  #lapply(read_csv) |>
  purrr::map(~ read_csv(.x) %>% mutate(across(all_of("DeployID"), as.character))) |>
  bind_rows() 
str(sharks)



whalesharks_raw <- sharks

# fix date 
sharks2 <- sharks |>
  dplyr::mutate(Date = as.POSIXct(Date, format = "%H:%M:%S %d-%b-%Y", tz="UTC"),
                id  = as.character(Ptt),
                #Longitude_360 = if_else(Longitude < 0, Longitude + 360, Longitude)
  )



str(sharks)


str(METAFILE)
meta_whalesharks <- METAFILE |>
  # Add double_tagged column before splitting rows
  dplyr::mutate(
    double_tagged = ifelse(grepl("_", Sat_PTT), TRUE, FALSE)  # Check if Sat_PTT contains "_"
  ) |>
  # split douhble tagged IDs in seperte rows
  tidyr::separate_rows(Sat_PTT, sep = "_") |> 
  dplyr::filter(Common_Name == "Whale_shark" & Sat_Tag == "TRUE") |>
  dplyr::distinct(Sat_PTT, .keep_all = TRUE) |>
  dplyr::mutate(id = Sat_PTT) |> 
  dplyr::mutate(
    id = Sat_PTT,
    # Update specific metadata based on `id`
    Sat_Tag_Model = dplyr::case_when(
      id == "243652" ~ "PSAT",
      id == "243953" ~ "Spot",
      id == "252216" ~ "PSAT",
      id == "176409" ~ "Spot",
      id == "253104" ~ "PSAT",
      id == "272349" ~ "Spot",
      TRUE ~ Sat_Tag_Model
    ),
    Name = dplyr::case_when(
      id == "252216" ~ "Christopher",
      id == "176409" ~ "Sapphire",
      TRUE ~ Name
    ),
    Sat_Attachment = dplyr::case_when(
      id == "243652" ~ "Tether",
      id == "243953" ~ "Fin_Clamp",
      id == "252216" ~ "Tether",
      id == "176409" ~ "Fin_Clamp",
      id == "253104" ~ "Tether",
      id == "272349" ~ "Fin_Clamp",
      TRUE ~ Name
    )
  ) |>
  dplyr::rename(Tag_date = Date,
                Name = Name,
                Species = Common_Name) |>
  dplyr::select(id, Sat_PTT, Tag_date, Name, Sex, TL_cm, Location, Species, Sat_Tag_Model, Sat_Tag_Model_Type, Sat_Attachment, double_tagged)

str(meta_whalesharks)
str(sharks)


unique(sharks$id)
unique(meta_whalesharks$Sat_PTT)
unique(meta_whalesharks$id)
unique(meta_whalesharks$Name)


# delete rows before tagging date:
sharks3 <-  sharks2 |> 
  dplyr::rename(Date_time = Date) |> 
  dplyr::mutate(Date = as.Date(Date_time, format = "%Y-%m-%d"))


# meta_whalesharks$Tag_Date <- as.Date(meta_whalesharks$Tag_date, format = "%Y-%m-%d")  # Adjust the date format if necessary

str(sharks3)
str(meta_whalesharks)

# Merge Tag_date from meta 
sharks4 <- dplyr::left_join(sharks3, meta_whalesharks, by = c("id"))

# Filter locs only from tag date 
sharks5 <- sharks4 |>
  dplyr::filter(Date >= Tag_date)



# transform data to animotum compatable format:
sharks_ssm_input <- aniMotum::format_data(sharks5,
                                          id = "id",
                                          date = "Date",
                                          lc = "Quality",
                                          coord = c("Longitude", "Latitude"),
                                          epar = c("Error Semi-major axis", "Error Semi-minor axis", "Error Ellipse orientation"),
                                          sderr = c("x.sd", "y.sd"),
                                          tz = "UTC")



sharks_ssm_input <- sharks_ssm_input |>
  dplyr::filter(lc != "Z") 




unique(sharks_ssm_input$id)

str(sharks_ssm_input)

# inspect data 


meta_data <- sharks_ssm_input |> 
  dplyr::group_by(id, Sat_Tag_Model) |> 
  dplyr::summarise(minDate = as.Date(min(date)),
                   maxDate = as.Date(max(date)),
                   N_Locations = n(),
                   DAL = maxDate - minDate) |>
  dplyr::arrange(minDate)

print(meta_data, n =50)


#filter id with low number of locs
sharks_ssm_input <- sharks_ssm_input |> 
  dplyr::filter(!id %in% c("178950"))

# Fit SSM for all ---------------------------------------------------------



fit_ssm <- aniMotum::fit_ssm(sharks_ssm_input, 
                             vmax= 2, #max travelling speed of 100 km/day 
                             model = "crw", 
                             time.step = 24, 
                             #map = list(psi = factor(NA)),
                             #control = ssm_control(verbose = 1, se = FALSE),
                             pf=FALSE) #just pre-filter the data, does not fit the SSM


# summary(fit_ssm)

fit.r <- aniMotum::route_path(fit_ssm, what = "fitted", map_scale = 10, dist = 1000, append = TRUE) 

my.aes <- aes_lst(obs=FALSE, line=TRUE, mp=FALSE, conf = FALSE)

aniMotum::map(fit.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=FALSE, by.id=TRUE)
aniMotum::map(fit.r, what = "rerouted", aes = my.aes, crs=NULL, group=FALSE, by.date=TRUE, by.id=FALSE)


# get crw location data
crw_locs <- aniMotum::grab(fit.r, what = "rerouted")

str(crw_locs)

unique(crw_locs$id)
min(crw_locs$lon)
max(crw_locs$lon)

# add metadata

## Merge files 


str(meta_whalesharks)
str(crw_locs)

crw_locs_meta <- crw_locs |>
  dplyr::left_join(meta_whalesharks, by = "id") |> 
  dplyr::mutate(Tag_Year = lubridate::year(Tag_date))

crw_locs_meta


unique(crw_locs_meta$id)

whalesharks_sf <- sf::st_as_sf(crw_locs_meta, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
whalesharks_sf |> mapview::mapview(zcol = "Name", legend = TRUE)

str(crw_locs_meta)
# manually filter out 2 false locations: (foudn using mapview above)
crw_locs_meta_f <- crw_locs_meta |> 
  dplyr::mutate(Date = lubridate::date(date)) |> 
  dplyr::filter(!(
    #Name == "Blancpain" & 
    Date %in% c('2022-07-28', '2022-07-27'))) |> 
  dplyr::select(-Date)

whalesharks_sf2 <- sf::st_as_sf(crw_locs_meta_f, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
whalesharks_sf2  |> mapview::mapview(zcol = "Name", legend = TRUE)


saveRDS(crw_locs_meta_f, "WhaleSharkTracks_SPOT_SPLASH_20250128.rds")



# Animations --------------------------------------------------------------


locs <- readRDS("WhaleSharkTracks_SPOT_SPLASH_20250128.rds")
str(locs)

locs_2024 <- locs |> 
  dplyr::filter(Tag_Year == "2024")

# use df2move to convert the data.frame into a moveStack
tracks <- moveVis::df2move(locs_2024,
                           proj = "+init=epsg:4326", 
                           x = "lon", y = "lat", time = "date", track_id = "Name")

str(tracks)
head(tracks)

# align move_data to a uniform time scale
# m <- moveVis::align_move(tracks, res = 'mean', unit = "days")
m <- moveVis::align_move(tracks, res = 1, unit = "days")
head(m)

summary(m@timestamps)
summary(m@data$x)
summary(m@data$y)



# Initialize the `Name column with NA values
m.df <- as.data.frame(m)
# m.df$Name <- NA
# 
# str(m.df)
# str(locs_2024)

# # Map the `tag_loc` information from input_df to move_data
# for (i in seq_len(nrow(locs_2024))) {
#   Name <- locs_2024$Name[i]
#   Name_value <- locs_2024$Name[i]
#   m.df$Name[m.df$Name == Name] <- Name_value
# }

unique(m.df$trackId)
str(m.df)

# Assign colors based on the `tag_loc` values
color_palette <- c(
  "Antoine" = "#FF0000", # Red
  "Arnaz" = "#0000FF", # Blue
  "Francky" = "#00FF00", # Green
  "Ossabaw" = "#FFFF00", # Yellow
  "Brunswick" = "#5C3317",
  "Darien" = "#FFA500", # Orange
  "Tybee" = "#800080", # Purple
  "Grover" = "#00FFFF", # Cyan
  "Henny" = "#FF00FF",  # Magenta
  "Richard" = "#008B8B", # Yellow
  "Ingo" = "#FDD017", # Orange
  "Adam" = "#C47451", # Purple
  "Brahm" = "#737CA1", # Cyan
  "Cruze" = "#D58A94",  # Magenta
  "Barry" = "#6F2DA8"
)


m.df$colour <- color_palette[m.df$trackId]
m.df$colour <- color_palette[as.character(m.df$trackId)]
head(m.df)

# Check for any NA values in the `color` column
if (any(is.na(m.df$colour))) {
  stop("There are NA values in the `color` column. Please check the `Name` mapping.")
}


str(m.df)
unique(m.df$colour)
unique(m.df$trackId)

# Update the MoveStack object with the new column
m$colour <- m.df$colour

# m$colour <- "red"


head(m)
str(m)

m@data
m@coords
m@bbox

# try to edit the bbox 
m@bbox <- matrix(c(141, 145.5, -30, 5), ncol = 2, 
                 dimnames = list(c("coords.x1", "coords.x2"), c("min", "max")))


m@bbox

# create spatial frames


get_maptypes()

frames <- moveVis::frames_spatial(m,
                                  map_service = "esri",
                                  map_type = "world_imagery",
                                  # map_service = "mapbox", 
                                  # map_type = "satellite", 
                                  map_token = "pk.eyJ1IjoiaW5nby1tIiwiYSI6ImNrYnAwa2tjZTFlN3MzNnI1bWQzeWVicTcifQ.s-9n23Ws6rsl02ZE6Jp2Dg",
                                  alpha = 1, 
                                  path_legend = TRUE, 
                                  path_legend_title = "Whale shark\nName", 
                                  path_size = 1.5, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.5,
                                  equidistant = NULL,
                                  cross_dateline = FALSE) |> 
  add_labels(title = "Whale shark tracks - 2 months post-tagging",
             #caption = "Note: Dates unified to common year"
             #tag = "",
             x = "Longitude", 
             y = "Latitude") |>  
  # add_gg(gg = expr(coord_cartesian(xlim = c(141, 145.5), ylim = c(-30, 5)))) |>
  # add_gg(gg = expr(scale_x_continuous(breaks = seq(142, 145, by = 1)))) |>
  # add_gg(gg = expr(scale_y_continuous(breaks = seq(-25, 0, by = 5)))) |>
  add_progress() 




frames[[35]] # preview one of the frames, e.g. the 100th frame

# add logo:
logo_path <- "Biopixel.OceansFoundation.Logo.Final.png"  # Replace with the actual path to your logo image
logo <- magick::image_read(logo_path)
logo_grob <-  grid::rasterGrob(image = as.raster(logo), interpolate = TRUE)

#add wahle sahrk scetch
ws_path <- "WhaleShark.png"  # Replace with the actual path to your logo image
ws_image <- magick::image_read(ws_path)
ws_image_rotated <- ws_image |> 
  magick::image_background("none") |> 
  magick::image_rotate(20) |> 
  magick::image_flop() |>     
  magick::image_trim() 

#ws_grob <-  grid::rasterGrob(image = as.raster(ws_image_rotated), interpolate = FALSE)





frames2 <- frames |>
  
  
  add_gg(gg = expr(annotation_custom(logo_grob, xmin = 143, xmax = 145.5, ymin = -20, ymax = -18))) |>
  #add_gg(gg = expr(annotation_custom(ws_grob, xmin = 143, xmax = 145.5, ymin = -19, ymax = -15))) |>
  add_gg(gg = expr(guides(linetype = "none"))) |> 
  add_scalebar(colour = "black", distance = 250, units = "km", x=143, y=-20.3) |> 
  add_northarrow(colour = "black", x=146, y=-20.3) |> 
  add_timestamps(type = "label") |> 
  add_text("Cairns", y = -16.918246,  x = 145.771359,
           colour = "white", size = 4) |> 
  add_text("Townsville", y = -19.289030, x = 146.768921,
           colour = "white", size = 4) |> 
  add_text("Port\nMoresby", y = -9.7, x = 147.1494,
           colour = "white", size = 4) |> 
  add_text("Coral Sea", y = -14, x = 147,
           colour = "black", size = 4, type = "label") 





frames2[[25]]

# animate frames

animate_frames(frames2, out_file = "Whale_Sharks_2024_Animation_20250128_TESTFORMAT.mov", end_pause = 0, overwrite=TRUE, res=200, fps = 5,
               width = 1400,
               height = 2000)





### BOF team competition 


team_2024 <- locs |> 
  dplyr::filter(Tag_Year == "2024" & Name %in% c("Ingo", "Adam", "Richard"))

# use df2move to convert the data.frame into a moveStack
tracks <- moveVis::df2move(team_2024,
                           proj = "+init=epsg:4326", 
                           x = "lon", y = "lat", time = "date", track_id = "Name")

str(tracks)
head(tracks)

# align move_data to a uniform time scale
# m <- moveVis::align_move(tracks, res = 'mean', unit = "days")
m <- moveVis::align_move(tracks, res = 1, unit = "days")
head(m)

summary(m@timestamps)
summary(m@data$x)
summary(m@data$y)



# Initialize the `Name column with NA values
m.df <- as.data.frame(m)
# m.df$Name <- NA
# 
# str(m.df)
# str(locs_2024)

# # Map the `tag_loc` information from input_df to move_data
# for (i in seq_len(nrow(locs_2024))) {
#   Name <- locs_2024$Name[i]
#   Name_value <- locs_2024$Name[i]
#   m.df$Name[m.df$Name == Name] <- Name_value
# }

unique(m.df$trackId)
str(m.df)

# Assign colors based on the `tag_loc` values
color_palette <- c(
  # "Antoine" = "#FF0000", # Red
  # "Arnaz" = "#0000FF", # Blue
  # "Francky" = "#00FF00", # Green
  # "Ossabaw" = "#FFFF00", # Yellow
  # "Brunswick" = "#5C3317",
  # "Darien" = "#FFA500", # Orange
  # "Tybee" = "#800080", # Purple
  # "Grover" = "#00FFFF", # Cyan
  # "Henny" = "#FF00FF",  # Magenta
  "Richard" = "#0000FF", # Yellow
  "Ingo" = "#FF00FF", # Orange
  "Adam" = "#FFA500" # Purple
  # "Brahm" = "#737CA1", # Cyan
  # "Cruze" = "#D58A94",  # Magenta
  # "Barry" = "#6F2DA8"
)


m.df$colour <- color_palette[m.df$trackId]
m.df$colour <- color_palette[as.character(m.df$trackId)]
head(m.df)

# Check for any NA values in the `color` column
if (any(is.na(m.df$colour))) {
  stop("There are NA values in the `color` column. Please check the `Name` mapping.")
}


str(m.df)
unique(m.df$colour)
unique(m.df$trackId)

# Update the MoveStack object with the new column
m$colour <- m.df$colour



# create spatial frames


# get_maptypes()

frames <- moveVis::frames_spatial(m,
                                  map_service = "esri", 
                                  map_type = "world_imagery", 
                                  alpha = 1, 
                                  path_legend = TRUE, 
                                  path_legend_title = "Whale shark\nName", 
                                  path_size = 1.5, 
                                  path_end = 'round', 
                                  path_join = 'round', 
                                  trace_show = TRUE,
                                  trace_colour = "white",
                                  tail_colour = "white",
                                  tail_size = 0.5,
                                  equidistant = NULL,
                                  cross_dateline = FALSE) |> 
  add_labels(title = "Whale shark tracks - 2 months post-tagging",
             #caption = "Note: Dates unified to common year"
             #tag = "",
             x = "Longitude", 
             y = "Latitude") |>  
  add_progress() 




frames[[35]] # preview one of the frames, e.g. the 100th frame

# add logo:
logo_path <- "Biopixel.OceansFoundation.Logo.Final.png"  # Replace with the actual path to your logo image
logo <- magick::image_read(logo_path)
logo_grob <-  grid::rasterGrob(image = as.raster(logo), interpolate = TRUE)

#add wahle sahrk scetch
ws_path <- "WhaleShark.png"  # Replace with the actual path to your logo image
ws_image <- magick::image_read(ws_path)
ws_image_rotated <- ws_image |> 
  magick::image_background("none") |> 
  magick::image_rotate(20) |> 
  magick::image_flop() |>     
  magick::image_trim() 

ws_grob <-  grid::rasterGrob(image = as.raster(ws_image_rotated), interpolate = FALSE)


frames2 <- frames |>
  add_gg(gg = expr(annotation_custom(logo_grob, xmin = 145, xmax = 146, ymin = -12.4, ymax = -12))) |>
  add_gg(gg = expr(annotation_custom(ws_grob, xmin = 145, xmax = 146, ymin = -11.9, ymax = -11.4))) |>
  add_gg(gg = expr(guides(linetype = "none"))) |> 
  add_scalebar(colour = "white", distance = 100, units = "km", x=145, y=-12.5) |> 
  add_northarrow(colour = "white", x=144.8, y=-12.4) |> 
  add_timestamps(type = "label") 



frames2[[50]]

# animate frames

animate_frames(frames2, out_file = "Whale_Sharks_2024_BOF_Team_Animation_20250128.mov", end_pause = 0, overwrite=TRUE, res=200, fps = 5,
               width = 1400,
               height = 1400)





# STATIC MAP --------------------------------------------------------------
library(ggspatial)
library(sf)
library(basemaps)
library(ggsci)
library(RColorBrewer)

locs <- readRDS("WhaleSharkTracks_SPOT_SPLASH_20250128.rds")
str(locs)


# add logo:
logo_path <- "Biopixel.OceansFoundation.Logo.Final.png"  # Replace with the actual path to your logo image
logo <- magick::image_read(logo_path)
logo_grob <-  grid::rasterGrob(image = as.raster(logo), interpolate = TRUE)

#add wahle sahrk scetch
ws_path <- "WhaleShark.png"  # Replace with the actual path to your logo image
ws_image <- magick::image_read(ws_path)
ws_image_rotated <- ws_image |> 
  magick::image_background("none") |> 
  magick::image_rotate(20) |> 
  magick::image_flop() |>     
  magick::image_trim() 

ws_grob <-  grid::rasterGrob(image = as.raster(ws_image_rotated), interpolate = FALSE)




basemaps::get_maptypes()

set_defaults(map_service = "esri", map_type = "world_imagery")
# ext <- basemaps::draw_ext()
# ext




ggplot() + 
  basemap_gglayer(ext, map_service = "esri", map_type = "world_imagery") +
  scale_fill_identity() + 
  coord_sf()




locs_sf <- locs |>
  dplyr::mutate(month = month(as.Date(date), label = TRUE, abbr = TRUE)) |>
  dplyr::mutate(month_numeric = as.numeric(as.factor(month))) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(3857) 
locs_sf


paths <- 
  locs_sf |>
  group_by(Name) |> #Group by animal ID so that each animal has it's own unique path
  arrange(date) |> # arrange by the date to ensure data are in the correct sequence 
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING") # converts our points sf to a path sf

paths

cities <- data.frame(Loc = c("Cairns", 
                             "Townsville", 
                             #"Brisbane",
                             "Cooktown",
                             "Mackay"
                             #"Gladstone"
                             # "Lockhard\nRv.",
                             # "Cape\nYork"
),
lat = c(-16.918246, 
        -19.289030, 
        #-27.4705,
        -15.4758,
        -21.1434
        # -12.7861,
        # -10.6891
), 
lon = c(145.771359, 
        146.768921, 
        # 153.026,
        145.2471,
        149.1868
        # 143.3419,
        # 142.5316
)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) 


PNG <- data.frame(Loc = c("Port\nMoresby"),
                  lat = c(-9.4790),
                  lon = c(147.1494)) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) 


WB <- data.frame(Loc = c("Wreck\nBay"),
                 lat = c(-12.132504),
                 lon = c(143.893818)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) 


tag.locs <- data.frame(Loc = c( "Wreck Bay", "Cairns Reefs"),
                       Loc_short = c("T1", "T2"),
                       lat = c(-12.132504, -15.441191),
                       lon = c(143.893818, 145.775393)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) 



ext <- st_bbox(c(xmin = 141, xmax = 157, ymin = -23, ymax = -5), crs = st_crs(4326)) 
ext

palette_33 <- colorRampPalette(brewer.pal(12, "Paired"))(33)

Map <- ggplot() + 
  basemap_gglayer(ext, map_service = "esri", map_type = "world_imagery") +
  #basemap_gglayer(ext, map_service = "esri", map_type = "world_ocean_reference") +
  scale_fill_identity() + 
  coord_sf(crs = 4326, expand = FALSE) +
  #geom_sf(data = locs_sf, aes(color = as.factor(id)), size = 2) +
  geom_sf(data = paths, aes(color = Name), linewidth = 1) +
  labs(x = "", y = "", title = "") +
  #cmocean::scale_colour_cmocean(name = "phase", discrete = TRUE, start = 0.1, end = 0.7) +
  
  scale_x_continuous(breaks = seq(145, 155, by = 5), expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(-20, -5, by = 5), expand = c(0,0)) +
  guides(alpha = "none") +
  geom_sf(data = cities, 
          #mapping = aes(shape = as.factor(Group)),
          shape = 21,
          colour = "black", fill = "red", 
          size = 3, 
          show.legend = FALSE) +
  
  ggsflabel::geom_sf_text_repel(data = cities, 
                                colour = "white", 
                                aes(label = Loc), 
                                nudge_x = -5, 
                                nudge_y = -2, 
                                size = 3, 
                                force = 1,
                                force_pull = 10,
                                seed = 10) +
  
  
  geom_sf(data = PNG, 
          #mapping = aes(shape = as.factor(Group)),
          shape = 21,
          colour = "black", fill = "red", 
          size = 3, 
          show.legend = FALSE) +
  
  ggsflabel::geom_sf_text_repel(data = PNG, 
                                colour = "white", 
                                aes(label = Loc), 
                                nudge_x = 1, 
                                nudge_y = 0, 
                                size = 3, 
                                force = 1,
                                force_pull = 10,
                                seed = 10) +
  
  
  
  geom_sf(data = WB,
          shape = 21,,
          colour = "white",
          #fill = "yellow",
          alpha = 1,
          size = 2,
          show.legend = FALSE) +
  
  ggsflabel::geom_sf_text_repel(data = WB,
                                colour = "white",
                                aes(label = Loc),
                                nudge_x = -0.75,
                                nudge_y = 0.25,
                                size = 3,
                                #fontface = "bold",
                                force = 1,
                                force_pull = 10,
                                seed = 10) +
  
  
  #scale_color_viridis_d(option = "plasma", direction = -1) +  # Use viridis discrete scale
  #cmocean::scale_colour_cmocean(name = "phase", discrete = TRUE, start = 0.1, end = 0.7) +
  #ggsci::scale_color_npg() +
  #ggsci::scale_color_d3(palette = "category20c") + # D3-style colors for large groups
  scale_color_manual(values = palette_33) +
  
  
  annotation_scale(location = "bl",
                   style = "ticks",
                   width_hint = 0.25,
                   line_width = 1,
                   text_cex = 0.75,
                   tick_height = 0.15,
                   height = unit(0.15, "cm"),
                   pad_x = unit(.5, "cm"),
                   pad_y = unit(.5, "cm")) +
  
  
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         height = unit(1.5, "cm"),
                         width = unit(1.5, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style =  north_arrow_fancy_orienteering) + #north_arrow_orienteering
  
  
  theme(
    #legend.position = "right",
    legend.position = "none",
    axis.text = element_text(size = 12)) 



Map





ggsave("Map_Tracks_BOF.png", plot = Map, path = NULL, scale = 1, width = 20, height = 20, units = "cm", dpi = 300)

ggsave("Map_Tracks_noLegend_BOF.png", plot = Map, path = NULL, scale = 1, width = 20, height = 20, units = "cm", dpi = 300)



# Try move package  -------------------------------------------------------

library(move)
library(move2)


filePath<-system.file("extdata","leroy.csv.gz",package="move")
data <- move(filePath)

## create a move object from non-Movebank data
file <- read.table(filePath, header=TRUE, sep=",", dec=".")
file

data <- move(x=file$location.long, y=file$location.lat, 
             time=as.POSIXct(file$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"), 
             data=file, proj=CRS("+proj=longlat +ellps=WGS84"), 
             animal="Leroy", sensor="GPS")
plot(data, type="b", pch=20)




locs <- readRDS("WhaleSharkTracks_SPOT_SPLASH_20250128.rds")
str(locs)

locs_2024 <- locs |> 
  dplyr::filter(Tag_Year == "2024") |> 
  as.data.frame()

min(locs_2024$date)
max(locs_2024$date)

data <- move::move(x = locs_2024$lon, 
                   y = locs_2024$lat, 
                   time = locs_2024$date, 
                   data = locs_2024, proj=CRS("+proj=longlat +ellps=WGS84"), 
                   animal = locs_2024$id, 
                   sensor="GPS")


t <- mt_as_move2(data)

t

# ggplot() +
#   #basemap_gglayer(ext, map_service = "esri", map_type = "world_imagery") +
#   ggspatial::annotation_map_tile(zoom = 5) +
#   ggspatial::annotation_scale() +
#   #theme_linedraw() +
#   geom_sf(data = t, color = "darkgrey", size = 1) +
#   geom_sf(data = mt_track_lines(t), aes(color = track)) +
#   coord_sf(
#     crs = sf::st_crs(4326),
#     xlim = c(142, 145),
#     ylim = c(-20, -5)
#   ) +
#   guides(color = "none")
# 
# 
animation_site <- ggplot() +
  annotation_map_tile(zoom = 5, progress = "none") +
  geom_sf(
    data = mt_track_lines(t),
    mapping = aes(group = track),
    color = "black"
  ) +
  #transition_states(study_site, state_length = 2) +
  enter_fade() +
  exit_fade() +
  ease_aes("cubic-in-out") +
  labs(title = "{closest_state}") +
  annotation_scale()
#> In total 386 empty location records are removed before summarizing.
animation_site


class(t)
t2 <- mt_stack(t) 
t2

data_interpolated <- mt_interpolate(
  data[!sf::st_is_empty(t2), ],
  time = seq(
    as.POSIXct("2024-11-26"),
    as.POSIXct("2025-01-27"), "1 day"
  ),
  max_time_lag = units::as_units(3, "days"),
  omit = TRUE
)
