library(RSocrata)
library(janitor)
library(lubridate)
library(rgdal)
library(sf)
library(tidyverse)
library(leaflet)
# library(spdplyr)
# library(geojsonio)
# library(rmapshaper)


# Get data and clean it up a bit
# we'll make it a tibble, standardize the headers, and remove unhelpful columns
# Then we'll clean up some whitespace nonsense and then
# strip out the longtitude and latitude into their own columns
# filter out locations with long/lat of 0; ain't no collisions in the ocean
# We have ages listed as 99 which we can assume to be unknown. Let's convert 99 to NA
# There are some junk values for vict_sex we'll need to get rid of
read.socrata("https://data.lacity.org/resource/k8cc-2d49.csv") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  select(-crm_cd:-crm_cd_desc, -location_1_address:-location_1_zip) %>% 
  mutate(cross_street = str_squish(cross_street), 
         location = str_squish(location),
         location_latlong = str_extract(location_1, "(?<=\\().*?(?=\\))"),
         time_occ = hm(time_occ/100),
         occ_to_rpt = difftime(date_rptd, date_occ, units = 'days'),
         vict_age = ifelse(vict_age == 99, NA, vict_age),
         vict_sex = ifelse(vict_sex %in% c("", "H", "N"), "X", vict_sex)) %>% 
  separate(location_latlong, c("long", "lat"), sep = " ") %>%
  mutate(long = as.double(long), lat = as.double(lat)) %>%
  filter(long != 0) %>%
  rowid_to_column("id") %>% 
  write_rds("traffic.rds", compress = "gz")

traffic <- read_rds("traffic.rds")

# read in shapefile without catalina island
la_county <- rgdal::readOGR(dsn = "CAMS_ZIPCODE_STREET_SPECIFIC/",
                            layer = "CAMS_ZIPCODE_STREET_SPECIFIC", 
                            verbose = FALSE)

# Set CRS to longlat
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# transform shapefile to use longlat
la_county_geo <- sp::spTransform(la_county, CRS(geo.prj))
# convert to sf object so we can join data and get zip
la_county_geo_sf <- sf::st_as_sf(la_county_geo) %>% filter(Name != 90704)
traffic_sf <- sf::st_as_sf(traffic, wkt = c("location_1"), coords = c("long", "lat"), crs = 4326)
traffic_tbl <- sf::st_join(traffic_sf, la_county_geo_sf)
traffic_tbl <- traffic_tbl %>% 
  mutate(year = lubridate::year(date_occ), 
         qtr = lubridate::quarter(date_occ), 
         month = lubridate::month(date_occ), 
         day = lubridate::day(date_occ), 
         hour = lubridate::hour(time_occ),
         month_tag = factor(month, 
                            levels = as.character(1:12),
                            labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                            ordered = TRUE),
         wday = lubridate::wday(date_occ, 
                                week_start = 1),
         wday_tag = factor(wday, 
                           levels = rev(1:7),
                           labels = rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
                           ordered = TRUE),
         week = as.numeric(format(date_occ, "%W"))) %>% 
  as_tibble()

# plot of traffic collisions by year
traffic_tbl %>% 
  group_by(year, month) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_histogram(stat = 'identity')

# plot of traffic collisions by qtr
traffic_tbl %>% 
  group_by(year, qtr) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n, fill = as.factor(qtr))) + 
  geom_histogram(stat = 'identity')

# plot of traffic collisions by month
traffic_tbl %>% 
  group_by(year, month) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n, fill = as.factor(month))) + 
  geom_histogram(stat = 'identity')

# plot of traffic collisions by weekday and hour
traffic_tbl %>% 
  group_by(wday, hour) %>% 
  count() %>% 
  ggplot(aes(x = hour, y = n)) + 
  geom_histogram(stat = 'identity') + 
  facet_grid(wday ~ .)

# plot calendar heatmap
traffic_tbl %>% 
  group_by(year, month, day) %>% 
  mutate(cnt = n()) %>% 
  ungroup() %>% 
  group_by(year, month) %>% 
  mutate(wmonth = 1 + week - min(week)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wmonth, y = wday_tag, fill = cnt)) + 
  geom_tile(color = "white") + 
  facet_grid(year ~ month_tag) + 
  scale_fill_viridis_c(direction = 1, option = 'plasma') +
  labs(x = "Week of Month", y = NULL) + 
  ggtitle(label = "Calendar Heatmap of LA Traffic Collisions by Year")


# calculate the mean collision for all zip codes and then
# calculate the distance from the mean for each zip
# traffic_avgs <- traffic_tbl %>% 
#   filter(!is.na(zip_num)) %>% 
#   count(zip_num) %>% 
#   mutate(avg_coll = mean(n, na.rm = T), dist_mean = n - avg_coll)

# total number of collisions per zip (all time)  
total_collisions_zip <- traffic_tbl %>% filter(!is.na(zip_num)) %>% count(zip_num)

# need to geographically join to plot totals and remove zip without date
la_county_totals <- left_join(la_county_geo_sf, total_collisions_zip, by = c("Zip_Num" = "zip_num")) %>% 
  filter(!is.na(n) & n > 20)

# la_county_avgs <- left_join(la_county_geo_sf, traffic_avgs, by = c("Zip_Num" = "zip_num")) %>% 
#   filter(!is.na(dist_mean))

# plot the counties with distance from mean traffic collisions
labels <- sprintf(
  "<strong>Zip: %s</strong><br/>%g collisions",
  la_county_totals$Name, la_county_totals$n
) %>% lapply(htmltools::HTML) 

bins <- c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, Inf)
pal <- colorBin("magma", domain = la_county_totals$n, bins = bins)

leaflet(la_county_totals) %>% 
  setView(lng = -118.5, lat = 34.05, zoom = 10) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal(n),
              weight = 2,
              opacity = 0.6,
              color = "#666",
              dashArray = "3",
              fillOpacity = 0.4,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.4,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal, values = ~n,
            title = "Total Collisions",
            opacity = 0.9)






# ------------ scrap --------------

# convert 'well-known text' (WKT) format to POINT object
# traffic$location_geo <- sf::st_as_sfc(traffic$location_1)

# Interestingly when we look at age distribution we get 'spikes' at every 5 year interval starting at 25.
# I'm not entirely certain why. Guess would be that data entry was rounded to these values.

# convert shapefile to geoJSON for reverse geocoding
# install.packages("rgdal")
# install.packages("spdplyr")
# install.packages("geojsonio")
# install.packages("rmapshaper")

# la_county_sans_island_geo@data <- data.frame(la_county_sans_island_geo@data, l
#                                              ong=coordinates(la_county_sans_island_geo)[,1], 
#                                              lat=coordinates(la_county_sans_island_geo)[,2])   
# 
# la_county_sans_islands_tidy <- broom::tidy(la_county_sans_islands, region = "Name")
# la_zip_names <- aggregate(cbind(long, lat) ~ id, data = la_county_sans_islands_tidy, FUN = mean)
# 
# ggplot() + 
#   geom_polygon(data = la_county_sans_islands, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
#   theme_void() +
#   geom_text(data = la_zip_names, aes(x = long, y = lat, label = id), size = 4) + theme_void()


# convert to geojson
# la_county_json <- geojsonio::geojson_json(la_county)
# simplify geojson to save some space
# la_county_json_simplified <- rmapshaper::ms_simplify(la_county_json)
# write out geojson
# geojsonio::geojson_write(la_county_json_simplified, file = "la_county.geojson")
# Load LA Geojson
# la_geojson <- st_read("la_county.geojson")

