# Let's get the weather report for the hour in which the collision occurred
# We'll use the Dark Sky API wrapper to do that.
# devtools::install_github("hrbrmstr/darksky")
library(darksky)
library(magrittr)
library(tidyverse)
library(progress)

# darksky_api_key(darksky_key)
# Assuming we already have our dataset, let's create
# a tibble with the information we need to get weather info
darksky_input <- traffic_tbl %>%
  select(lat, long, date_occ, time_occ, hour) %>%
  mutate(dttm = format(ymd_hms(date_occ + time_occ, tz = "America/Los_Angeles"), format = c("%Y-%m-%dT%H:%M:%S")))

weather <- data.frame()

# get output
# total <- 462854 
total <- 415675
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = total)
for (i in 1:nrow(darksky_input_2)) {
  pb$tick()
  response <- get_forecast_for(darksky_input_2[i, ]$lat, 
                               darksky_input_2[i, ]$long, 
                               darksky_input_2[i, ]$dttm, 
                               exclude = "currently,minutely,daily") %>% 
    extract2('hourly')
  obs <- response[darksky_input_2[i, ]$hour + 1,]
  weather <- bind_rows(weather, obs)
}

# pmap(list(darksky_input$lat, darksky_input$long, darksky_input$dttm), get_forecast_for, exclude = "currently,minutely,daily") %>% 
#   write_rds("darksky_output.rds", compress = "gz")

# create a function to make a request and then retrieve the weather for the hour
# in which the collision occurred.
# get_weather <- function(tibb) {
#   # provide the values to get_forecast_for and retrieve response
#   response <- pmap(list(tibb$lat, tibb$long, tibb$dttm), get_forecast_for)
#   # get hourly df from each list item in response
#   response_hrly <- map(response, "hourly")
#   # create an empty tibble to add weather data
#   weather <- tribble(
#     ~time, ~summary, ~icon, ~precipIntensity, 
#     ~precipProbability, ~temperature, ~apparentTemperature, 
#     ~dewPoint, ~humidity, ~pressure, ~windSpeed, ~windGust, 
#     ~windBearing, ~cloudCover, ~uvIndex, ~visibility
#   )
#   # iterate over smallsky_output_hourly and extract row indicated by tibb$hour + 1
#   for(i in seq_along(response_hrly)) {
#       hour <- tibb[i,]$hour + 1
#       weather <- bind_rows(weather, response_hrly[i][[1]][hour,])
#   }
#   return(weather)
# }

# get weather for collision datetimes
# collision_weather <- get_weather(darksky_input)


weather_all <- weather_all %>% mutate(year = lubridate::year(time), 
       qtr = lubridate::quarter(time), 
       month = lubridate::month(time), 
       day = lubridate::day(time), 
       hour = lubridate::hour(time),
       month_tag = factor(month, 
                          levels = as.character(1:12),
                          labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                          ordered = TRUE),
       wday = lubridate::wday(time, 
                              week_start = 1),
       wday_tag = factor(wday, 
                         levels = rev(1:7),
                         labels = rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
                         ordered = TRUE),
       week = as.numeric(format(time, "%W")))
