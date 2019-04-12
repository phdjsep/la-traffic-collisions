library(RSocrata)
library(janitor)
library(tidyverse)

# Get data and clean it up a bit
# we'll make it a tibble, standardize the headers, and remove unhelpful columns
# Then we'll clean up some whitespace nonsense and then
# strip out the longtitude and latitude into their own columns
# filter out locations with long/lat of 0; ain't no collisions in the ocean
read.socrata("https://data.lacity.org/resource/k8cc-2d49.csv") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  select(-crm_cd:-crm_cd_desc, -location_1_address:-location_1_zip) %>% 
  mutate(cross_street = str_squish(cross_street), 
         location = str_squish(location),
         location_1 = str_extract(location_1, "(?<=\\().*?(?=\\))")) %>% 
  separate(location_1, c("long", "lat"), sep = " ") %>% 
  mutate(long = as.double(long), lat = as.double(lat)) %>% 
  filter(long != 0) %>% 
  write_rds("traffic.rds", compress = "gz")

