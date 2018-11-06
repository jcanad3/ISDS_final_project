# Jude Canady
# Final project
library(RSocrata)
library(here)
library(tidyverse)
source(here('final_project/R/tokensocrata.R'))
library(lubridate)
library(ggmap)

ggmap::register_google(key = token['googleAPIkey'])

apiEndpoint <- 'https://data.brla.gov/resource/uqxt-dtpe.csv?'

query <- "$where=createdate between '2015-12-31' and '2018-10-26'"


dt_311 <- read.socrata(paste0(apiEndpoint, query), app_token = token[['app']])
dt_311 <- as_tibble(dt_311)

dt_311 <- dt_311 %>% 
  mutate(geolocation = str_extract_all(geolocation, '[-,.,0-9]+')) %>% 
  mutate(long = map_chr(geolocation, 1), lat = map_chr(geolocation, 2)) %>% 
  mutate_at(vars(long, lat), as.double) # same as mutate(long = as.double(long), lat = as.double(lat))

road_issues <- filter(dt_311, typename == 'POTHOLE' | typename == 'CONCRETE ROAD REPAIRS NEEDED' |
                      typename == 'ROADSIDE EROSION ISSUE' | typename == 'REPAIR A BROKEN STREET CURB' |
                      typename == 'SHOULDER REPAIR NEEDED' | typename == 'ROAD STRIPING NEEDED' |
                      typename == 'CAVE-IN/SINK HOLE (SEWER RELATED)' | typename == 'ROADSIDE DRAINAGE ISSUE' |
                      typename == 'ROAD BUCKLE/BLOWOUT/FAULT' | typename == 'MANHOLE ISSUE' |
                      typename == 'GUARDRAIL ISSUES' | typename == 'STORMDRAIN ISSUES') %>%
                      mutate(time_to_complete = closeddate - createdate)

#brMap <- readRDS(here::here('assignment5/mapTerrainBR.RDS')) 

br_tract_map <-readRDS(here::here('br_tract'))

#brMap <- get_map(location = 'baton rouge', zoom = 10)

# returns values in seconds
median_fix_time <- road_issues %>% group_by(streetname) %>% 
  summarise(median_time_for_street = median(time_to_complete, na.rm=TRUE), num_reports = n()) %>% drop_na()

# removing values where time_to_complete = 0
#mean_fix_time <- mean_fix_time[mean_fix_time$time_to_complete > 0,]

# convert to date time
#median_fix_time$time_to_complete <- seconds_to_period(median_fix_time$time_to_complete)

roads_avg_response = inner_join(road_issues, median_fix_time, by = "streetname")

ggmap(brMap, extent = "device") + 
 # geom_density_2d(data = roads_avg_response, aes(x = long, y = lat), size = 0.5) + 
  stat_density_2d(data = roads_avg_response, 
                 aes(x = long, y = lat, fill = ..level.., alpha = 0.3), size = 0.1, 
                  geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  #geom_polygon(data = roads_avg_response, aes(x=long, y=lat, fill=median_time_for_street))
  #geom_point(data=roads_avg_response, aes(x=long, y=lat), color='blue', 
  #           alpha=0.1, size = roads_avg_response$median_time_for_street*0.00000008) +
  scale_alpha(range = c(0, 0.3), guide = FALSE)

sort_desc <- roads_avg_response[order(roads_avg_response$long),]
ggmap(br_tract_map) + 
   # geom_point(data=roads_avg_response, aes(x=long, y=lat), color='blue', alpha=0.1, size = roads_avg_response$median_time_for_street*0.0000001)
  geom_path(data=sort_desc, aes(x=long, y=lat, color = seconds(median_time_for_street)))


# shows descending of completion time
#sort_desc <- mean_fix_time[order(mean_fix_time$time_to_complete),]

