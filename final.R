# Jude Canady
# Final project
library(RSocrata)
library(here)
library(tidyverse)
source(here('tokensocrata.R'))
library(httr)
library(jsonlite)
library(lubridate)
library(ggmap)
library(sf)
library(scales)

#brMap <- readRDS(here::here('assignment5/mapTerrainBR.RDS')) 
br_tract_map <- st_read(here::here('br_tract/tl_2018_22_tract.shp'))
br_tract_map <- filter(br_tract_map, COUNTYFP=='033')

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

# adding index to road_issues

road_issues <- road_issues %>% mutate(index=row_number(), GEOID = NA)

backup_road <- road_issues

tract_data <- fromJSON("https://api.datausa.io/api/?sort=desc&show=geo&required=income&sumlevel=tract&year=2016&where=geo%3A16000US2205000")$data
# format data
tract_data[,2] <- substr(tract_data[,2],8,100)
tract_data <- as_tibble(tract_data)
colnames(tract_data)[(1:3)] <- c("year", "GEOID", "median_income")


# overwriting shape file with tract income data
br_tract_income_map <- br_tract_map %>% inner_join(tract_data, by="GEOID")
br_tract_income_map$median_income <- as.numeric(br_tract_income_map$median_income)

#brMap <- get_map(location = 'baton rouge', zoom = 10)

# find which point fall within each geometry
road_geom <- st_as_sf(road_issues, coords=c("long", "lat"))
st_crs(road_geom) <- 4269
inside <- st_intersects(br_tract_income_map, road_geom)

count = 1
for (tract in inside) {
    geo_id <- br_tract_income_map[count,]$GEOID
    print(geo_id)
    print(road_issues %>% filter(index %in% tract) %>% mutate(GEOID = geo_id))
    road_issues$GEOID[road_issues$index == tract] <- geo_id
    #road_issues[tract,] <- mutate(GEOID = geo_id)
    count <- count + 1
}

# returns values in seconds
median_fix_time <- road_issues %>% group_by(streetname) %>% 
  summarise(median_time_for_tract = median(time_to_complete, na.rm=TRUE), num_reports = n()) %>% drop_na()

# removing values where time_to_complete = 0
#mean_fix_time <- mean_fix_time[mean_fix_time$time_to_complete > 0,]

# convert to date time
#median_fix_time$time_to_complete <- seconds_to_period(median_fix_time$time_to_complete)

roads_avg_response = inner_join(road_issues, median_fix_time, by = "streetname")


# median income plot
ggplot() +
  geom_sf(data = br_tract_income_map, aes(fill=median_income)) +
  scale_fill_gradient(labels=dollar_format(), name="Median Income")

# mean fix time


# shows descending of completion time
#sort_desc <- mean_fix_time[order(mean_fix_time$time_to_complete),]

