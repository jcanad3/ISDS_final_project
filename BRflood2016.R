# Jude Canady
# assignment 5
library(RSocrata)
library(here)
library(tidyverse)
source(here('assignment5/tokensocrata.R'))

library(ggmap)

######################################
####        calls to 311         #####
######################################

apiEndpoint <- 'https://data.brla.gov/resource/uqxt-dtpe.csv?'

query <- "$where=createdate between '2016-08-12' and '2016-08-22'"


dt_311 <- read.socrata(paste0(apiEndpoint, query), app_token = token[['app']])
dt_311 <- as_tibble(dt_311)

dt_311 <- dt_311 %>% 
              mutate(geolocation = str_extract_all(geolocation, '[-,.,0-9]+')) %>% 
              mutate(long = map_chr(geolocation, 1), lat = map_chr(geolocation, 2)) %>% 
              mutate_at(vars(long, lat), as.double) # same as mutate(long = as.double(long), lat = as.double(lat))


#' In case you cannot connect to the API, I saved for you the object brMap to data/mapTerrainBR.RDS
brMap <- readRDS(here::here('assignment5/mapTerrainBR.RDS')) 


######################################
####   Layer of fire dep. calls  #####
######################################

# fire incidents endpoint
fireIncidens <- 'https://data.brla.gov/resource/4w4d-4es6.csv?'
query <- "$where=disp_date between '2016-08-12' and '2016-08-22'"

fireIncidens <- read.socrata(paste0(fireIncidens, query), app_token = token[['app']])
fireIncidens <- as_tibble(fireIncidens)
fireIncidens <- filter(fireIncidens, geolocation != "") %>% 
  mutate(geolocation = str_extract_all(geolocation, '[-,.,0-9]+')) %>% 
  mutate(long = map_chr(geolocation, 1), lat = map_chr(geolocation, 2)) %>% 
  mutate_at(vars(long, lat), as.double) # same as mutate(long = as.double(long), lat = as.double(lat))

######################################
####    LOOTING 911 calls        #####
######################################


# police incidents endpoint
crimeIncidents <- 'https://data.brla.gov/resource/5rji-ddnu.csv?'
query <- "$where=offense_date between '2016-08-12' and '2016-08-22'"

crimeIncidents <-read.socrata(paste0(crimeIncidents, query), app_token = token[['app']])
crimeIncidents <- as_tibble(crimeIncidents)
crimeIncidents <- filter(crimeIncidents, geolocation != "") %>%
  mutate(geolocation = str_extract_all(geolocation, '[-,.,0-9]+')) %>% 
  mutate(long = map_chr(geolocation, 1), lat = map_chr(geolocation, 2)) %>% 
  mutate_at(vars(long, lat), as.double) # same as mutate(long = as.double(long), lat = as.double(lat))


######################################
####    Layer of inundate areas  #####
######################################
library(rgdal)
library(ggpolypath)
shpFile <- here('assignment5/Estimated_Flood_Inundation_Area/Estimated_Flood_Inundation_Area.shp')
indundationArea <- readOGR(shpFile)
ogrInfo(shpFile)
indundationArea <- spTransform(indundationArea, CRS("+proj=longlat +datum=WGS84"))
indundationArea <- fortify(indundationArea)

m <- ggmap::ggmap(brMap) +  
      geom_polypath(data = indundationArea, aes(x = long, y = lat, group=group), fill = 'blue', alpha=.2) +
      geom_point(data = filter(dt_311, parenttype == "DRAINAGE, EROSION, FLOODING OR HOLES"),
             aes(x = long, y = lat, color='311'), alpha = .35) +
      geom_point(data = filter(fireIncidens, inci_descript == "Severe weather or natural disaster, Other" 
                           | inci_descript == "Water evactuation"),
             aes(x = long, y = lat, color='Firebrigage'), alpha = .35) +
      geom_point(data = filter(crimeIncidents, offense_desc == "LOOTING"),
             aes(x = long, y = lat, color='Police'), alpha = .35) +
      scale_color_manual(values=c("yellow", "red", "blue")) +
      labs(color="Organization") +
      ggtitle('Density of calls to emergeny numbers')

ggsave('calls_for_Flood.png', m, path = here('assignment5/'))








