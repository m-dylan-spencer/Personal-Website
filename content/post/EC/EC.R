setwd("/Volumes/GoogleDrive/My Drive/Carolina/CRJU 311- Policing/EC")

library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(tmap)
library(leaflet)
library(rmapshaper)
library(tidygeocoder)
library(sp)
library(rgeos) # geometry ops
library(rgdal) # input/output, projections
library(GISTools)

# load crime data
crime  <- read_csv("robbery.csv")
names(crime)

# select columns
crime <- dplyr::select(crime, ObjectID, X, Y)

# load city geography
cb <- core_based_statistical_areas(year = 2019, cb = T)
sc <- places(state = "SC", year = 2019, cb = T)
cola <- filter(sc, NAME == "Columbia")
plot(cola)

# load columbia census tracts
#cola.tracts <- tracts(state = "SC", "Richland",  cb = T)
#cola.tracts <- ms_clip(target = cola.tracts, clip = cola, remove_slivers = TRUE)
#colnames(cola.tracts)
#cola.tracts <- cola.tracts %>%
#  select(c("GEOID","geometry"))
#plot(cola.tracts)

v17 <- load_variables(2019, "acs5", cache = TRUE)

View(v17)

cola.tracts <- get_acs(geography = "tract", 
              year = 2019,
              variables = (tpopr = "B01003_001"),
              state = "SC",
              survey = "acs5",
              geometry = T)

cola.tracts <- ms_clip(target = cola.tracts, clip = cola, remove_slivers = TRUE)

# wrangle crime data

summary(crime)
crime.sf <- st_as_sf(crime, coords = c("X", "Y"), crs = 2273)
crime.sf <- st_as_sf(as.data.frame(crime.sf, coords = c("Y", "X"), crs = 2273))
summary(crime.sf)

qtm(crime.sf)

tmap_mode("view")
tm_shape(crime.sf) +
  tm_dots(col = "red")

# aggregate crime to tract

st_crs(cola.tracts) == st_crs(crime.sf)
cola.tracts <- st_transform(cola.tracts, crs = 2273)
st_crs(cola.tracts) == st_crs(crime.sf)

#test <- crime.sf  %>% 
#  st_join(cola.tracts) %>%
#  group_by(GEOID) %>% 
#  summarize(crime = n())

#test <- st_drop_geometry(test)

#test.sf <- cola.tracts %>%
#  left_join(test, by = "GEOID") %>%
#  mutate(crime = replace_na(crime, 0))

crime.sp <- as(crime.sf, "Spatial")
cola.tracts.sp <- as(cola.tracts, "Spatial")
poly.counts(crime.sp, cola.tracts.sp) -> res

cola.tracts.sp$count <- setNames(res, cola.tracts$TRACT_NAME)

cola.tracts.sf <- st_as_sf(cola.tracts.sp)

tmap_mode('plot')
tm_shape(cola.tracts.sf) +
  tm_polygons("count", palette = "Reds", n = 8)

# calculate crime rate per tract

#test.sf <- test.sf %>%
#  mutate(crimeperpop = (crime/estimate)*100)

#tm_shape(test.sf) +
#  tm_polygons("crimeperpop", palette = "Reds", style = "jenks")

summary(test.sf$estimate)
sum(test.sf$estimate)

cola.tracts.sf <- cola.tracts.sf %>%
  dplyr::select(-(moe)) %>%
  separate(NAME, c("Tract", "County", "State"),
           sep = ", ") %>%
  mutate(crimeperpop = (count/estimate)*5000)

names(cola.tracts.sf)

tm_shape(cola.tracts.sf) +
  tm_polygons("crimeperpop", palette = "Reds", style = "jenks")

# geocode class addresses

addresses.df <- read_csv("ec.csv")
names(addresses.df)

addresses.geo <- geocode(addresses.df, street = address, 
                        city = city, state = state, method = "cascade")

summary(addresses.geo$lat)

addresses.geo <- addresses.geo %>%
  filter(is.na(lat) == FALSE & is.na(long) == FALSE)

addresses.sf <- st_as_sf(addresses.geo, coords = c("long", "lat"), crs = 4326)
addresses.sf <- st_as_sf(as.data.frame(addresses.sf, coords = c("long", "lat"), crs = 2273))
qtm(addresses.sf)

tmap_mode("view") # or plot
tm_shape(cola.tracts.sf) +
  tm_polygons("crimeperpop", palette = "Reds", style = "jenks", alpha = 0.5) +
  tm_shape(addresses.sf) +
  tm_dots(col = "black") 

st_crs(cola.tracts.sf2)
wgs84 <- st_transform(cola.tracts.sf, "+init=epsg:4326")
wgs84$crimeperpop <- round(wgs84$crimeperpop, digits = 2)

leaflet() %>%
  addPolygons(data = wgs84, 
              color = "gray", 
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE))
leaflet() %>%
  addTiles() %>%
  addPolygons(data = wgs84, 
              color = ~colorQuantile("Reds", crimeperpop, n = 5)(crimeperpop), 
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              label = ~wgs84$crimeperpop) %>%
  addMarkers(data = addresses.sf, label = "Perceived Crime")



qpal <- colorQuantile("Reds", wgs84$crimeperpop, n = 5)
qpal2 <- colorNumeric("Reds", wgs84$crimeperpop, n = 5)

m <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = wgs84, 
              color = ~qpal2(crimeperpop), 
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              label = ~wgs84$crimeperpop) %>%
  addLegend(pal = qpal2, values = wgs84$crimeperpop, opacity = 1,
            title = "Robbery rate<br>per 5,000 persons") %>%
  addMarkers(data = addresses.sf, label = "Perceived Crime") 

m %>% addProviderTiles(providers$Stamen.Watercolor)
