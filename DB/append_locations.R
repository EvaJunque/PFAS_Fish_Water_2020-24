## Read the combined csv file, then for each location make a
## http request for the coordinates, caching in a local file
## to make subsequent queries cheap, and write the result into
## a new csv file with trailing columns

library(httr2)
library(tidyverse)
library(data.table)

library(maps)
library(ggplot2)
library(sf)


# https://www.gov.uk/guidance/uk-geospatial-data-standards-coordinate-reference-systems
# https://digimap.edina.ac.uk/help/our-maps-and-data/bng/

# as originally written, appended locations to fluor_combined.csv
# moving towards using the without-locations data instead

filename <- "DB/fluor_combined.csv"
output <- "DB/fluor_with_locations.csv"
geog_output <- "DB/geographic_lookup.csv" # table of sample.samplingPoint to derived information 

data <- read.csv(filename)

locations <- unique(data$sample.samplingPoint)

source("get_location_to_coord.R")

st <- get_location_to_coord(unique(data$sample.samplingPoint))


# Appending longitude, latitude fields to the data frame and hence to the output csv file
data$longitude <- sapply(data$sample.samplingPoint, function (x) { st$get(x)[1] })
data$latitude <- sapply(data$sample.samplingPoint, function (x) { st$get(x)[2]})


data$area <- sapply(data$sample.samplingPoint, function (x) { st$get(x)[3] })
data$subarea <- sapply(data$sample.samplingPoint, function (x) { st$get(x)[4] })

# Can convert from easting/northing to long/lat directly
GeomTmp <-
  data %>%
  st_as_sf(coords = c("sample.samplingPoint.easting", 
                      "sample.samplingPoint.northing"),
           crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates()

data$GeomLong <- GeomTmp[,"X"]
data$GeomLat <- GeomTmp[,"Y"]

data$area_easting <- integer(nrow(data))
data$area_northing <- integer(nrow(data))

data$subarea_easting <- integer(nrow(data))
data$subarea_northing <- integer(nrow(data))

for (a in unique(data$area))
{
  sub <- data$area == a
  data[sub,]$area_easting <-  mean(data[sub,]$sample.samplingPoint.easting)
  data[sub,]$area_northing <-  mean(data[sub,]$sample.samplingPoint.northing)
}

for (a in unique(data$subarea))
{
  sub <- data$subarea == a
  data[sub,]$subarea_easting <-  mean(data[sub,]$sample.samplingPoint.easting)
  data[sub,]$subarea_northing <-  mean(data[sub,]$sample.samplingPoint.northing)
}


worldmap = map_data('world')


plot <- ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black') +
  coord_fixed(ratio = 1.3, # todo, work out what ratio matches the usual presentation
              xlim = c(-10,3), ylim = c(50.3, 59) # carve the UK out of the world map dataset
  )

# Get some distinct colours
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

plot <- plot +  geom_point(data = data,
                           aes(x = longitude,
                               y = latitude,
                               color = area)) + 
  theme(legend.position="none")
plot





ggplot(data = data, aes(x = longitude, y = GeomLong)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)





geog_names <- c("sample.samplingPoint",
               "longitude",
               "latitude",
               "area",
               "subarea")

geog <- data.frame(matrix(ncol=length(geog_names), nrow=0), stringsAsFactors=FALSE)
colnames(geog) <- geog_names

for (l in locations) {
    stval <- st$get(l)
    geog[nrow(geog)+1,] <- list(l,
                                stval[1],
                                stval[2],
                                stval[3],
                                stval[4])
}



i <- 0
for (area in unique(geog$area))
{
i <- i + 1
subtab <- geog[geog$area == area, ]

plot <- ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black') +
  coord_fixed(ratio = 1.3, # todo, work out what ratio matches the usual presentation
              xlim = c(-10,3), ylim = c(50.3, 59) # carve the UK out of the world map dataset
  )


plot <- plot +  geom_point(data = subtab,
                            aes(x = longitude,
                                y = latitude,
                                color = 100*i))
plot

Sys.sleep(5)

}


plot


# print(geog_output)
# fwrite(geog, geog_output, quote="auto")



# write it out, possibly with broken quotation (but diffdf suggests success)

#p rint(output)
# fwrite(data, output, quote="auto")


# checkdata <- read.csv(output)
# diffdf(data, checkdata) # worked

