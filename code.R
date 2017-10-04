library(jsonlite)
library(ggplot2)
library(ggmap)
library(plyr)

# (Mostly) helpful resources that I used:
# https://www.r-bloggers.com/how-to-map-your-google-location-history-with-r/
# http://emelineliu.com/2016/10/21/LocationHistory/
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
# http://www.geo.ut.ee/aasa/LOOM02331/heatmap_in_R.html

#### Load data ####

system.time(x <- fromJSON("/Users/toridykes1/Projects/Google Location Data/Takeout/Location History/Location History.json"))

## System time isn't really necessary here as far as I can tell; just tells you how long the process took

#### Extract the locations ####

## X is a huge file with a lot of info we don't necessarily need

locs <- x$locations

#### Convert the time + coordinates columns to something workable ####

## For some reason for Unix, time began on Jan. 1, 1970.

locs$time <- as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01")

## Convert long and lat values from e^7 notation

locs$lat <- locs$latitudeE7 / 1e7
locs$long <- locs$longitudeE7 / 1e7

#### Maps ####

# Get a Google map of Berlin via ggmap
# The get_map command errors out for me sometimes - just try again if it does that to you

berlin <- get_map(location = 'Berlin', zoom = 12)

# Since I spend time in Köpenick, I need a more zoomed out map if I want to see that
berlin_large <- get_map(location = "Berlin", zoom = 11)

options(stringsAsFactors = T) # Ima be straight up, not sure if this is essential or not

# Create a df of lat-long pair frequencies
freq <- count(locs, c('long','lat'))
# I didn't end up using this, but another approach could be limiting your map to the most visited places
highfreq <- freq[which(freq$freq >300),]

# Let's see what the map of all of Berlin looks like

berlin_full <- ggmap(berlin_large, extent = "device") +
  geom_density2d(data = freq, aes(x = long, y = lat), size = .3) + 
  stat_density2d(data = freq, aes(x = long, y = lat, fill = ..level.., alpha = .3), 
                 size = .1, bins = 200, geom = "polygon") + 
  scale_fill_gradient(low = "#FFC300", high = "#C70039", guide = F) + 
  scale_alpha(range = c(0, .3), guide = F)

berlin_full

# And here the map that is zoomed in on the center of the city

berlin_center <- ggmap(berlin, extent = "device") +
  geom_density2d(data = freq, aes(x = long, y = lat), size = .3) + 
  stat_density2d(data = freq, aes(x = long, y = lat, fill = ..level.., alpha = .3), 
                 size = .1, bins = 200, geom = "polygon") + 
  scale_fill_gradient(low = "#FFC300", high = "#C70039", guide = F) + 
  scale_alpha(range = c(0, .3), guide = F)

berlin_center

### Maps by Years ###

## 2015 --> Not enough data for me

## 2016

# Filter by all rows that match 2016 in their time column
locs2016 <- locs[grep("2016", locs$time),]

# Count frequencies again
freq2016 <- count(locs2016, c('long','lat'))

berlin_center2016 <- ggmap(berlin, extent = "device") + 
  geom_density2d(data = freq2016, aes(x = long, y = lat), size = .3) + 
  stat_density2d(data = freq2016, aes(x = long, y = lat, fill = ..level.., alpha = .3), 
                 size = .1, bins = 200, geom = "polygon") + 
  scale_fill_gradient(low = "#FFC300", high = "#C70039", guide = F) + 
  scale_alpha(range = c(0, .3), guide = F)

berlin_center2016

# 2017

locs2017 <- locs[grep("2017", locs$time),]

freq2017 <- count(locs2017, c('long','lat'))

berlin_center2017 <- ggmap(berlin, extent = "device") + geom_density2d(data = freq2017, aes(x = long, y = lat), size = .3) + 
  stat_density2d(data = freq2017, aes(x = long, y = lat, fill = ..level.., alpha = .3), 
                 size = .1, bins = 200, geom = "polygon") + 
  scale_fill_gradient(low = "#FFC300", high = "#C70039", guide = F) + 
  scale_alpha(range = c(0, .3), guide = F)

berlin_center2017
