# Special thanks for insights from flowingdata.com regarding this.

# install.packages("fpc")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("mapproj")
# install.packages("rgdal")
# install.packages("plotKML")

# library(RgoogleMaps)
library(ggmap)
library(magrittr)
library(plotKML)
# library(plyr)
library(dplyr)
# library(fpc)
# library(mapproj)
require(RColorBrewer)

setwd("~/Dropbox/R/runkeeper/")

num_locations <- 3

if(!file.exists(file.path("./data","routes.Rdata"))) {
  
  # https://gist.github.com/danielecook/6c555937144d4955073b
  
  # GPX files downloaded from Runkeeper
  files <- dir(file.path("./data"), pattern="\\.gpx", full.names=TRUE)
  
  # Generate vectors for data frame
  index     <- c()
  latitude  <- c()
  longitude <- c()
  file      <- c()
  
  k <- 1 # Set up Counter
  
  # 
  for (f in 1:length(files)) {
   curr_route <- readGPX(files[f])

    # Treat interrupted GPS paths as seperate routes (useful if you occasionally stop running..walk for a bit, and start again like I do.)
    for (i in curr_route$tracks[[1]]) {
      k <- k + 1
      location  <- i
      file      <- c(file, rep(files[f], dim(location)[1])) 
      index     <- c(index, rep(k, dim(location)[1]))
      latitude  <- c(latitude,  location$lat)
      longitude <- c(longitude, location$lon)
    }
  }
  routes <- data.frame(index, latitude, longitude, file)
  # routes <- data.frame(cbind(index, latitude, longitude, file))
  
  # Because the routes dataframe takes a while to generate for some folks - save it!
  save(routes, file=file.path("./data","routes.Rdata"))
} else {
  # Use to load as needed.
  load(file.path("./data","routes.Rdata"))
}

# Fix data types
# routes$file      <- as.character(routes$file)
# routes$latitude  <- as.numeric(levels(routes$latitude)[routes$latitude])
# routes$longitude <- as.numeric(levels(routes$longitude)[routes$longitude])
# routes           <- transform(routes, index=as.numeric(index))

# Load Meta Data
meta_data <- read.csv(file.path("./data","cardioActivities.csv"), stringsAsFactors=FALSE)
meta_data %<>% mutate(file=paste0("./data/",GPX.File), GPX.File=NULL)

# Bind routes
routes <- left_join(routes, meta_data, by="file") %>% arrange(index)

## separate the cities
routes_hobart <- routes %>% filter(index %in% c(184,185,186))
routes_trento <- routes %>% filter(index==400)
routes_nuri   <- routes %>% filter(index %in% c(72:80))
routes_falls  <- routes %>% filter(index==454)
routes_cycle  <- routes %>% filter(Type=="Cycling")
routes_toconf <- routes %>% filter(index==380)
routes        %<>% filter(Type=="Walking" & !index %in% c(72:80,184:186,380,400,454))

## longest walks (possible errors)
routes %>% group_by(index) %>% summarise(n_distinct(Distance..km.)) -> num_dists
names(num_dists) <- c("index", "num_events")
num_dists %>% filter(num_events > 1)
# routes %>% filter(index==485)
routes %<>% filter(index != 485)
## now this should work
# trip_lengths <- routes %>% group_by(index) %>% summarise(Distance=unique(Distance..km.)) %>% arrange(desc(Distance))
## 345 is the cycle
##  70 is the massive loop walk
##  71 is the same
##  78 is city to bay

## alternatively, specify my own clusters;
## HOME, CITY, WORK
clusters <- data.frame(latitude=numeric(), longitude=numeric())
clusters["HOME", ] <- c(-34.846067, 138.714315)
clusters["CITY", ] <- c(-34.921001, 138.606216)
clusters["WORK", ] <- c(-34.953555, 138.507304)

routes$closest_route <- sapply(1:nrow(routes), 
                               function(w) which.min(sqrt((clusters$longitude - routes$longitude[w])^2 + 
                                                          (clusters$latitude  - routes$latitude[w])^2)))


# Plot Everything
for (r in 1:nrow(clusters)) {
  
  locName <- row.names(clusters)[r]
  
  cat(paste0("Plotting ",locName,"\n"))
  
  setroutes <- filter(routes, closest_route==r)
  
  routeIds <- unique(setroutes$index)
  
  # Map the projected points
  png(sprintf("%d-%s-all-map.png", r, locName), width=1280, height=1280)
  
  lat <- range(setroutes$latitude)  
  lon <- range(setroutes$longitude) 
  center = c(mean(lat), mean(lon))  
  zoom <- switch(locName,
                 HOME=13,
                 CITY=14,
                 WORK=15
                 )
  
  if(!file.exists(file.path("./data",sprintf("%d-%s-map.Rdata", r, locName)))) {
    thisMap <- get_map(location=rev(center),
                       color = "color",
                       source = "google",
                       maptype = "roadmap",
                       zoom = zoom)
    save(thisMap, file=file.path("./data",sprintf("%d-%s-map.Rdata", r, locName)))
  } else {
    load(file=file.path("./data",sprintf("%d-%s-map.Rdata", r, locName)))
  }
  
  g <- ggmap(thisMap,
             extent = "device",
             ylab = "Latitude",
             xlab = "Longitude") + 
    # labs(title=locName) + 
    theme(legend.position="none")
  
  g <- g + annotate("rect", 
                    xmin=attr(thisMap, "bb")$ll.lon, 
                    xmax=attr(thisMap, "bb")$ur.lon, 
                    ymin=attr(thisMap, "bb")$ll.lat, 
                    ymax=attr(thisMap, "bb")$ur.lat, 
                    fill="white", alpha=0.85)
  
  for (i in routeIds) {
    currRoute <- subset(setroutes, index==i)
    g <- g + geom_path(aes(y=latitude, x=longitude, color=sample(brewer.pal(11, "Spectral"), 1)), data=currRoute, lwd=0.6)
  }
  
  print(g)
  
  dev.off()
}

# routes %>% group_by(index) %>% summarise(mean_dist=mean(Distance..km.)) %>% full_join(routes)
# routes %>% group_by(index) %>% summarise(meand=mean(Distance..km.), d=Distance..km.[1]) -> routes_d
# routes_d %>% mutate(diff=meand-d) %>% select(diff) %>% unique

routes %>% select(-c(latitude, longitude)) %>% group_by(index) %>% unique -> unique_routes

png("hist_dist.png", width=1200, height=1200, res=128, pointsize=18)
unique_routes %$% hist(Distance..km., breaks=seq(0,15,0.1), xlab="Distance [km]", main="Histogram of Distances Walked", col="#31a4d9")
dev.off()

unique_routes %<>% mutate(newDate=as.Date(Date, format="%Y-%m-%d %H:%M:%S"))

png("timeline_dist.png", width=1200, height=1200, res=128, pointsize=18)
unique_routes %$% plot(x=newDate, y=Distance..km., type="s", col="#31a4d9", xlab="Date", ylab="Distance [km]", 
                       main=paste0("Distances Walked over Time\n",unique_routes %$% min(newDate)," --- ",unique_routes %$% max(newDate)))
dev.off()
