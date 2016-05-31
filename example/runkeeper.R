# Special thanks for insights from flowingdata.com regarding this.

# install.packages("fpc")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("mapproj")
# install.packages("rgdal")
# install.packages("plotKML")

# library(RgoogleMaps)
# library(ggmap)
# library(magrittr)
# library(plotKML)
# library(maptools)
# library(plyr)
# library(dplyr)
# library(XML)
# library(fpc)
# library(mapproj)
# require(RColorBrewer)

# setwd("~/Dropbox/R/runkeeper/")
# setwd("~/Dropbox/Freelancer/runkeepR-test/")
setwd("~/Dropbox/Freelancer/runkeepR-test/2016/")

library(runkeepR)
routes <- load_tracks(".")
save(routes, file="saved_routes.rds")
load("saved_routes.rds")
plot_ggplot(routes, trackPal=topo.colors(10, alpha=0.5))
plot_leaflet(routes, trackPal=topo.colors(10, alpha=0.5))
summarise_runs(routes)

setwd("~/Dropbox/Freelancer/runkeepR/data/")
routes <- load_tracks(".")
summarise_runs(routes)
plot_leaflet(routes)

system.file("data", package="runkeepR")
routes <- load_tracks(system.file("data", package="runkeepR"))

## multiple tracks: ./data/2012-09-06-0702.gpx
# data <- xmlTreeParse("./data/2012-09-06-0702.gpx")
# xmltop = xmlRoot(data)[["trk"]]
# xmlcat1 <- xmlSApply(xmltop[["trkseg"]], function(x) xmlSApply(x, xmlValue))
# xmlcat2 <- xmlSApply(xmltop[["trkseg"]][["trkpt"]], function(x) xmlSApply(x, xmlValue))
# xmlcat_df <- data.frame(t(xmlcat),row.names=NULL)
# curr_route <- xmlToList(data)
# 

# # Parse the GPX file
# pfile <- htmlTreeParse("./data/2012-09-06-0702.gpx", error = function (…) {}, useInternalNodes = T)
# # Get all elevations, times and coordinates via the respective xpath
# elevations <- as.numeric(xpathSApply(pfile, path = “//trkpt/ele”, xmlValue))
# times <- xpathSApply(pfile, path = “//trkpt/time”, xmlValue)
# coords <- xpathSApply(pfile, path = “//trkpt”, xmlAttrs)
# # Extract latitude and longitude from the coordinates
# lats <- as.numeric(coords[“lat”,])
# lons <- as.numeric(coords[“lon”,])
# # Put everything in a dataframe and get rid of old variables
# geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
# rm(list=c(“elevations”, “lats”, “lons”, “pfile”, “times”, “coords”))

# 
# ReadGPX_MMR <- function(gpx.file) {








  # ReadGPX_MMR("./data/2012-09-06-0702.gpx")

# num_locations <- 3

# if(!file.exists(file.path("./data","routes.Rdata"))) {
#   
#   # https://gist.github.com/danielecook/6c555937144d4955073b
#   
#   # GPX files downloaded from Runkeeper
#   files <- dir(file.path("./data"), pattern="\\.gpx", full.names=TRUE)
#   
#   # Generate vectors for data frame
#   index     <- c()
#   name      <- c()
#   latitude  <- c()
#   longitude <- c()
#   file      <- c()
#   elevation <- c()
#   time      <- c(.POSIXct(character(0)))
#   
#   k <- 0 # Set up Counter
#   
#   # 
#   for (f in 1:length(files)) {
#   # for (f in 1:20) {
#    # curr_route <- readGPS(i="gpx", f=files[1], type="w")
#    # curr_route2 <- readGPX(files[1])
#    # 
#    # library(XML)
#    data <- xmlParse(files[f])
#    curr_route <- xmlToList(data)
#    
#     # Treat interrupted GPS paths as seperate routes (useful if you occasionally stop running..walk for a bit, and start again like I do.)
#     # for (i in curr_route$tracks[[1]]) {
#       k <- k + 1
#       # location  <- i
#       
#       
#       
#       # latitude  <- c(latitude,  location$lat)
#       # longitude <- c(longitude, location$lon)
#       # elevation <- c(elevation, location$ele)
#       # time      <- c(time, location$time)
#       
#       this_lat <- as.numeric(unname(unlist(lapply(lapply(curr_route[["trk"]][["trkseg"]], "[[", 3), "[", 1))))
#       latitude  <- c(latitude, this_lat)
#       
#       nseg <- length(this_lat)
#       
#       name      <- c(name, rep(as.character(xmlToDataFrame(data)$name), nseg))
#       file      <- c(file, rep(files[f], nseg)) 
#       index     <- c(index, rep(k, nseg))
#       
#       
#       longitude <- c(longitude, as.numeric(unname(unlist(lapply(lapply(curr_route[["trk"]][["trkseg"]], "[[", 3), "[", 2)))))
#       elevation <- c(elevation, as.numeric(unname(unlist(lapply(curr_route[["trk"]][["trkseg"]], "[[", 1)))))
#       time      <- c(time, as.POSIXct(as.character(unname(unlist(lapply(curr_route[["trk"]][["trkseg"]], "[[", 2)))), format="%Y-%m-%dT%H:%M:%SZ"))
#       # latitude  <- c(latitude, as.numeric(xml_data[["trk"]][["trkseg"]][["trkpt"]][[".attrs"]][["lat"]]))
#       # longitude <- c(longitude, as.numeric(xml_data[["trk"]][["trkseg"]][["trkpt"]][[".attrs"]][["lon"]]))
#       # elevation <- c(elevation, as.numeric(xml_data[["trk"]][["trkseg"]][["trkpt"]][["ele"]]))
#       # time      <- c(time, as.POSIXct(xml_data[["trk"]][["trkseg"]][["trkpt"]][["time"]], format="%Y-%m-%dT%H:%M:%SZ"))
#       
#     # }
#   }
#   routes <- data.frame(index, name, time, latitude, longitude, elevation, file, stringsAsFactors=FALSE)
#   # routes <- data.frame(cbind(index, latitude, longitude, file))
#   
#   # Because the routes dataframe takes a while to generate for some folks - save it!
#   save(routes, file=file.path("./data","routes.Rdata"))
# } else {
#   # Use to load as needed.
#   load(file.path("./data","routes.Rdata"))
# }

# Fix data types
# routes$file      <- as.character(routes$file)
# routes$latitude  <- as.numeric(levels(routes$latitude)[routes$latitude])
# routes$longitude <- as.numeric(levels(routes$longitude)[routes$longitude])
# routes           <- transform(routes, index=as.numeric(index))

# # Load Meta Data
# meta_data <- read.csv(file.path("./data","cardioActivities.csv"), stringsAsFactors=FALSE)
# meta_data %<>% mutate(file=paste0("./data/",GPX.File), GPX.File=NULL)
# 
# # Bind routes
# routes_ext <- left_join(routes, meta_data, by="file") %>% arrange(index)




# ## separate the cities
# routes_hobart <- routes %>% filter(index %in% c(184,185,186))
# routes_trento <- routes %>% filter(index==400)
# routes_nuri   <- routes %>% filter(index %in% c(72:80))
# routes_falls  <- routes %>% filter(index==454)
# routes_cycle  <- routes %>% filter(Type=="Cycling")
# routes_toconf <- routes %>% filter(index==380)
# routes %<>% filter(Type=="Walking" & !index %in% c(46,72:80,184:186,380,400,454))

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
# for (r in 1:2) {
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
  
  if(!file.exists(file.path(".",sprintf("%d-%s-map.Rdata", r, locName)))) {
    thisMap <- get_map(location=rev(center),
                       color = "color",
                       source = "google",
                       maptype = "roadmap",
                       zoom = zoom)
    save(thisMap, file=file.path(".",sprintf("%d-%s-map.Rdata", r, locName)))
  } else {C
    load(file=file.path(".",sprintf("%d-%s-map.Rdata", r, locName)))
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





### plot routes within x km of geocode

Adelaide <- geocode("Adelaide, Australia")

within_R <- 5
zoom <- 13
# routes_Adl <- routes %>% filter(acos(sin(latitude)*sin(Adelaide$lat) + cos(latitude)*cos(Adelaide$lat)*cos(Adelaide$lon-longitude)) * 6371L < within_R)
# routes_Adl <- routes %>% filter(acos(sin(Adelaide$lat*pi/180)*sin(latitude*pi/180) + cos(Adelaide$lat*pi/180)*cos(latitude*pi/180)*cos(longitude*pi/180-Adelaide$lon*pi/180)) * 6371L < within_R)
# routes_Adl <- routes %>% mutate(dist=geosphere::distHaversine(Adelaide, data.frame(longitude, latitude))) %>% filter(dist < 1000L*within_R)

## search along latitude for a point at dist R [m] from the centre
radfunLAT <- function(p, r, centre) {
  return(( geosphere::distHaversine(centre, c(centre$lon, p)) - r)^2)
}
radfunLON <- function(p, r, centre) {
  return((geosphere::distHaversine(centre, c(p, centre$lat)) - r)^2)
}
rsolvedLAT <- optimise(radfunLAT, interval=c(-90,90), within_R*1000L, Adelaide)
rsolvedLON <- optimise(radfunLON, interval=c(-180,180), within_R*1000L, Adelaide)

# g <- g + annotate("path",
#                   x=c(Adelaide$lon-abs(rsolvedLON$minimum - Adelaide$lon),Adelaide$lon+abs(rsolvedLON$minimum - Adelaide$lon)),
#                   y=c(Adelaide$lat-abs(rsolvedLAT$minimum - Adelaide$lat),Adelaide$lat-abs(rsolvedLAT$minimum - Adelaide$lat)))
# g <- g + annotate("path",
#                   x=c(Adelaide$lon-abs(rsolvedLON$minimum - Adelaide$lon),Adelaide$lon+abs(rsolvedLON$minimum - Adelaide$lon)),
#                   y=c(Adelaide$lat+abs(rsolvedLAT$minimum - Adelaide$lat),Adelaide$lat+abs(rsolvedLAT$minimum - Adelaide$lat)))
# g <- g + annotate("path",
#                   x=c(Adelaide$lon-abs(rsolvedLON$minimum - Adelaide$lon),Adelaide$lon-abs(rsolvedLON$minimum - Adelaide$lon)),
#                   y=c(Adelaide$lat-abs(rsolvedLAT$minimum - Adelaide$lat),Adelaide$lat+abs(rsolvedLAT$minimum - Adelaide$lat)))
# g <- g + annotate("path",
#                   x=c(Adelaide$lon+abs(rsolvedLON$minimum - Adelaide$lon),Adelaide$lon+abs(rsolvedLON$minimum - Adelaide$lon)),
#                   y=c(Adelaide$lat-abs(rsolvedLAT$minimum - Adelaide$lat),Adelaide$lat+abs(rsolvedLAT$minimum - Adelaide$lat)))

# my.shape <- matrix(runif(10), 5)
my.shape <- data.frame(x=c(Adelaide$lon-abs(rsolvedLON$minimum - Adelaide$lon),Adelaide$lon+abs(rsolvedLON$minimum - Adelaide$lon)),
                       y=c(Adelaide$lat-abs(rsolvedLAT$minimum - Adelaide$lat),Adelaide$lat+abs(rsolvedLAT$minimum - Adelaide$lat)))
my.points <- routes %>% select(longitude, latitude)
routes_Adl <- routes %>% mutate_(inside=1:nrow(my.points) %in% inpip(my.points, my.shape)) %>% filter(isTRUE(inside))

routes_Adl <- routes %>% filter(longitude > my.shape$x[1] & longitude < my.shape$x[2] &
                                latitude > my.shape$y[1] & latitude < my.shape$y[2])


## find points on the circle at that radius


routeIds <- unique(routes_Adl$index)

# Map the projected points
png(sprintf("%d-%s-all-map.png", r, "Adelaide"), width=1280, height=1280)

lat <- range(routes_Adl$latitude)  
lon <- range(routes_Adl$longitude) 
center = c(mean(lon), mean(lat))
# center <- Adelaide

if(!file.exists(file.path(".",sprintf("%d-%s-map.Rdata", r, "Adelaide")))) {
  thisMap <- get_map(location=center,
                     color = "color",
                     source = "google",
                     maptype = "roadmap",
                     zoom = zoom)
  save(thisMap, file=file.path(".",sprintf("%d-%s-map.Rdata", r, "Adelaide")))
} else {
  load(file=file.path(".",sprintf("%d-%s-map.Rdata", r, "Adelaide")))
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
  currRoute <- subset(routes_Adl, index==i)
  g <- g + geom_path(aes(y=latitude, x=longitude), color=sample(brewer.pal(11, "Spectral"), 1), data=currRoute, lwd=0.6)
}

polygon <- expand.grid(my.shape)

g <- g + geom_point(data=polygon, aes(x=x, y=y), size=2)
g <- g + geom_path(data=polygon %>% filter(x==min(x)), aes(x=x, y=y))
g <- g + geom_path(data=polygon %>% filter(x==max(x)), aes(x=x, y=y))
g <- g + geom_path(data=polygon %>% filter(y==min(y)), aes(x=x, y=y))
g <- g + geom_path(data=polygon %>% filter(y==max(y)), aes(x=x, y=y))
                  
                  # x=Adelaide$lon+abs(rsolvedLON$minimum - Adelaide$lon)*cos(seq(0,2*pi,length.out=100)),
                  # y=Adelaide$lat+abs(rsolvedLAT$minimum - Adelaide$lat)*sin(seq(0,2*pi,length.out=100)))
# g <- g + annotate("path",
#                   x=Adelaide$lon+abs(rsolved$minimum - Adelaide$lat)*cos(seq(0,2*pi,length.out=100)),
#                   y=Adelaide$lat+abs(rsolved$minimum - Adelaide$lat)*sin(seq(0,2*pi,length.out=100)))

print(g)


dev.off()




## convert routes to a SpatialLinesDataFrame
## https://rpubs.com/walkerke/points_to_line
library(sp)
library(maptools)

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

routes_lines <- points_to_line(data = routes, 
                               long = "longitude", 
                               lat = "latitude", 
                               id_field = "index")

save(routes_lines, file="./data/routes_lines.rds")


Adelaide <- structure(c(34.929, 138.600972222222), .Names = c("lat", "lon"))

library(leaflet)
leaflet(data = routes_lines) %>%
  addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png", 
           attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
  setView(lng=-Adelaide[1], lat=Adelaide[2], zoom = 11) %>%
  addPolylines(color="red", weight=5)


cols <- c("steelblue", "palegreen", "yellow", "orange", "red", "purple")

map <-  leaflet(data=routes_lines)
# map <- addTiles(map, 
#                 urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png", 
#                 attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')
map <- addProviderTiles(map, "CartoDB.Positron")
map <- setView(map, lng=-Adelaide[1], lat=Adelaide[2], zoom = 11)
for(group in 1:length(routes_lines)){
  # for(rt in 1:length(routes_lines@lines[[group]]@Lines))
  map <- addPolylines(map, lng=~longitude, lat=~latitude, 
                      data=data.frame(routes_lines@lines[[group]]@Lines[[1]]@coords), color=sample(cols, 1),
                      popup=routes_lines@lines[[group]]@ID)
}
map


## sample data: line lengths
library(rgeos)
df <- data.frame(len = sapply(1:length(routes_lines), function(i) gLength(routes_lines[i, ])))
rownames(df) <- sapply(1:length(routes_lines), function(i) routes_lines@lines[[i]]@ID)


## SpatialLines to SpatialLinesDataFrame
routes_sldf <- SpatialLinesDataFrame(routes_lines, data = df)
proj4string(routes_sldf) <- CRS("+proj=longlat +init=epsg:3113 +lat_0=-28 +lon_0=153 +k=0.99999 +x_0=50000 +y_0=100000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0
+units=m +no_defs")
save(routes_sldf, file="./example/leaflet_test/data/routes_sldf.rds")
