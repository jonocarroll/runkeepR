### plot routes within x km of geocode

plot_ggplot <- function(routes_all, center=NULL, latString=NULL, lonString=NULL, idString=NULL, zoom=13, ...) {
  
  # Adelaide <- geocode("Adelaide, Australia")
  
  # within_R <- 5
  
  ## update defaults
  latString <- ifelse(is.null(latString), "latitude",  latString) 
  lonString <- ifelse(is.null(lonString), "longitude", lonString) 
  idString  <- ifelse(is.null(idString),  "trkname",   idString)
  
  # routes_all <- routes %>% filter(acos(sin(latitude)*sin(Adelaide$lat) + cos(latitude)*cos(Adelaide$lat)*cos(Adelaide$lon-longitude)) * 6371L < within_R)
  # routes_all <- routes %>% filter(acos(sin(Adelaide$lat*pi/180)*sin(latitude*pi/180) + cos(Adelaide$lat*pi/180)*cos(latitude*pi/180)*cos(longitude*pi/180-Adelaide$lon*pi/180)) * 6371L < within_R)
  # routes_all <- routes %>% mutate(dist=geosphere::distHaversine(Adelaide, data.frame(longitude, latitude))) %>% filter(dist < 1000L*within_R)
  
  # ## search along latitude for a point at dist R [m] from the centre
  # radfunLAT <- function(p, r, centre) {
  #   return(( geosphere::distHaversine(centre, c(centre$lon, p)) - r)^2)
  # }
  # radfunLON <- function(p, r, centre) {
  #   return((geosphere::distHaversine(centre, c(p, centre$lat)) - r)^2)
  # }
  # rsolvedLAT <- optimise(radfunLAT, interval=c(-90,90), within_R*1000L, Adelaide)
  # rsolvedLON <- optimise(radfunLON, interval=c(-180,180), within_R*1000L, Adelaide)
  
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
  # my.shape <- data.frame(x=c(Adelaide$lon-abs(rsolvedLON$minimum - Adelaide$lon),Adelaide$lon+abs(rsolvedLON$minimum - Adelaide$lon)),
  #                        y=c(Adelaide$lat-abs(rsolvedLAT$minimum - Adelaide$lat),Adelaide$lat+abs(rsolvedLAT$minimum - Adelaide$lat)))
  # my.points <- routes %>% select(longitude, latitude)
  # routes_all <- routes %>% mutate_(inside=1:nrow(my.points) %in% inpip(my.points, my.shape)) %>% filter(isTRUE(inside))
  # 
  # routes_all <- routes %>% filter(longitude > my.shape$x[1] & longitude < my.shape$x[2] &
  #                                   latitude > my.shape$y[1] & latitude < my.shape$y[2])
  # 
  

  
  ## find points on the circle at that radius
  
  
  # Map the projected points
  # png(sprintf("%d-%s-all-map.png", r, "Adelaide"), width=1280, height=1280)
  if(is.null(center)) {
    lat    <- median(routes_all[[latString]])  
    lon    <- median(routes_all[[lonString]]) 
    center <- c(lon, lat)
  } else {
    center   <- geocode(center)
  }
  # center <- Adelaide
  
  # if(!file.exists(file.path(".",sprintf("%d-%s-map.Rdata", r, "Adelaide")))) {
  thisMap <- get_map(location = center,
                     color    = "color",
                     source   = "google",
                     maptype  = "roadmap",
                     zoom     = zoom)
  # save(thisMap, file=file.path(".",sprintf("%d-%s-map.Rdata", r, "Adelaide")))
  # } else {
  # load(file=file.path(".",sprintf("%d-%s-map.Rdata", r, "Adelaide")))
  # }

  my.points <- routes_all %>% select(y=get(latString), x=get(lonString))
  my.shape  <- attr(thisMap, "bb")
  my.shape.df <- with(my.shape, data.frame(x=c(ll.lon, ur.lon), y=c(ll.lat, ur.lat)))
  # routes_box <- routes_all %>% mutate_(inside=1:nrow(my.points) %in% inpip(my.points, my.shape.df)) %>% filter(isTRUE(inside))
  
  `%between%`<-function(x,rng) x>rng[1] & x<rng[2]
  
  routes_box <- routes_all[routes_all[[latString]] %between% range(my.shape.df$y) & routes_all[[lonString]] %between% range(my.shape.df$x), ]
  
    
  g <- ggmap(thisMap,
             extent = "device",
             ylab   = "Latitude",
             xlab   = "Longitude") + 
    # labs(title=locName) + 
    theme(legend.position="none")
  
  g <- g + annotate("rect", 
                    xmin=attr(thisMap, "bb")$ll.lon, 
                    xmax=attr(thisMap, "bb")$ur.lon, 
                    ymin=attr(thisMap, "bb")$ll.lat, 
                    ymax=attr(thisMap, "bb")$ur.lat, 
                    fill="white", alpha=0.65)
  
  cols <- c("steelblue", "palegreen", "yellow", "orange", "red", "purple")

  routeIds <- unique(routes_box[[idString]])
    
  for (i in routeIds) {
    currRoute <- subset(routes_box, get(idString)==i)
      g <- g + geom_path(aes(y=get(latString), x=get(lonString), group=1), color=sample(cols, 1), data=currRoute, lwd=0.6)
  }
  
  # polygon <- expand.grid(my.shape)
  
  # g <- g + geom_point(data=polygon, aes(x=x, y=y), size=2)
  # g <- g + geom_path(data=polygon %>% filter(x==min(x)), aes(x=x, y=y))
  # g <- g + geom_path(data=polygon %>% filter(x==max(x)), aes(x=x, y=y))
  # g <- g + geom_path(data=polygon %>% filter(y==min(y)), aes(x=x, y=y))
  # g <- g + geom_path(data=polygon %>% filter(y==max(y)), aes(x=x, y=y))
  
  # x=Adelaide$lon+abs(rsolvedLON$minimum - Adelaide$lon)*cos(seq(0,2*pi,length.out=100)),  routeIds <- unique(routes_all[[idString]])
  routeIds <- unique(routes_all[[idString]])
  
  # y=Adelaide$lat+abs(rsolvedLAT$minimum - Adelaide$lat)*sin(seq(0,2*pi,length.out=100)))
  # g <- g + annotate("path",
  #                   x=Adelaide$lon+abs(rsolved$minimum - Adelaide$lat)*cos(seq(0,2*pi,length.out=100)),
  #                   y=Adelaide$lat+abs(rsolved$minimum - Adelaide$lat)*sin(seq(0,2*pi,length.out=100)))
  
  print(g)
  
  
  # dev.off()
  
}