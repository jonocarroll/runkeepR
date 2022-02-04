#' Plot Runkeeper(TM) route data using ggplot2
#' 
#' This function performs several tasks to produce a layered map of routes. A map, centered 
#' on the median lat/lon (most common location in the filtered data) at the requested zoom level 
#' is obtained from Google Maps via the maps API. Routes are then added one by one in 
#' distinct colours.
#'
#' @param routes_all loaded and processed routes
#' @param filterType classification of routes to filter by (must be an existing type in the routes)
#' @param trackPal RColorBrewer palette name
#' @param center where to center the map. Defaults to the median lat/lon from the filtered data.
#' @param zoom zoom level (1:20; smaller number is tighter zoom)
#'
#' @return NULL (plots a ggplot map)
#' 
#' @importFrom grDevices rainbow
#' @importFrom stats median time
#' @import     ggplot2
#' @importFrom ggmap geocode ggmap get_map
#' @import     dplyr
#'  
#'
#' @examples
#' \dontrun{plot_ggplot(routes, filterType="Walking", trackPal=rainbow(8), zoom=14)}
#' 
#' @export
plot_ggplot <- function(routes_all, filterType=NULL, trackPal=rainbow(7), center=NULL, zoom=13) {
  
  # Adelaide <- geocode("Adelaide, Australia")
  
  # within_R <- 5
  
  ## update defaults
  # latString <- "latitude"
  # lonString <- "longitude" 
  # idString  <- "trkname"
  
  ntracks <- length(unique(routes_all$trkname))
  if(ntracks > 1) {
    # idString  <- "trkname"
    message(paste0(ntracks," tracks loaded."))
  } else if(ntracks==1) {
    message("Single track loaded.")
  }
  
  ## filter by type if requested
  if(!is.null(filterType)) {
    if(filterType %in% unique(routes_all$Type)) {
      routes_filtered <- routes_all %>% filter_(~Type==filterType)
    } else {
      stop("filterType must be a Type classification in the data.")
    }
  } else {
    routes_filtered <- routes_all
  }
  
  if(nrow(routes_filtered) == 0) stop("no routes remaining in selection.")
  
  
  if(is.null(center)) {
    # lat    <- median(routes_filtered[[latString]])  
    lat    <- median(routes_filtered$latitude)  
    # lon    <- median(routes_filtered[[lonString]]) 
    lon    <- median(routes_filtered$longitude) 
    center <- c(lon, lat)
  } else {
    center   <- geocode(center)
  }

  thisMap <- get_map(location = center,
                     color    = "color",
                     source   = "google",
                     maptype  = "roadmap",
                     zoom     = zoom)

  # my.points <- routes_filtered %>% select(y=get(latString), x=get(lonString))
  my.points <- routes_filtered %>% select(y="latitude", x="longitude")
  my.shape  <- attr(thisMap, "bb")
  my.shape.df <- with(my.shape, data.frame(x=c(ll.lon, ur.lon), y=c(ll.lat, ur.lat)))

  `%between%` <- function(x, rng) x > rng[1] & x < rng[2]
  
  routes_box <- routes_filtered[routes_filtered$latitude %between% range(my.shape.df$y) & 
                                  routes_filtered$longitude %between% range(my.shape.df$x), ]
  # routes_box <- routes_filtered[routes_filtered[[latString]] %between% range(my.shape.df$y) & 
  #                                 routes_filtered[[lonString]] %between% range(my.shape.df$x), ]
  g <- ggmap(thisMap,
             extent = "device",
             ylab   = "Latitude",
             xlab   = "Longitude") + 
    theme(legend.position="none")
  
  g <- g + annotate("rect", 
                    xmin=attr(thisMap, "bb")$ll.lon, 
                    xmax=attr(thisMap, "bb")$ur.lon, 
                    ymin=attr(thisMap, "bb")$ll.lat, 
                    ymax=attr(thisMap, "bb")$ur.lat, 
                    fill="white", alpha=0.65)
  
  # cols <- c("steelblue", "palegreen", "yellow", "orange", "red", "purple")
  
  # routeIds <- unique(routes_box[[idString]])
  routeIds <- unique(routes_box$trkname)
  
  factpal <- colorFactor(trackPal, domain=routeIds)
    
  for (i in routeIds) {
    currRoute <- dplyr::filter(routes_box[, c("latitude", "longitude", "trkname")], trkname==i)
      # g <- g + geom_path(aes(y=get(latString), x=get(lonString), group=1), color=factpal(i), data=currRoute, lwd=0.6)
      g <- g + geom_path(aes_(y=~latitude, x=~longitude, group=1), color=factpal(i), data=currRoute, lwd=0.6)
  }
  
  # polygon <- expand.grid(my.shape)
  
  # g <- g + geom_point(data=polygon, aes(x=x, y=y), size=2)
  # g <- g + geom_path(data=polygon %>% filter(x==min(x)), aes(x=x, y=y))
  # g <- g + geom_path(data=polygon %>% filter(x==max(x)), aes(x=x, y=y))
  # g <- g + geom_path(data=polygon %>% filter(y==min(y)), aes(x=x, y=y))
  # g <- g + geom_path(data=polygon %>% filter(y==max(y)), aes(x=x, y=y))
  
  # x=Adelaide$lon+abs(rsolvedLON$minimum - Adelaide$lon)*cos(seq(0,2*pi,length.out=100)),  routeIds <- unique(routes_all[[idString]])
  # routeIds <- unique(routes_filtered[[idString]])
  routeIds <- unique(routes_filtered$trkname)
  
  # y=Adelaide$lat+abs(rsolvedLAT$minimum - Adelaide$lat)*sin(seq(0,2*pi,length.out=100)))
  # g <- g + annotate("path",
  #                   x=Adelaide$lon+abs(rsolved$minimum - Adelaide$lat)*cos(seq(0,2*pi,length.out=100)),
  #                   y=Adelaide$lat+abs(rsolved$minimum - Adelaide$lat)*sin(seq(0,2*pi,length.out=100)))
  
  print(g)
  
  return(invisible(NULL))
  # dev.off()
  
}