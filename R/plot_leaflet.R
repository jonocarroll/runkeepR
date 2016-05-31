points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  ## courtesy https://rpubs.com/walkerke/points_to_line
  
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




#' Plot the loaded routes using leaflet
#'
#' @param routes_all loaded and processed routes
#' @param filterType classification of routes to filter by (must be an existing type in the routes)
#' @param trackPal RColorBrewer palette name
#' @param ... other options to pass to addPolylines
#'
#' @return NULL (loads a leaflet map)
#' @export
#'
#' @examples
#' \dontrun{
#'    plot_leaflet(routes, filterType="Walking", trackPal="Accent")
#' }
plot_leaflet <- function(routes_all, filterType=NULL, trackPal=rainbow(7), ...) {
  
  ## update defaults
  # latString <- ifelse(is.null(latString), "latitude",  latString) 
  # lonString <- ifelse(is.null(lonString), "longitude", lonString) 
  # idString  <- ifelse(is.null(idString),  "trkname",   idString) 

  if(nrow(routes_all) == 0) stop("no routes loaded.")
  
  latString <- "latitude"
  lonString <- "longitude"
  idString  <- NULL ## default if ntracks == 0
  ntracks <- length(unique(routes_all$trkname))
  if(ntracks > 1) {
    idString  <- "trkname"
    message(paste0(ntracks," tracks loaded."))
  } else if(ntracks==1) {
    message("Single track loaded.")
  }
  
  ## filter by type if requested
  if(!is.null(filterType)) {
    if(filterType %in% unique(routes_all$Type)) {
      routes_filtered <- routes_all %>% filter(Type==filterType)
    } else {
      stop("filterType must be a Type classification in the data.")
    }
  } else {
    routes_filtered <- routes_all
  }
  
  if(nrow(routes_filtered) == 0) stop("no routes remaining in selection.")
  
  ## convert data.frame to SpatialLines
  routes_lines <- points_to_line(data     = routes_filtered, 
                                 long     = lonString, 
                                 lat      = latString, 
                                 id_field = idString)
  
  ## define some bright rainbow colors

  # if(trackPal[1] == "rainbow") {
  #   cols <- rainbow(5)
  #   # cols <- c("blue", "green", "yellow", "orange", "red", "purple")
  # } else {
  #   cols <- trackPal
  # }
    
  # factpal <- colorFactor(topo.colors(5), ntracks)
  factpal <- colorFactor(trackPal, domain=1:ntracks)
  
  # if(trackCol=="rainbow") {
  #   cols 
  # } else {
  #   cols <- c(trackCol)
  # }
  
  ## generate the leaflet plot
  map <- leaflet(data=routes_lines)
  map <- addProviderTiles(map, "CartoDB.Positron")
  map <- setView(map, lng=median(routes_filtered[[lonString]]), lat=median(routes_filtered[[latString]]), zoom = 11)
  # rtnames <- unique(routes_all[[idString]])[order(unique(routes_all$trkname))]
  rtnames <- ifelse(!is.null(idString), sort(unique(routes_filtered[[idString]])), unique(routes_filtered$trkname))
  for(group in 1:length(routes_lines)){
    # map <- addPolylines(map, lng=~longitude, lat=~latitude, 
    map <- addPolylines(map, lng=~get(lonString), lat=~get(latString), 
                        data=data.frame(routes_lines@lines[[group]]@Lines[[1]]@coords), color=~factpal(group),
                        # data=data.frame(routes_lines@lines[[group]]@Lines[[1]]@coords), color=sample(cols, 1),
                        popup=paste0("ID: ",group,"; ",rtnames[group]), ...)
  }
 
  print(map)
  
}