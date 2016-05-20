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



plot_leaflet <- function(routes_all, latString=NULL, lonString=NULL, idString=NULL, ...) {
  
  ## update defaults
  latString <- ifelse(is.null(latString), "latitude",  latString) 
  lonString <- ifelse(is.null(lonString), "longitude", lonString) 
  idString  <- ifelse(is.null(idString),  "trkname",   idString) 
  
  ## convert data.frame to SpatialLines
  routes_lines <- points_to_line(data     = routes_all, 
                                 long     = lonString, 
                                 lat      = latString, 
                                 id_field = idString)
  
  ## define some bright rainbow colors
  cols <- c("steelblue", "palegreen", "yellow", "orange", "red", "purple")
  
  ## generate the leaflet plot
  map <- leaflet(data=routes_lines)
  map <- addProviderTiles(map, "CartoDB.Positron")
  map <- setView(map, lng=median(routes_all[[lonString]]), lat=median(routes_all[[latString]]), zoom = 11)
  rtnames <- unique(routes_all[[idString]])
  for(group in 1:length(routes_lines)){
    # map <- addPolylines(map, lng=~longitude, lat=~latitude, 
    map <- addPolylines(map, lng=~get(lonString), lat=~get(latString), 
                        data=data.frame(routes_lines@lines[[group]]@Lines[[1]]@coords), color=sample(cols, 1),
                        popup=rtnames[group], ...)
  }
 
  print(map)
  
}