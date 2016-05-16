load_runs <- function(rundir) {
  
  # if(!file.exists(file.path("./data","routes.Rdata"))) {
    
    # https://gist.github.com/danielecook/6c555937144d4955073b
    
    # GPX files downloaded from Runkeeper
    files <- dir(file.path(rundir), pattern="\\.gpx", full.names=TRUE)
    # files <- dir(file.path("./data"), pattern="\\.gpx", full.names=TRUE)
    
    message("Loading files:")
    print(files)
    
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
    # save(routes, file=file.path("./data","routes.Rdata"))
  # } else {
  #   # Use to load as needed.
  #   load(file.path("./data","routes.Rdata"))
  # }
    
    return(routes)
  
}