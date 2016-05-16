load_runs <- function(gpxdir, save=TRUE, overwrite=FALSE) {
  
  if(!file.exists(file.path(gpxdir, "routes.Rdata")) | overwrite) {
    
    # https://gist.github.com/danielecook/6c555937144d4955073b
    
    # GPX files downloaded from Runkeeper
    files <- dir(file.path(gpxdir), pattern="\\.gpx", full.names=TRUE)
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
    
    meta_data <- read.csv(file.path(gpxdir, "cardioActivities.csv"), stringsAsFactors=FALSE)
    meta_data %<>% mutate(file=ifelse(GPX.File=="", "", paste0(path.expand(gpxdir),"/",GPX.File)), GPX.File=NULL)
    
    # Bind routes
    routes <- left_join(routes, meta_data, by="file") %>% arrange(index)
    
    ## process dates
    routes$Date <- as.POSIXct(routes$Date)
    routes$Year <- year(routes$Date)
    routes$Month <- month(routes$Date)
    routes$Day <- day(routes$Date)
    
    ## copy durations to total minutes
    routes$Duration..seconds. <- unlist(lapply(strsplit(routes$Duration, ":"), function(x) {
      x <- as.integer(x)
      if(length(x)==3) {
        x[1]*60L*60L + x[2]*60L + x[3]
      } else if(length(x)==2) {
        x[1]*60L + x[2]
      }
    }))
    
    ## re-arrange, put POSIX fields next to each other
    routes %<>% select(index:Date, Year, Month, Day, Duration, Duration..seconds., everything())
    
    # Because the routes dataframe takes a while to generate for some folks - save it!
    if(save) save(routes, file=file.path(gpxdir, "routes.Rdata"))
    
  } else {
    # Use to load as needed.
    load(file.path(gpxdir, "routes.Rdata"))
  }
  
  return(routes)
  
}