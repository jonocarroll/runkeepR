#' Read a Runkeeper(TM) \code{.gpx} file into a data.frame
#' 
#' Raw read of a single \code{.gpx} file. Only applicable to 
#' Runkeeper(TM) \code{.gpx} files. Exported in case it is of use.
#' 
#' For a more general use version see \code{plotKML::readGPx} which fails to 
#' read in elevation from Runkeeper(TM) files.
#' 
#' @param gpxfile name of the .gpx file to process
#'
#' @return data.frame of track details containing all available details for the tracked routes  
#' with additional class \code{runkeepR_data}
#' 
#' @import XML
#' 
#'
#' @examples
#' ## load test data distributed with this package
#' ## test data is a single track; 
#' ## a hike around Anstey Hill, Adelaide, South Australia
#' ## https://en.wikipedia.org/wiki/Anstey_Hill_Recreation_Park
#' 
#' library(runkeepR)
#' routes <- load_tracks(system.file("extdata", package = "runkeepR"))
#' class(routes) 
#' ## [1] "runkeepR_data" "data.frame"  
#'
#' @export
read_RK_GPX <- function(gpxfile) {
  
  ret     <- xmlTreeParse(gpxfile, useInternalNodes = TRUE)
  top     <- xmlRoot(ret)  
  trkname <- xmlValue(top[[1]][[1]])
  trkdesc <- as.POSIXct(xmlValue(top[[1]][[2]]), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  ## (length - 2) because we're subtracting name and description
  ntracks <- length(xmlChildren(top[[1]])) - 2 
  
  longitude <- c()
  latitude  <- c()
  trackid   <- c()
  elevation <- c()
  time      <- as.POSIXct(c())
  
  for (itrack in (2 + seq_len(ntracks))) {  
    top1track     <- top[[1]][[itrack]]
    longitude     <- c(longitude, as.numeric(xmlSApply(top1track, xmlGetAttr, "lon")))
    this_latitude <- as.numeric(xmlSApply(top1track, xmlGetAttr, "lat"))
    latitude      <- c(latitude, this_latitude)
    trackid       <- c(trackid, rep(itrack - 2, length(this_latitude)))
    for (ipt in 1:length(xmlChildren(top1track))) {
      elevation <- c(elevation, as.numeric(xmlSApply(top1track[[ipt]], xmlValue)[["ele"]]))
      time      <- c(time, as.POSIXct(xmlSApply(top1track[[ipt]], xmlValue)[["time"]], format = "%Y-%m-%dT%H:%M:%SZ"))
    }
  }
  
  outObject <- data.frame(trackid, trkname, trkdesc, latitude, longitude, elevation, time, gpxfile, stringsAsFactors = FALSE)
  
  # class(outObject) <- c("runkeepR_data", class(outObject))
  
  return(outObject)
}


#' Load Runkeeper(TM) .gpx files into a data.frame
#' 
#' Starting at \code{gpxdir}, loads all \code{.gpx} files and merges them 
#' with CardioActivities.csv to produce a \code{data.frame} of all tracked
#' details.
#'
#' @param gpxdir directory containing .gpx files and cardioActivities.csv extracted from a Runkeeper(TM) zip file.
#' 
#' @section Obtaining your data:
#'
#' You can get a zipped export of your Runkeeper(TM) data from the logged-in settings page 
#' on Runkeeper's website, e.g. runkeeper-data-export-12517482-2016-05-20-1550.zip.
#'
#' \if{html}{\figure{runkeeperexport.png}{options: width="80\%" alt="Figure: runkeeperexport.png"}}
#' \if{latex}{\figure{runkeeperexport.png}{options: width=7cm}}
#'
#' Unzip the contents of this \code{.zip} file to a directory (refer to this as \code{gpxdir}).
#' 
#' @return data.frame of tracked data with additional class \code{runkeepR_data}
#' 
#' @import magrittr
#' @import dplyr
#' @importFrom lubridate year month day
#' @importFrom utils read.csv
#' 
#' @export
#'
#' @examples
#' ## load test data distributed with this package
#' ## test data is a single track; 
#' ## a hike around Anstey Hill, Adelaide, South Australia
#' ## https://en.wikipedia.org/wiki/Anstey_Hill_Recreation_Park
#' 
#' library(runkeepR)
#' routes <- load_tracks(system.file("extdata", package = "runkeepR"))
#' class(routes) 
#' ## [1] "runkeepR_data" "data.frame"  
#'
load_tracks <- function(gpxdir) {
  
  files <- dir(file.path(gpxdir), pattern = "\\.gpx", full.names = TRUE)
  
  routes_list <- lapply(files, read_RK_GPX)
  
  routes <- as.data.frame(do.call(rbind, routes_list), stringsAsFactors = FALSE)  
  
  # save(routes, file="~/Dropbox/Freelancer/runkeepR/example/routes_all.rds")
  
  meta_data <- read.csv(file.path(gpxdir, "cardioActivities.csv"), stringsAsFactors = FALSE)
  # meta_data %<>% mutate_(gpxfile=lazyeval::interp(~ifelse(y=="", NA, paste0(path.expand(gpxdir),"/",y)), y="GPX.File"), "GPX.File"=NULL)
  # meta_data %<>% mutate_(gpxfile = lazyeval::interp(~ifelse(y == "", 
  #                                                           NA, 
  #                                                           paste0(path.expand(gpxdir),"/",y)), 
  #                                                   y = as.name("GPX.File")))
  meta_data %<>%
    dplyr::mutate(gpxfile = dplyr::case_when(.data$GPX.File == "" ~ NA_character_,
                                             TRUE ~ file.path(path.expand(gpxdir), .data$GPX.File)))
  
  # Bind routes
  # routes <- left_join(routes, meta_data, by="gpxfile") %>% arrange(index)
  routes_all <- merge(meta_data, routes, by = "gpxfile")
  
  ## process dates
  routes_all$Date  <- as.POSIXct(routes_all$Date)
  routes_all$Year  <- year(routes_all$Date)
  routes_all$Month <- month(routes_all$Date)
  routes_all$Day   <- day(routes_all$Date)
  
  ## copy durations to total minutes
  routes_all$Duration..seconds. <- unlist(lapply(strsplit(routes_all$Duration, ":"), 
                                                 function(x) {
                                                   x <- as.integer(x)
                                                   if (length(x) == 3) {
                                                     x[1] * 60L * 60L + x[2] * 60L + x[3]
                                                   } else if (length(x) == 2) {
                                                     x[1] * 60L + x[2]
                                                   }
                                                 }))
  
  ## re-arrange, put POSIX fields next to each other
  routes_all %<>% select("gpxfile", "trkname", "trkdesc", "Type", 
                         "trackid", "Date", "Year", "Month", "Day", 
                         "time", "Duration", "Duration..seconds.", 
                         everything())
  
  # class(routes_all) <- c("runkeepR_data", class(routes_all))
  
  return(routes_all)
  
}






# load_runs <- function(gpxdir, save=TRUE, overwrite=FALSE) {
#   
#   if(!file.exists(file.path(gpxdir, "routes.Rdata")) | overwrite) {
#     
#     # https://gist.github.com/danielecook/6c555937144d4955073b
#     
#     # GPX files downloaded from Runkeeper
#     files <- dir(file.path(gpxdir), pattern="\\.gpx", full.names=TRUE)
#     # files <- dir(file.path("./data"), pattern="\\.gpx", full.names=TRUE)
#     
#     message("Loading files:")
#     print(files)
#     
#     # Generate vectors for data frame
#     index     <- c()
#     latitude  <- c()
#     longitude <- c()
#     file      <- c()
#     
#     k <- 1 # Set up Counter
#     
#     # 
#     for (f in 1:length(files)) {
#       curr_route <- readGPX(files[f])
#       
#       # Treat interrupted GPS paths as seperate routes (useful if you occasionally stop running..walk for a bit, and start again like I do.)
#       for (i in curr_route$tracks[[1]]) {
#         k <- k + 1
#         location  <- i
#         file      <- c(file, rep(files[f], dim(location)[1])) 
#         index     <- c(index, rep(k, dim(location)[1]))
#         latitude  <- c(latitude,  location$lat)
#         longitude <- c(longitude, location$lon)
#       }
#     }
#     routes <- data.frame(index, latitude, longitude, file)
#     # routes <- data.frame(cbind(index, latitude, longitude, file))
#     
#     meta_data <- read.csv(file.path(gpxdir, "cardioActivities.csv"), stringsAsFactors=FALSE)
#     meta_data %<>% mutate(file=ifelse(GPX.File=="", "", paste0(path.expand(gpxdir),"/",GPX.File)), GPX.File=NULL)
#     
#     # Bind routes
#     routes <- left_join(routes, meta_data, by="file") %>% arrange(index)
#     
#     ## process dates
#     routes$Date <- as.POSIXct(routes$Date)
#     routes$Year <- year(routes$Date)
#     routes$Month <- month(routes$Date)
#     routes$Day <- day(routes$Date)
#     
#     ## copy durations to total minutes
#     routes$Duration..seconds. <- unlist(lapply(strsplit(routes$Duration, ":"), function(x) {
#       x <- as.integer(x)
#       if(length(x)==3) {
#         x[1]*60L*60L + x[2]*60L + x[3]
#       } else if(length(x)==2) {
#         x[1]*60L + x[2]
#       }
#     }))
#     
#     ## re-arrange, put POSIX fields next to each other
#     routes %<>% select(index:Date, Year, Month, Day, Duration, Duration..seconds., everything())
#     
#     # Because the routes dataframe takes a while to generate for some folks - save it!
#     if(save) save(routes, file=file.path(gpxdir, "routes.Rdata"))
#     
#   } else {
#     # Use to load as needed.
#     load(file.path(gpxdir, "routes.Rdata"))
#   }
#   
#   return(routes)
#   
# }
