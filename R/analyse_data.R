summarise_runs <- function(rundata, by=NULL, dashboard=TRUE) {
  
  numcols <- sapply(rundata, is.numeric)
  numcols["Date"] <- TRUE
  
  .data <- rundata[numcols] %>% select(-latitude, -longitude) %>% unique
  
  .data$monthBin <- as.character(cut(as.Date(.data$Date), breaks="month"))
  
  if(is.null(by)) by <- "index"
  
  mins  <- .data %>% group_by_(by) %>% summarise_each(funs(min(., na.rm=TRUE)),    Duration..seconds., Distance..km., Calories.Burned, Climb..m.)
  means <- .data %>% group_by_(by) %>% summarise_each(funs(mean(., na.rm = TRUE)), Duration..seconds., Distance..km., Calories.Burned, Climb..m.)
  maxs  <- .data %>% group_by_(by) %>% summarise_each(funs(max(., na.rm = TRUE)),  Duration..seconds., Distance..km., Calories.Burned, Climb..m.)
  sums  <- .data %>% group_by_(by) %>% summarise_each(funs(sum(., na.rm = TRUE)),  Duration..seconds., Distance..km., Calories.Burned, Climb..m.)
  
  if(by!="index") {
    message("min:")
    print(data.frame(mins))
    message("mean:")
    print(data.frame(means))
    message("max:")
    print(data.frame(maxs))
    message("total:")
    print(data.frame(sums))
  }
  
  if(dashboard) {
    
    .data_sum      <- .data %>% select(index, monthBin, Duration..seconds., Distance..km., Calories.Burned, Climb..m.) %>% group_by(monthBin) %>% summarise_each(funs(sum)) 
    .data_long     <- .data_sum %>% ungroup %>% select(-index) %>% ungroup %>% gather(QUANTITY, VALUE, -monthBin)
    gg <- ggplot(.data_long, aes(x=as.Date(monthBin), y=VALUE)) 
    gg <- gg + geom_bar(stat="identity", fill="steelblue1") 
    gg <- gg + facet_wrap(~QUANTITY, scales="free_y") 
    gg <- gg + theme_bw()
    gg <- gg + labs(title="Runkeeper Data", subtitle=paste0(length(unique(rts$index)), " records processed"), x="YYYY-MM", y="Value")
    gg <- gg + scale_x_date(date_breaks="1 month")
    gg <- gg + theme(axis.text.x=element_text(angle=90, hjust=0))
    print(gg)
    
  }
  
}