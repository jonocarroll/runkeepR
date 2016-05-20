summarise_runs <- function(rundata, by="trkname", dashboard=TRUE) {
  
  rundata$yday <- yday(rundata$Date)
  
  numcols <- sapply(rundata, is.numeric)
  numcols["Date"] <- TRUE
  numcols[by]     <- TRUE
  
  numdata <- rundata[numcols] %>% select(-latitude, -longitude) %>% unique
  
  numdata$monthBin <- as.character(cut(as.Date(numdata$Date), breaks="month"))
  
  # if(is.null(by)) by <- "trkname"
  
  mins  <- numdata %>% group_by_(by) %>% summarise_each(funs(min(., na.rm=TRUE)),    Duration..seconds., Distance..km., Calories.Burned, Climb..m., elevation)
  means <- numdata %>% group_by_(by) %>% summarise_each(funs(mean(., na.rm = TRUE)), Duration..seconds., Distance..km., Calories.Burned, Climb..m., elevation)
  maxs  <- numdata %>% group_by_(by) %>% summarise_each(funs(max(., na.rm = TRUE)),  Duration..seconds., Distance..km., Calories.Burned, Climb..m., elevation)
  sums  <- numdata %>% group_by_(by) %>% summarise_each(funs(sum(., na.rm = TRUE)),  Duration..seconds., Distance..km., Calories.Burned, Climb..m., elevation)
  
  if(by!="trkname") {
    message("min:")
    print(data.frame(mins))
    message("mean:")
    print(data.frame(means))
    message("max:")
    print(data.frame(maxs))
    message("total:")
    print(data.frame(sums))
  }
  
  if(!dashboard) {
    
    numdata_sum      <- numdata %>% select(monthBin, Duration..seconds., Distance..km., Calories.Burned, Climb..m., elevation) %>% group_by(monthBin) %>% summarise_each(funs(sum)) 
    numdata_long     <- numdata_sum %>% ungroup %>% ungroup %>% gather(QUANTITY, VALUE, -monthBin)
    gg <- ggplot(numdata_long, aes(x=as.Date(monthBin), y=VALUE)) 
    gg <- gg + geom_bar(stat="identity", fill="steelblue1") 
    gg <- gg + facet_wrap(~QUANTITY, scales="free_y") 
    gg <- gg + theme_bw()
    gg <- gg + labs(title="Runkeeper Data", subtitle=paste0(nrow(rundata), " records processed"), x="YYYY-MM", y="Value")
    gg <- gg + scale_x_date(date_breaks="1 month")
    gg <- gg + theme(axis.text.x=element_text(angle=90, hjust=0))
    print(gg)
    
  } else {
    
    ## app.R ##
    # library(shinydashboard)
    
    ui <- dashboardPage(
      dashboardHeader(title = "runkeepR Data"),
      dashboardSidebar(),
      dashboardBody(
        # shinyjs::useShinyjs(),
        # actionButton("showSidebar", "Show sidebar"),
        # actionButton("hideSidebar", "Hide sidebar"),
        # Boxes need to be put in a row (or column)
        fluidRow(box(plotOutput("plot1"), width=12)),
        
        fluidRow(
          box(title = "Year",
              sliderInput("slider", "Year:", min(unique(rundata$Year)), max(unique(rundata$Year)), 1, animate=TRUE), width=4
          ),
          box(title = "Function",
              radioButtons("fn", "Function:", c("sum", "mean", "max"), selected="mean"), width=4
          ),
          box(title = "Window",
              radioButtons("window", "Window:", c("daily", "monthly"), selected="monthly"), width=4
          )
          
          
        )
        
      )
    )
    
    server <- function(input, output, session) {
      
      library(magrittr)
      library(dplyr)
      library(magrittr)
      library(tidyr)
      library(shiny)
      library(ggplot2)
      # library(shinyjs)
      
      # observeEvent(input$showSidebar, {
      #   shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      # })
      # observeEvent(input$hideSidebar, {
      #   shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      # })
      
      output$plot1 <- renderPlot({
        if(input$window=="monthly") {
          numdata_sum      <- numdata %>% filter(Year==input$slider) %>% select(monthBin, Duration..seconds., Distance..km., Calories.Burned, Climb..m., elevation) %>% group_by(monthBin) %>% summarise_each(funs_(input$fn))
          numdata_long     <- numdata_sum %>% ungroup %>% ungroup %>% gather(QUANTITY, VALUE, -monthBin)
          gg <- ggplot(numdata_long, aes(x=as.Date(monthBin), y=VALUE)) 
          gg <- gg + scale_x_date(date_breaks="1 month", date_labels="%B")
          gg <- gg + labs(title=paste0(input$slider," Runkeeper Data"), subtitle=paste0(nrow(rundata %>% filter(Year==input$slider)), " records processed"), x="Month", y=paste0(input$fn,"(Value)"))
        } else if(input$window=="daily") {
          numdata_sum      <- numdata %>% filter(Year==input$slider) %>% select(yday, Duration..seconds., Distance..km., Calories.Burned, Climb..m., elevation) %>% group_by(yday) %>% summarise_each(funs_(input$fn))
          numdata_long     <- numdata_sum %>% ungroup %>% ungroup %>% gather(QUANTITY, VALUE, -yday)
          gg <- ggplot(numdata_long, aes(x=yday, y=VALUE)) 
          gg <- gg + xlim(c(0,365))
          gg <- gg + labs(title=paste0(input$slider," Runkeeper Data"), subtitle=paste0(nrow(rundata %>% filter(Year==input$slider)), " records processed"), x="Day of Year", y=paste0(input$fn,"(Value)"))
          # gg <- gg + scale_x_date(date_breaks="1 month")
        }
        gg <- gg + geom_bar(stat="identity", fill="steelblue1") 
        gg <- gg + facet_wrap(~QUANTITY, scales="free_y") 
        gg <- gg + theme_bw()
        gg <- gg + theme(axis.text.x=element_text(angle=90, hjust=0))
        print(gg)
      })
    }
    
    shinyApp(ui, server)
    
  }
  
}