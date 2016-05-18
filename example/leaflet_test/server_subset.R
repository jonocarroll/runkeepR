
  observe({
    print(input$overplot)
  })
  
  state <- reactiveValues(
    last_plot_north = 0,
    last_plot_west = 0,
    last_plot_south = 0,
    last_plot_east = 0,
    last_plot_zoom = 0,
    last_plot_overplot = 0.2
  )
  
needs_new_data <- reactive({
    
    if(is.null(input$plot_bounds$north)  || is.null(input$plot_bounds$north)){
      print("plots bounds is NULL")
      return(FALSE)
      
    } else{  
      #print("last state")
      #print( c(state$last_plot_north, state$last_plot_west,state$last_plot_south,state$last_plot_east) )
      #print("plot bounds")
      #print( c(input$plot_bounds$north,input$plot_bounds$west,input$plot_bounds$south,input$plot_bounds$east))
      
      
      within <- (state$last_plot_north >= input$plot_bounds$north && 
                   state$last_plot_west <= input$plot_bounds$west &&
                   state$last_plot_south <= input$plot_bounds$south &&
                   state$last_plot_east >= input$plot_bounds$east)
      
      zoom_change <- (state$last_plot_zoom != input$plot_zoom)
      
      overplot_change <- (state$last_plot_overplot != input$overplot)
      
      
      if(overplot_change){
        print("overplot change")
        return(TRUE)
      }
      
      if(zoom_change){
        print("zoom change")
        return(TRUE)
      }
      
      if(!within){
        print("not within")
        return(TRUE)
      }
      
      return(FALSE)
      
      #if( overplot_change || zoom_change || !within){
      #  return(TRUE)
      #}
    }

  })
  
  plot_data <- reactive({
    
    #if(needs_new_data)
    
    #print(names(input))
    
    if(is.null(input$plot_bounds$north)){
      
      print("plot_data plotbounds = NULL")
      
      return(NULL)
    }
    
    top=input$plot_bounds$north
    left=input$plot_bounds$west
    bottom=input$plot_bounds$south
    right=input$plot_bounds$east
    zoom = input$plot_zoom
    
    expand_scale <- input$overplot
    width <- (right - left)
    height <- (top - bottom)
    
    left <- left - expand_scale*width
    right <- right + expand_scale*width
    top <- top + expand_scale*height
    bottom <- bottom - expand_scale*height
    
    state$last_plot_north <- top
    state$last_plot_west <- left
    state$last_plot_south <- bottom
    state$last_plot_east <- right
    state$last_plot_zoom <- zoom
    state$last_plot_overplot <- expand_scale
    
    
    g <- geom.subset(spgeom=get_data_for_level(),top=top, left=left, bottom=bottom, right=right)
    print("size of data")
    print(length(g@lines))
    print("s.o.d. done")
    return(g)
    
  })
  
  get_data_for_level <- reactive({
    # return(bikeways_simple[[as.integer(input$plot_zoom)]])
    return(routes_simple[[as.integer(input$plot_zoom)]])
  })
  
  
  output$plot <- renderLeaflet  ({
    
    print("rendering leaflet")
    leaflet() %>% addTiles() %>% setView(lng = 153.0292, lat= -27.47228, zoom = 15)
    
  })
  
  observe({
    if (needs_new_data()){

      data.plot <- plot_data()
      if (is.null(data.plot)) {return()}
      
      print("updating plot")
      
      #print(data.plot)
      #removeLayer
      
      
      
      #leafletProxy("plot")%>%clearGroup( "map_group")
      
      leafletProxy("plot")%>%clearGroup( "map_group") %>% addPolylines(data=data.plot, group = "map_group")
    } 
    else {
      print("no data update required") 
    }
  })

