
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

titlePanel("Runkeeper Data")

shinyUI(fluidPage(
  
  sliderInput("overplot", "overplot_factor", min = -0.5, max = 1, step= 0.05, value = 0.2),
  leafletOutput("plot")

))
