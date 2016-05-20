[![Linux/Mac Travis Build Status](https://img.shields.io/travis/jonocarroll/runkeepR/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/jonocarroll/runkeepR)
[![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jonocarroll/runkeepR/master.svg?label=Windows)](https://ci.appveyor.com/project/jonocarroll/runkeepR)
[![Coverage Status](https://codecov.io/gh/jonocarroll/runkeepR/branch/master/graph/badge.svg)](https://codecov.io/gh/jonocarroll/runkeepR)

# <img src="https://d2b4ufapzmnxpw.cloudfront.net/build/13556/static/web/images/rk_app_logo_blue_150x142.png"> runkeepR

Extract, plot, and analyse Runkeeper(TM) data.

## Installation:

Assuming the TravisCI badges above are green (_i.e._ the current build is stable) this package can be installed via

    devtools::install_github("jonocarroll/runkeepR")
    
or

    pacman::p_load_gh("jonocarroll/runkeepR")

## Usage

    setwd("~/runkeepR-test/") ## set directory to location of .gpx files

load the installed package

    library(runkeepR)
    
load and process the route information, converting information to a data.frame
    
    routes_pkg <- load_tracks(".")
    save(routes_pkg, file="saved_routes.rds") ## save the data to avoid re-processing

the data can be plotted either with `ggplot` 

    load("saved_routes.rds")
    plot_ggplot(routes_pkg, center="Adelaide, Australia", zoom=14)

<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/all_data_ggplot.png?height=600">

or `leaflet`

    load("saved_routes.rds")
    plot_leaflet(routes_pkg)

<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/all_data_leaflet.png?height=600">

Summary statistics can be viewed 

    summarise_runs(routes_pkg, dashboard=FALSE)

and presented in a `shinydashboard`

    summarise_runs(routes_pkg)

summarised either monthly 

<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/shiny_dashboard_monthly.png?height=600">

or daily

<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/shiny_dashboard_daily.png?height=600">

Not affiliated with Runkeeper(TM). Runkeeper(TM) logo © FitnessKeeper 2016
