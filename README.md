[![Linux/Mac Travis Build Status](https://img.shields.io/travis/jonocarroll/runkeepR/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/jonocarroll/runkeepR)
[![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jonocarroll/runkeepR/master.svg?label=Windows)](https://ci.appveyor.com/project/jonocarroll/runkeepR)
[![Coverage Status](https://codecov.io/gh/jonocarroll/runkeepR/branch/master/graph/badge.svg)](https://codecov.io/gh/jonocarroll/runkeepR)
[![GitHub forks](https://img.shields.io/github/forks/jonocarroll/runkeepR.svg)](https://github.com/jonocarroll/runkeepR/network)
[![GitHub stars](https://img.shields.io/github/stars/jonocarroll/runkeepR.svg)](https://github.com/jonocarroll/runkeepR/stargazers)
[![Twitter](https://img.shields.io/twitter/url/https/github.com/jonocarroll/runkeepR.svg?style=social)](https://twitter.com/intent/tweet?text=Wow:&url=%5Bobject%20Object%5D)

# <img src="https://d2b4ufapzmnxpw.cloudfront.net/build/13556/static/web/images/rk_app_logo_blue_150x142.png"> runkeepR

Extract, plot, and analyse Runkeeper(TM) data.

## Installation:

Assuming the TravisCI badges above are green (_i.e._ the current build is stable) this package can be installed via

    devtools::install_github("jonocarroll/runkeepR")
    
or

    pacman::p_load_gh("jonocarroll/runkeepR")

## Usage

load the installed package

    library(runkeepR)
    
You can get a zipped export of your Runkeeper(TM) data from the [logged-in settings page on Runkeeper's website](https://runkeeper.com/exportDataForm), _e.g._ `runkeeper-data-export-12517482-2016-05-20-1550.zip`.
    
<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/runkeeper_export.png?height=600">
    
Save the `.zip` file to a directory (e.g. `~/runkeepR-test/`) and unzip the contents (mainly `.gpx` files and a couple of `.csv` files). Set this directory as your working directory in `R`.

    setwd("~/runkeepR-test/") ## set directory to location of .gpx files

Loading and processing the route information contained in the `.gpx` and `.csv` files into a data.frame is as simple as
    
    routes <- load_tracks(".")
    save(routes, file="saved_routes.rds") ## save the data to avoid re-processing

The data can be plotted either with `ggplot` 

    load("saved_routes.rds")
    plot_ggplot(routes, center="Adelaide, Australia", zoom=14)

<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/all_data_ggplot.png?height=600">

or `leaflet`; this plots all paths but is clever about which ones to load depending on the current viewport, so it's faster. 

    load("saved_routes.rds")
    plot_leaflet(routes)

<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/all_data_leaflet.png?height=600">
<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/all_data_leaflet_home.png?height=600">

Summary statistics can be viewed 

    summarise_runs(routes, dashboard=FALSE)

and presented in a `shinydashboard`

    summarise_runs(routes)

summarised either monthly 

<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/shiny_dashboard_monthly.png?height=600">

or daily

<img src="https://github.com/jonocarroll/runkeepR/blob/master/img/shiny_dashboard_daily.png?height=600">

Not affiliated with Runkeeper(TM). Runkeeper(TM) logo © FitnessKeeper 2016
