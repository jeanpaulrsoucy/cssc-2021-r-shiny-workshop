# What makes R Shiny so shiny? A step-by-step introduction to interactive dashboards in R

This workshop was presented at the virtual [Ninth Annual Canadian Statistics Student Conference](https://ssc.ca/en/meetings/annual/2021-annual-meeting/student-conference) on June 5, 2021.

Slides are provided in the file `cssc-2021-r-shiny-workshop-slides.pdf`.

## Purpose

The purpose of this workshop is to introduce the basics of creating interactive visualizations and dashboards in [R Shiny](https://shiny.rstudio.com/).

The core concepts of Shiny are demonstrated in a series of dashboards themed around horror films using data provided by IMDb.

## Data

The raw datasets used in this workshop are available from the Internet Movie Database (IMDb) [website](https://www.imdb.com/interfaces/). These data are available for personal and non-commercial use (see [website](https://www.imdb.com/interfaces/) for more details).

Pre-processed data (downloaded June 5, 2021) are provided in the `data` directory.

## Running the scripts

First, update your packages and install all required packages by running `0_required_packages.R`.

Scripts 1 (`1_download-data.R`) and 2 (`2_process_data.R`) are optional, as the necessary data are already provided in the `data` directory. Only run these scripts if you want the latest data from IMDB (note that these are very large files, so the scripts may take a long time to run).

Script 3 through 7 are self-contained R Shiny dashboards. Each is more complex than the last.
