# ===================================================================
# Title: Exploratory Data Analysis of IBTRACS Storm Data
# Description:
#   This script computes descriptive statistics regarding various
#   points of interest across storm records between the years of 2010
#   and 2015.
# Input(s): data file 'ibtracs-2010-2015.csv'
# Output(s): summary data files, and plots
# Author: Wyatt Hollister
# Date: 10-16-2019
# ===================================================================

library(readr)

col_names <- c("serial_num", "season", "num", "basin", "sub_basin", "name", 
               "iso_time", "nature", "latitude", "longitude", "wind", 
               "press")

col_types <- c("character", "integer", "character", "factor", "character",
               "character", "character", "character", "real", "real", "real",
               "real")

ibtracs <- read.csv(file = '../data/ibtracs-2010-2015.csv', sep = ",", col.names = col_names, colClasses = col_types, na.strings = c("-999.", "-1.0", "0.0"))

sink(file = '../output/data-summary.txt')
summary(ibtracs)
sink()


### GRAPHS ###

library(ggplot2)
library(dplyr)
library(lubridate)

plot1 <- ggplot(data = ibtracs) +
  geom_smooth(aes(latitude, longitude, size = nature), alpha = 0.2) + 
  xlim(-50, 50) + ylim(-200, 200)

plot(plot1)


plot2 <- ggplot() +
  ggtitle("Storm Paths in the Eastern Pacific and North Atlantic Basins by Year") +
        geom_smooth(data = filter(ibtracs, as.integer(basin) == 1), aes(latitude, longitude, color = 'red')) +
        geom_smooth(data = filter(ibtracs, as.integer(basin) == 2), aes(latitude, longitude, color = 'blue')) +
        scale_color_manual(labels = c("EP", "NA"), values = c('red', 'blue')) +
        facet_wrap(~ season)
        


ibtracs$month <- month(ibtracs$iso_time, label = TRUE)

plot3 <- ggplot() +
  ggtitle("Storm Paths in the Eastern Pacific and North Atlantic Basins by Month") +
  geom_point(data = filter(ibtracs, as.integer(basin) == 1), aes(latitude, longitude, color = 'red')) +
  geom_point(data = filter(ibtracs, as.integer(basin) == 2), aes(latitude, longitude, color = 'blue')) +
  scale_color_manual(labels = c("EP", "NA"), values = c('red', 'blue')) +
  facet_grid(~ month)

storm_size <- ggplot() +
  ggtitle("Storm Sizes Relative to Wind Speed") +
  geom_histogram(data = ibtracs, aes(wind, size = wind)) +
  facet_wrap( ~month)

storm_intensity_frequency <- ggplot() +
  ggtitle("Storm Intensity Relative to Basin (Wind Speed") +
  geom_histogram(data = ibtracs, aes(wind), fill = "green") +
  facet_wrap(~ basin)


storm_intensity_press <- ggplot() +
  ggtitle("Storm Intensity Relative to Basin (Pressure)") +
  geom_histogram(data = ibtracs, aes(press), fill = "red") +
  facet_wrap(~ basin)

