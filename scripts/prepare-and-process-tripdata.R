## PREPARE

# install and load packages
install.packages("tidyverse")
install.packages("geosphere")
install.packages("leaflet")
library(tidyverse)
library(geosphere)
library(leaflet)

# import the trip data for all 12 months
jan2024 <- read.csv("data/2024-tripdata-raw/202401-divvy-tripdata.csv")
feb2024 <- read.csv("data/2024-tripdata-raw/202402-divvy-tripdata.csv")
mar2024 <- read.csv("data/2024-tripdata-raw/202403-divvy-tripdata.csv")
apr2024 <- read.csv("data/2024-tripdata-raw/202404-divvy-tripdata.csv")
may2024 <- read.csv("data/2024-tripdata-raw/202405-divvy-tripdata.csv")
jun2024 <- read.csv("data/2024-tripdata-raw/202406-divvy-tripdata.csv")
jul2024 <- read.csv("data/2024-tripdata-raw/202407-divvy-tripdata.csv")
aug2024 <- read.csv("data/2024-tripdata-raw/202408-divvy-tripdata.csv")
sep2024 <- read.csv("data/2024-tripdata-raw/202409-divvy-tripdata.csv")
oct2024 <- read.csv("data/2024-tripdata-raw/202410-divvy-tripdata.csv")
nov2024 <- read.csv("data/2024-tripdata-raw/202411-divvy-tripdata.csv")
dec2024 <- read.csv("data/2024-tripdata-raw/202412-divvy-tripdata.csv")

# check column names for consistency
colnames(jan2024)
colnames(feb2024) == colnames(jan2024)
colnames(mar2024) == colnames(jan2024)
colnames(apr2024) == colnames(jan2024)
colnames(may2024) == colnames(jan2024)
colnames(jun2024) == colnames(jan2024)
colnames(jul2024) == colnames(jan2024)
colnames(aug2024) == colnames(jan2024)
colnames(sep2024) == colnames(jan2024)
colnames(oct2024) == colnames(jan2024)
colnames(nov2024) == colnames(jan2024)
colnames(dec2024) == colnames(jan2024)

# check column data types for consistency
sapply(jan2024, class)
sapply(feb2024, class) == sapply(jan2024, class)
sapply(mar2024, class) == sapply(jan2024, class)
sapply(apr2024, class) == sapply(jan2024, class)
sapply(may2024, class) == sapply(jan2024, class)
sapply(jun2024, class) == sapply(jan2024, class)
sapply(jul2024, class) == sapply(jan2024, class)
sapply(aug2024, class) == sapply(jan2024, class)
sapply(sep2024, class) == sapply(jan2024, class)
sapply(oct2024, class) == sapply(jan2024, class)
sapply(nov2024, class) == sapply(jan2024, class)
sapply(dec2024, class) == sapply(jan2024, class)

# merge the data frames into one data frame
tripdata <- bind_rows(jan2024, feb2024, mar2024, apr2024, may2024, jun2024,
                      jul2024, aug2024, sep2024, oct2024, nov2024, dec2024)

# remove data frames that are no longer needed
rm(jan2024, feb2024, mar2024, apr2024, may2024, jun2024,
   jul2024, aug2024, sep2024, oct2024, nov2024, dec2024)
gc()

## PROCESS

# check the merged data
head(tripdata)
str(tripdata)

# convert character columns to date-time columns
tripdata$started_at <- as.POSIXct(tripdata$started_at)
tripdata$ended_at <- as.POSIXct(tripdata$ended_at)

# create new columns from date-time
tripdata <- tripdata %>%
  mutate(
    year = year(started_at),
    month = month(started_at, label = TRUE),
    day = day(started_at),
    day_of_week = wday(started_at, label = TRUE),
    start_time = hour(started_at),
    trip_length = as.numeric(difftime(ended_at, started_at, units = "mins"))
  )

# create column for distance traveled in km
tripdata <- tripdata %>%
  mutate(
    trip_distance = distVincentySphere(
      cbind(start_lng, start_lat), cbind(end_lng, end_lat)) / 1000
  )

# clean by removing data where the trip length is 0 or negative and
# where the trip distance is NA
tripdata_clean <- tripdata %>%
  filter(trip_length > 0 & !is.na(trip_distance))

# check the cleaned data
str(tripdata_clean)

# remove the data frame that is no longer needed
rm(tripdata)
gc()
