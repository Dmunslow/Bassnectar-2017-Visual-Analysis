######################
##
##  Data Preparation
##
######################


## Note : This script preps the original data set obtained from reddit user /u/Musiik
##        into a long-form, tidy data set.          

library(readr)
library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
library(xlsx)

song_data <- read_csvq("./Bass Data R Ready.csv")

show_data <- read.csv("./Show Info.csv")

#convert Show_date column to proper format
show_data$Date <- as.character(show_data$Date)
show_data$Date <- as.Date(show_data$Date,  format="%m/%d/%Y")


# Gather columns to create tidy data set and eliminate NAs
song_data_melt <- melt(song_data, 
                       id.vars = c("Song",
                                   "Times played", 
                                   "Album", 
                                   "Type", 
                                   "Original Artist"), 
                       measure.vars = c("Location1",
                                        "Location2",
                                        "Location3",
                                        "Location4",
                                        "Location5",
                                        "Location6",
                                        "Location7",
                                        "Location8",
                                        "Location9",
                                        "Location10",
                                        "Location11",
                                        "Location12",
                                        "Location13",
                                        "Location14"))
## drop Location column and NA values for songs that were not played more than X times
song_data_tidy <- song_data_melt[!is.na(song_data_melt$value), -6]

## Replace "N/A" with NA values in Album column for R purposes
song_data_tidy$Album <- ifelse(song_data_tidy$Album == "N/A", NA, song_data_tidy$Album)

## Create show_type, and show_date columns by matching with show data
song_data_tidy$show_type <- show_data[match(song_data_tidy$value, show_data$ï..Show), 3]

song_data_tidy$show_date <- show_data[match(song_data_tidy$value, show_data$ï..Show), 2]

colnames(song_data_tidy) <- c("song", 
                              "times_played", 
                              "album", 
                              "type", 
                              "original_artist", 
                              "show", 
                              "show_type", 
                              "show_date")


###  Save Data to RDS and CSV

saveRDS(song_data_tidy, "./Tidy Song Data - Original Data set.RDS")
write.xlsx(song_data_tidy, "./Tidy Song Data - Original Data set.xlsx", row.names = F)





