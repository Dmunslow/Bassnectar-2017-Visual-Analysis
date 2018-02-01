######################
##
##  Festival Set Similarity Comparison
##
######################

library(ggplot2)
library(data.table)
library(xlsx)
library(ggthemes)

## NOTE:  This script documents analysis in similarity between festival sets,
##        in an attempt to see which sets had the most overlap in songs played.
##

## Read in Data
bass_data_2017 <- read.xlsx("./Tidy Song Data - Updated - LAN - Deca - NYE.xlsx", 
                            sheetIndex = 1)

## convert name of show/song to character
bass_data_2017$show <- as.character(bass_data_2017$show)
bass_data_2017$song <- as.character(bass_data_2017$song)

## create summary data frame, with festival set names, date
festival_set_data <- setDT(bass_data_2017)[show_type == "Festival",
                                           .(num_songs_played = .N),
                                           by = .(show, show_date)]

## create festival ID for outer for loop
festival_set_data$show_id <- 1:length(festival_set_data$show)

## Create empty rows values for most similar show and number of songs shared
festival_set_data$most_similar_show <- 'NA'
festival_set_data$songs_in_common_most_similar <- 0

festival_set_data$least_similar_show <- 'NA'
festival_set_data$songs_in_common_least_similar <- 100

## initiate for loop

for(i in 1:length(festival_set_data$show)){
    
    ## Store show name for primary show for later use
    primary_show <- festival_set_data$show[festival_set_data$show_id == i]
    
    ## Create song list for show i
    song_list_primary <-  bass_data_2017$song[bass_data_2017$show == festival_set_data$show[festival_set_data$show_id ==i]]
    
    ## Create comparison data frame of shows not including i
    comparison_df <- festival_set_data[festival_set_data$show_id!=i,]
    
    # Create comparison ID variable for inner for loop
    comparison_df$comparison_id <- 1:length(comparison_df$show)
    

    # initiate 2nd for loop to compare common elements of lists
    for(j in 1:length(comparison_df$show)){
        
        comparison_show <- comparison_df$show[comparison_df$comparison_id == j]
        
        comparison_song_list <- bass_data_2017$song[bass_data_2017$show == comparison_df$show[comparison_df$comparison_id == j]]
        
        num_songs_in_common <- length(song_list_primary[song_list_primary %in% comparison_song_list])
        
        if(num_songs_in_common > festival_set_data$songs_in_common_most_similar[festival_set_data$show_id == i]){
            
            ## if the number of songs is greater than the previous max, then replace with new show info
            festival_set_data$songs_in_common_most_similar[i] <- num_songs_in_common
            festival_set_data$most_similar_show[i] <- comparison_show
        }
        
        if(num_songs_in_common < festival_set_data$songs_in_common_least_similar[festival_set_data$show_id == i]){
            
            ## if the number of songs is greater than the previous max, then replace with new show info
            festival_set_data$songs_in_common_least_similar[i] <- num_songs_in_common
            festival_set_data$least_similar_show[i] <- comparison_show
        } 
       
     
        
    }
}


