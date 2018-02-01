######################
##
##  Data Visualizations
##
######################

## NOTE:  This script uses data that was originally sourced on the /r/Bassnectar
##        subreddit, provided by user /u/Musiik (link below).  I personally added
##        data for Lights All Night, Decadence and NYE, using the setlists on the 
##        bassnectar subreddit. I also enriched the data with the following variables:
##                -- song type - (original, remix, OPM - other peoples music)
##                -- album - (for bassnectar orginals) song was realeased on - (did my best here, I'm sure I missed some)
##                -- original_artist - OPM and remixes 
##                -- Show_type - Festival or Special Event
##                -- Show_Date - Date of the show for a given setlist
##
##        BIG Shout out to all the dedicated, crazy
##        knowledgeable bassheads that put up the setlists to make this possible.

## (https://www.reddit.com/r/bassnectar/comments/7l3r2g/ever_wonder_what_songs_bassnectar_played_live/)

library(ggplot2)
library(data.table)
library(xlsx)
library(ggthemes)

## read in data
bass_data_2017 <- read.xlsx("./Tidy Song Data - Updated - LAN - Deca - NYE.xlsx", 
                            sheetIndex = 1)

## convert name of show to character
bass_data_2017$show <- as.character(bass_data_2017$show)


#################### Overview

# 277 unique songs played all year
total_songs <- unique(bass_data_2017$song)


songs_played_by_type <- setDT(bass_data_2017)[, .(num_songs = .N) , by = type]

## Number of songs played by type
gg_spbt <- ggplot(data = songs_played_by_type, aes(x = type, y = num_songs)) +
            geom_bar(aes(fill = type), stat = "identity") + theme_dark() + 
            labs(title = "Bassnectar 2017 - Songs Played By Song Type",
                 x = "",
                 y = "Times Played",
                 fill = "Song Type\n") +
            theme(text = element_text(size = 18),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())

ggsave("./Finalized Plots/Songs Played by type.png", gg_spbt)

######

# Create a variable for total number of times a song was played
song_data_2017 <- setDT(bass_data_2017)[, total_times_played := .N, by = song]

## Create variable for number of times song was played by show type
song_data_2017 <- song_data_2017[, .(times_played_by_show_type = .N), 
                                 by = .(song, type, show_type, total_times_played, original_artist)][order(-total_times_played, song)]

##  Top 10 most played songs

top_10_songs <- song_data_2017[1:20,]

top_10_songs$song <- factor(top_10_songs$song, levels = c("Underground",
                                                          "Pineapple",
                                                          "Get deaded",
                                                          "Generate",
                                                          "Light",
                                                          "Elephant Party",
                                                          "Wobbly",
                                                          "I'm up",
                                                          "Didgeridoo",
                                                          "Interlock"))


gg_tts <- ggplot(data = top_10_songs, aes(x = song, y = times_played_by_show_type))+ 
                geom_bar(aes(fill = show_type), stat = "identity") +
                labs(title = "Bassnectar 2017 - Top 10 Most Played Songs - By Show Type",
                     x = "",
                     y = "",
                     fill = "Show Type\n") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                theme(text = element_text(size = 18))
gg_tts 


## BN originals by album

songs_by_album <- setDT(bass_data_2017)[!is.na(album), 
                                        .(num_times_song_played = .N ), 
                                        by = .(album, show_type)][order(-show_type, -num_times_song_played)]



## get list of albums in order of most played at special events for factor variable
songs_by_album$album <- as.character(songs_by_album$album)
ordered_album_list <- songs_by_album$album[songs_by_album$show_type == "Special_Event"]

## remove NA, ID, Unreleased
songs_by_album <- songs_by_album[!(songs_by_album$album %in% c("ID", "Unreleased")),]
songs_by_album <- songs_by_album[!is.na(songs_by_album$album),]

#convert back to factor, in right order
songs_by_album$album <- factor(songs_by_album$album, levels = ordered_album_list)

gg_sba <- ggplot(data = songs_by_album, aes(x = album, y = num_times_song_played))


gg_sba + geom_bar(aes(fill = show_type), stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Song Type data by show

song_type_by_show <- setDT(bass_data_2017)[, total_songs := .N, by = .(show, show_type,show_date)]
song_type_by_show <- song_type_by_show[, total_times_played := .N, by = song]



## convert "show" variable to character 
song_type_by_show$show <- as.character(song_type_by_show$show)

song_type_by_show <- song_type_by_show[, .(num_of_songs = .N),
                                           by = .(song, show, show_type,show_date,type, total_songs, total_times_played)][order(show_date)]

# Create list of shows in order of occurance
shows <- unique(song_type_by_show$show)

## Create factor variables in desired order
song_type_by_show$type <- factor(song_type_by_show$type, levels = c("OPM","Remix", "Original"))
song_type_by_show$show <- factor(song_type_by_show$show, levels = shows)

## All songs - stacked bar plot with song type as fill
gg_stbs <- ggplot(data = song_type_by_show, aes(x = show, y = num_of_songs)) 

gg_stbs + geom_bar(aes(fill = type), stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))


## One Offs

one_offs <- song_type_by_show[total_times_played == 1,]

one_off_by_type <- one_offs[, .(num_songs = .N), by = type]


gg_oofbt <- ggplot(data = one_offs, aes(x = type, y = num_songs))

gg_oofbt + geom_bar(aes(fill = type), stat = "identity")

## order factor variables 
one_offs$type <- factor(one_offs$type, levels = c("OPM","Remix", "Original"))
one_offs$show <- factor(one_offs$show, levels = shows)


gg_ofs <- ggplot(data = one_offs, aes(x = show, y = total_times_played))

gg_ofs + geom_bar(aes(fill = type), stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))




