library("rvest")
library ("xml2")
library("plyr")
library("dplyr")
library("stringr")

# Reading in data from 
politiken = data.frame(read.csv("X:\\MikkelM\\SDS\\R\\Group_16\\politiken_data.csv"))
politiken_facebook = data.frame(read.csv("X:\\MikkelM\\SDS\\R\\Group_16\\politiken10000.csv"))

# Rearranging data so it can be merged with  facebook data
politiken$link=politiken$p.link
politiken_facebook$link = as.character(politiken_facebook$link)
politiken=left_join(politiken, politiken_facebook, "link")

# Selecting the variables of interest
politiken = select(politiken, link, Title, Text, Date, section,
                   medie, likes_count, comments_count, shares_count)




### HELLO PRETTY LADY !!! 
save.image("X://MikkelM//SDS//R//Group_16//politiken_all.RData")