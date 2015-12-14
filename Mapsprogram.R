library(plyr)
library(dplyr)
library(MASS)
library(rvest)
library("stringr")
library("ggmap")
library("countrycode")

## This program generates data to country graphs and average likes * distance ##
# We start by taking in data from all articles
load(url("https://github.com/bjarkedahl/Group_16/blob/master/DR%20and%20Politiken%20all.RData?raw=true"))
## Collecting all country names in one dataframe ##
css.selector = ".MOB"
link = "https://www.skolekom.dk/~geolog.821009@skolekom.dk/"

lande.data = read_html(link) %>% 
  html_nodes(css = css.selector) %>% 
  html_text()
cbind(lande.data)
lande.df = ldply(lande.data, data.frame)
lande.df$land = tolower(as.character(lande.df$X..i..))

# Changing speciel letters
lande.df$land= gsub("ã¦", "æ", lande.df$land)
lande.df$land= gsub("ã¸", "ø", lande.df$land)
lande.df$land= gsub("ã~", "ø", lande.df$land)

#Generating vector only containing the text strings
t1 = c(as.character(DR_pol_all$text))


# Collecting the number of articles where the different countries are named
p.data = list() # initialize empty list
for (i in lande.df$land){
  print(paste("processing", i, sep = " "))
  p.data[[i]] = length(grep(i,t1))
  cat(" done!\n")
}

# Gathering number of articles for each country
artikler_land = ldply(p.data, data.frame)

# Collecting ISO codes and merging them with countrynames for mapping
link = "https://da.wikipedia.org/wiki/ISO_3166-1"

land = read_html(link) %>% 
  html_nodes(".wikitable") %>% 
  html_table()
iso = ldply(land, data.frame)
iso$land = gsub("Â", "", iso$Land)
iso$land = str_trim(iso$land)
iso$land = tolower(as.character(iso$land))
iso$land= gsub("ã¦", "æ", iso$land)
iso$land= gsub("ã¸", "ø", iso$land)
iso$land= gsub("ã~", "ø", iso$land)
iso$land= gsub("ã~", "ø", iso$land)
iso$land= gsub("usa", "u.s.a", iso$land)

artikler_land$land = artikler_land$.id

# Megring so we have ISO codes for all countries
lande_iso = left_join(artikler_land, iso, by = "land")
lande_iso$number_of_articles = lande_iso$X..i..
lande_iso$iso = lande_iso$Alfa.2

#Collecting map data
map.df = map_data("world")
map.df$iso = countrycode(map.df$region, origin = "country.name", destination = "iso2c")

# Merging with Article Data
df_map = right_join(lande_iso, map.df, by = "iso")


# Next we only want to Collect number of articles on Facebook for each country in a Dataframe

# Selecting articles on Facebook
facebook_article = subset(DR_pol_all, DR_pol_all$likes_count!="NA")

t3 = c(as.character(facebook_article$text))

# Collecting number of articles each country is named in 
facebook.data = list() # initialize empty list
for (i in lande.df$land){
  print(paste("processing", i, sep = " "))
  facebook.data[[i]] = length(grep(i,t3))
  cat(" done!\n")
}

# Gathering number of articles for each country and merging with iso codes
facebook_land = ldply(facebook.data, data.frame)
facebook_land$land = facebook_land$.id
facebook_iso = left_join(facebook_land, iso, by = "land")

facebook_iso$number_of_articles = facebook_iso$X..i..
facebook_iso$iso = lande_iso$Alfa.2

face_map = right_join(facebook_iso, map.df, by = "iso")

## the Scatter plot
## Calculating Distance to Denmark
# First calculating average longitude and lattitude for each country
distance = ddply(face_map, .(region), summarize,  lat=mean(lat), long=mean(long))


#calculating longitude and latitude as radian  
distance$latrad <- (distance$lat*pi/180)
distance$longrad <- (distance$long*pi/180)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
distance$d <- acos(sin(0.972235562)*sin(distance$latrad) + cos(0.972235562)*cos(distance$latrad) * cos(distance$longrad-0.18723309)) * 6371

# Getting the article number for each article from each country
p.data = list() # initialize empty list
for (i in lande.df$land){
  print(paste("processing", i, sep = " "))
  p.data[[i]] = grep(i,t3)
  cat(" done!\n")
}

oversigt = ldply(p.data, data.frame)

# Getting the number of likes
oversigt$likes = facebook_article[oversigt$X..i..,7]
oversigt$land = oversigt$.id
distance$land = tolower(distance$region)

df = left_join(oversigt, distance, by = "land")
df = ddply(df,.(land), summarize, avg_like=mean(likes), distance = mean(d))

remove(artikler_land)
remove(distance, DR_pol_all)
remove(facebook_article, facebook_iso, facebook_land, iso, lande_iso, lande.df, map.df, oversigt)
remove(t1)
remove(t3,p.data,link,lande.data,land,i,facebook.data, css.selector)


############################### Plots ##################################################
# Plotting the number of articles from Politiken and DR in a Map
p = ggplot(df_map, aes(x = long, y = lat, group = group, fill = number_of_articles))
p = p + geom_polygon()
p = p + scale_fill_gradient(limits = c(0,7000))
p = p + theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
p = p + labs(title = "Article intensity by country")
plot(p)

arrange(artikler_land, -X..i..)


## Plotting the number of articles on Facebook
p = ggplot(face_map, aes(x = long, y = lat, group = group, fill = number_of_articles))
p = p + geom_polygon()
p = p + scale_fill_gradient(limits = c(0,7000))
p = p + theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
p = p + labs(title = "Article intensity by country on Facebook")
plot(p)

arrange(facebook_land, -X..i..)

# Plotting average number of likes as a function of distance to Denmark
plot(df$distance, df$avg_like, pch = 1, xlab = "Distance from DK, km", ylab = "Average number of likes", main = "Number of likes vs. distance")
     abline(lm(df$avg_like~df$distance), col="red")
