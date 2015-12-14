library(plyr)
library(dplyr)
library(MASS)
library(rvest)
library("stringr")
library("ggmap")
library("countrycode")

# Plotting the distribution of likes, to see if there is a centering around zero
plot(table(DR_pol_all$likes_count))

#Countdata model
fm_count = glm(likes_count~section, data = DR_pol_sub, family=poisson)
summary(fm_count)

fm_countnb = glm.nb(likes_count~section, data = DR_pol_sub)
summary(fm_countnb)


## We want to map the distributions of articles ##
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
# Megring sp we have ISO codes
lande_iso = left_join(artikler_land, iso, by = "land")
lande_iso$number_of_articles = lande_iso$X..i..
lande_iso$iso = lande_iso$Alfa.2

#Collecting map material
map.df = map_data("world")
map.df$iso = countrycode(map.df$region, origin = "country.name", destination = "iso2c")

df_map = right_join(lande_iso, map.df, by = "iso")

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
p = p + labs(title = "Number of articles from each country")
plot(p)

# Next we only want to plot them if they are on Facebook
facebook_article = subset(DR_pol_all, DR_pol_all$likes_count!="NA")

t3 = c(as.character(facebook_article$text))

facebook.data = list() # initialize empty list
for (i in lande.df$land){
  print(paste("processing", i, sep = " "))
  facebook.data[[i]] = length(grep(i,t3))
  cat(" done!\n")
}

# Gathering number of articles for each country
facebook_land = ldply(facebook.data, data.frame)
facebook_land$land = facebook_land$.id
facebook_iso = left_join(facebook_land, iso, by = "land")

facebook_iso$number_of_articles = facebook_iso$X..i..
facebook_iso$iso = lande_iso$Alfa.2

face_map = right_join(facebook_iso, map.df, by = "iso")

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
p = p + labs(title = "Number of articles from each country on Facebook")
plot(p)

