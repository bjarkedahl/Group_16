library(plyr)
library(dplyr)
library(MASS)
library(rvest)

# Plotting the distribution of likes, to see if there is a centering around zero
plot(table(DR_pol_sub$likes_count))

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


# Changing the country names to english for mapping, doesnt work. think we need to get it from elsewhere
link = "https://da.wikipedia.org/w/index.php?title=ISO_3166-1&action=edit&section=1"

land = read_html(link) %>% 
  html_nodes(#wpTextbox1) %>% 
  html_text()
iso3166 = read_html(link) %>% 
  html_nodes(td+ td .new) %>% 
  html_text()
cbind(land)
lande.df1 = ldply(lande.data, data.frame)
lande.df1$land = tolower(as.character(lande.df1$X..i..))
