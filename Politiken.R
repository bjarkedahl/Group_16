install.packages("rvest")
install.packages("xml2")
install.packages("plyr")
install.packages("dplyr")
install.packages("stringr")
library("rvest")
library ("xml2")
library("plyr")
library("dplyr")
library("stringr")

# This is the program collecting all articles from politiken from the 18. feb 2014 - 18. feb 2015
" it is made into smaller loops since it takes foreer to run. "
# Setting the dates to loop over
Period = seq(as.Date("2014-11-18"), as.Date("2015-01-01"), by="days")
Dates1 = format(Period, format = "%Y-%m-%d")

Period = seq(as.Date("2015-01-02"), as.Date("2015-03-01"), by="days")
Dates2 = format(Period, format = "%Y-%m-%d")

Period = seq(as.Date("2015-03-02"), as.Date("2015-05-01"), by="days")
Dates3 = format(Period, format = "%Y-%m-%d")

Period = seq(as.Date("2015-05-02"), as.Date("2015-07-01"), by="days")
Dates4 = format(Period, format = "%Y-%m-%d")

Period = seq(as.Date("2015-07-02"), as.Date("2015-09-01"), by="days")
Dates5 = format(Period, format = "%Y-%m-%d")

Period = seq(as.Date("2015-09-02"), as.Date("2015-11-18"), by="days")
Dates6 = format(Period, format = "%Y-%m-%d")

# Setting the main link to Politiken for each date
link_politiken1 = paste("http://politiken.dk/arkiv/?arkivdato=",Dates1, sep = "")
link_politiken2 = paste("http://politiken.dk/arkiv/?arkivdato=",Dates2, sep = "")
link_politiken3 = paste("http://politiken.dk/arkiv/?arkivdato=",Dates3, sep = "")
link_politiken4 = paste("http://politiken.dk/arkiv/?arkivdato=",Dates4, sep = "")
link_politiken5 = paste("http://politiken.dk/arkiv/?arkivdato=",Dates5, sep = "")
link_politiken6 = paste("http://politiken.dk/arkiv/?arkivdato=",Dates6, sep = "")

# Generating function collecting article links and dates
scrape_politiken = function(link_politiken) {
  link = read_html(link_politiken)
  p.link = link %>%
    html_nodes(".archive-bottom a") %>%
    html_attr(name = 'href')
  Date = link %>%
    html_nodes(".archive-date") %>%
    html_text()
  closeAllConnections()
return(cbind(p.link, Date))
}


# Generating function to collect Title, subtitle and text for each article
scrape_article = function(p.link) {
link_article = read_html(p.link)
Text = link_article %>%
  html_nodes("#art-body") %>%
  html_text() %>%
  paste(collapse = "")
Title = link_article %>%
  html_nodes(".rubrik") %>%
  html_text()
return(cbind(Title, Text))
}

# collecting all article links in a dataframe in 3 steps because it seems to crash if its to large
p.data1 = list() # initialize empty list
for (i in link_politiken1){
  print(paste("processing", i, sep = " "))
  p.data1[[i]] = scrape_politiken(i)
  # waiting one second between hits
  cat(" done!\n")
}


p.data2 = list() # initialize empty list
for (i in link_politiken2){
  print(paste("processing", i, sep = " "))
  p.data2[[i]] = scrape_politiken(i)
  # waiting one second between hits
  cat(" done!\n")
}


p.data3 = list() # initialize empty list
for (i in link_politiken3){
  print(paste("processing", i, sep = " "))
  p.data3[[i]] = scrape_politiken(i)
  # waiting one second between hits
  cat(" done!\n")
}

p.data4 = list() # initialize empty list
for (i in link_politiken4){
  print(paste("processing", i, sep = " "))
  p.data4[[i]] = scrape_politiken(i)
  # waiting one second between hits
  cat(" done!\n")
}


p.data5 = list() # initialize empty list
for (i in link_politiken5){
  print(paste("processing", i, sep = " "))
  p.data5[[i]] = scrape_politiken(i)
  # waiting one second between hits
  cat(" done!\n")
}



p.data6 = list() # initialize empty list
for (i in link_politiken6){
  print(paste("processing", i, sep = " "))
  p.data6[[i]] = scrape_politiken(i)
  # waiting one second between hits
  cat(" done!\n")
}



df1=ldply(p.data1, data.frame)
df2=ldply(p.data2, data.frame)
df3=ldply(p.data3, data.frame)
df4=ldply(p.data4, data.frame)
df5=ldply(p.data5, data.frame)
df6=ldply(p.data6, data.frame)



# Collecting information on all articles in a dataframe

## DF 1
p.article1 = list()
for (i in df1$p.link[1:2000]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article1[[i]] = article.i
  cat(" done!\n") 
}
save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df1$p.link[2001:4772]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article1[[i]] = article.i
  cat(" done!\n") 
}

df_article1=ldply(p.article1, data.frame)

save.image("X:/MikkelM/SDS/R/politiken.RData")

## DF 2
-+

for (i in df2$p.link[2001:4000]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article2[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df2$p.link[4001:5500]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article2[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df2$p.link[5501:6292]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article2[[i]] = article.i
  cat(" done!\n") 
}

df_article2=ldply(p.article2, data.frame)
save.image("X:/MikkelM/SDS/R/politiken.RData")



## DF3
p.article3 = list()
for (i in df3$p.link) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article3[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df3$p.link) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article3[[i]] = article.i
  cat(" done!\n") 
}
save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df3$p.link) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article3[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df3$p.link[6001:6393]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article3[[i]] = article.i
  cat(" done!\n") 
}


df_article3=ldply(p.article3, data.frame)
save.image("X:/MikkelM/SDS/R/politiken.RData")

## DF4
p.article4 = list()
for (i in df4$p.link) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article4[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df4$p.link) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article4[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df4$p.link[5001:5277]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article4[[i]] = article.i
  cat(" done!\n") 
}


df_article4=ldply(p.article4, data.frame)
save.image("X:/MikkelM/SDS/R/politiken.RData")

## DF 5 
p.article5 = list()
for (i in df5$p.link[701:3500]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article5[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df5$p.link[1701:3500]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article5[[i]] = article.i
  cat(" done!\n") 
}
save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df5$p.link[3501:4940]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article5[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

df_article5=ldply(p.article5, data.frame)

## DF 6
p.article6 = list()
for (i in df6$p.link[1:1700]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article6[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df6$p.link[1701:3500]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article6[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df6$p.link[3501:5000]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article6[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df6$p.link[5001:6200]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article6[[i]] = article.i
  cat(" done!\n") 
}

save.image("X:/MikkelM/SDS/R/politiken.RData")

for (i in df6$p.link[6200:7454]) {
  print(paste("processing", i, sep = " "))
  article.i = try(scrape_article(i), silent = TRUE)
  if (class(article.i) =="try-error")
    article.i = data.frame()
  p.article6[[i]] = article.i
  cat(" done!\n") 
}



df_article6=ldply(p.article6, data.frame)
save.image("X:/MikkelM/SDS/R/politiken.RData")


politiken = rbind(df_article1, df_article2, df_article3, df_article4, df_article5, df_article6)
df = rbind(df1, df2, df3, df4, df5, df6)

# Merging the 2 dataframes and cleaning them
politiken$p.link = politiken$.id
df$p.link = as.character(df$p.link)
politiken = left_join(df, politiken, by = "p.link")
drops <- c(".id.x",".id.y")
politiken = politiken[,!(names(politiken) %in% drops)]

# Generating Variable for media type
politiken$medie = "politiken"

# Generating variable for section
politiken$section = str_extract(politiken$p.link, ".dk/[a-z]*")
politiken$section = gsub(".dk/", "", politiken$section)


# Replacing characters from danish that R doesn't understand with real letters
politiken$Title = gsub("Ã¦", "æ", politiken$Title)
politiken$Title = gsub("Ã???", "Æ", politiken$Title)
politiken$Title = gsub("Ã¸", "ø", politiken$Title)
politiken$Title = gsub("Ã~", "Ø", politiken$Title)
politiken$Title = gsub("Ã¥", "å", politiken$Title)
politiken$Title = gsub("Ã.", "Å", politiken$Title)
politiken$Title = gsub("Ã©", "é", politiken$Title)
politiken$Title = gsub("Ã^", "É", politiken$Title)
politiken$Title = gsub("â???", "-", politiken$Title)
politiken$Title = gsub("-T", "'", politiken$Title)

politiken$Text = gsub("Ã¦", "æ", politiken$Text)
politiken$Text = gsub("Ã???", "Æ", politiken$Text)
politiken$Text = gsub("Ã¸", "ø", politiken$Text)
politiken$Text = gsub("Ã~", "Ø", politiken$Text)
politiken$Text = gsub("Ã¥", "å", politiken$Text)
politiken$Text = gsub("Ã.", "Å", politiken$Text)
politiken$Text = gsub("Ã©", "é", politiken$Text)
politiken$Text = gsub("Ã^", "É", politiken$Text)
politiken$Text = gsub("â???", "-", politiken$Text)
politiken$Text = gsub("-T", "'", politiken$Text)


politiken_1 = unique(politiken[, ])



save.image("X://MikkelM//SDS//R//Group_16//politiken.RData")
write.csv(politiken, file = "X:\\MikkelM\\SDS\\R\\Group_16\\politiken_data.csv")



