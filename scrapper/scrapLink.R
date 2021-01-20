#library global
library(rvest)
library(xml2)
library(purr)
library(textclean)
library(tokenizers)
library(wordcloud)
library(corpus)
library(dplyr)
library(tm)

baseUrl = "https://www.tripadvisor.com"
hotelUrl = "/Hotels-g14782503-Yogyakarta_Yogyakarta_Region_Java-Hotels.html"
#menjadikan satu base url dengan hotelUrl untuk mencari hotel diyogyakarta
url = paste(baseUrl, hotelUrl, sep="")
webPage = read_html(url)


hotelName = webPage %>% 
  html_nodes('.prominent') %>% 
  html_text()

hotelLink = webPage %>%
  html_nodes('.prominent') %>% 
  html_attr('href')

dfHotel = data.frame(name = hotelName, link = hotelLink, stringsAsFactors = F)
View(dfHotel)

#set working directory untuk simpan data
setwd("D:/FAHMI/sem5/praktikum data science/project/last project/data")
#simpan data 
saveRDS(dfHotel, "yogyakarta.rds")
write.csv(dfHotel, "yogyakarta.csv", row.names = F)

#membaca data
doc = read.csv("yogyakarta.csv", stringsAsFactors = T)

corpusName = Corpus(VectorSource(doc$name))
inspect(corpusName[1:10])

#cleaning data name
cleanName = function(name){
  sub("^\\s+", "", name)
}
name_clean = tm_map(corpusName, content_transformer(cleanName))
name_clean = tm_map(corpusName, content_transformer(removeNumbers))
name_clean = tm_map(corpusName, content_transformer(removePunctuation))
name_clean = tm_map(corpusName, content_transformer(stripWhitespace))

dfHotel = data.frame(name=unlist(sapply(name_clean,`[`)), link = hotelLink, stringsAsFactors = F)
setwd("D:/FAHMI/sem5/praktikum data science/project/last project/data")
saveRDS(dfHotel, "hotel.rds")
