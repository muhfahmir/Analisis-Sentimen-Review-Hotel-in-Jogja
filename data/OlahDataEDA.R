library(xml2)
library(ggplot2)

setwd("D:/FAHMI/sem5/praktikum data science/project/last project/data")
dataHotel = read.csv('dataEDAFix.csv')

types = dataHotel$type
types = ifelse(types == "Traveled with family","Families",
               ifelse(types == "Traveled as a couple","Couples",
                      ifelse(types == "Traveled solo", "Solo",
                             ifelse(types == "Traveled on business", "Business","Friends"))))

stays = dataHotel$stay
years = gsub('.* ','',stays)
months = gsub(' .*','',stays)

#sentiment
data = dataHotel %>%
  mutate (type=types,month = months, year = years)
glimpse(data)

#buat EDA
dataJenis = data %>%
  select(type)%>%
  count(type)

dataJenis %>%
  ggplot(aes(x = type, y = n,fill=type)) +
  geom_col() + 
  labs(
    x = "Jenis Pelanggan",
    y = "Jumlah",
    title = "Jenis Pelanggan yang menginap di Hotel Tentrem Yogyakarta",
    subtitle = "Jenis Pelanggan di Hotel Tentrem Yogyakarta 2012-2021",
    caption = "Sumber: Tripadvisor.com"
  ) +
  theme_light()

dataTahun = data %>%
  select(year)%>%
  count(year)

dataTahun %>%
  ggplot(aes(x = year, y = n , fill=year)) +
  geom_col() + 
  labs(
    x = "Tahun",
    y = "Jumlah",
    title = "Data Pengunjung Tiap tahun di Hotel Tentrem Yogyakarta",
    subtitle = "Data Pengunjung di Hotel Tentrem Yogyakarta 2012-2021",
    caption = "Sumber: Tripadvisor.com"
  ) +
  theme_light()

dataBulan = data %>%
  select(month)%>%
  count(month)

dataBulan %>%
  ggplot(aes(x = month, y = n , fill=month)) +
  geom_col() + 
  coord_flip() +
  labs(
    x = "Bulan",
    y = "Jumlah",
    title = "Data Pengunjung Tiap Bulan di Hotel Tentrem Yogyakarta",
    subtitle = "Data Pengunjung di Hotel Tentrem Yogyakarta 2012-2021",
    caption = "Sumber: Tripadvisor.com"
  ) +
  theme_light()

dataSentiment = data %>%
  select(sentiment)%>%
  count(sentiment)

chartSentiment = dataSentiment$n
sentiment = dataSentiment$sentiment
ibls = paste(sentiment)
pct = round(chartSentiment/sum(chartSentiment) *100)
ibls = paste(sentiment,pct)
ibls = paste(ibls,"%",sep = "")
pie(chartSentiment, labels = ibls, col = rainbow(length(ibls)),
    main = "Analisa Sentiment Review Hotel Tentrem Yogyakarta")

dataRating = data%>%
  select(score)%>%
  count(score)

dataRating %>%
  ggplot(aes(x = score, y = n , fill=score)) +
  geom_col() + 
  labs(
    x = "Rating",
    y = "Jumlah",
    title = "Rating Pengunjung di Hotel Tentrem Yogyakarta",
    subtitle = "Rating Pengunjung di Hotel Tentrem Yogyakarta 2012-2021",
    caption = "Sumber: Tripadvisor.com"
  ) +
  theme_light()

