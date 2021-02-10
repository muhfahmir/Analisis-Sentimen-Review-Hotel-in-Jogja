library(rvest)
baseUrl = "https://www.tripadvisor.com"
setwd("D:/FAHMI/sem5/praktikum data science/project/last project/data")
hotels = read.csv('yogyakarta.csv',stringsAsFactors = T)
classificationScore <- function(score){
  if("bubble_50" == score){
    score = 5
    #return(5)
  }else if("bubble_40" == score){
    score = 4
    #return(4)
  }else if("bubble_30" == score){
    score = 3
    #return(3)
  }else if("bubble_20" == score){
    score = 2
    #return(2)
  }else if("bubble_10" == score){
    score = 1
    #return(1)
  }
}

get_hotel_reviews <- function(url,hotel_name) {
  #deklarasi data frame
  reviews <- character()
  reviewers <- character()
  scores <- numeric()
  types <- character()
  stays <- character()
  
  url = paste(baseUrl, url, sep="")
  
  reviewPage <- read_html(url)
  #reviewPage
  
  #comment review
  review <- reviewPage %>%
    html_nodes('.IRsGHoPm') %>%
    html_text()
  
  #data nama
  reviewer <- reviewPage %>%
    html_nodes('._1r_My98y') %>%
    html_text()
  
  #data Score
  score <- reviewPage %>% 
    html_nodes('._2UEC-y30 .nf9vGX55 .ui_bubble_rating') %>% 
    html_attr('class')
  score <- gsub('ui_bubble_rating ','',score)
  score <- sapply(score, classificationScore)%>%
    as.numeric()
  
  
  #data type
  type_trav <- reviewPage %>% 
    html_nodes('._2bVY3aT5') %>% 
    html_text()
  type_trav <- gsub('Trip type: ','',type_trav)
  
  #data stay
  stay <- reviewPage %>%
    html_nodes('._34Xs-BQm') %>%
    html_text()
  stay <- gsub('Date of stay: ','',stay)
  
  
  #data framenya
  reviews <- c(reviews, review)
  reviewers <- c(reviewers, reviewer)
  scores <- c(scores,score)
  types <- c(types,type_trav)
  stays <- c(stays,stay)
  
  #next page data 
  nextPage <- reviewPage %>%
    html_nodes('._16gKMTFp .next') %>%
    html_attr('href')
  
  #mengecheck next page hrefnya ada atau kosong
  if(is_empty(nextPage)) {
    nextPage = NA
  }
  
  while (!is.na(nextPage)) {
    print(paste(length(reviews), "data", "collected"))
    
    reviewUrl <- paste(baseUrl, nextPage, sep = "")
    #print(reviewUrl)
    
    reviewPage <- read_html(reviewUrl)
    
    #comment review
    review <- reviewPage %>%
      html_nodes('.IRsGHoPm') %>%
      html_text()
    
    #data nama
    reviewer <- reviewPage %>%
      html_nodes('._1r_My98y') %>%
      html_text()
    
    #data Score
    score <- reviewPage %>% 
      html_nodes('._2UEC-y30 .nf9vGX55 .ui_bubble_rating') %>% 
      html_attr('class')
    score <- gsub('ui_bubble_rating ','',score)
    score <- sapply(score, classificationScore)%>%
      as.numeric()
    
    #data type
    type_trav <- reviewPage %>% 
      html_nodes('._2bVY3aT5') %>% 
      html_text()
    type_trav <- gsub('Trip type: ','',type_trav)
    
    #data stay
    stay <- reviewPage %>%
      html_nodes('._34Xs-BQm') %>%
      html_text()
    stay <- gsub('Date of stay: ','',stay)
    
    #data framenya
    reviews <- c(reviews, review)
    reviewers <- c(reviewers, reviewer)
    scores <- c(scores,score)
    types <- c(types,type_trav)
    stays <- c(stays,stay)
    
    #mengecheck data apakah kosong atau tidak
    nextPage <- reviewPage %>%
      html_nodes('._16gKMTFp .next') %>%
      html_attr('href')
    
    #mengecheck next page hrefnya ada atau kosong
    if(is_empty(nextPage)) {
      nextPage = NA
    }
  }
  #reviewPariwisata = data.frame(reviewer = reviewers, review = reviews,score = score,stringsAsFactors = FALSE)[1 : size,]
  #write.csv(reviewPariwisata, "Review Pariwisata.csv",row.names = F)
  print(paste(length(reviews), "data", "collected"))
  
  return(data.frame(reviewer = reviewers, review = reviews,score = scores, type = types, stay = stays, stringsAsFactors = FALSE)[1 : size,])
}

hotel1 = get_hotel_reviews(hotels$link[1],hotels$name[1])


