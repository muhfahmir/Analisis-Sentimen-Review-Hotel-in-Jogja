library(rvest)
baseUrl = "https://www.tripadvisor.com"

get_hotel_reviews <- function(url, size = -1, incProgress = NULL) {
    #deklarasi data frame
    reviews <- character()
    reviewers <- character()
    #scores <- numeric()
    
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
    
    #data framenya
    reviews <- c(reviews, review)
    reviewers <- c(reviewers, reviewer)
    #scores <- c(scores,score)
    
    #mengecheck data apakah kosong atau tidak
    if(!is.null(incProgress)) {
      incProgress(5/size) 
    }
    
    #next page data 
    nextPage <- reviewPage %>%
      html_nodes('._16gKMTFp .next') %>%
      html_attr('href')
    
    #mengecheck next page hrefnya ada atau kosong
    if(is_empty(nextPage)) {
      nextPage = NA
    }
    
    while (!is.na(nextPage) & (length(reviews) < size | size == -1)) {
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

      #data framenya
      reviews <- c(reviews, review)
      reviewers <- c(reviewers, reviewer)
      #scores <- c(scores,score)
      
      #mengecheck data apakah kosong atau tidak
      nextPage <- reviewPage %>%
        html_nodes('._16gKMTFp .next') %>%
        html_attr('href')
      
      #mengecheck next page hrefnya ada atau kosong
      if(is_empty(nextPage)) {
        nextPage = NA
      }
      
      if(!is.null(incProgress)) {
        incProgress(5/size) 
      }
    }
    
    totalReviews <- length(reviews)
    if(totalReviews < size || size == -1) {
      size = totalReviews
    }
    #reviewPariwisata = data.frame(reviewer = reviewers, review = reviews,score = score,stringsAsFactors = FALSE)[1 : size,]
    #write.csv(reviewPariwisata, "Review Pariwisata.csv",row.names = F)
    print(paste(length(reviews), "data", "collected"))
    
    #return(data.frame(reviewer = reviewers, review = reviews,score = scores, stringsAsFactors = FALSE)[1 : size,])
    return(data.frame(reviewer = reviewers, review = reviews, stringsAsFactors = FALSE)[1 : size,])
}
